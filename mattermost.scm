#!/run/current-system/profile/bin/guile \
-e main --no-auto-compile -s
!#

;;; run as: guix shell guile guile-sqlite3 guile-json -- ./mattermost.scm 7777 hook-id [tokens]
;;; license: AGPL 3.0 or later

(use-modules (web client)
             (web request)
             (web response)
             (web server)
             (web uri)
             (ice-9 match)
             (ice-9 peg)
             (ice-9 regex)
             (ice-9 threads)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (rnrs bytevectors)
             (sqlite3)
             (system repl error-handling)
             (json))


;; This is set at launch
(define %tokens (list))
(define %hook #false)

;; Needed for non-predictable choosing of candidates, even after restarts.
(set! *random-state* (random-state-from-platform))


(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define* (render-json json #:optional (headers '()))
  (list (append '((content-type . (application/json))) headers)
        (lambda (port)
          (scm->json json port))))

(define (no-content)
  (list (build-response #:code 204) #false))

(define* (internal-error #:optional (msg "internal error"))
  (list (build-response #:code 500)
        msg))

(define (make-pair-list arg)
  (match arg
    (() '())
    ((single)
     (error "Broken arguments count for pair list"))
    ((name value . args)
     (cons (cons name value)
           (make-pair-list args)))))

(define (with-atomic-file-output file proc)
  "Call PROC with an output port for the file that is going to replace FILE.
Upon success, FILE is atomically replaced by what has been written to the
output port, and PROC's result is returned."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template)))
    (with-throw-handler #t
      (lambda ()
        (let ((result (proc out)))
          (force-output out)
          (close-port out)
          (rename-file template file)
          result))
      (lambda (key . args)
        (false-if-exception (delete-file template))
        (close-port out)))))


(define* (post-message #:key channel text)
  (catch #t
   (lambda ()
    (http-post %hook
     #:headers '((content-type . (application/json)))
     #:body
     (scm->json-string
      `(("channel" . ,channel)
        ("text" .
         ,text)))))
   (lambda exception
     (display exception)
     (display (newline))
     (format (current-error-port)
             "failed to post message to channel `~a': ~a"
             channel text))))


(define* (sqlite-exec* db sql . args)
  (let ((stmt (sqlite-prepare db sql)))
    (apply sqlite-bind-arguments stmt args)
    (let ((result (sqlite-map identity stmt)))
      (sqlite-finalize stmt)
      result)))

(define %db-name "mattermost.db")
(define %db #false)
(define (init-db!)
  (set! %db (sqlite-open %db-name
                         (logior SQLITE_OPEN_CREATE
                                 SQLITE_OPEN_READWRITE)))
  (sqlite-exec %db
               "\
CREATE TABLE IF NOT EXISTS reminders (
  id INTEGER PRIMARY KEY,
  by TEXT NOT NULL,
  whom TEXT NOT NULL,
  time INTEGER NOT NULL,
  recur INTEGER,
  what TEXT NOT NULL,
  created INTEGER NOT NULL
)")
  (sqlite-exec %db
               "\
CREATE TABLE IF NOT EXISTS candidates (
  handle TEXT PRIMARY KEY,
  last_participation INTEGER NOT NULL
)"))



(define (readable-date-from-seconds sec)
  (date->string
   (time-utc->date
    (make-time time-utc 0 sec)
    (%tz))))

(define* (remind #:key by whom when recur? message)
  (sqlite-exec* %db
                "INSERT INTO reminders \
(by, whom, time, recur, what, created) VALUES \
(:by, :whom, :time, :recur, :what, :created)"
                #:by by #:whom whom
                #:time (time-second (date->time-utc when))
                #:recur
                (and=> recur? time-second)
                #:what message
                #:created (time-second (current-time time-utc)))
  (format #false "Okay, I'll remind ~a at ~a~a with this message: ~a"
          (if (string=? whom "me") "you" whom)
          (date->string when)
          (if recur? (format #false " (and again after ~a seconds)"
                             (time-second recur?)) "")
          message))

(define* (list-reminders #:optional by)
  (let ((reminders (if by
                       (sqlite-exec* %db
                                     "SELECT * FROM reminders WHERE by = :by ORDER BY time ASC" #:by by)
                       (sqlite-exec* %db "SELECT * FROM reminders ORDER BY time ASC")) ))
    (match reminders
      (() "There are none.")
      (results
       (string-join
        (append
            (list "---"
                  "| id | by | whom | when | recur? | what | created |"
                  "|:---|:---|:-----|:-----|:-------|:-----|:--------|")
            (map (match-lambda
                   (#(id by whom time recur what created)
                    (format #false "| ~a | ~a | ~a | ~a | ~a | ~a | ~a |"
                            id by whom
                            (readable-date-from-seconds time)
                            (or recur "no")
                            (string-delete (char-set #\newline) what)
                            (readable-date-from-seconds created))) )
                 results)
          (list "---"))
        "\n")))))

(define (delete-reminder id)
  (sqlite-exec* %db
                "DELETE FROM reminders WHERE id = :id" #:id id))

(define (process-reminders)
  (match (sqlite-exec* %db
                       "SELECT * FROM reminders WHERE time < :time"
                       #:time (time-second (current-time time-utc)))
    (() #false)
    (some
     (map (match-lambda
            (#(id by whom time recur what created)
             (if recur
                 (sqlite-exec* %db
                               "UPDATE reminders SET time = :time WHERE id = :id"
                               #:time
                               (time-second
                                (let ((activation (make-time time-utc 0 time)))
                                  (add-duration
                                   activation
                                   (make-time time-duration 0 recur))))
                               #:id id)
                 (delete-reminder id))
             (let ((channel (cond
                             ((string=? whom "me")
                              (string-append "@" by))
                             ((string-prefix? "~" whom)
                              (string-drop whom 1))
                             (else whom))))
               (post-message #:channel channel
                             #:text (string-append "Reminder: " what)))))
          some))))


(define-peg-pattern WS none
  (or " " "\t"))
(define-peg-pattern punctuation none
  (or "," "." "[" "]" "(" ")"))
(define-peg-pattern string body
  (+ (and (not-followed-by (or WS punctuation)) peg-any)))
(define-peg-pattern whom all
  (or "me" (and "~" string) (and "@" string)))
(define-peg-pattern unit all
  (or "minutes" "minute" "mins" "min"
      "hours" "hour"
      "days" "day"
      "weeks" "week"))
(define-peg-pattern weekday all
  (or "monday"    "Monday"    "Mon"  "mon"
      "tuesday"   "Tuesday"   "Tue"  "tue"
      "wednesday" "Wednesday" "Wed"  "wed"
      "thursday"  "Thursday"  "Thu"  "thu"
      "friday"    "Friday"    "Fri"  "fri"
      "saturday"  "Saturday"  "Sat"  "sat"
      "sunday"    "Sunday"    "Sun"  "sun"))
(define-peg-pattern time all
  (and (range #\0 #\2) (range #\0 #\9)
       (? (and ":" (range #\0 #\5) (range #\0 #\9)))))
(define-peg-pattern delay all
  (+ (range #\0 #\9)))
(define-peg-pattern when all
  (or (and "now")
      (and "at" (+ WS) time)
      (and "in" (+ WS) delay (+ WS) unit)
      (and "every day at" (+ WS) time)
      (and (? (and "every" (+ WS)))
           weekday
           (? (and (+ WS) "at" (+ WS) time)))
      (and "every" (+ WS) unit)
      (and "every" (+ WS) delay (+ WS) unit)))
(define-peg-pattern sep none
  (and (* WS) (or "to" ":") (* WS)))
(define-peg-pattern what all
  (+ peg-any))
(define-peg-pattern command all
  (and whom (+ WS) when sep
       what))


(define (%tz)
  (date-zone-offset (current-date)))

(define (unit->seconds n unit)
  (match unit
    ((or "minute" "minutes" "mins" "min") (* 60 n))
    ((or "hour" "hours") (* 60 60 n))
    ((or "day" "days") (* 24 60 60 n))
    ((or "week" "weeks") (* 7 24 60 60 n))
    (_ (throw 'unknown-unit unit))))

(define (weekday->index day)
  "Turn the string DAY into a weekday index from 0 (Sunday) to
6 (Saturday)."
  (let ((day* (string-take (string-downcase day) 3)))
    (list-index (cut string= day* <>)
                '("sun" "mon" "tue" "wed" "thu" "fri" "sat"))))

(define (weekday->date day)
  "Given the weekday DAY, a string, return the next date that falls
onto that week day."
  (let* ((today (current-date (%tz)))
         (num-days-away
          (match (modulo (- (weekday->index day)
                            (date-week-day today)) 7)
            (0 7)
            (n n)))
         (num-seconds-away
          (unit->seconds num-days-away "days"))
         (time-until-reminder
          (make-time time-duration 0
                     num-seconds-away)))
    (date+time today time-until-reminder)))

(define (string->time time)
  "Return a time object from the string TIME, which may be a number or
a colon-separated pair of hours and minutes."
  (match (string-split time #\:)
    ((hh)
     (make-time time-duration 0
                (unit->seconds (string->number hh) "hours")))
    ((hh mm)
     (make-time time-duration 0
                (+ (unit->seconds (string->number hh) "hours")
                   (unit->seconds (string->number mm) "minutes"))))))

(define (date+time date time)
  "Add duration TIME to DATE, returning a new date."
  (time-utc->date
   (add-duration (date->time-utc date) time)
   (%tz)))

(define (date-0 date)
  "Return DATE at the early morning."
  (make-date 0 0 0 0
             (date-day date)
             (date-month date)
             (date-year date)
             (%tz)))

(define (text->reminder text user)
  (match (peg:tree (match-pattern command text))
    (('command ('whom whom)
               ('when . when)
               ('what what))
     (match when
       (("now")
        (remind #:by user
                #:whom whom
                #:when (current-date (%tz))
                #:recur? #false
                #:message what))
       (("in" ('delay n) ('unit unit))
        (remind #:by user
                #:whom whom
                #:when
                (date+time (current-date (%tz))
                           (make-time time-duration 0
                                      (unit->seconds (string->number n) unit)))
                #:recur? #false
                #:message what))
       (("every" ('unit unit))
        (let ((duration (make-time time-duration 0
                                   (unit->seconds 1 unit))))
          (remind #:by user
                  #:whom whom
                  #:when
                  (date+time (current-date (%tz)) duration)
                  #:recur? duration
                  #:message what)))
       (("every" ('delay n) ('unit unit))
        (let ((duration (make-time time-duration 0
                                   (unit->seconds (string->number n) unit))))
          (remind #:by user
                  #:whom whom
                  #:when
                  (date+time (current-date (%tz)) duration)
                  #:recur? duration
                  #:message what)))
       (("at" ('time time))
        (remind #:by user
                #:whom whom
                #:when
                (let ((time* (string->time time)))
                  (date+time (date-0 (current-date (%tz))) time*))
                #:recur? #false
                #:message what))
       ((('weekday day) ("at" ('time time)))
        (remind #:by user
                #:whom whom
                #:when
                (let ((date (weekday->date day))
                      (time* (string->time time)))
                  (date+time (date-0 date) time*))
                #:recur? #false
                #:message what))
       (("every" ('weekday day))
        (remind #:by user
                #:whom whom
                #:when
                (weekday->date day) 
                #:recur?
                (make-time time-duration 0
                           (unit->seconds 1 "week"))
                #:message what))
       (("every" "day" "at" ('time time))
        (remind #:by user
                #:whom whom
                #:when
                (let ((time* (string->time time)))
                  (date+time (date-0 (current-date (%tz))) time*))
                #:recur? (make-time time-duration 0
                                    (unit->seconds 1 "day"))
                #:message what))
       (("every" ('weekday day) ("at" ('time time)))
        (remind #:by user
                #:whom whom
                #:when
                (let ((date (weekday->date day))
                      (time* (string->time time)))
                  (date+time (date-0 date) time*))
                #:recur? (make-time time-duration 0
                                    (unit->seconds 1 "week"))
                #:message what))
       (parse
        (format #false "unhandled parse tree: ~y" parse))))
    (_ #false)))

(define (make-help)
 (string-join
  '("---"
    "| Command                              | Description                    |"
    "|:-------------------------------------|:-------------------------------|"
    "| /sudo make me a sandwich             | Okay.                          |"
    "---")
  "\n"))

(define (remind-help)
  (string-join
   '("---"
     "| Command                              | Description          |"
     "|:-------------------------------------|:---------------------|"
     "| /sudo remind help                    | Show help            |"
     "| /sudo remind list                    | List all reminders   |"
     "| /sudo remind list mine               | List my reminders    |"
     "| /sudo remind delete [id]             | Delete reminder [id] |"
     "| /sudo remind [whom] [when]: [what]   | Do the thing.        |"
     "| /sudo remind [whom] [when] to [what] | Do the thing.        |"
     ""
     "### Example targets"
     ""
     "| Command        | Description                        |"
     "|:---------------|:-----------------------------------|"
     "| me             | Remind me, duh                     |"
     "| ~general       | Send reminder to channel `general` |"
     "| @vonnegut      | Send reminder to user `vonnegut`   |"
     ""
     "### Example time formats"
     ""
     "- now"
     "- every Sunday at 13"
     "- every mon"
     "- Wednesday at 19:30"
     "- at 23:12"
     "- in 3 hours"
     "---")
  "\n"))

(define (handle-exists? handle)
  (match (sqlite-exec*
          %db
          "SELECT * FROM candidates WHERE handle = :handle LIMIT 1"
          #:handle handle)
    (() #false)
    (entry entry)))
  
(define (add-handles handles)
  (string-join
   (map
    (lambda (handle)
     (let ((last-update (time-second (current-time time-utc))))
       (if (handle-exists? handle)
         (format #false "~a was already present" handle)
         (begin
           (sqlite-exec*
                %db
                (string-join 
                 '("INSERT INTO candidates"
                   "(handle, last_participation) VALUES"
                   "(:handle, :last_update)")
                  " ")
                #:handle handle #:last_update last-update)
           (format #false "~a was newly added" handle)))))
     handles)
  "\n"))

(define (remove-handles handles)
  (string-join
   (map
    (lambda (handle)
     (if (handle-exists? handle)
       (begin
        (sqlite-exec*
         %db
         (string-join 
         '("DELETE FROM candidates"
           "WHERE handle = :handle")
          " ")
         #:handle handle)
        (format #false "~a was deleted" handle))
       (format #false "~a doesn't exist" handle)))
    handles)
   "\n"))

(define (list-handles)
  (let ((handles (sqlite-exec* %db "SELECT * FROM candidates ORDER BY last_participation ASC")))
    (match handles
      (() "There are no candidates in the database.")
      (results
       (string-join
        (append
            (list "---"
                  "| Handle | Last update |"
                  "|:-------|:------------|")
            (map (match-lambda
                   (#(handle last-update)
                    (format #false "| ~a | ~a |"
                            handle
                            (readable-date-from-seconds last-update))))
                 results)
          (list "---"))
        "\n")))))

(define (weighted-random weights)
  ;; TODO Ensure no zeros in weights.
  (let* ((weights-sum (apply + weights))
         (thresh (random weights-sum)))
    (let loop ((loop-idx 0) (weights-accum (car weights)))
      (if (or (> weights-accum thresh)
              (>= loop-idx (length weights)))
          loop-idx
          (loop (+ loop-idx 1) (+ weights-accum (list-ref weights (+ loop-idx 1))))))))

(define (weighted-sample x weights n)
  ;; No provisions are made for duplicates in x.
  (define (weighted-sample x n n-max choices)
    (if (>= n n-max)
       choices
       (let* ((choice-idx (weighted-random (map cdr x)))
              (choice-pair (list-ref x choice-idx)))
         (weighted-sample
          (assoc-remove! x (car choice-pair))
          (+ n 1)
          n-max
          (cons (car choice-pair) choices)))))
  (let ((x (map cons x weights))
        (choices '()))
    (weighted-sample x 0 n choices)))

(define (choose-candidates n)
  (let ((results (sqlite-exec*
                  %db
                  "SELECT * FROM candidates ORDER BY last_participation")))
    (match results
      (() "There seem to be no candidates to choose from.")
      (some
       (let ((weights (map
                       (match-lambda
                        (#(handle last-update)
                          (- (time-second (current-time time-utc)) last-update)))
                       results)))
        (string-join
         (map
          (match-lambda
           (#(handle last-update)
            (format
             #false
             "~a (last update on ~a, confirm with `/sudo update confirm ~a`)" 
             handle
             (readable-date-from-seconds last-update)
             handle)))
          (weighted-sample results weights n))
         "\n"))))))

(define (confirm-candidates handles)
  (string-join
   (map
    (lambda (handle)
      (if (handle-exists? handle)
        (begin
         (sqlite-exec*
          %db
          (string-join '("UPDATE candidates"
                         "SET last_participation = :last_update"
                         "WHERE handle = :handle")
                       " ")
          #:last_update (time-second (current-time time-utc))
          #:handle handle)
         (format
          #false
          "updated last presentation time of ~a"
          handle))
        (format
         #false
         "couldn't find an entry for ~a"
         handle)))
    handles)
   "\n"))

(define (all-help)
  (string-join
   '("---"
     "# Sudo - All purpose mattermost bot"
     ""
     "Invoke via `/sudo subcommand args`"
     "See subcommand help via `/sudo subcommand help`"
     ""
     "## Subcommands"
     ""
     "* `remind` Set and manage reminders."
     "* `update` Update round help."
     "* `make` Experience the power of `sudo`."
     "---")
   "\n"))

(define (update-help)
 (string-join
  '("---"
    "### General workflow:"
    ""
    "* Add handles whose people regularly participate in the lab meeting (`add`)"
    "* Let the bot choose any number of them at random, weighted by how long it has been since their last presentation (`choose`)"
    "* If one of them actually presents, confirm this to update their last update date to the current time (`confirm`)"
    ""
    "| Command                              | Description                    |"
    "|:-------------------------------------|:-------------------------------|"
    "| /sudo update add [handles]           | Add presentation candidates.   |"
    "| /sudo update remove [handles]        | Remove candidates.             |"
    "| /sudo update list                    | List all candidates.           |"
    "| /sudo update choose [n]              | Choose a number of candidates. |"
    "| /sudo update confirm [handles]       | Update presentation date.      |"
    "---")
  "\n"))

(define (subcommand-make command-tokens)
  (match command-tokens
    (("help")
      (render-json
        `(("text" . ,(make-help)))))
    (("me" . something)
      (render-json
        `(("text"
          . ,(string-append "Why don't you make "
            (string-join something " ")
            " yourself?")))))
    (("love")
      (render-json
        `(("text" . "make love, not war."))))
    ((unexpected-thing)
      (render-json
        `(("text" . ,(format #false "I have no idea how to make ~a." unexpected-thing)))))))

(define (subcommand-remind command-tokens user)
  (match command-tokens
    (("help")
      (render-json
        `(("text" . ,(remind-help)))))
    (("list")
      (render-json
        `(("text" . ,(list-reminders)))))
    (("list" "mine")
      (render-json
        `(("text" . ,(list-reminders user)))))
    (("delete" id)
      (catch #t
        (lambda ()
          (delete-reminder id)
          (render-json
            `(("text" . ,(list-reminders user)))))
        (lambda _
          (render-json
            `(("text" . ,(format #false "failed to delete reminder ~a" id)))))))
    (_
      (let* ((text (string-join command-tokens " "))
              (reminder (text->reminder text user)))
        (if reminder
          (render-json
            `(("text" . ,reminder)))
          (render-json
            `(("text" . ,(format #false "I couldn't understand the reminder: `~a`" text)))))))))

(define (subcommand-update command-tokens) 
  (match command-tokens
    (("help")
      (render-json
        `(("text" . ,(update-help)))))
    (("add" . handles)
      (render-json
        `(("text" . ,(add-handles handles)))))
    (("list")
      (render-json
        `(("text" . ,(list-handles)))))  
    (("remove" . handles)
      (render-json
        `(("text" . ,(remove-handles handles)))))
    (("choose" . n-list)
      (let ((n (if (null? n-list)
                   1
                   (if (string-match "[0-9]+" (car n-list))
                       (string->number (car n-list))
                       #f))))
        (if n
          (let ((choose-string (choose-candidates n))
                (complaint (if (> (length n-list) 1)
                               (format
                                #false
                                "\n(Ignoring additional information ~a)"
                                (cdr n-list))
                               ""))
                (channel "town-square")) ; Every server has this channel.
            (begin
             (post-message
              #:channel channel
              #:text (string-append
                      choose-string 
                      (string-append
                       (string-join
                        '("\nIf you are currently in a meeting,"
                          "please tell the others what you have been up to.")
                        " ")
                       complaint)))
             (render-json
              `(("text" .
                 ,(format
                   #false
                   "Chose candidates, check ~~town-square\n~a"
                   (string-append choose-string complaint))
                   )))))
          (render-json
            `(("text" .
               ,(format
                 #false
                 "Could not parse number of handles to choose: ~a"
                 (car n-list))))))))
    (("confirm" . handles)
      (render-json
        `(("text" . ,(confirm-candidates handles)))))
    ((unexpected-thing)
      (render-json
        `(("text" .
           ,(format
             #false
             "I don't understand this update subcommand: `~a`"
             unexpected-thing)))))))

(define (controller request body)
  (catch 'bad-token
    (lambda ()
      (match (cons (request-method request)
                   (request-path-components request))
        (('POST . ignore)
         (let* ((payload (utf8->string body))
                (data (make-pair-list
                       (map uri-decode
                            (string-split payload (char-set #\& #\=)))))
                (valid-token? (or (member (assoc-ref data "token") %tokens)
                                  (throw 'bad-token)))
                (text (assoc-ref data "text"))
                (user (assoc-ref data "user_name")))
           (match (string-tokenize text)
            (("help")
              (render-json `(("text" . ,(all-help)))))
            (("make" . make-tokens)
              (subcommand-make make-tokens))
            (("remind" . remind-tokens)
              (subcommand-remind remind-tokens user))
            (("update" . update-tokens)
              (subcommand-update update-tokens))
            (_
              (render-json
               `(("text" . ,(string-append "Pong: " text))))))))
        (_ (no-content))))
    (lambda _
      (internal-error))))

(define (handler request body)
  (format (current-error-port)
          "~a ~a~%"
          (request-method request)
          (uri-path (request-uri request)))
  (call-with-error-handling
    (lambda ()
      (apply values (controller request body)))))

(define (run-my-server port)
  (pk 'listening 'on port)
  (run-server handler
              'http
              `(#:addr ,INADDR_ANY
                #:port ,port)))


(define (show-usage)
  (format (current-error-port)
          "
    `mattermost port hook-id tokens ...'

         start the app server on the given port.  Post messages via
         hook and accept all messages with any of the given auth tokens.
 ~%")
  (exit 1))

(define (main . args)
  (init-db!)
  (match (cdr (program-arguments))
    ((url port hook-id . tokens)
     (set! %tokens tokens)
     (set! %hook (format #false "~a/hooks/~a" url hook-id))
     (call-with-new-thread
      (lambda ()
        (let loop ()
          (process-reminders)
          (sleep 5)
          (loop))))
     (run-my-server (string->number port)))
    (_ (show-usage))))
