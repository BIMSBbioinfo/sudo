(use-modules (srfi srfi-64))

(include "mattermost.scm")

(test-begin "mattermost")

(define (parse text)
  (peg:tree (match-pattern command text)))

(test-equal "parse at"
  '(command
    "remind"
    (whom "me")
    (when "at" (time "10"))
    (what "eat"))
  (parse "remind me at 10 to eat"))

(test-equal "parse every"
  '(command
    "remind"
    (whom "me")
    (when "every"
      (weekday "Tue")
      ("at" (time "12:23")))
    (what "bring out the trash"))
  (parse "remind me every Tue at 12:23: bring out the trash"))

(test-equal "weekday->index"
  0 (weekday->index "Sunday"))
(test-equal "weekday->index"
  1 (weekday->index "mon"))
(test-equal "weekday->index"
  2 (weekday->index "tuesday"))
(test-equal "weekday->index"
  3 (weekday->index "Wed"))
(test-equal "weekday->index"
  4 (weekday->index "Thursday"))

(test-equal "string->time"
  0
  (time-second (string->time "0")))
(test-equal "string->time"
  (* 12 60 60)
  (time-second (string->time "12")))
(test-equal "string->time"
  (+ (* 12 60 60) (* 10 60))
  (time-second (string->time "12:10")))
(test-equal "string->time"
  (+ (* 2 60 60) (* 50 60))
  (time-second (string->time "2:50")))

(test-end "mattermost")

