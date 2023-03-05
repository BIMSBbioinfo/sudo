This is a simple mattermost bot for reminders.

Run as:

    guix shell guile guile-sqlite3 guile-json -- ./mattermost.scm url 7777 hook-id [tokens]

Where `hook-id` is the generated key of the incoming webhook, allowing
the bot to post.  Tokens is a list of at least one token, which is
generated when registering the slash command in the first place.

The bot writes reminders to a sqlite database file `mattermost.db`.  A
separate thread polls the database to see if any reminders require
action.

Run tests: `guile -s tests.scm`

License: AGPL 3.0 or later.
