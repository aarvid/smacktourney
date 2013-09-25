;;;; package.lisp

(defpackage #:smacktourney
  (:nicknames :tourney)
  (:use #:cl #:smackfeebs #:cl-fad #:alexandria)
  (:shadowing-import-from #:alexandria :copy-stream :copy-file)
  (:export #:tourney-ranking-list
           #:print-tourney-ranking
           #:run-tourney
           #:create-tourney
           #:create-tourney-user
           #:check-tourney-user-password
           #:tourney-user-exists-p
           #:valid-user-name-p))

