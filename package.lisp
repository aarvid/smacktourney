;;;; package.lisp

(defpackage #:smacktourney
  (:nicknames :tourney)
  (:use #:cl #:smackfeebs #:uiop #:alexandria #:local-time)
  (:shadowing-import-from #:alexandria
   :copy-file :if-let :emptyp :ensure-list
   :ensure-function :featurep :appendf)
  (:export #:tourney-ranking-list
           #:print-tourney-ranking
           #:run-tourney
           #:create-tourney
           #:create-tourney-user
           #:check-tourney-user-password
           #:tourney-user-exists-p
           #:valid-user-name-p
           #:tourney-feeb-exists-p
           #:create-tourney-feeb
           #:valid-feeb-name-p
           #:upload-feeb-source
           #:expunge-tourney-user
           #:expunge-tourney-user-feeb
           #:change-user-password
           #:tourney-timer-active-p
           #:start-tourney-timer
           #:stop-tourney-timer
           #:feeb-source-name-text
           #:feeb-source-open-p
           #:set-feeb-open-source))

