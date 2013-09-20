;;;; package.lisp

(defpackage #:smacktourney
  (:nicknames :tourney)
  (:use #:cl #:smackfeebs #:cl-fad #:alexandria)
  (:shadowing-import-from #:alexandria :copy-stream :copy-file))

