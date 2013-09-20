;;;; smacktourney.asd

(asdf:defsystem #:smacktourney
  :serial t
  :version "0.1"
  :description "A framework to run smackfeeb tournaments."
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:smackfeebs #:cl-fad #:group-by
                            #:local-time #:babel #:ironclad)
  :components ((:file "package")
               (:file "smacktourney")))

