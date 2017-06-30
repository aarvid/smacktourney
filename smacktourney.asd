;;;; smacktourney.asd

(asdf:defsystem "smacktourney"
  :serial t
  :version "0.1"
  :description "A framework to run smackfeeb tournaments."
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on ("smackfeebs" "group-by"
                            "local-time" "babel" "ironclad"
                            "trivial-timers"
                            #-asdf3 "uiop")
  :components ((:file "package")
               (:file "smacktourney")))

