(asdf:defsystem :process-stream
     :description "process manager with stream"
     :version "0.0.1"
     :author "@other-otter"
     :depends-on (:http-string
                  :bordeaux-threads
                  :log4cl)
     :components ((:file "package")
                  (:module code
                   :serial t
                   :components ((:file "process-manager.lisp")))))
