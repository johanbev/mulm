(defsystem :mime
  :description "Mime Experiment Manager"
  :version "0.1"
  :author "Johan Benum Evensberget, André Lynum"
  :license "GPL"
  :components ((:file "mime")
               (:file "experiment" :depends-on ("mime")))
  :depends-on ("mulm"))
