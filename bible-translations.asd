;;;; bible-translations.asd

(asdf:defsystem #:bible-translations
  :serial t
  :description "Describe bible-translations here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma #:cl-html-parse)
  :components ((:file "package")
               (:file "bible-translations")))
