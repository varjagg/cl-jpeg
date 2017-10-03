(asdf:defsystem :cl-jpeg
  :name "cl-jpeg"
  :version "2.9"
  :license "BSD"
  :description "A self-contained baseline JPEG codec implementation"
  :author "Eugene Zaikonnikov; contributions by Kenan Bölükbaşı, Manuel Giraud, Cyrus Harmon and William Halliburton"
  :components ((:file "package")
	       (:file "globals")
	       (:file "conditions")
	       (:file "jpeg" :depends-on ("globals" "conditions"))
	       (:file "io" :depends-on ("jpeg"))))
