;;;; ASDF file:
;;;;          L-geometry.asd
;;;;

(asdf:defsystem #:lgeometry
  :description "Implementations of computational geometry algorithms."
  :author ("ludanpr")
  :maintainer ("ludanpr")
  :license "MIT"
  :homepage ""
  :source-control (:git "https://github.com/ludanpr/lgeometry.git")
  :bug-tracker "https://github.com/ludanpr/lgeometry/issues"
  :version "0.0.1"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:bordeaux-threads)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "conditions")
               (:file "n-dim-methods")
               (:file "point2d")))
