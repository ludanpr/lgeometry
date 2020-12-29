;;;; ASDF file:
;;;;          L-geometry.asd
;;;;

(asdf:defsystem #:L-geometry
  :description "Implementations of computational geometry algorithms."
  :author ("L")
  :maintainer ("L")
  :license "MIT"
  :homepage ""
  :source-control (:git "https://github.com/luandelpreto/L-geometry.git")
  :bug-tracker "https://github.com/luandelpreto/L-geometry/issues"
  :version "0.0.1"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:bordeaux-threads)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "n-dim-methods")
               (:file "point2d")))
