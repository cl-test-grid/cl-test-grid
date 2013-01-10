(asdf:defsystem #:some-application
  :depends-on (#:local-package-aliases
               #:com.my-company.some-library)
  :around-compile "local-package-aliases:call-with-aliasing-readtable"
  :components ((:file "some-application")))