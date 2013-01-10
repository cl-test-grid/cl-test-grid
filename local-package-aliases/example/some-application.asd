(asdf:defsystem #:some-application
  :depends-on (#:com.my-company.some-library #:local-package-aliases)
  :around-compile "local-package-aliases:call-with-aliasing-readtable"
  :components ((:file "some-application")))