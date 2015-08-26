
(:SYSTEM->DEPENDENCIES
 (("1am")
  ("3b-swf" "alexandria" "chipz" "cl-jpeg" "cxml" "flexi-streams"
   "ieee-floats" "salza2" "vecto" "zpng")
  ("3b-swf-swc" "3b-swf" "cxml" "zip")
  ("3bmd" "alexandria" "esrap" "split-sequence")
  ("3bmd-ext-code-blocks" "3bmd" "alexandria" "colorize")
  ("3bmd-ext-definition-lists" "3bmd" "alexandria" "colorize")
  ("3bmd-ext-tables" "3bmd") ("3bmd-ext-wiki-links" "3bmd")
  ("a-cl-cairo2-loader" "cl-cairo2")
  ("able" "cl-fad" "ltk" "trivial-gray-streams")
  ("abnf" "cl-ppcre" "esrap") ("abort-ks-execution-example")
  ("abstract-classes" "closer-mop")
  ("access" "alexandria" "anaphora" "cl-interpol" "closer-mop"
   "iterate")
  ("access-test" "access" "lisp-unit2")
  ("acl-compat" "cl-fad" "cl-ppcre" "ironclad" "puri")
  ("acm-random" "com.google.base" "random")
  ("acm-random-test" "acm-random" "hu.dwim.stefil")
  ("advanced-readtable" "named-readtables")
  ("adw-charting" "iterate")
  ("adw-charting-google" "adw-charting" "drakma")
  ("adw-charting-vecto" "adw-charting" "vecto") ("agenda-shell")
  ("agenda-shell-user") ("alexandria")
  ("alexandria-tests" "alexandria")
  ("algebraic-data-library" "cl-algebraic-data-type")
  ("amazon-ecs" "alexandria" "bordeaux-threads" "cl-ppcre" "drakma"
   "hunchentoot" "ironclad" "net-telent-date" "parse-number"
   "trivial-http" "xml-mop")
  ("anaphora") ("anaphora-test" "anaphora" "rt")
  ("anaphoric-variants" "map-bind")
  ("antik" "alexandria" "asdf-system-connections" "cffi"
   "cffi-grovel" "cl-ppcre" "drakma" "iterate" "split-sequence"
   "static-vectors" "trivial-garbage")
  ("antik-tests" "antik" "lisp-unit") ("apply-argv" "alexandria")
  ("apply-argv-tests" "apply-argv" "fiveam")
  ("arc-compat" "babel" "bordeaux-threads" "cl-fad" "fiveam"
   "ironclad" "named-readtables")
  ("architecture.service-provider" "alexandria" "let-plus"
   "more-conditions")
  ("architecture.service-provider-and-hooks"
   "architecture.service-provider" "cl-hooks")
  ("architecture.service-provider-and-hooks-test" "alexandria"
   "architecture.service-provider-and-hooks"
   "architecture.service-provider-test" "fiveam")
  ("architecture.service-provider-test" "alexandria"
   "architecture.service-provider" "fiveam" "let-plus"
   "more-conditions")
  ("archive" "cl-fad" "trivial-gray-streams") ("arnesi")
  ("arnesi.cl-ppcre-extras" "arnesi" "cl-ppcre")
  ("arnesi.slime-extras" "arnesi" "swank")
  ("array-operations" "alexandria" "anaphora" "let-plus" "optima")
  ("array-operations-tests" "array-operations" "clunit")
  ("array-utils") ("array-utils-test" "array-utils")
  ("arrow-macros" "hu.dwim.walker")
  ("arrow-macros-test" "arrow-macros" "fiveam")
  ("ascii-strings" "alexandria" "babel") ("asdf-defdoc")
  ("asdf-dependency-grovel" "asdf") ("asdf-driver" "uiop")
  ("asdf-encodings" "asdf")
  ("asdf-encodings/test" "asdf-encodings" "fare-utils"
   "hu.dwim.stefil")
  ("asdf-finalizers" "asdf")
  ("asdf-finalizers-test" "asdf-finalizers" "fare-utils"
   "hu.dwim.stefil" "list-of")
  ("asdf-finalizers-test/1" "asdf-finalizers" "fare-utils"
   "hu.dwim.stefil" "list-of")
  ("asdf-linguist" "asdf" "parenscript")
  ("asdf-linguist-test" "asdf-linguist") ("asdf-nst")
  ("asdf-package-system" "asdf") ("asdf-system-connections")
  ("aserve" "acl-compat" "htmlgen")
  ("asn.1" "ieee-floats" "trivial-gray-streams" "usocket")
  ("asteroids" "lispbuilder-sdl" "lispbuilder-sdl-gfx"
   "lispbuilder-sdl-mixer")
  ("atdoc" "cl-ppcre" "closer-mop" "cxml" "split-sequence" "swank"
   "xuriella")
  ("automaton" "rt")
  ("avatar-api" "cl-json" "crypto-shortcuts" "drakma")
  ("avatar-api-test" "avatar-api" "fiveam")
  ("aws-sign4" "flexi-streams" "ironclad" "local-time"
   "secret-values" "split-sequence")
  ("aws-sign4-example" "aws-sign4" "drakma")
  ("aws-sign4-tests" "aws-sign4") ("ayah-captcha" "cl-json" "drakma")
  ("ayah-captcha-demo" "ayah-captcha" "cl-who" "hunchentoot")
  ("babel" "alexandria" "trivial-features")
  ("babel-streams" "alexandria" "babel" "trivial-gray-streams")
  ("babel-tests" "babel" "hu.dwim.stefil")
  ("backports" "alexandria" "bordeaux-threads" "closer-mop"
   "trivial-backtrace")
  ("backports-test" "backports" "fiveam") ("bacteria" "alexandria")
  ("bacteria.js" "bacteria" "parenscript") ("base")
  ("basic-binary-ipc" "cffi-grovel")
  ("basic-binary-ipc-tests" "basic-binary-ipc" "bordeaux-threads"
   "lisp-unit")
  ("beirc" "cl-fad" "cl-irc" "cl-ppcre" "mcclim" "split-sequence")
  ("bencode" "flexi-streams")
  ("bencode-test" "bencode" "hu.dwim.stefil") ("bermuda" "pal")
  ("bert" "alexandria" "erlang-term") ("bibtex" "split-sequence")
  ("big-string") ("binary-lass" "lass") ("binary-types") ("binascii")
  ("binascii-tests" "binascii") ("binfix") ("binge" "npg" "sclf")
  ("binge-tests" "binge") ("binomial-heap")
  ("birch" "alexandria" "flexi-streams" "split-sequence" "usocket")
  ("birch.test" "birch" "flexi-streams" "prove")
  ("bit-smasher" "cl-base58" "cl-base64" "ironclad")
  ("bitfield-schema" "iterate") ("bk-tree")
  ("bknr.data.impex" "bknr.datastore" "bknr.impex" "bknr.indices"
   "bknr.utils" "cl-interpol" "unit-test")
  ("bknr.datastore" "alexandria" "bknr.indices" "bknr.utils"
   "cl-interpol" "closer-mop" "trivial-utf-8" "unit-test" "yason")
  ("bknr.impex" "bknr.indices" "bknr.utils" "bknr.xml" "cl-interpol"
   "closer-mop" "cxml")
  ("bknr.indices" "bknr.skip-list" "bknr.utils" "cl-interpol"
   "closer-mop")
  ("bknr.modules" "bknr.utils" "bknr.web" "cl-gd" "cl-interpol"
   "cl-ppcre" "cl-smtp" "closer-mop" "cxml" "md5" "parenscript"
   "puri" "stem" "unit-test")
  ("bknr.skip-list")
  ("bknr.skip-list.test" "bknr.skip-list" "unit-test")
  ("bknr.utils" "alexandria" "bordeaux-threads" "cl-interpol"
   "cl-ppcre" "flexi-streams" "md5")
  ("bknr.web" "alexandria" "bknr.data.impex" "bknr.datastore"
   "bknr.utils" "bknr.xml" "cl-gd" "cl-interpol" "cl-ppcre" "cxml"
   "drakma" "hunchentoot" "md5" "parenscript" "puri" "unit-test"
   "usocket" "xhtmlgen" "yason")
  ("bknr.xml" "cl-interpol" "cxml") ("black-tie") ("blackbird" "vom")
  ("blackbird-test" "blackbird" "cl-async" "fiveam")
  ("blackthorn" "alexandria" "cl-containers" "cl-fad" "cl-opengl"
   "cl-store" "command-line-arguments" "iterate" "lispbuilder-sdl"
   "lispbuilder-sdl-image" "lispbuilder-sdl-mixer" "mt19937"
   "trivial-features" "usocket")
  ("blackthorn-collision-test" "blackthorn")
  ("blackthorn-stress-test" "blackthorn")
  ("blas-complex" "blas-real" "f2cl")
  ("blas-hompack" "blas-package" "f2cl") ("blas-package")
  ("blas-real" "blas-hompack" "f2cl") ("blocks-world")
  ("bordeaux-fft") ("bordeaux-threads" "alexandria")
  ("bordeaux-threads-test" "bordeaux-threads" "fiveam") ("bourbaki")
  ("bt-semaphore" "bordeaux-threads")
  ("bt-semaphore-test" "bt-semaphore" "clunit")
  ("btrie" "arnesi" "lift" "split-sequence")
  ("btrie-tests" "btrie" "lift" "metabang-bind")
  ("bubble-operator-upwards") ("buffalo" "sparseset") ("buildapp")
  ("buildnode" "alexandria" "cl-interpol" "cl-ppcre" "closure-html"
   "collectors" "cxml" "flexi-streams" "iterate" "split-sequence"
   "swank" "symbol-munger")
  ("buildnode-excel" "buildnode") ("buildnode-html5" "buildnode")
  ("buildnode-kml" "buildnode")
  ("buildnode-test" "buildnode" "buildnode-xhtml" "lisp-unit2")
  ("buildnode-xhtml" "buildnode") ("buildnode-xul" "buildnode")
  ("bunnyslayer" "blackthorn")
  ("burgled-batteries" "alexandria" "cffi" "cffi-grovel" "cl-fad"
   "parse-declarations-1.0" "trivial-garbage")
  ("burgled-batteries-tests" "burgled-batteries" "cl-quickcheck"
   "lift")
  ("burgled-batteries.syntax" "burgled-batteries" "esrap"
   "named-readtables")
  ("burgled-batteries.syntax-test" "burgled-batteries.syntax" "lift")
  ("bus" "gwl-graphics")
  ("bytecurry.asdf-ext" "asdf" "asdf-package-system")
  ("bytecurry.mocks" "asdf" "asdf-package-system"
   "bytecurry.asdf-ext")
  ("bytecurry.mocks/test" "bytecurry.mocks" "fiveam")
  ("calispel" "bordeaux-threads" "eager-future" "jpl-queues"
   "jpl-util")
  ("cambl" "alexandria" "cl-containers" "fprog" "local-time"
   "periods")
  ("cambl-test" "cambl" "uiop" "xlunit")
  ("caramel" "alexandria" "buildnode" "closure-html" "css-selectors"
   "cxml" "cxml-dom" "iterate")
  ("carrier" "alexandria" "babel" "blackbird" "cl-async"
   "cl-async-ssl" "fast-http" "fast-io" "quri")
  ("cartesian-product-switch" "map-bind")
  ("caveman" "anaphora" "cl-emb" "cl-fad" "cl-ppcre" "cl-project"
   "cl-syntax" "cl-syntax-annot" "clack-v1-compat" "do-urlencode"
   "local-time" "myway")
  ("caveman-middleware-dbimanager" "clack-v1-compat" "dbi")
  ("caveman-test" "caveman" "cl-fad" "cl-test-more" "drakma"
   "usocket")
  ("caveman2" "cl-project" "cl-syntax-annot" "dbi" "lack-request"
   "lack-response" "myway" "ningle" "quri")
  ("caveman2-db" "caveman-middleware-dbimanager" "dbi" "sxql")
  ("caveman2-test" "caveman2" "cl-fad" "drakma" "lack-component"
   "prove" "prove-asdf" "trivial-types" "usocket")
  ("cells" "utils-kt")
  ("cells-gtk" "bordeaux-threads" "cells" "cl-cairo2"
   "cl-cairo2-xlib" "cl-glu" "cl-glut" "cl-opengl" "gtk-ffi"
   "ph-maths" "pod-utils" "utils-kt")
  ("ceramic" "archive" "buildapp" "cl-fad" "cl-json"
   "clack-handler-hunchentoot" "external-program" "jonathan" "osicat"
   "trivial-download" "trivial-extract" "uiop" "uuid" "zip")
  ("ceramic-hello-world" "ceramic" "lucerne")
  ("ceramic-ipc-example" "ceramic" "lucerne")
  ("ceramic-test-app" "ceramic" "drakma")
  ("cerberus" "alexandria" "babel" "flexi-streams" "glass" "ironclad"
   "nibbles" "usocket")
  ("cerberus-kdc" "cerberus" "frpc" "pounds")
  ("cffi" "alexandria" "babel" "trivial-features" "uiop")
  ("cffi-examples" "cffi") ("cffi-grovel" "alexandria" "cffi")
  ("cffi-libffi" "cffi" "cffi-grovel" "trivial-features")
  ("cffi-objects" "cffi" "closer-mop" "trivial-garbage")
  ("cffi-objects.tests" "cffi-objects" "hu.dwim.stefil")
  ("cffi-tests" "bordeaux-threads" "cffi-grovel" "cffi-libffi" "rt"
   "trivial-features")
  ("cffi-uffi-compat" "cffi") ("changed-stream")
  ("changed-stream.test" "changed-stream")
  ("chanl" "bordeaux-threads") ("chanl.examples" "chanl")
  ("chanl.tests" "chanl" "fiveam")
  ("cheat-js" "cl-uglify-js" "fiveam")
  ("check-it" "alexandria" "closer-mop" "optima")
  ("check-it-test" "check-it" "stefil") ("checkl" "marshal")
  ("checkl-docs" "checkl" "cl-gendoc")
  ("checkl-test" "checkl" "fiveam")
  ("chemical-compounds" "periodic-table")
  ("chillax" "chillax.core" "chillax.yason")
  ("chillax.core" "alexandria" "drakma" "flexi-streams")
  ("chillax.jsown" "chillax.core" "jsown")
  ("chillax.view-server" "alexandria" "yason")
  ("chillax.yason" "chillax.core" "yason") ("chipz")
  ("chirp" "alexandria" "cl-base64" "cl-ppcre" "drakma"
   "flexi-streams" "ironclad" "local-time" "split-sequence" "uuid"
   "yason")
  ("chrome-native-messaging" "trivial-utf-8")
  ("chronicity" "cl-interpol" "cl-ppcre" "local-time")
  ("chronicity-test" "chronicity")
  ("chtml-matcher" "cl-ppcre" "closure-html" "f-underscore"
   "stdutils")
  ("chunga" "trivial-gray-streams")
  ("circular-streams" "cl-annot" "cl-syntax-annot"
   "trivial-gray-streams")
  ("circular-streams-test" "circular-streams" "cl-test-more"
   "flexi-streams")
  ("city-hash" "com.google.base" "nibbles")
  ("city-hash-test" "city-hash" "hu.dwim.stefil")
  ("cl+ssl" "bordeaux-threads" "cffi" "flexi-streams"
   "trivial-garbage" "trivial-gray-streams")
  ("cl-6502" "alexandria" "cl-ppcre")
  ("cl-6502-tests" "cl-6502" "fiveam") ("cl-aa") ("cl-aa-misc")
  ("cl-acronyms" "alexandria" "split-sequence")
  ("cl-actors" "bordeaux-threads") ("cl-alc" "cffi" "cl-openal")
  ("cl-algebraic-data-type") ("cl-alut" "cffi" "cl-openal")
  ("cl-ana" "cl-ana.binary-tree" "cl-ana.calculus"
   "cl-ana.clos-utils" "cl-ana.csv-table" "cl-ana.error-propogation"
   "cl-ana.file-utils" "cl-ana.fitting" "cl-ana.generic-math"
   "cl-ana.hash-table-utils" "cl-ana.hdf-table" "cl-ana.histogram"
   "cl-ana.int-char" "cl-ana.linear-algebra" "cl-ana.lorentz"
   "cl-ana.makeres" "cl-ana.makeres-block" "cl-ana.makeres-branch"
   "cl-ana.makeres-graphviz" "cl-ana.makeres-macro"
   "cl-ana.makeres-progress" "cl-ana.makeres-table" "cl-ana.map"
   "cl-ana.math-functions" "cl-ana.ntuple-table"
   "cl-ana.package-utils" "cl-ana.pathname-utils" "cl-ana.plotting"
   "cl-ana.quantity" "cl-ana.reusable-table" "cl-ana.serialization"
   "cl-ana.statistics" "cl-ana.table" "cl-ana.table-utils"
   "cl-ana.table-viewing" "cl-ana.tensor")
  ("cl-ana.binary-tree" "cl-ana.list-utils" "cl-ana.macro-utils")
  ("cl-ana.calculus" "cl-ana.generic-math")
  ("cl-ana.clos-utils" "cl-ana.list-utils" "cl-ana.symbol-utils"
   "cl-ana.tensor" "closer-mop")
  ("cl-ana.csv-table" "alexandria" "antik" "cl-ana.list-utils"
   "cl-ana.table" "cl-csv" "iterate")
  ("cl-ana.error-propogation" "cl-ana.generic-math")
  ("cl-ana.file-utils" "external-program" "split-sequence")
  ("cl-ana.fitting" "alexandria" "cl-ana.error-propogation"
   "cl-ana.generic-math" "cl-ana.map" "gsll")
  ("cl-ana.functional-utils")
  ("cl-ana.generic-math" "cl-ana.list-utils" "cl-ana.package-utils")
  ("cl-ana.gnuplot-interface" "external-program")
  ("cl-ana.gsl-cffi" "cffi") ("cl-ana.hash-table-utils")
  ("cl-ana.hdf-cffi" "cffi")
  ("cl-ana.hdf-table" "alexandria" "cl-ana.binary-tree"
   "cl-ana.hdf-cffi" "cl-ana.hdf-typespec" "cl-ana.hdf-utils"
   "cl-ana.list-utils" "cl-ana.memoization" "cl-ana.table"
   "cl-ana.typed-table" "cl-ana.typespec")
  ("cl-ana.hdf-typespec" "alexandria" "cffi" "cl-ana.hdf-cffi"
   "cl-ana.list-utils" "cl-ana.memoization" "cl-ana.string-utils"
   "cl-ana.symbol-utils" "cl-ana.typespec")
  ("cl-ana.hdf-utils" "alexandria" "cffi" "cl-ana.hdf-cffi"
   "cl-ana.macro-utils" "cl-ana.pathname-utils"
   "cl-ana.string-utils")
  ("cl-ana.histogram" "alexandria" "cl-ana.binary-tree"
   "cl-ana.clos-utils" "cl-ana.fitting" "cl-ana.functional-utils"
   "cl-ana.generic-math" "cl-ana.hash-table-utils"
   "cl-ana.list-utils" "cl-ana.macro-utils" "cl-ana.map"
   "cl-ana.symbol-utils" "cl-ana.tensor" "iterate")
  ("cl-ana.int-char")
  ("cl-ana.linear-algebra" "cl-ana.generic-math" "cl-ana.list-utils"
   "cl-ana.tensor")
  ("cl-ana.list-utils" "alexandria" "cl-ana.functional-utils"
   "cl-ana.string-utils")
  ("cl-ana.lorentz" "cl-ana.generic-math" "cl-ana.linear-algebra"
   "cl-ana.tensor" "iterate")
  ("cl-ana.macro-utils" "alexandria" "cl-ana.list-utils"
   "cl-ana.string-utils" "cl-ana.symbol-utils" "split-sequence")
  ("cl-ana.makeres" "alexandria" "cl-ana.error-propogation"
   "cl-ana.file-utils" "cl-ana.functional-utils"
   "cl-ana.generic-math" "cl-ana.hash-table-utils" "cl-ana.hdf-utils"
   "cl-ana.histogram" "cl-ana.list-utils" "cl-ana.macro-utils"
   "cl-ana.map" "cl-ana.pathname-utils" "cl-ana.reusable-table"
   "cl-ana.serialization" "cl-ana.string-utils" "cl-ana.symbol-utils"
   "cl-ana.table" "cl-fad" "external-program")
  ("cl-ana.makeres-block" "alexandria" "cl-ana.list-utils"
   "cl-ana.macro-utils" "cl-ana.makeres")
  ("cl-ana.makeres-branch" "alexandria" "cl-ana.generic-math"
   "cl-ana.hash-table-utils" "cl-ana.list-utils" "cl-ana.makeres"
   "cl-ana.map")
  ("cl-ana.makeres-graphviz" "cl-ana.makeres" "external-program")
  ("cl-ana.makeres-macro" "cl-ana.list-utils" "cl-ana.makeres")
  ("cl-ana.makeres-progress" "alexandria" "cl-ana.generic-math"
   "cl-ana.makeres")
  ("cl-ana.makeres-table" "cl-ana.csv-table"
   "cl-ana.hash-table-utils" "cl-ana.hdf-table" "cl-ana.hdf-utils"
   "cl-ana.list-utils" "cl-ana.macro-utils" "cl-ana.makeres"
   "cl-ana.makeres-macro" "cl-ana.ntuple-table"
   "cl-ana.reusable-table" "cl-ana.string-utils" "cl-ana.table")
  ("cl-ana.map" "cl-ana.hash-table-utils")
  ("cl-ana.math-functions" "cl-ana.generic-math" "gsll")
  ("cl-ana.memoization" "alexandria")
  ("cl-ana.ntuple-table" "alexandria" "cffi" "cl-ana.gsl-cffi"
   "cl-ana.list-utils" "cl-ana.table" "cl-ana.typed-table"
   "cl-ana.typespec" "gsll")
  ("cl-ana.package-utils" "alexandria") ("cl-ana.pathname-utils")
  ("cl-ana.plotting" "alexandria" "cl-ana.error-propogation"
   "cl-ana.generic-math" "cl-ana.gnuplot-interface"
   "cl-ana.histogram" "cl-ana.list-utils" "cl-ana.macro-utils"
   "cl-ana.map" "cl-ana.math-functions" "cl-ana.string-utils"
   "cl-ana.tensor")
  ("cl-ana.quantity" "alexandria" "cl-ana.error-propogation"
   "cl-ana.generic-math" "cl-ana.list-utils" "cl-ana.macro-utils"
   "cl-ana.symbol-utils")
  ("cl-ana.reusable-table" "alexandria" "cl-ana.table")
  ("cl-ana.serialization" "cl-ana.error-propogation"
   "cl-ana.hdf-table" "cl-ana.hdf-utils" "cl-ana.histogram"
   "cl-ana.int-char" "cl-ana.macro-utils" "cl-ana.typespec")
  ("cl-ana.statistics" "cl-ana.generic-math" "cl-ana.histogram"
   "cl-ana.list-utils" "cl-ana.macro-utils" "cl-ana.map")
  ("cl-ana.string-utils") ("cl-ana.symbol-utils" "cl-ana.list-utils")
  ("cl-ana.table" "alexandria" "cl-ana.functional-utils"
   "cl-ana.list-utils" "cl-ana.macro-utils" "cl-ana.string-utils"
   "cl-ana.symbol-utils")
  ("cl-ana.table-utils" "cl-ana.string-utils" "cl-ana.symbol-utils"
   "cl-ana.table")
  ("cl-ana.table-viewing" "alexandria" "cl-ana.generic-math"
   "cl-ana.histogram" "cl-ana.macro-utils" "cl-ana.plotting"
   "cl-ana.string-utils" "cl-ana.table")
  ("cl-ana.tensor" "alexandria" "cl-ana.generic-math"
   "cl-ana.list-utils" "cl-ana.macro-utils" "cl-ana.symbol-utils")
  ("cl-ana.typed-table" "alexandria" "cl-ana.list-utils"
   "cl-ana.string-utils" "cl-ana.symbol-utils" "cl-ana.table"
   "cl-ana.typespec")
  ("cl-ana.typespec" "alexandria" "cffi" "cl-ana.int-char"
   "cl-ana.list-utils" "cl-ana.memoization" "cl-ana.string-utils"
   "cl-ana.symbol-utils" "cl-ana.tensor")
  ("cl-android" "cl-json" "usocket") ("cl-annot" "alexandria")
  ("cl-anonfun") ("cl-ansi-term" "alexandria" "anaphora")
  ("cl-ansi-text" "alexandria" "cl-colors")
  ("cl-ansi-text-test" "alexandria" "cl-ansi-text" "cl-colors"
   "fiveam")
  ("cl-apple-plist" "html-encode") ("cl-arff-parser") ("cl-arrows")
  ("cl-async" "babel" "cffi" "cl-async-base" "cl-async-util"
   "cl-libuv" "cl-ppcre" "static-vectors" "trivial-features"
   "trivial-gray-streams" "uiop")
  ("cl-async-base" "bordeaux-threads" "cffi" "cl-libuv")
  ("cl-async-future" "blackbird")
  ("cl-async-repl" "bordeaux-threads" "cl-async")
  ("cl-async-ssl" "cffi" "cl-async" "vom")
  ("cl-async-test" "bordeaux-threads" "cffi" "cl-async"
   "cl-async-ssl" "fiveam" "flexi-streams" "ironclad" "usocket")
  ("cl-async-util" "cffi" "cl-async-base" "cl-libuv" "cl-ppcre"
   "fast-io" "vom")
  ("cl-authorize-net" "alexandria" "cl-creditcard" "drakma"
   "split-sequence" "symbol-munger")
  ("cl-authorize-net-tests" "alexandria" "cl-authorize-net"
   "lisp-unit")
  ("cl-autorepo")
  ("cl-autowrap" "alexandria" "cffi" "cl-fad" "cl-json" "cl-ppcre"
   "external-program" "trivial-features")
  ("cl-autowrap-test" "cl-autowrap")
  ("cl-azure" "babel" "cl-base64" "cl-json" "cl-ppcre" "cxml"
   "drakma" "ironclad" "puri" "rt")
  ("cl-base32") ("cl-base32-tests" "cl-base32" "lisp-unit")
  ("cl-base58") ("cl-base58-test" "cl-base58" "cl-test-more")
  ("cl-base64") ("cl-base64-tests" "cl-base64" "kmrcl" "ptester")
  ("cl-bayesnet" "cffi" "s-xml" "trivial-shell")
  ("cl-beanstalk" "flexi-streams" "split-sequence" "usocket")
  ("cl-binary-file-0.4" "trivial-gray-streams")
  ("cl-bloom" "cl-murmurhash") ("cl-bplustree")
  ("cl-bson" "arrow-macros" "babel" "cl-intbytes" "fast-io"
   "ieee-floats" "let-over-lambda" "local-time" "named-readtables"
   "rutils" "trivial-shell")
  ("cl-bson-test" "cl-bson" "prove" "prove-asdf")
  ("cl-btree-0.5" "cl-binary-file-0.4" "cl-swap-file-0.5")
  ("cl-buchberger") ("cl-ca")
  ("cl-cairo2" "cffi" "cl-colors" "cl-utilities" "metabang-bind"
   "trivial-features" "trivial-garbage")
  ("cl-cairo2-demos" "cl-cairo2")
  ("cl-cairo2-gtk2" "cl-cairo2" "cl-cairo2-xlib" "cl-gtk2-cairo")
  ("cl-cairo2-xlib" "cl-cairo2" "cl-freetype2")
  ("cl-case-control" "trivial-types")
  ("cl-cffi-gtk" "bordeaux-threads" "cffi" "cl-cffi-gtk-cairo"
   "cl-cffi-gtk-gdk" "cl-cffi-gtk-gdk-pixbuf" "cl-cffi-gtk-gio"
   "cl-cffi-gtk-glib" "cl-cffi-gtk-gobject" "cl-cffi-gtk-pango"
   "iterate" "trivial-features")
  ("cl-cffi-gtk-cairo" "cffi" "cl-cffi-gtk-glib" "iterate")
  ("cl-cffi-gtk-demo-cairo" "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-gdk" "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-glib" "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-gobject" "cl-cffi-gtk-gobject")
  ("cl-cffi-gtk-demo-gtk" "cl-cffi-gtk")
  ("cl-cffi-gtk-example-gtk" "cl-cffi-gtk")
  ("cl-cffi-gtk-gdk" "cffi" "cl-cffi-gtk-cairo"
   "cl-cffi-gtk-gdk-pixbuf" "cl-cffi-gtk-gio" "cl-cffi-gtk-glib"
   "cl-cffi-gtk-gobject" "cl-cffi-gtk-pango")
  ("cl-cffi-gtk-gdk-pixbuf" "cffi" "cl-cffi-gtk-glib"
   "cl-cffi-gtk-gobject")
  ("cl-cffi-gtk-gio" "cl-cffi-gtk-glib" "cl-cffi-gtk-gobject")
  ("cl-cffi-gtk-glib" "cffi" "iterate" "trivial-features")
  ("cl-cffi-gtk-gobject" "bordeaux-threads" "cffi" "cl-cffi-gtk-glib"
   "closer-mop" "iterate" "trivial-garbage")
  ("cl-cffi-gtk-pango" "cl-cffi-gtk-cairo" "cl-cffi-gtk-glib"
   "cl-cffi-gtk-gobject" "iterate")
  ("cl-charms" "alexandria" "cffi" "cffi-grovel")
  ("cl-charms-paint" "cl-charms") ("cl-charms-timer" "cl-charms")
  ("cl-cheshire-cat" "alexandria" "cl-fad" "cl-ppcre" "cl-store"
   "hunchentoot" "split-sequence" "usocket")
  ("cl-cli" "split-sequence") ("cl-colors" "alexandria" "let-plus")
  ("cl-colors-tests" "cl-colors" "lift")
  ("cl-conspack" "alexandria" "closer-mop" "fast-io" "ieee-floats"
   "trivial-garbage" "trivial-utf-8")
  ("cl-conspack-test" "checkl" "cl-conspack" "fiveam")
  ("cl-cont" "alexandria" "closer-mop")
  ("cl-cont-test" "cl-cont" "rt")
  ("cl-containers" "asdf-system-connections" "metatilities-base")
  ("cl-containers-test" "cl-containers" "lift")
  ("cl-cookie" "alexandria" "cl-ppcre" "local-time" "proc-parse"
   "quri")
  ("cl-cookie-test" "cl-cookie" "prove" "prove-asdf")
  ("cl-coroutine" "alexandria" "cl-cont")
  ("cl-coroutine-test" "cl-coroutine" "cl-test-more")
  ("cl-coveralls" "alexandria" "cl-fad" "cl-ppcre" "drakma"
   "flexi-streams" "ironclad" "jsown" "lquery" "split-sequence"
   "uiop")
  ("cl-coveralls-test" "cl-coveralls" "prove" "prove-asdf")
  ("cl-crc64") ("cl-creditcard" "iterate")
  ("cl-cron" "bordeaux-threads") ("cl-css")
  ("cl-csv" "alexandria" "cl-interpol" "iterate")
  ("cl-csv-clsql" "cl-csv" "clsql-helper" "data-table-clsql")
  ("cl-csv-data-table" "cl-csv" "data-table")
  ("cl-csv-test" "cl-csv" "lisp-unit2") ("cl-ctrnn") ("cl-curlex")
  ("cl-curlex-tests" "cl-curlex" "eos" "iterate")
  ("cl-custom-hash-table")
  ("cl-custom-hash-table-test" "cl-custom-hash-table"
   "hu.dwim.stefil")
  ("cl-data-frame" "alexandria" "anaphora" "array-operations"
   "cl-num-utils" "cl-slice" "let-plus")
  ("cl-data-frame-tests" "cl-data-frame" "clunit")
  ("cl-date-time-parser" "alexandria" "anaphora" "cl-ppcre"
   "local-time" "parse-float" "split-sequence")
  ("cl-dbi" "dbi") ("cl-devil" "alexandria" "cffi") ("cl-difflib")
  ("cl-difflib-tests" "cl-difflib") ("cl-dot" "uiop")
  ("cl-dropbox" "cl-json" "cl-oauth" "cl-ppcre" "drakma") ("cl-dsl")
  ("cl-dsl-tests" "cl-dsl" "eos") ("cl-durian") ("cl-emacs-if")
  ("cl-emb" "cl-ppcre") ("cl-epoch")
  ("cl-ewkb" "flexi-streams" "ieee-floats")
  ("cl-ewkb-tests" "cl-ewkb" "postmodern")
  ("cl-factoring" "cl-primality" "iterate")
  ("cl-factoring-test" "cl-factoring" "cl-primality" "iterate"
   "stefil")
  ("cl-fad" "alexandria" "bordeaux-threads")
  ("cl-fad-test" "cl-fad" "cl-ppcre" "unit-test")
  ("cl-fam" "cffi" "cffi-grovel" "trivial-garbage")
  ("cl-fastcgi" "cffi" "usocket") ("cl-fbclient" "cffi")
  ("cl-flowd" "cl-annot")
  ("cl-fluiddb" "bordeaux-threads" "cl-json" "drakma" "flexi-streams"
   "split-sequence")
  ("cl-fluiddb-test" "cl-fluiddb" "lift")
  ("cl-fluidinfo" "cl-fluiddb")
  ("cl-freetype2" "alexandria" "cffi" "cffi-grovel"
   "trivial-garbage")
  ("cl-freetype2-doc" "cl-freetype2" "cl-markdown" "cl-who")
  ("cl-freetype2-tests" "cl-freetype2" "fiveam")
  ("cl-fsnotify" "cffi" "cffi-grovel")
  ("cl-ftp" "split-sequence" "usocket")
  ("cl-fuse" "bordeaux-threads" "cffi" "cffi-grovel" "cl-utilities"
   "iterate" "trivial-backtrace" "trivial-utf-8")
  ("cl-fuse-meta-fs" "bordeaux-threads" "cl-fuse" "iterate" "pcall")
  ("cl-gap-buffer") ("cl-gd" "uffi") ("cl-gd-test" "cl-gd")
  ("cl-gdata" "alexandria" "cl-fad" "cl-json" "cl-ppcre" "closer-mop"
   "cxml" "drakma" "flexi-streams" "gzip-stream" "local-time"
   "parse-number" "split-sequence" "string-case" "trivial-utf-8"
   "url-rewrite" "xpath")
  ("cl-gearman" "alexandria" "babel" "split-sequence" "usocket")
  ("cl-gearman-test" "cl-gearman" "cl-test-more")
  ("cl-gendoc" "3bmd" "3bmd-ext-code-blocks" "cl-who")
  ("cl-gendoc-docs" "cl-gendoc") ("cl-gene-searcher" "clsql-sqlite3")
  ("cl-generic-arithmetic" "conduit-packages")
  ("cl-geocode" "acl-compat" "aserve" "cl-ppcre") ("cl-geoip" "cffi")
  ("cl-geometry" "iterate" "trees")
  ("cl-geometry-tests" "cl-geometry" "iterate" "vecto")
  ("cl-gists" "alexandria" "babel" "cl-syntax" "cl-syntax-annot"
   "dexador" "jonathan" "local-time" "osicat" "quri" "trivial-types")
  ("cl-gists-test" "cl-gists" "closer-mop" "prove" "prove-asdf")
  ("cl-git" "alexandria" "anaphora" "asdf" "cffi" "cffi-grovel"
   "cl-fad" "closer-mop" "flexi-streams" "local-time"
   "trivial-garbage" "uiop")
  ("cl-git/tests" "alexandria" "asdf" "cl-fad" "cl-git" "fiveam"
   "flexi-streams" "inferior-shell" "local-time" "unix-options")
  ("cl-github-v3" "alexandria" "drakma" "yason")
  ("cl-glfw" "cffi" "cl-glfw-types") ("cl-glfw-ftgl" "cffi")
  ("cl-glfw-glu" "cffi" "cl-glfw-types")
  ("cl-glfw-opengl-3dfx_multisample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-3dfx_tbuffer" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-3dfx_texture_compression_fxt1"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_blend_minmax_factor" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_depth_clamp_separate" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_draw_buffers_blend" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_multi_draw_indirect" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_name_gen_delete" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_performance_monitor" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_sample_positions" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_seamless_cubemap_per_texture"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-amd_vertex_shader_tesselator"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_aux_depth_stencil" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_client_storage" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_element_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_fence" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_float_pixels" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_flush_buffer_range" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_object_purgeable" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_rgb_422" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_row_bytes" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_specular_vector" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_texture_range" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_transform_hint" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_vertex_array_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_vertex_array_range" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_vertex_program_evaluators"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-apple_ycbcr_422" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_blend_func_extended" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_color_buffer_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_copy_buffer" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_depth_buffer_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_depth_clamp" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_depth_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_draw_buffers" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_draw_buffers_blend" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_draw_elements_base_vertex"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_draw_indirect" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_draw_instanced" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_es2_compatibility" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_fragment_program" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_fragment_shader" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_framebuffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_framebuffer_object_deprecated"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_framebuffer_srgb" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_geometry_shader4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_get_program_binary" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_gpu_shader5" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_gpu_shader_fp64" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_half_float_pixel" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_half_float_vertex" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_imaging" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_imaging_deprecated" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_instanced_arrays" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_map_buffer_range" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_matrix_palette" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_multisample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_multitexture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_occlusion_query" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_occlusion_query2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_pixel_buffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_point_parameters" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_point_sprite" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_provoking_vertex" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_robustness" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_sample_shading" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_sampler_objects" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_seamless_cube_map" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_separate_shader_objects"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_shader_objects" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_shader_subroutine" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_shading_language_100" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_shading_language_include"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_shadow" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_shadow_ambient" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_tessellation_shader" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_border_clamp" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_buffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_buffer_object_rgb32"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_compression" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_compression_bptc"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_compression_rgtc"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_cube_map" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_cube_map_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_env_combine" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_env_dot3" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_gather" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_mirrored_repeat"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_multisample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_rectangle" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_rg" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_rgb10_a2ui" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_texture_swizzle" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_timer_query" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_transform_feedback2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_transpose_matrix" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_uniform_buffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_array_bgra" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_array_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_attrib_64bit" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_blend" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_buffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_program" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_shader" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_vertex_type_2_10_10_10_rev"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_viewport_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-arb_window_pos" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_draw_buffers" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_element_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_envmap_bumpmap" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_fragment_shader" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_map_object_buffer" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_meminfo" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_pixel_format_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_pn_triangles" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_separate_stencil" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_text_fragment_shader" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_texture_env_combine3" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_texture_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_texture_mirror_once" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_vertex_array_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_vertex_attrib_array_object"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ati_vertex_streams" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-core" "cffi" "cl-glfw-types")
  ("cl-glfw-opengl-ext_422_pixels" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_abgr" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_bgra" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_bindable_uniform" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_blend_color" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_blend_equation_separate"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_blend_func_separate" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_blend_minmax" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_blend_subtract" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_clip_volume_hint" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_cmyka" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_color_subtable" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_compiled_vertex_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_convolution" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_coordinate_frame" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_copy_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_cull_vertex" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_depth_bounds_test" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_direct_state_access" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_draw_buffers2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_draw_instanced" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_draw_range_elements" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_fog_coord" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_framebuffer_blit" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_framebuffer_multisample"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_framebuffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_framebuffer_srgb" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_geometry_shader4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_gpu_program_parameters" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_gpu_shader4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_histogram" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_index_array_formats" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_index_func" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_index_material" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_light_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_multi_draw_arrays" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_multisample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_packed_depth_stencil" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_packed_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_packed_pixels" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_paletted_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_pixel_buffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_pixel_transform" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_point_parameters" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_polygon_offset" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_provoking_vertex" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_secondary_color" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_separate_shader_objects"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_separate_specular_color"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_shader_image_load_store"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_stencil_clear_tag" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_stencil_two_side" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_stencil_wrap" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_subtexture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture3d" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_buffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_compression_latc"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_compression_rgtc"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_compression_s3tc"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_cube_map" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_env_combine" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_env_dot3" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_filter_anisotropic"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_integer" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_lod_bias" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_mirror_clamp" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_perturb_normal" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_shared_exponent"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_snorm" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_srgb" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_srgb_decode" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_texture_swizzle" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_timer_query" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_transform_feedback" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_vertex_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_vertex_array_bgra" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_vertex_attrib_64bit" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_vertex_shader" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ext_vertex_weighting" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-gremedy_frame_terminator" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-gremedy_string_marker" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-hp_convolution_border_modes"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-hp_image_transform" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-hp_occlusion_test" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-hp_texture_lighting" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ibm_cull_vertex" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ibm_multimode_draw_arrays" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ibm_rasterpos_clip" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ibm_texture_mirrored_repeat"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ibm_vertex_array_lists" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ingr_blend_func_separate" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ingr_color_clamp" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-ingr_interlace_read" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-intel_parallel_arrays" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_pack_invert" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_packed_depth_stencil" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_program_debug" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_resize_buffers" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_shader_debug" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_trace" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_window_pos" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesa_ycbcr_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-mesax_texture_stack" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_conditional_render" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_copy_depth_to_color" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_copy_image" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_depth_buffer_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_depth_clamp" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_evaluators" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_explicit_multisample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_fence" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_float_buffer" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_fog_distance" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_fragment_program" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_fragment_program2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_framebuffer_multisample_coverage"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_geometry_program4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_gpu_program4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_gpu_program5" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_gpu_shader5" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_half_float" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_light_max_exponent" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_multisample_coverage" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_multisample_filter_hint" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_occlusion_query" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_packed_depth_stencil" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_parameter_buffer_object" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_pixel_data_range" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_point_sprite" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_present_video" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_primitive_restart" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_register_combiners" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_register_combiners2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_shader_buffer_load" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_shader_buffer_store" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_tessellation_program5" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texgen_emboss" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texgen_reflection" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_barrier" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_env_combine4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_expand_normal" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_multisample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_rectangle" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_shader" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_shader2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_texture_shader3" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_transform_feedback" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_transform_feedback2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_array_range" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_array_range2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_attrib_integer_64bit"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_buffer_unified_memory"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_program" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_program2_option" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_program3" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-nv_vertex_program4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-oes_read_format" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-oml_interlace" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-oml_resample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-oml_subsample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-pgi_misc_hints" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-pgi_vertex_hints" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-rend_screen_coordinates" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-s3_s3tc" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgi_color_table" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgi_depth_pass_instrument" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_detail_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_fog_function" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_multisample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_pixel_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_point_parameters" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_sharpen_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_texture4d" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_texture_color_mask" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_texture_filter4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgis_texture_select" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_async" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_depth_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_flush_raster" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_fog_scale" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_fragment_lighting" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_framezoom" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_igloo_interface" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_instruments" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_line_quality_hint" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_list_priority" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_pixel_texture" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_polynomial_ffd" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_reference_plane" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_resample" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_scalebias_hint" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_shadow" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_shadow_ambient" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_slim" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_sprite" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_tag_sample_buffer" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_texture_coordinate_clamp"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_texture_lod_bias" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_texture_multi_buffer" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sgix_ycrcba" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sun_convolution_border_modes"
   "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sun_global_alpha" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sun_mesh_array" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sun_slice_accum" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sun_triangle_list" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sun_vertex" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-sunx_constant_data" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_1_0" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_1_1" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_1_2" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_1_3" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_1_4" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_1_5" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_2_0" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-version_2_1" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-win_phong_shading" "cl-glfw-opengl-core")
  ("cl-glfw-opengl-win_specular_fog" "cl-glfw-opengl-core")
  ("cl-glfw-types" "cffi")
  ("cl-glfw3" "alexandria" "cffi" "cffi-libffi")
  ("cl-glfw3-examples" "cl-glfw3" "cl-opengl")
  ("cl-glu" "cffi" "cl-opengl")
  ("cl-glut" "alexandria" "cffi" "cl-opengl")
  ("cl-glut-examples" "cffi" "cl-glu" "cl-glut" "cl-opengl")
  ("cl-gobject-introspection" "alexandria" "cffi" "iterate"
   "trivial-garbage")
  ("cl-gobject-introspection-test" "cl-gobject-introspection"
   "fiveam" "iterate")
  ("cl-gpu" "cffi" "cl-gpu.core" "hu.dwim.asdf")
  ("cl-gpu.buffers" "cffi" "hu.dwim.asdf" "hu.dwim.def+contextl"
   "hu.dwim.util" "trivial-garbage")
  ("cl-gpu.core" "bordeaux-threads" "cffi" "cl-gpu.buffers"
   "hu.dwim.asdf" "hu.dwim.walker")
  ("cl-gpu.test" "cffi" "cl-gpu" "hu.dwim.asdf"
   "hu.dwim.stefil+hu.dwim.def")
  ("cl-grace" "cl-fad")
  ("cl-graph" "asdf-system-connections" "cl-containers"
   "metabang-bind" "metatilities-base")
  ("cl-graph+hu.dwim.graphviz" "cl-graph" "hu.dwim.graphviz")
  ("cl-graph-and-cl-mathstats" "cl-graph" "cl-mathstats")
  ("cl-graph-and-dynamic-classes" "cl-graph" "dynamic-classes")
  ("cl-graph-and-metacopy" "cl-graph" "metacopy")
  ("cl-graph-and-moptilities" "cl-graph" "moptilities")
  ("cl-graph-test" "cl-graph" "lift")
  ("cl-growl" "flexi-streams" "ironclad" "trivial-utf-8"
   "usocket-udp")
  ("cl-gss" "cffi" "cffi-grovel" "trivial-garbage" "trivial-utf-8")
  ("cl-gtk2-cairo" "cffi" "cl-cairo2" "cl-gtk2-gdk" "cl-gtk2-glib"
   "cl-gtk2-gtk" "iterate")
  ("cl-gtk2-gdk" "cffi" "cl-gtk2-glib" "cl-gtk2-pango")
  ("cl-gtk2-glib" "bordeaux-threads" "cffi" "closer-mop" "iterate"
   "trivial-garbage")
  ("cl-gtk2-gtk" "bordeaux-threads" "cffi" "cl-gtk2-gdk"
   "cl-gtk2-glib" "cl-gtk2-pango" "iterate")
  ("cl-gtk2-pango" "cl-gtk2-glib" "iterate") ("cl-haml" "cl-who")
  ("cl-haml-test" "cl-haml" "cl-test-more") ("cl-hash-util")
  ("cl-heap") ("cl-heap-tests" "cl-heap" "xlunit") ("cl-heredoc")
  ("cl-heredoc-test" "cl-heredoc" "stefil")
  ("cl-hooks" "alexandria" "closer-mop" "let-plus" "trivial-garbage")
  ("cl-hooks-test" "cl-hooks" "lift") ("cl-html-diff" "cl-difflib")
  ("cl-html-parse")
  ("cl-html5-parser" "cl-ppcre" "flexi-streams" "string-case")
  ("cl-html5-parser-cxml" "cl-html5-parser" "cxml")
  ("cl-html5-parser-tests" "cl-html5-parser" "cl-json"
   "split-sequence" "stefil")
  ("cl-hue" "alexandria" "drakma" "yason")
  ("cl-i18n" "alexandria" "babel" "cl-ppcre-unicode")
  ("cl-ilu" "alexandria" "cffi" "cl-devil")
  ("cl-ilut" "alexandria" "cffi" "cl-devil")
  ("cl-indeterminism" "alexandria" "cl-curlex" "hu.dwim.walker")
  ("cl-indeterminism-tests" "cl-indeterminism" "fiveam")
  ("cl-inflector" "alexandria" "cl-ppcre")
  ("cl-inflector-test" "cl-inflector" "lisp-unit2")
  ("cl-influxdb" "cl-annot" "cl-json" "do-urlencode" "drakma"
   "flexi-streams")
  ("cl-influxdb.doc" "cl-annot" "cl-influxdb" "cl-influxdb.examples"
   "cl-influxdb.examples-async")
  ("cl-influxdb.examples" "cl-annot" "cl-influxdb" "parse-number"
   "split-sequence")
  ("cl-influxdb.examples-async" "cl-annot" "cl-csv-data-table"
   "cl-influxdb" "data-table" "lparallel" "parse-number"
   "split-sequence")
  ("cl-inotify" "binary-types" "cffi" "cffi-grovel" "iolib"
   "iolib.base" "osicat" "trivial-utf-8")
  ("cl-intbytes" "fast-io")
  ("cl-intbytes-test" "cl-intbytes" "prove" "prove-asdf")
  ("cl-interpol" "cl-unicode")
  ("cl-interpol-test" "cl-interpol" "flexi-streams")
  ("cl-irc" "flexi-streams" "split-sequence" "usocket")
  ("cl-irc-test" "cl-irc" "rt" "split-sequence")
  ("cl-irregsexp" "alexandria")
  ("cl-irregsexp-test" "cl-irregsexp" "fiveam") ("cl-isaac")
  ("cl-jpeg") ("cl-js" "cl-ppcre" "local-time" "parse-js")
  ("cl-json") ("cl-json.test" "cl-json" "fiveam")
  ("cl-junit-xml" "alexandria" "cxml" "iterate")
  ("cl-junit-xml.lisp-unit" "alexandria" "cl-junit-xml" "cl-ppcre"
   "iterate" "lisp-unit")
  ("cl-junit-xml.lisp-unit2" "alexandria" "cl-junit-xml" "cl-ppcre"
   "iterate" "lisp-unit2")
  ("cl-junit-xml.test" "cl-junit-xml" "lisp-unit2")
  ("cl-kyoto-cabinet" "cffi")
  ("cl-l10n" "alexandria" "cl-fad" "cl-l10n-cldr" "cl-ppcre"
   "closer-mop" "cxml" "flexi-streams" "iterate" "local-time"
   "metabang-bind")
  ("cl-l10n-cldr")
  ("cl-l10n.test" "cl-l10n" "hu.dwim.stefil" "parse-number")
  ("cl-larval" "alexandria" "cl-curlex" "cl-interpol"
   "cl-package-locks" "iterate" "named-readtables" "rutils" "swank")
  ("cl-lastfm" "cxml-stp" "drakma" "trivial-utf-8" "url-rewrite")
  ("cl-lastfm-test" "cl-lastfm" "lisp-unit") ("cl-launch" "asdf")
  ("cl-ledger" "cambl" "cl-ppcre" "local-time" "periods-series")
  ("cl-lex" "cl-ppcre")
  ("cl-liballegro" "cffi" "cffi-libffi" "trivial-garbage")
  ("cl-libevent2" "cffi") ("cl-libevent2-ssl" "cffi" "cl-libevent2")
  ("cl-liblinear" "cffi" "trivial-garbage") ("cl-libpuzzle" "cffi")
  ("cl-libpuzzle-test" "cl-libpuzzle" "cl-test-more")
  ("cl-libsvm" "cffi" "trivial-garbage")
  ("cl-libusb" "libusb-ffi" "trivial-garbage")
  ("cl-libuv" "alexandria" "cffi" "cffi-grovel")
  ("cl-libxml2" "alexandria" "cffi" "flexi-streams" "garbage-pools"
   "iterate" "metabang-bind" "puri")
  ("cl-libxml2-test" "cl-libxml2" "lift") ("cl-libyaml" "cffi")
  ("cl-libyaml-test" "cl-libyaml" "fiveam") ("cl-lite" "glisp")
  ("cl-locale" "anaphora" "arnesi" "cl-annot" "cl-syntax"
   "cl-syntax-annot")
  ("cl-locale-syntax" "cl-locale" "cl-syntax") ("cl-locatives")
  ("cl-log") ("cl-log-test" "cl-log" "eos")
  ("cl-logic" "alexandria" "quine-mccluskey") ("cl-ltsv")
  ("cl-ltsv-test" "cl-ltsv" "cl-test-more")
  ("cl-m4" "alexandria" "cffi" "cffi-grovel" "cl-fad" "cl-ppcre"
   "external-program" "graylex")
  ("cl-m4-test" "cl-heredoc" "cl-m4" "hu.dwim.stefil")
  ("cl-markdown" "anaphora" "cl-containers" "cl-ppcre"
   "dynamic-classes" "metabang-bind" "metatilities-base")
  ("cl-markdown-comparisons" "cl-html-diff" "cl-markdown"
   "html-encode" "lift" "lml2" "trivial-shell")
  ("cl-markdown-test" "cl-markdown" "lift" "trivial-shell")
  ("cl-marklogic" "drakma" "fiveam" "local-time") ("cl-markup")
  ("cl-markup-test" "cl-markup" "cl-test-more")
  ("cl-match" "standard-cl")
  ("cl-match-test" "cl-match" "pcl-unit-test")
  ("cl-mathstats" "cl-containers" "metatilities-base")
  ("cl-mathstats-test" "cl-mathstats" "lift")
  ("cl-mechanize" "cl-ppcre" "closure-html" "cxml-stp" "drakma"
   "puri")
  ("cl-mediawiki" "alexandria" "cxml" "drakma")
  ("cl-memcached" "babel" "pooler" "split-sequence" "usocket")
  ("cl-messagepack" "access" "babel" "cl-json" "closer-mop" "fiveam"
   "flexi-streams")
  ("cl-migrations" "clsql")
  ("cl-mime" "cl-base64" "cl-ppcre" "cl-qprint")
  ("cl-mock" "cl-mock-basic" "optima")
  ("cl-mock-basic" "alexandria" "closer-mop")
  ("cl-mock-tests" "cl-mock" "cl-mock-tests-basic")
  ("cl-mock-tests-basic" "cl-mock-basic" "fiveam")
  ("cl-monad-macros") ("cl-moneris" "drakma" "s-xml")
  ("cl-moneris-test" "cl-moneris" "eos")
  ("cl-mongo" "babel" "bordeaux-threads" "documentation-template"
   "lisp-unit" "parenscript" "split-sequence" "usocket" "uuid")
  ("cl-mongo-id" "bordeaux-threads" "local-time" "md5") ("cl-mop")
  ("cl-mpi" "cffi" "cffi-grovel") ("cl-mpi-test" "cl-mpi" "par-eval")
  ("cl-mtgnet" "cl-json" "cl-netstring+" "trivial-utf-8" "usocket")
  ("cl-murmurhash" "babel") ("cl-mustache" "uiop")
  ("cl-mustache-test" "cl-mustache" "prove" "prove-asdf")
  ("cl-mw" "alexandria" "cffi" "cl-ppcre" "hu.dwim.serializer"
   "iolib")
  ("cl-mw.examples.argument-processing" "cl-mw")
  ("cl-mw.examples.hello-world" "cl-mw")
  ("cl-mw.examples.higher-order" "cl-mw")
  ("cl-mw.examples.monte-carlo-pi" "cl-mw")
  ("cl-mw.examples.ping" "cl-mw")
  ("cl-mw.examples.with-task-policy" "cl-mw") ("cl-mysql" "cffi")
  ("cl-mysql-test" "cl-mysql" "stefil") ("cl-ncurses" "uffi")
  ("cl-neo4j" "alexandria" "anaphora" "babel" "cl-json" "cl-ppcre"
   "drakma" "split-sequence")
  ("cl-neo4j.tests" "cl-neo4j" "fiveam")
  ("cl-netstring+" "flexi-streams" "trivial-utf-8")
  ("cl-netstrings" "arnesi" "iterate") ("cl-ntriples" "alexandria")
  ("cl-num-utils" "alexandria" "anaphora" "array-operations"
   "cl-slice" "let-plus")
  ("cl-num-utils-tests" "cl-num-utils" "clunit")
  ("cl-oauth" "alexandria" "anaphora" "babel" "cl-base64"
   "closer-mop" "drakma" "f-underscore" "hunchentoot" "ironclad"
   "puri" "split-sequence" "trivial-garbage")
  ("cl-oauth.tests" "cl-oauth" "fiveam") ("cl-olefs")
  ("cl-one-time-passwords" "ironclad")
  ("cl-one-time-passwords-test" "cl-one-time-passwords" "fiveam")
  ("cl-oneliner" "cl-ppcre" "lisp-unit" "split-sequence") ("cl-op")
  ("cl-openal" "cffi")
  ("cl-openal-examples" "cffi" "cl-alc" "cl-alut" "cl-openal")
  ("cl-opengl" "alexandria" "cffi")
  ("cl-openid" "bordeaux-threads" "cl-base64" "cl-html-parse"
   "drakma" "ironclad" "puri" "secure-random" "split-sequence"
   "trivial-utf-8" "xmls")
  ("cl-openid.test" "cl-openid" "fiveam" "flexi-streams")
  ("cl-openstack-client" "alexandria" "cl-json" "drakma" "local-time"
   "uri-template")
  ("cl-openstack-client-test" "chunga" "cl-openstack-client"
   "cl-ppcre" "drakma" "fiveam" "flexi-streams" "local-time"
   "trivial-gray-streams")
  ("cl-opsresearch") ("cl-org-mode" "alexandria" "closer-mop")
  ("cl-package-locks") ("cl-parallel" "bordeaux-threads")
  ("cl-pass" "ironclad" "split-sequence" "trivial-utf-8")
  ("cl-pass-test" "cl-pass" "fiveam") ("cl-paths")
  ("cl-paths-ttf" "cl-paths" "zpb-ttf")
  ("cl-pattern" "alexandria" "cl-annot" "cl-syntax"
   "cl-syntax-annot")
  ("cl-pattern-benchmark" "cl-pattern")
  ("cl-paymill" "cl+ssl" "drakma" "st-json")
  ("cl-paypal" "cl-ppcre" "drakma" "hunchentoot")
  ("cl-pdf" "iterate" "zpb-ttf")
  ("cl-pdf-doc" "cl-pdf" "cl-typesetting") ("cl-pdf-parser" "cl-pdf")
  ("cl-performance-tuning-helper")
  ("cl-performance-tuning-helper-test" "cl-performance-tuning-helper"
   "rt")
  ("cl-permutation") ("cl-permutation-examples" "cl-permutation")
  ("cl-permutation-tests" "cl-permutation" "fiveam")
  ("cl-photo" "kmrcl") ("cl-photo-tests" "cl-photo" "rt")
  ("cl-plplot" "cffi")
  ("cl-plumbing" "bordeaux-threads" "iterate" "trivial-gray-streams")
  ("cl-plumbing-test" "cl-plumbing" "iterate" "stefil")
  ("cl-plus-c" "cl-autowrap") ("cl-ply" "cl-pattern" "cl-ppcre")
  ("cl-ply-test" "cl-ply" "prove" "prove-asdf") ("cl-poker-eval")
  ("cl-pop" "cl-ppcre" "usocket") ("cl-popen" "iolib.streams")
  ("cl-popen-test" "cl-popen" "lift") ("cl-portaudio" "cffi" "ffa")
  ("cl-portaudio-doc" "atdoc" "cl-portaudio")
  ("cl-portaudio-tests" "cl-portaudio") ("cl-postgres" "md5")
  ("cl-postgres+local-time" "cl-postgres" "local-time")
  ("cl-postgres+local-time-duration" "cl-postgres"
   "local-time-duration")
  ("cl-postgres-tests" "cl-postgres" "eos" "simple-date")
  ("cl-ppcre") ("cl-ppcre-template" "cl-ppcre" "cl-unification")
  ("cl-ppcre-test" "cl-ppcre" "flexi-streams")
  ("cl-ppcre-unicode" "cl-ppcre" "cl-unicode")
  ("cl-ppcre-unicode-test" "cl-ppcre-test" "cl-ppcre-unicode")
  ("cl-prevalence" "s-sysdeps" "s-xml")
  ("cl-prevalence-test" "cl-prevalence" "fiveam")
  ("cl-primality" "iterate")
  ("cl-primality-test" "cl-primality" "iterate" "stefil")
  ("cl-prime-maker") ("cl-proj" "cffi" "parse-number")
  ("cl-project" "anaphora" "cl-annot" "cl-emb" "cl-fad" "cl-ppcre"
   "cl-syntax" "cl-syntax-annot" "local-time" "prove")
  ("cl-project-test" "cl-project" "prove" "prove-asdf")
  ("cl-protobufs" "babel" "closer-mop" "trivial-garbage")
  ("cl-protobufs-tests" "cl-protobufs")
  ("cl-pslib" "alexandria" "cffi" "cl-colors" "cl-ppcre-unicode")
  ("cl-pslib-barcode" "alexandria" "cffi" "cl-colors"
   "cl-ppcre-unicode" "cl-pslib")
  ("cl-qprint" "flexi-streams") ("cl-qrencode" "zpng")
  ("cl-qrencode-test" "cl-qrencode") ("cl-quakeinfo" "cl-geocode")
  ("cl-quickcheck")
  ("cl-rabbit" "alexandria" "babel" "cffi" "cffi-grovel"
   "cffi-libffi" "cl-ppcre")
  ("cl-rabbit-tests" "cl-rabbit" "fiveam") ("cl-randist")
  ("cl-random" "alexandria" "anaphora" "array-operations"
   "cl-num-utils" "cl-rmath" "cl-slice" "gsll" "let-plus" "lla")
  ("cl-random-tests" "cl-random" "clunit")
  ("cl-rdfxml" "cxml" "puri")
  ("cl-read-macro-tokens" "defmacro-enhance")
  ("cl-read-macro-tokens-tests" "cl-read-macro-tokens" "fiveam"
   "iterate")
  ("cl-readline" "alexandria" "cffi")
  ("cl-recaptcha" "cl-ppcre" "drakma" "flexi-streams" "jsown")
  ("cl-reddit" "drakma" "yason")
  ("cl-redis" "babel" "cl-ppcre" "flexi-streams" "rutils" "usocket")
  ("cl-redis-test" "bordeaux-threads" "cl-redis" "flexi-streams"
   "should-test")
  ("cl-reexport" "alexandria")
  ("cl-reexport-test" "cl-reexport" "cl-test-more")
  ("cl-rethinkdb" "blackbird" "cl-async" "cl-base64" "cl-hash-util"
   "cl-ppcre" "event-glue" "fast-io" "jonathan" "local-time" "vom")
  ("cl-rethinkdb-test" "blackbird" "cl-async" "cl-ppcre"
   "cl-rethinkdb" "fiveam")
  ("cl-rfc2047" "babel" "cl-base64")
  ("cl-rfc2047-test" "cl-ppcre" "cl-rfc2047" "lift")
  ("cl-riff" "alexandria") ("cl-rlimit" "cffi" "cffi-grovel")
  ("cl-rmath" "cffi") ("cl-rrd" "cffi")
  ("cl-rrt" "alexandria" "anaphora" "cl-syntax-annot" "iterate")
  ("cl-rrt.benchmark" "cl-rrt" "cl-rrt.rtree" "cl-rrt.test" "fiveam"
   "vecto")
  ("cl-rrt.rtree" "alexandria" "anaphora" "cl-rrt" "cl-syntax-annot"
   "iterate" "optima" "spatial-trees" "spatial-trees.nns")
  ("cl-rrt.test" "cl-rrt" "cl-rrt.rtree" "fiveam" "vecto")
  ("cl-rsvg2" "cffi" "cl-cairo2" "cl-gtk2-glib"
   "trivial-gray-streams")
  ("cl-rsvg2-pixbuf" "cl-gtk2-gdk" "cl-rsvg2")
  ("cl-rsvg2-test" "asdf" "cffi" "cl-rsvg2" "eos")
  ("cl-s3" "ironclad" "s-base64" "s-http-client" "s-utils" "s-xml")
  ("cl-sam" "deoxybyte-gzip" "deoxybyte-systems" "deoxybyte-unix")
  ("cl-sam-test" "cl-sam" "deoxybyte-io" "lift")
  ("cl-sasl" "ironclad") ("cl-scribd" "cxml" "drakma" "ironclad")
  ("cl-scripting")
  ("cl-scrobbler" "arnesi" "cl-store" "drakma" "flexi-streams" "md5"
   "st-json")
  ("cl-scrobbler-tests" "cl-scrobbler" "fiveam")
  ("cl-secure-read" "alexandria" "defmacro-enhance" "iterate"
   "named-readtables" "rutils" "yaclanapht")
  ("cl-secure-read-tests" "cl-secure-read" "fiveam")
  ("cl-sendmail" "babel-streams" "cl-base64" "cl-mime" "cl-qprint"
   "external-program" "trivial-gray-streams" "xmls" "xmls-tools")
  ("cl-sentiment" "cl-ppcre" "rt")
  ("cl-server-manager" "alexandria" "hunchentoot" "prepl" "swank")
  ("cl-shellwords" "cl-ppcre")
  ("cl-shellwords-test" "cl-shellwords" "prove")
  ("cl-simple-concurrent-jobs" "bordeaux-threads" "chanl")
  ("cl-simple-table") ("cl-singleton-mixin" "closer-mop" "metap")
  ("cl-singleton-mixin-test" "cl-singleton-mixin" "fiveam")
  ("cl-skip-list" "cffi")
  ("cl-slice" "alexandria" "anaphora" "let-plus")
  ("cl-slice-tests" "cl-slice" "clunit") ("cl-slp" "cffi")
  ("cl-slug" "cl-ppcre")
  ("cl-slug-test" "cl-slug" "prove" "prove-asdf")
  ("cl-smtp" "cl+ssl" "cl-base64" "flexi-streams"
   "trivial-gray-streams" "usocket")
  ("cl-sophia" "alexandria" "cffi" "cl-fad")
  ("cl-sophia-test" "alexandria" "cl-fad" "cl-sophia" "lisp-unit")
  ("cl-soup" "cffi") ("cl-spark")
  ("cl-spark-test" "cl-spark" "fiveam") ("cl-speedy-queue")
  ("cl-splicing-macro")
  ("cl-stm" "arnesi" "bordeaux-threads" "closer-mop")
  ("cl-stm.test" "cl-stm" "fiveam") ("cl-stomp" "babel" "usocket")
  ("cl-stopwatch") ("cl-store") ("cl-store-tests" "cl-store" "rt")
  ("cl-strftime" "alexandria" "cl-ppcre" "local-time" "serapeum")
  ("cl-strftime/tests" "cffi" "cl-strftime" "fiveam" "uiop")
  ("cl-string-complete")
  ("cl-string-match" "alexandria" "ascii-strings" "iterate"
   "jpl-queues" "yacc")
  ("cl-string-match-test" "cl-string-match" "lisp-unit") ("cl-svg")
  ("cl-svm")
  ("cl-swap-file-0.5" "cl-binary-file-0.4" "cl-wal-0.4"
   "trivial-garbage")
  ("cl-syntax" "named-readtables" "trivial-types")
  ("cl-syntax-annot" "cl-annot" "cl-syntax")
  ("cl-syntax-anonfun" "cl-anonfun" "cl-syntax")
  ("cl-syntax-clsql" "cl-syntax" "clsql")
  ("cl-syntax-fare-quasiquote" "cl-syntax" "fare-quasiquote")
  ("cl-syntax-interpol" "cl-interpol" "cl-syntax")
  ("cl-syntax-markup" "cl-markup" "cl-syntax")
  ("cl-syslog" "cffi" "simple-date-time" "usocket")
  ("cl-syslog-tests" "cl-syslog" "nst") ("cl-table" "iterate")
  ("cl-template") ("cl-template-tests" "cl-template" "fiveam")
  ("cl-test-more" "prove") ("cl-tga") ("cl-tidy" "cffi")
  ("cl-timing") ("cl-tk" "cffi") ("cl-tld")
  ("cl-tokyo-cabinet" "cffi" "deoxybyte-systems")
  ("cl-tokyo-cabinet-test" "cl-tokyo-cabinet" "deoxybyte-io"
   "deoxybyte-utilities" "lift")
  ("cl-tulip-graph") ("cl-tuples" "alexandria" "iterate")
  ("cl-twit-repl" "cl-twitter")
  ("cl-twitter" "anaphora" "cl-json" "cl-oauth" "cl-ppcre"
   "closer-mop" "drakma" "trivial-http" "url-rewrite")
  ("cl-typesetting" "cl-pdf")
  ("cl-uglify-js" "cl-ppcre" "cl-ppcre-unicode" "iterate" "parse-js"
   "parse-number")
  ("cl-unicode") ("cl-unicode/base" "cl-ppcre")
  ("cl-unicode/build" "cl-unicode" "flexi-streams")
  ("cl-unicode/test" "cl-unicode") ("cl-unification")
  ("cl-unification-lib" "cl-ppcre" "cl-unification")
  ("cl-unification-test" "cl-unification" "ptester") ("cl-utilities")
  ("cl-v4l2" "cffi-grovel" "closer-mop" "iolib.syscalls"
   "trivial-garbage")
  ("cl-variates" "asdf-system-connections")
  ("cl-vectors" "cl-aa" "cl-paths")
  ("cl-virtualbox" "alexandria" "cl-ppcre" "uiop" "usocket")
  ("cl-voxelize" "alexandria")
  ("cl-voxelize-examples" "cl-ply" "cl-voxelize")
  ("cl-voxelize-test" "cl-voxelize" "prove" "prove-asdf")
  ("cl-wal-0.4" "cl-binary-file-0.4")
  ("cl-wav" "alexandria" "cl-riff")
  ("cl-web-crawler" "cl-html-parse" "cl-ppcre" "drakma" "puri")
  ("cl-webdav" "cl-fad" "cxml" "hunchentoot") ("cl-who")
  ("cl-who-test" "cl-who" "flexi-streams") ("cl-xkeysym")
  ("cl-xmlspam" "cl-ppcre" "cxml")
  ("cl-xmpp" "cxml" "ironclad" "usocket")
  ("cl-xmpp-sasl" "cl-base64" "cl-sasl" "cl-xmpp")
  ("cl-xmpp-tls" "cl+ssl" "cl-xmpp-sasl") ("cl-xspf" "s-xml")
  ("cl-xul" "alexandria" "cl-fad" "cl-json" "closer-mop" "clws"
   "cxml" "log5" "md5" "parenscript")
  ("cl-xul-test" "cl-xul" "fiveam")
  ("cl-yaclyaml" "alexandria" "cl-interpol" "cl-ppcre"
   "cl-read-macro-tokens" "cl-test-more" "defmacro-enhance"
   "esrap-liquid" "iterate" "parse-number" "rutils" "yaclanapht")
  ("cl-yaclyaml-tests" "cl-interpol" "cl-yaclyaml" "fiveam")
  ("cl-yahoo-finance" "babel" "cl-csv" "drakma" "url-rewrite"
   "yason")
  ("cl-yaml" "alexandria" "cl-libyaml" "cl-ppcre" "parse-number")
  ("cl-yaml-test" "alexandria" "cl-fad" "cl-yaml" "fiveam"
   "generic-comparability" "trivial-benchmark" "yason")
  ("cl4store" "cl-ppcre" "cl-rdfxml" "drakma" "log5"
   "parser-combinators" "puri" "split-sequence")
  ("cl4store-tests" "cl4store" "fiveam")
  ("clache" "alexandria" "babel" "cl-annot" "cl-fad" "cl-store"
   "cl-syntax" "cl-syntax-annot" "ironclad" "trivial-garbage")
  ("clache-test" "cl-test-more" "clache")
  ("clack" "alexandria" "bordeaux-threads" "lack" "lack-util")
  ("clack-errors" "cl-ppcre" "clack" "closer-mop" "djula"
   "local-time" "trivial-backtrace")
  ("clack-errors-demo" "cl-markup" "clack-errors")
  ("clack-errors-test" "clack" "clack-errors" "drakma" "fiveam")
  ("clack-handler-fcgi" "alexandria" "cl-fastcgi" "flexi-streams"
   "quri" "usocket")
  ("clack-handler-hunchentoot" "alexandria" "bordeaux-threads"
   "flexi-streams" "hunchentoot" "split-sequence")
  ("clack-handler-toot" "alexandria" "bordeaux-threads" "cl-ppcre"
   "flexi-streams" "split-sequence" "toot")
  ("clack-handler-woo" "woo")
  ("clack-handler-wookie" "alexandria" "babel" "cl-async" "fast-http"
   "fast-io" "flexi-streams" "quri" "split-sequence" "wookie")
  ("clack-middleware-auth-basic" "arnesi" "cl-base64" "cl-ppcre"
   "cl-syntax" "cl-syntax-annot" "clack-v1-compat")
  ("clack-middleware-clsql" "cl-syntax" "cl-syntax-annot"
   "clack-v1-compat" "clsql")
  ("clack-middleware-csrf" "alexandria" "cl-syntax" "cl-syntax-annot"
   "clack-v1-compat" "lack-util")
  ("clack-middleware-dbi" "cl-syntax" "cl-syntax-annot"
   "clack-v1-compat" "dbi")
  ("clack-middleware-oauth" "cl-oauth" "cl-syntax" "cl-syntax-annot"
   "clack-v1-compat")
  ("clack-middleware-postmodern" "cl-syntax" "cl-syntax-annot"
   "clack-v1-compat" "postmodern")
  ("clack-middleware-rucksack" "cl-syntax" "cl-syntax-annot"
   "clack-v1-compat" "rucksack")
  ("clack-session-store-dbi" "cl-base64" "clack-v1-compat" "dbi"
   "marshal")
  ("clack-test" "bordeaux-threads" "clack" "drakma" "flexi-streams"
   "http-body" "prove" "usocket")
  ("clack-v1-compat" "alexandria" "circular-streams" "cl-base64"
   "cl-fad" "cl-ppcre" "cl-syntax-annot" "clack" "clack-test"
   "flexi-streams" "http-body" "ironclad" "lack" "lack-util"
   "local-time" "marshal" "quri" "split-sequence" "trivial-backtrace"
   "trivial-mimes" "trivial-types")
  ("classimp" "cffi")
  ("classimp-samples" "cl-fad" "cl-glu" "cl-glut" "cl-ilut"
   "classimp")
  ("clavatar" "babel" "drakma" "iolib" "ironclad")
  ("clavier" "alexandria" "chronicity" "cl-fad" "cl-ppcre"
   "closer-mop")
  ("clavier.test" "clavier" "stefil") ("clawk" "regex") ("clazy")
  ("clem") ("clem-benchmark" "clem") ("clem-test" "clem")
  ("cleric" "alexandria" "com.gigamonkeys.binary-data" "epmd"
   "erlang-term" "md5" "usocket")
  ("cleric-test" "cleric" "erlang-term-test" "fiveam"
   "flexi-streams")
  ("clesh" "named-readtables" "trivial-shell")
  ("clesh-tests" "clesh" "lisp-unit") ("cletris" "cl-ppcre" "pal")
  ("cletris-network" "cl-log" "cl-ppcre" "cletris" "usocket")
  ("clfswm" "clx") ("clhs") ("cli-parser")
  ("clickr" "cl-ppcre" "md5" "s-xml" "s-xml-rpc" "trivial-http")
  ("clim" "clim-core" "clim-postscript" "drei-mcclim" "goatee-core")
  ("clim-basic" "clim-lisp" "flexichain" "spatial-trees")
  ("clim-clx" "clim" "clx")
  ("clim-core" "clim-basic" "clim-postscript" "goatee-core")
  ("clim-examples" "mcclim") ("clim-gtkairo" "cffi" "clim")
  ("clim-lisp") ("clim-listener" "mcclim")
  ("clim-looks" "clim" "clim-clx" "clim-null" "clim-postscript")
  ("clim-null" "clim") ("clim-postscript" "clim-basic")
  ("clim-widgets" "cl-fad" "closer-mop" "local-time" "manifest"
   "mcclim" "nsort" "perlre" "simple-date-time")
  ("climacs" "flexichain" "mcclim")
  ("climc" "cl-ppcre" "cl-xmpp-tls" "mcclim")
  ("climc-test" "climc" "lisp-unit") ("climon" "pal")
  ("climon-test" "climon" "lisp-unit") ("clinch" "cl-opengl")
  ("clinch-cairo" "cffi" "cl-cairo2" "clinch")
  ("clinch-freeimage" "cffi" "clinch" "freeimage")
  ("clinch-glfw" "cffi" "cl-glfw" "clinch")
  ("clinch-pango" "cffi" "cl-cairo2" "clinch" "clinch-cairo" "pango"
   "xmls")
  ("clinch-sdl" "cffi" "clinch" "lispbuilder-sdl")
  ("clip" "array-utils" "lquery")
  ("clipper" "alexandria" "cl-fad" "cl-syntax-annot" "closer-mop"
   "dexador" "fast-io" "opticl" "quri" "split-sequence" "zs3")
  ("clipper-test" "clipper" "integral" "prove" "prove-asdf")
  ("clite")
  ("clml.blas" "clml.blas.complex" "clml.blas.hompack"
   "clml.blas.real")
  ("clml.blas.complex" "f2cl-lib") ("clml.blas.hompack" "f2cl-lib")
  ("clml.blas.real" "f2cl-lib")
  ("clml.lapack" "clml.blas" "clml.lapack-real" "f2cl-lib")
  ("clml.lapack-real" "clml.blas" "f2cl-lib")
  ("clml.statistics" "clml.statistics.rand") ("clml.statistics.rand")
  ("clml.utility" "alexandria" "cl-fad" "cl-ppcre" "drakma" "iterate"
   "parse-number" "trivial-garbage")
  ("clnuplot" "cl-containers" "cl-mathstats" "metabang-bind"
   "trivial-shell")
  ("clobber") ("clod" "cl-ppcre" "closer-mop" "iterate")
  ("clods-export" "alexandria" "cl-fad" "cxml" "iterate" "local-time"
   "zip")
  ("clon" "bordeaux-threads" "trivial-timers") ("clon-test" "clon")
  ("clonsigna" "alexandria" "babel" "cl+ssl" "cl-base64" "cl-ppcre"
   "iolib" "split-sequence")
  ("clos-diff" "closer-mop") ("clos-fixtures" "asdf")
  ("clos-fixtures-test" "clos-fixtures" "fiveam") ("closer-mop")
  ("closure-common" "babel" "trivial-gray-streams")
  ("closure-html" "closure-common" "flexi-streams")
  ("closure-template" "alexandria" "babel" "closer-mop" "esrap"
   "iterate" "parse-number" "split-sequence")
  ("closure-template-test" "closure-template" "lift")
  ("clot" "cl-gd" "sclf")
  ("clouchdb" "closer-mop" "drakma" "flexi-streams" "parenscript"
   "s-base64")
  ("clouchdb-examples" "clouchdb" "parenscript")
  ("clouseau" "mcclim")
  ("clpmr" "cl-ppcre" "mime4cl" "net4cl" "sclf" "smtp4cl")
  ("clpython" "clpython.basic" "clpython.compiler" "clpython.contrib"
   "clpython.lib" "clpython.parser" "clpython.runtime")
  ("clpython.basic" "closer-mop")
  ("clpython.compiler" "closer-mop" "clpython.basic"
   "clpython.parser" "clpython.runtime")
  ("clpython.contrib" "clpython.basic" "clpython.compiler"
   "clpython.runtime")
  ("clpython.lib" "clpython.basic" "clpython.compiler"
   "clpython.runtime")
  ("clpython.parser" "closer-mop" "clpython.basic" "yacc")
  ("clpython.runtime" "cl-fad" "closer-mop" "clpython.basic")
  ("clpython.test" "clpython" "ptester")
  ("cls" "alexandria" "antik" "cl-data-frame" "cl-variates" "clunit"
   "data-format-validation" "fare-csv" "gsll" "lift" "lisp-matrix"
   "listoflist" "xarray")
  ("clsql" "uffi") ("clsql-aodbc") ("clsql-cffi" "clsql")
  ("clsql-fluid" "bordeaux-threads" "closer-mop" "clsql")
  ("clsql-helper" "access" "alexandria" "cl-interpol" "cl-ppcre"
   "closer-mop" "clsql" "collectors" "iterate" "md5" "symbol-munger")
  ("clsql-helper-local-time" "clsql-helper" "local-time")
  ("clsql-helper-slot-coercer" "closer-mop" "clsql-helper")
  ("clsql-helper-slot-coercer-test" "clsql-helper-slot-coercer"
   "lisp-unit2")
  ("clsql-helper-test" "clsql-helper" "clsql-tests" "lisp-unit2")
  ("clsql-mysql" "clsql" "clsql-uffi" "uffi")
  ("clsql-odbc" "clsql" "clsql-uffi")
  ("clsql-orm" "cl-inflector" "cl-interpol" "cl-ppcre" "clsql"
   "iterate" "symbol-munger")
  ("clsql-postgresql" "clsql" "clsql-uffi")
  ("clsql-postgresql-socket" "clsql" "md5" "uffi")
  ("clsql-postgresql-socket3" "cl-postgres" "clsql" "md5")
  ("clsql-sqlite" "clsql" "clsql-uffi")
  ("clsql-sqlite3" "clsql" "clsql-uffi")
  ("clsql-tests" "clsql" "rt" "uffi") ("clsql-uffi" "clsql" "uffi")
  ("clss" "array-utils" "plump") ("clunit")
  ("clws" "chunga" "cl-base64" "flexi-streams" "iolib" "ironclad"
   "split-sequence")
  ("clx") ("clx-cursor" "cl-fad" "clx")
  ("clx-truetype" "cl-aa" "cl-fad" "cl-paths-ttf" "cl-store"
   "cl-vectors" "clx" "trivial-features" "zpb-ttf")
  ("clx-truetype-test" "clx-truetype")
  ("cobstor" "net4cl" "npg" "sclf")
  ("cobstor-tests" "cobstor" "rt" "sclf")
  ("cocoahelper" "cffi" "lispbuilder-sdl-binaries")
  ("codata-recommended-values")
  ("coleslaw" "3bmd" "3bmd-ext-code-blocks" "alexandria" "cl-fad"
   "cl-ppcre" "cl-unicode" "closer-mop" "closure-template"
   "inferior-shell" "local-time")
  ("coleslaw-tests" "coleslaw" "stefil")
  ("collectors" "alexandria" "closer-mop")
  ("collectors-test" "collectors" "lisp-unit2")
  ("colleen" "bordeaux-threads" "cl-ppcre" "flexi-streams"
   "trivial-arguments" "universal-config" "usocket" "uuid" "verbose")
  ("colnew" "f2cl") ("colnew-test-1" "colnew")
  ("colnew-test-2" "colnew") ("colnew-test-3" "colnew")
  ("colorize" "alexandria" "html-encode" "split-sequence")
  ("com.clearly-useful.generic-collection-interface"
   "bordeaux-threads" "com.clearly-useful.protocols" "lparallel")
  ("com.clearly-useful.generic-collection-interface.test"
   "com.clearly-useful.generic-collection-interface")
  ("com.clearly-useful.iterate+"
   "com.clearly-useful.generic-collection-interface"
   "com.clearly-useful.iterator-protocol"
   "com.clearly-useful.protocols" "iterate")
  ("com.clearly-useful.iterator-protocol"
   "com.clearly-useful.generic-collection-interface"
   "com.clearly-useful.protocols")
  ("com.clearly-useful.protocols" "iterate") ("com.dvlsoft.asdf-flv")
  ("com.dvlsoft.rcfiles")
  ("com.elbeno.curve" "com.elbeno.vector" "vecto")
  ("com.elbeno.vector") ("com.gigamonkeys.binary-data" "alexandria")
  ("com.gigamonkeys.json" "com.gigamonkeys.parser"
   "com.gigamonkeys.utilities")
  ("com.gigamonkeys.macro-utilities")
  ("com.gigamonkeys.markup" "cl-ppcre" "com.gigamonkeys.pathnames"
   "com.gigamonkeys.utilities")
  ("com.gigamonkeys.parser" "com.gigamonkeys.macro-utilities"
   "com.gigamonkeys.utilities")
  ("com.gigamonkeys.pathnames")
  ("com.gigamonkeys.prose-diff" "cl-ppcre"
   "com.gigamonkeys.macro-utilities" "com.gigamonkeys.markup"
   "com.gigamonkeys.pathnames" "com.gigamonkeys.utilities"
   "monkeylib-markup-html")
  ("com.gigamonkeys.test-framework"
   "com.gigamonkeys.macro-utilities")
  ("com.gigamonkeys.utilities" "alexandria" "split-sequence")
  ("com.google.base")
  ("com.google.base-test" "com.google.base" "hu.dwim.stefil")
  ("com.google.flag" "com.google.base")
  ("com.google.flag-test" "com.google.flag" "hu.dwim.stefil")
  ("com.informatimago" "com.informatimago.clext"
   "com.informatimago.clisp" "com.informatimago.clmisc"
   "com.informatimago.common-lisp" "com.informatimago.editor"
   "com.informatimago.future" "com.informatimago.languages"
   "com.informatimago.lispdoc" "com.informatimago.objcl"
   "com.informatimago.rdp" "com.informatimago.small-cl-pgms"
   "com.informatimago.susv3" "com.informatimago.tools"
   "com.informatimago.xcode")
  ("com.informatimago.clext" "com.informatimago.clext.association"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.clext.association" "closer-mop")
  ("com.informatimago.clext.association.test" "closer-mop"
   "com.informatimago.clext.association"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.clext.run-program"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.clext.run-program.test"
   "com.informatimago.clext.run-program"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.clext.test" "com.informatimago.clext"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.clisp") ("com.informatimago.clisp.test")
  ("com.informatimago.clmisc")
  ("com.informatimago.clmisc.test" "com.informatimago.clmisc"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp"
   "com.informatimago.common-lisp.arithmetic"
   "com.informatimago.common-lisp.bank"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.csv"
   "com.informatimago.common-lisp.data-encoding"
   "com.informatimago.common-lisp.diagram"
   "com.informatimago.common-lisp.ed"
   "com.informatimago.common-lisp.graphviz"
   "com.informatimago.common-lisp.heap"
   "com.informatimago.common-lisp.html-base"
   "com.informatimago.common-lisp.html-generator"
   "com.informatimago.common-lisp.html-parser"
   "com.informatimago.common-lisp.http"
   "com.informatimago.common-lisp.interactive"
   "com.informatimago.common-lisp.invoice"
   "com.informatimago.common-lisp.lisp-reader"
   "com.informatimago.common-lisp.lisp-sexp"
   "com.informatimago.common-lisp.lisp-text"
   "com.informatimago.common-lisp.parser"
   "com.informatimago.common-lisp.picture"
   "com.informatimago.common-lisp.regexp"
   "com.informatimago.common-lisp.rfc2822"
   "com.informatimago.common-lisp.rfc3548"
   "com.informatimago.common-lisp.unix")
  ("com.informatimago.common-lisp.apple-file"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.apple-file.test"
   "com.informatimago.common-lisp.apple-file"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.arithmetic"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.arithmetic.test"
   "com.informatimago.common-lisp.arithmetic"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.bank"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.bank.test"
   "com.informatimago.common-lisp.bank"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp-sexp")
  ("com.informatimago.common-lisp.cesarum.test"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.csv"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.csv.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.csv")
  ("com.informatimago.common-lisp.data-encoding"
   "com.informatimago.common-lisp.arithmetic"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.data-encoding.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.data-encoding")
  ("com.informatimago.common-lisp.diagram"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.diagram.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.diagram")
  ("com.informatimago.common-lisp.ed"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.regexp")
  ("com.informatimago.common-lisp.ed.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.ed")
  ("com.informatimago.common-lisp.graphviz"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.graphviz.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.graphviz")
  ("com.informatimago.common-lisp.heap"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.data-encoding")
  ("com.informatimago.common-lisp.heap.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.heap")
  ("com.informatimago.common-lisp.html-base"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp-sexp")
  ("com.informatimago.common-lisp.html-base.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.html-base")
  ("com.informatimago.common-lisp.html-generator"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.html-generator.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.html-generator")
  ("com.informatimago.common-lisp.html-parser"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.html-base")
  ("com.informatimago.common-lisp.html-parser.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.html-parser")
  ("com.informatimago.common-lisp.http"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.html-generator")
  ("com.informatimago.common-lisp.http.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.http")
  ("com.informatimago.common-lisp.interactive"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.interactive.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.interactive")
  ("com.informatimago.common-lisp.invoice"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.invoice.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.invoice")
  ("com.informatimago.common-lisp.lisp" "closer-mop"
   "com.informatimago.common-lisp.lisp.ibcl"
   "com.informatimago.common-lisp.lisp.stepper")
  ("com.informatimago.common-lisp.lisp-reader"
   "com.informatimago.common-lisp.lisp-sexp")
  ("com.informatimago.common-lisp.lisp-reader.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp-reader")
  ("com.informatimago.common-lisp.lisp-sexp")
  ("com.informatimago.common-lisp.lisp-sexp.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp-sexp")
  ("com.informatimago.common-lisp.lisp-text"
   "com.informatimago.common-lisp.lisp-reader")
  ("com.informatimago.common-lisp.lisp-text.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp-text")
  ("com.informatimago.common-lisp.lisp.ibcl"
   "com.informatimago.common-lisp.lisp-sexp")
  ("com.informatimago.common-lisp.lisp.ibcl.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp.ibcl")
  ("com.informatimago.common-lisp.lisp.stepper"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp-sexp")
  ("com.informatimago.common-lisp.lisp.stepper.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp.stepper")
  ("com.informatimago.common-lisp.lisp.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp")
  ("com.informatimago.common-lisp.parser"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.regexp")
  ("com.informatimago.common-lisp.parser.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.parser")
  ("com.informatimago.common-lisp.picture"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.picture.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.picture")
  ("com.informatimago.common-lisp.regexp" "cl-ppcre"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.picture")
  ("com.informatimago.common-lisp.regexp.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.regexp")
  ("com.informatimago.common-lisp.rfc2822"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.rfc2822.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.rfc2822")
  ("com.informatimago.common-lisp.rfc3548"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.rfc3548.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.rfc3548")
  ("com.informatimago.common-lisp.scanner"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.regexp")
  ("com.informatimago.common-lisp.scanner.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.scanner")
  ("com.informatimago.common-lisp.telnet"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.telnet.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.telnet")
  ("com.informatimago.common-lisp.test"
   "com.informatimago.common-lisp"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.unix"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.common-lisp.unix.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.unix")
  ("com.informatimago.editor" "cl-charms"
   "com.informatimago.clext.run-program"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.lisp-sexp" "split-sequence")
  ("com.informatimago.editor.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.editor")
  ("com.informatimago.future") ("com.informatimago.future.empty")
  ("com.informatimago.future.empty.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.future.empty")
  ("com.informatimago.future.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.future")
  ("com.informatimago.languages" "com.informatimago.languages.c11"
   "com.informatimago.languages.cpp"
   "com.informatimago.languages.cxx"
   "com.informatimago.languages.linc"
   "com.informatimago.languages.lua")
  ("com.informatimago.languages.c11"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.parser"
   "com.informatimago.common-lisp.scanner"
   "com.informatimago.languages.cpp" "com.informatimago.rdp"
   "com.informatimago.tools.reader-macro" "yacc")
  ("com.informatimago.languages.cpp" "babel"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.scanner")
  ("com.informatimago.languages.cpp.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.languages.cpp")
  ("com.informatimago.languages.cxx"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.languages.cxx.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.languages.cxx")
  ("com.informatimago.languages.linc" "closer-mop"
   "com.informatimago.common-lisp.cesarum" "split-sequence")
  ("com.informatimago.languages.linc.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.languages.linc")
  ("com.informatimago.languages.lua"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.parser" "com.informatimago.rdp")
  ("com.informatimago.languages.lua.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.languages.lua")
  ("com.informatimago.lispdoc" "cl-ppcre" "closer-mop"
   "com.informatimago.clext" "com.informatimago.clmisc"
   "com.informatimago.common-lisp" "com.informatimago.rdp"
   "split-sequence")
  ("com.informatimago.lispdoc.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.lispdoc")
  ("com.informatimago.objcl" "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.objcl.test"
   "com.informatimago.common-lisp.cesarum" "com.informatimago.objcl")
  ("com.informatimago.rdp" "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.scanner")
  ("com.informatimago.rdp.basic" "com.informatimago.rdp")
  ("com.informatimago.rdp.basic.example" "com.informatimago.rdp"
   "com.informatimago.rdp.basic")
  ("com.informatimago.rdp.basic.example.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.rdp.basic.example")
  ("com.informatimago.rdp.basic.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.rdp.basic")
  ("com.informatimago.rdp.example" "com.informatimago.rdp")
  ("com.informatimago.rdp.example.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.rdp.example")
  ("com.informatimago.rdp.test"
   "com.informatimago.common-lisp.cesarum" "com.informatimago.rdp")
  ("com.informatimago.small-cl-pgms"
   "com.informatimago.small-cl-pgms.brainfuck"
   "com.informatimago.small-cl-pgms.life"
   "com.informatimago.small-cl-pgms.quine"
   "com.informatimago.small-cl-pgms.what-implementation")
  ("com.informatimago.small-cl-pgms.botihn" "cl-irc" "cl-json"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.interactive" "drakma"
   "split-sequence")
  ("com.informatimago.small-cl-pgms.brainfuck")
  ("com.informatimago.small-cl-pgms.brainfuck.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.small-cl-pgms.brainfuck")
  ("com.informatimago.small-cl-pgms.life")
  ("com.informatimago.small-cl-pgms.life.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.small-cl-pgms.life")
  ("com.informatimago.small-cl-pgms.quine")
  ("com.informatimago.small-cl-pgms.quine.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.small-cl-pgms.quine")
  ("com.informatimago.small-cl-pgms.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.small-cl-pgms"
   "com.informatimago.small-cl-pgms.brainfuck"
   "com.informatimago.small-cl-pgms.life"
   "com.informatimago.small-cl-pgms.quine"
   "com.informatimago.small-cl-pgms.what-implementation")
  ("com.informatimago.small-cl-pgms.what-implementation")
  ("com.informatimago.small-cl-pgms.what-implementation.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.small-cl-pgms.what-implementation")
  ("com.informatimago.susv3" "com.informatimago.clisp"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.heap")
  ("com.informatimago.susv3.test"
   "com.informatimago.common-lisp.cesarum" "com.informatimago.susv3")
  ("com.informatimago.test" "com.informatimago"
   "com.informatimago.clext.test" "com.informatimago.clisp.test"
   "com.informatimago.clmisc.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.test"
   "com.informatimago.editor.test" "com.informatimago.future.test"
   "com.informatimago.lispdoc.test" "com.informatimago.objcl.test"
   "com.informatimago.rdp.test"
   "com.informatimago.small-cl-pgms.test"
   "com.informatimago.susv3.test" "com.informatimago.tools.test"
   "com.informatimago.xcode.test")
  ("com.informatimago.tools" "com.informatimago.tools.check-asdf"
   "com.informatimago.tools.make-depends"
   "com.informatimago.tools.manifest"
   "com.informatimago.tools.pathname"
   "com.informatimago.tools.quicklisp"
   "com.informatimago.tools.reader-macro"
   "com.informatimago.tools.script" "com.informatimago.tools.source"
   "com.informatimago.tools.summary" "com.informatimago.tools.symbol"
   "com.informatimago.tools.thread"
   "com.informatimago.tools.try-systems"
   "com.informatimago.tools.undefmethod")
  ("com.informatimago.tools.check-asdf" "com.informatimago.clext"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.script" "com.informatimago.tools.source")
  ("com.informatimago.tools.check-asdf.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.check-asdf"
   "com.informatimago.tools.script")
  ("com.informatimago.tools.make-depends" "com.informatimago.clext"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.html-generator"
   "com.informatimago.tools.source")
  ("com.informatimago.tools.make-depends.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.make-depends")
  ("com.informatimago.tools.manifest"
   "com.informatimago.common-lisp.cesarum" "split-sequence")
  ("com.informatimago.tools.manifest.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.manifest")
  ("com.informatimago.tools.pathname")
  ("com.informatimago.tools.pathname.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.pathname")
  ("com.informatimago.tools.quicklisp"
   "com.informatimago.tools.pathname")
  ("com.informatimago.tools.quicklisp.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.quicklisp")
  ("com.informatimago.tools.reader-macro")
  ("com.informatimago.tools.script")
  ("com.informatimago.tools.script.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.script")
  ("com.informatimago.tools.source" "closer-mop"
   "com.informatimago.clext" "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.graphviz"
   "com.informatimago.common-lisp.picture" "split-sequence")
  ("com.informatimago.tools.source.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.source")
  ("com.informatimago.tools.summary" "com.informatimago.clext"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.html-generator"
   "com.informatimago.tools.source")
  ("com.informatimago.tools.summary.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.summary")
  ("com.informatimago.tools.symbol"
   "com.informatimago.common-lisp.cesarum")
  ("com.informatimago.tools.symbol.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.symbol")
  ("com.informatimago.tools.test"
   "com.informatimago.tools.check-asdf.test"
   "com.informatimago.tools.make-depends.test"
   "com.informatimago.tools.manifest.test"
   "com.informatimago.tools.pathname.test"
   "com.informatimago.tools.quicklisp.test"
   "com.informatimago.tools.script.test"
   "com.informatimago.tools.source.test"
   "com.informatimago.tools.summary.test"
   "com.informatimago.tools.symbol.test"
   "com.informatimago.tools.undefmethod.test")
  ("com.informatimago.tools.thread" "bordeaux-threads")
  ("com.informatimago.tools.try-systems"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.script" "com.informatimago.tools.source"
   "split-sequence")
  ("com.informatimago.tools.undefmethod")
  ("com.informatimago.tools.undefmethod.test"
   "com.informatimago.common-lisp.cesarum"
   "com.informatimago.tools.undefmethod")
  ("com.informatimago.xcode" "com.informatimago.common-lisp.cesarum"
   "com.informatimago.common-lisp.parser" "com.informatimago.rdp")
  ("com.informatimago.xcode.test"
   "com.informatimago.common-lisp.cesarum" "com.informatimago.rdp"
   "com.informatimago.xcode")
  ("com.ogamita.swig" "cffi")
  ("com.ogamita.swig.test" "com.informatimago.common-lisp.cesarum"
   "com.ogamita.swig")
  ("command-line-arguments")
  ("common-doc" "alexandria" "anaphora" "local-time" "quri"
   "trivial-types")
  ("common-doc-contrib" "common-doc-gnuplot" "common-doc-graphviz"
   "common-doc-include" "common-doc-split-paragraphs"
   "common-doc-tex")
  ("common-doc-gnuplot" "common-doc" "split-sequence")
  ("common-doc-graphviz" "common-doc" "trivial-shell")
  ("common-doc-include" "common-doc" "split-sequence")
  ("common-doc-plump" "anaphora" "cl-markup" "common-doc"
   "common-doc-split-paragraphs" "plump")
  ("common-doc-plump-test" "common-doc-plump" "fiveam")
  ("common-doc-split-paragraphs" "cl-ppcre" "common-doc")
  ("common-doc-test" "common-doc" "common-doc-contrib" "fiveam")
  ("common-doc-tex" "common-doc")
  ("common-html" "anaphora" "common-doc" "plump")
  ("common-html-test" "common-html" "fiveam") ("comp-set")
  ("computable-reals") ("conditional-commands" "mcclim")
  ("conduit-packages") ("conium" "closer-mop")
  ("consix" "alexandria" "cl-glu" "cl-glut" "cl-opengl")
  ("containers-and-utilities" "cl-containers" "metatilities-base")
  ("containers-moptilities" "cl-containers" "moptilities")
  ("contextl" "closer-mop" "lw-compat")
  ("corona" "anaphora" "cl-fad" "cl-virtualbox" "ironclad" "log4cl"
   "trivial-download" "trivial-extract" "trivial-types")
  ("corona-test" "archive" "cl-fad" "clack" "clack-v1-compat"
   "corona" "fiveam")
  ("corona-web" "3bmd" "3bmd-ext-code-blocks"
   "3bmd-ext-definition-lists" "cl-markup" "corona" "lass")
  ("cqlcl" "alexandria" "bordeaux-threads" "fiveam" "flexi-streams"
   "lparallel" "pooler" "split-sequence" "usocket" "uuid")
  ("cqlcl-test" "alexandria" "cqlcl" "fiveam" "flexi-streams" "uuid")
  ("crane" "anaphora" "cl-fad" "clos-fixtures" "closer-mop" "dbi"
   "iterate" "local-time" "sxql" "uiop")
  ("crane-test" "crane" "fiveam")
  ("croatoan" "cffi" "trivial-gray-streams") ("crypt")
  ("crypto-shortcuts" "cl-base64" "flexi-streams" "ironclad")
  ("csound" "cffi") ("css-lite")
  ("css-selectors" "alexandria" "buildnode" "cl-interpol" "cl-ppcre"
   "cxml" "iterate" "symbol-munger" "yacc")
  ("css-selectors-simple-tree" "cl-html5-parser" "css-selectors")
  ("css-selectors-stp" "css-selectors" "cxml-stp")
  ("css-selectors-test" "buildnode-xhtml" "css-selectors"
   "lisp-unit2")
  ("csv-parser") ("curly") ("curly.test" "curly" "fiveam")
  ("curry-compose-reader-macros" "alexandria")
  ("cxml" "cxml-dom" "cxml-klacks" "cxml-test")
  ("cxml-dom" "cxml-xml") ("cxml-klacks" "cxml-xml")
  ("cxml-rng" "cl-base64" "cl-ppcre" "cxml" "parse-number" "yacc")
  ("cxml-rpc" "cl-base64" "cxml" "drakma" "hunchentoot"
   "parse-number")
  ("cxml-stp" "alexandria" "cxml" "xpath")
  ("cxml-stp-test" "cxml-stp" "rt")
  ("cxml-test" "cxml-dom" "cxml-klacks" "cxml-xml")
  ("cxml-xml" "closure-common" "puri" "trivial-gray-streams")
  ("daemon")
  ("darts.lib.hashtree-test" "darts.lib.hashtrie" "darts.lib.wbtree"
   "stefil")
  ("darts.lib.hashtrie")
  ("darts.lib.message-pack" "babel" "ieee-floats")
  ("darts.lib.message-pack-test" "darts.lib.message-pack" "stefil"
   "trivial-octet-streams")
  ("darts.lib.sequence-metrics") ("darts.lib.wbtree")
  ("data-format-validation" "cl-ppcre")
  ("data-sift" "alexandria" "cl-ppcre" "parse-number" "puri")
  ("data-sift-test" "data-sift" "lift")
  ("data-table" "alexandria" "cl-interpol" "iterate" "symbol-munger")
  ("data-table-clsql" "clsql" "clsql-helper" "collectors"
   "data-table" "iterate")
  ("data-table-test" "data-table" "lisp-unit2")
  ("datafly" "alexandria" "babel" "cl-json" "cl-syntax-annot"
   "closer-mop" "dbi" "function-cache" "iterate" "local-time"
   "log4cl" "optima" "sxql" "trivial-types")
  ("datafly-test" "cl-test-more" "datafly" "sxql") ("date-calc")
  ("db3")
  ("dbd-mysql" "cl-mysql" "cl-syntax" "cl-syntax-annot" "dbi")
  ("dbd-postgres" "cl-postgres" "cl-syntax" "cl-syntax-annot" "dbi")
  ("dbd-sqlite3" "cl-syntax" "cl-syntax-annot" "dbi" "sqlite" "uiop")
  ("dbi" "cl-syntax" "cl-syntax-annot" "closer-mop" "split-sequence")
  ("dbi-test" "cl-syntax" "cl-syntax-annot" "closer-mop" "dbi"
   "prove" "trivial-types")
  ("dbus" "alexandria" "babel" "cl-xmlspam" "flexi-streams"
   "ieee-floats" "iolib" "ironclad" "split-sequence"
   "trivial-garbage")
  ("dcm" "elephant") ("decimals")
  ("defclass-std" "alexandria" "anaphora")
  ("defclass-std-test" "defclass-std" "prove" "prove-asdf")
  ("defcontract" "closer-mop") ("defdoc" "defcontract") ("defenum")
  ("deferred" "named-readtables") ("define-json-expander")
  ("deflate")
  ("defmacro-enhance" "alexandria" "cl-indeterminism"
   "cl-splicing-macro" "hu.dwim.walker" "iterate")
  ("defmacro-enhance-tests" "defmacro-enhance" "eos")
  ("defmemo" "alexandria" "trivial-garbage")
  ("defmemo-test" "defmemo") ("defpackage-plus" "alexandria")
  ("defrec" "alexandria") ("defstar")
  ("defsystem-compatibility" "metatilities-base")
  ("defsystem-compatibility-test" "defsystem-compatibility" "lift")
  ("defvariant") ("degree-symbol" "antik" "cl-unicode")
  ("delorean" "local-time") ("delorean-test" "delorean" "fiveam")
  ("delta-debug" "alexandria" "curry-compose-reader-macros")
  ("delta-debug-exe" "alexandria" "cl-launch"
   "curry-compose-reader-macros" "delta-debug" "diff" "metabang-bind"
   "split-sequence" "trivial-shell")
  ("delta-debug-test" "alexandria" "curry-compose-reader-macros"
   "delta-debug" "stefil")
  ("deoxybyte-gzip" "deoxybyte-io" "deoxybyte-systems"
   "deoxybyte-unix")
  ("deoxybyte-gzip-test" "deoxybyte-gzip" "lift")
  ("deoxybyte-io" "cl-fad" "deoxybyte-systems" "deoxybyte-utilities"
   "getopt")
  ("deoxybyte-io-test" "deoxybyte-io" "lift")
  ("deoxybyte-systems" "cl-fad")
  ("deoxybyte-unix" "cffi" "deoxybyte-io" "deoxybyte-systems")
  ("deoxybyte-unix-test" "deoxybyte-unix" "lift")
  ("deoxybyte-utilities" "deoxybyte-systems")
  ("deoxybyte-utilities-test" "deoxybyte-utilities" "lift")
  ("descriptions" "alexandria" "anaphora" "closer-mop" "sheeple")
  ("descriptions-test" "descriptions" "descriptions.serialization"
   "descriptions.validation" "stefil")
  ("descriptions.serialization" "cl-json" "descriptions")
  ("descriptions.validation" "clavier" "descriptions")
  ("dexador" "alexandria" "babel" "bordeaux-threads" "chipz" "chunga"
   "cl+ssl" "cl-base64" "cl-cookie" "cl-ppcre" "cl-reexport"
   "fast-http" "fast-io" "flexi-streams" "quri" "trivial-mimes"
   "usocket")
  ("dexador-test" "babel" "cl-cookie" "clack-test" "dexador"
   "lack-request" "prove" "prove-asdf")
  ("diff" "cl-ppcre" "trivial-gray-streams") ("dissect")
  ("djula" "access" "alexandria" "anaphora" "babel" "cl-fad"
   "cl-locale" "cl-ppcre" "cl-slice" "closer-mop" "gettext" "iterate"
   "local-time" "parser-combinators" "split-sequence"
   "trivial-backtrace")
  ("djula-demo" "djula" "hunchentoot")
  ("djula-test" "djula" "fiveam") ("dlist")
  ("dlist-test" "dlist" "lisp-unit")
  ("do-urlencode" "babel" "babel-streams")
  ("docbrowser" "alexandria" "babel" "bordeaux-threads" "cl-json"
   "closer-mop" "colorize" "flexi-streams" "hunchentoot"
   "parse-number" "split-sequence" "string-case" "swank" "yacc")
  ("docparser" "alexandria" "anaphora" "cffi" "trivial-types")
  ("docparser-test" "docparser" "fiveam")
  ("docparser-test-system" "cffi")
  ("documentation-template" "cl-who")
  ("docutils" "cl-ppcre" "data-format-validation"
   "trivial-gray-streams")
  ("dom" "cl-who" "yadd") ("donuts" "cl-ppcre" "trivial-shell")
  ("doplus" "parse-declarations-1.0") ("doplus-fset" "doplus" "fset")
  ("doplus-tests" "doplus" "eos") ("double-metaphone")
  ("drakma" "chipz" "chunga" "cl+ssl" "cl-base64" "cl-ppcre"
   "flexi-streams" "puri" "usocket")
  ("drakma-async" "alexandria" "cl-async-future" "cl-async-ssl"
   "drakma" "fast-http" "fast-io" "flexi-streams")
  ("drakma-test" "drakma" "fiveam") ("draw-cons-tree")
  ("drei-mcclim" "clim-core" "esa-mcclim" "flexichain" "swank")
  ("drei-tests" "drei-mcclim" "fiveam")
  ("dso-lex" "cl-ppcre" "dso-util") ("dso-util" "cl-ppcre")
  ("duologue" "alexandria" "anaphora" "chronicity" "cl-ansi-text"
   "cl-fad" "cl-readline" "clavier" "drakma")
  ("dweet" "babel" "com.gigamonkeys.json" "drakma")
  ("dyna" "alexandria" "cl-base64" "cl-syntax-annot" "closer-mop"
   "dexador" "flexi-streams" "ironclad" "jsown" "local-time" "quri"
   "split-sequence" "sxql")
  ("dyna-test" "dyna" "local-time" "prove" "prove-asdf")
  ("dynamic-classes" "metatilities-base")
  ("dynamic-classes-test" "dynamic-classes" "lift")
  ("dynamic-collect") ("dynamic-mixins" "alexandria" "closer-mop")
  ("dynamic-wind" "lw-compat") ("eager-future" "bordeaux-threads")
  ("eager-future.test" "eager-future" "fiveam")
  ("eager-future2" "bordeaux-threads" "trivial-garbage")
  ("eazy-gnuplot" "alexandria" "eazy-process" "iterate" "optima")
  ("eazy-gnuplot.test" "eazy-gnuplot" "fiveam")
  ("eazy-process" "alexandria" "cffi" "cl-ppcre" "cl-rlimit" "iolib"
   "iterate" "optima" "optima.ppcre" "trivial-garbage")
  ("eazy-process.test" "eazy-process" "fiveam")
  ("eazy-project" "asdf" "bordeaux-threads" "cl-emb" "cl-syntax"
   "cl-syntax-annot" "introspect-environment" "iterate"
   "lisp-namespace" "local-time" "optima" "osicat" "trivial-shell")
  ("eazy-project.test" "eazy-project" "fiveam")
  ("ec2" "drakma" "ironclad" "s-base64" "s-xml")
  ("eco" "cl-who" "esrap" "split-sequence")
  ("eco-test" "eco" "fiveam") ("ele-bdb" "elephant" "uffi")
  ("ele-clp" "cl-containers" "cl-prevalence" "elephant")
  ("ele-clsql" "cl-base64" "clsql" "elephant" "uffi")
  ("ele-postgresql" "clsql-postgresql-socket" "ele-clsql")
  ("elephant" "cl-base64" "uffi")
  ("elephant-tests" "bordeaux-threads" "elephant" "fiveam")
  ("elf" "alexandria" "cl-ppcre" "com.gigamonkeys.binary-data"
   "flexi-streams" "metabang-bind" "split-sequence" "trivial-shell")
  ("elf-test" "alexandria" "elf" "metabang-bind" "stefil"
   "trivial-timeout")
  ("enchant" "cffi") ("enchant-autoload" "enchant")
  ("enhanced-eval-when") ("enhanced-multiple-value-bind")
  ("enumerations") ("envy")
  ("envy-test" "cl-test-more" "envy" "osicat") ("eos")
  ("eos-tests" "eos") ("epigraph" "alexandria")
  ("epigraph-test" "epigraph" "fiveam")
  ("epmd" "com.gigamonkeys.binary-data" "usocket")
  ("epmd-test" "epmd" "fiveam" "flexi-streams") ("equals")
  ("erlang-term" "alexandria" "ieee-floats" "nibbles" "zlib")
  ("erlang-term-optima" "erlang-term" "optima")
  ("erlang-term-test" "erlang-term" "erlang-term-optima" "fiveam"
   "nibbles")
  ("ernestine" "cl-ppcre" "cl-prevalence" "drakma" "split-sequence")
  ("ernestine-gui" "cl-log" "cl-xspf" "ernestine" "mcclim")
  ("ernestine-tests" "ernestine" "lisp-unit")
  ("erudite" "alexandria" "cl-fad" "cl-ppcre" "cl-template" "log4cl"
   "split-sequence")
  ("erudite-test" "erudite" "fiveam") ("esa" "mcclim")
  ("esa-mcclim" "clim-core") ("escalator" "iterate")
  ("escalator-bench" "escalator" "iterate") ("esrap" "alexandria")
  ("esrap-liquid" "alexandria" "cl-indeterminism" "cl-interpol"
   "cl-ppcre" "cl-read-macro-tokens" "defmacro-enhance" "iterate"
   "rutils")
  ("esrap-liquid-tests" "cl-interpol" "esrap-liquid" "fiveam")
  ("esrap-peg" "alexandria" "cl-unification" "esrap" "iterate")
  ("esrap-tests" "esrap" "fiveam") ("ev" "cffi" "trivial-garbage")
  ("event-emitter") ("event-emitter-test" "event-emitter" "prove")
  ("event-glue") ("event-glue-test" "event-glue" "fiveam")
  ("evol" "alexandria" "bordeaux-threads" "cl-fad" "cl-ppcre"
   "external-program" "patron" "unix-options")
  ("evol-test" "evol" "stefil") ("exponential-backoff")
  ("exscribe" "alexandria" "cl-typesetting" "fare-memoization"
   "fare-quasiquote-optima" "fare-utils" "scribble")
  ("exscribe/typeset" "cl-typesetting" "exscribe")
  ("ext-blog" "cl-fad" "cl-store" "closure-template" "image"
   "kl-verify" "local-time" "restas" "restas.file-publisher"
   "s-xml-rpc")
  ("extended-reals" "alexandria")
  ("extensible-sequences" "sequence-iterators") ("external-program")
  ("external-program-test" "external-program" "fiveam")
  ("f-underscore") ("f2cl") ("f2cl-lib") ("fare-csv")
  ("fare-memoization")
  ("fare-memoization/test" "fare-memoization" "hu.dwim.stefil")
  ("fare-mop" "closer-mop" "fare-utils")
  ("fare-quasiquote" "fare-utils")
  ("fare-quasiquote-extras" "fare-quasiquote-optima"
   "fare-quasiquote-readtable")
  ("fare-quasiquote-optima" "fare-quasiquote" "optima")
  ("fare-quasiquote-readtable" "fare-quasiquote" "named-readtables")
  ("fare-quasiquote-test" "fare-quasiquote-extras" "hu.dwim.stefil")
  ("fare-utils" "asdf")
  ("fare-utils-test" "fare-utils" "hu.dwim.stefil")
  ("fast-http" "alexandria" "babel" "cl-fad" "cl-utilities"
   "flexi-streams" "proc-parse" "xsubseq")
  ("fast-http-test" "babel" "cl-syntax-interpol" "fast-http" "prove"
   "prove-asdf" "xsubseq")
  ("fast-io" "alexandria" "static-vectors" "trivial-gray-streams")
  ("fast-io-test" "checkl" "fast-io" "fiveam")
  ("femlisp" "cl-gd" "cl-ppcre" "femlisp-basic" "femlisp-matlisp"
   "femlisp-parallel" "infix")
  ("femlisp-basic")
  ("femlisp-matlisp" "femlisp-basic" "femlisp-parallel")
  ("femlisp-parallel" "bordeaux-threads" "cl-ppcre" "femlisp-basic"
   "lparallel")
  ("ffa" "cffi" "cl-utilities" "iterate" "metabang-bind") ("fft")
  ("fgraph" "fset") ("fiasco" "alexandria")
  ("filtered-functions" "closer-mop") ("find-port" "usocket")
  ("find-port-test" "find-port" "fiveam")
  ("firephp" "cl-json" "hunchentoot")
  ("firephp-tests" "cl-json" "firephp" "hu.dwim.stefil"
   "hunchentoot")
  ("fishpack" "f2cl") ("fishpack-test-hstcrt" "fishpack")
  ("fishpack-test-hstcsp" "fishpack")
  ("fishpack-test-hstcyl" "fishpack")
  ("fishpack-test-hstplr" "fishpack")
  ("fishpack-test-hstssp" "fishpack")
  ("fishpack-test-hwscrt" "fishpack")
  ("fishpack-test-hwscsp" "fishpack")
  ("fishpack-test-hwscyl" "fishpack")
  ("fishpack-test-hwsplr" "fishpack")
  ("fishpack-test-hwsssp" "fishpack")
  ("fishpack-test-sepx4" "fishpack") ("fiveam" "alexandria")
  ("fiveam-test" "fiveam") ("flac" "cffi" "cffi-grovel")
  ("flexi-streams" "trivial-gray-streams")
  ("flexi-streams-test" "flexi-streams") ("flexichain")
  ("flexichain-doc") ("floating-point")
  ("floating-point-test" "floating-point" "lisp-unit")
  ("fmarshal" "closer-mop") ("fmarshal-test" "fiveam" "fmarshal")
  ("fmt" "alexandria") ("fmt-test" "fiveam" "fmt")
  ("fmt-time" "fmt" "local-time")
  ("fn" "macroexpand-dammit" "named-readtables")
  ("folio" "folio.as" "folio.boxes" "folio.collections"
   "folio.functions")
  ("folio.as") ("folio.boxes")
  ("folio.collections" "folio.as" "folio.functions" "fset")
  ("folio.functions")
  ("folio2" "alexandria" "folio2-as" "folio2-as-syntax"
   "folio2-boxes" "folio2-functions" "folio2-functions-syntax"
   "folio2-make" "folio2-maps" "folio2-maps-syntax" "folio2-pairs"
   "folio2-sequences" "folio2-sequences-syntax" "folio2-series"
   "folio2-taps" "fset" "series")
  ("folio2-as") ("folio2-as-syntax" "folio2-as")
  ("folio2-as-tests" "folio2-as" "folio2-as-syntax" "lift")
  ("folio2-boxes" "folio2-as" "folio2-make")
  ("folio2-boxes-tests" "folio2-boxes" "lift")
  ("folio2-functions" "alexandria" "folio2-as" "folio2-make")
  ("folio2-functions-syntax" "alexandria" "folio2-functions")
  ("folio2-functions-tests" "folio2-functions"
   "folio2-functions-syntax" "lift")
  ("folio2-make") ("folio2-make-tests" "folio2-make" "lift")
  ("folio2-maps" "folio2-as" "folio2-make" "fset")
  ("folio2-maps-syntax" "folio2-maps")
  ("folio2-maps-tests" "folio2-maps" "folio2-maps-syntax" "lift")
  ("folio2-pairs" "folio2-as" "folio2-make")
  ("folio2-pairs-tests" "folio2-pairs" "lift")
  ("folio2-sequences" "folio2-as" "folio2-make" "folio2-pairs" "fset"
   "series")
  ("folio2-sequences-syntax" "folio2-sequences")
  ("folio2-sequences-tests" "folio2-sequences"
   "folio2-sequences-syntax" "lift")
  ("folio2-series" "folio2-as" "folio2-make" "folio2-pairs"
   "folio2-sequences" "fset" "series")
  ("folio2-series-tests" "folio2-series" "lift")
  ("folio2-taps" "closer-mop" "folio2-as" "folio2-make" "folio2-maps"
   "folio2-pairs" "folio2-sequences" "folio2-series" "fset")
  ("folio2-taps-tests" "folio2-taps" "lift")
  ("folio2-tests" "folio2-as-tests" "folio2-boxes-tests"
   "folio2-functions-tests" "folio2-make-tests" "folio2-maps-tests"
   "folio2-pairs-tests" "folio2-sequences-tests"
   "folio2-series-tests" "folio2-taps-tests")
  ("fomus") ("fork-future" "cffi" "cl-store") ("form-fiddle")
  ("formlets" "cl-ppcre" "cl-who" "drakma" "hunchentoot")
  ("formlets-test" "cl-ppcre" "cl-who" "drakma" "formlets"
   "hunchentoot")
  ("fprog") ("fred" "drakma" "s-xml") ("freeimage" "cffi")
  ("frpc" "alexandria" "babel" "bordeaux-threads" "flexi-streams"
   "glass" "nibbles" "pounds" "usocket")
  ("frpc-des" "frpc" "ironclad") ("frpc-gss" "cerberus" "frpc")
  ("frpcgen" "cl-lex" "frpc" "yacc")
  ("fs-watcher" "alexandria" "com.gigamonkeys.pathnames")
  ("fset" "misc-extensions" "mt19937") ("fsvd") ("ftp" "cl-ftp")
  ("fucc-generator" "fucc-parser") ("fucc-parser")
  ("function-cache" "alexandria" "cl-interpol" "closer-mop" "iterate"
   "symbol-munger")
  ("function-cache-clsql" "clsql" "clsql-helper" "function-cache")
  ("function-cache-test" "function-cache" "lisp-unit2")
  ("functional-geometry" "clim-listener") ("funds")
  ("g-lib-cffi" "cffi-objects" "gtk-cffi-utils" "iterate")
  ("g-object-cffi" "g-lib-cffi" "gtk-cffi-utils") ("gambol")
  ("garbage-pools") ("garbage-pools-test" "garbage-pools" "lift")
  ("gbbopen") ("gbbopen-core") ("gbbopen-modules") ("gbbopen-test")
  ("gbbopen-tools") ("gbbopen-tools-test") ("gbbopen-tools-user")
  ("gbbopen-user") ("gcm" "babel" "com.gigamonkeys.json" "drakma")
  ("gdk-cffi" "cl-cairo2" "g-lib-cffi" "g-object-cffi")
  ("gendl" "cl-lite" "gwl-graphics" "robot" "tasty" "yadd")
  ("general-accumulator")
  ("generators" "alexandria" "cl-cont" "iterate")
  ("generic-comparability" "alexandria") ("generic-sequences")
  ("generic-sequences-cont" "cl-cont" "generic-sequences")
  ("generic-sequences-iterate" "generic-sequences" "iterate")
  ("generic-sequences-stream" "bordeaux-threads" "generic-sequences")
  ("generic-sequences-test" "generic-sequences"
   "generic-sequences-cont" "generic-sequences-iterate"
   "generic-sequences-stream")
  ("genhash") ("geo" "jpl-util" "spatial-trees")
  ("geom-base" "glisp") ("getopt")
  ("getopt-tests" "getopt" "ptester")
  ("gettext" "flexi-streams" "split-sequence" "yacc")
  ("gettext-example" "gettext") ("gettext-tests" "gettext" "stefil")
  ("gi-cffi" "gtk-cffi")
  ("gio-cffi" "g-lib-cffi" "g-object-cffi" "gtk-cffi-utils")
  ("glass") ("glaw" "cl-alc" "cl-openal" "cl-opengl")
  ("glaw-examples" "glaw" "glaw-imago" "glop")
  ("glaw-imago" "glaw" "imago")
  ("glaw-sdl" "glaw" "lispbuilder-sdl" "lispbuilder-sdl-image")
  ("glisp" "acl-compat" "babel" "base" "cl-base64" "cl-ppcre"
   "cl-typesetting" "cl-who" "uiop")
  ("glkit" "alexandria" "cl-opengl" "defpackage-plus" "mathkit"
   "static-vectors")
  ("glkit-examples" "glkit" "sdl2kit-examples") ("global-vars")
  ("global-vars-test" "global-vars") ("glop" "cffi")
  ("glop-test" "cl-glu" "cl-opengl" "glop") ("glu-tessellate" "cffi")
  ("glyphs" "cl-ppcre" "named-readtables" "parenscript")
  ("glyphs-test" "glyphs" "stefil") ("goatee-core" "clim-basic")
  ("gordon")
  ("graph" "alexandria" "curry-compose-reader-macros"
   "metabang-bind")
  ("graph-dot" "alexandria" "cl-ppcre" "curry-compose-reader-macros"
   "graph" "metabang-bind")
  ("graph-json" "alexandria" "curry-compose-reader-macros" "graph"
   "metabang-bind" "yason")
  ("graph-matrix" "alexandria" "curry-compose-reader-macros"
   "femlisp" "graph" "metabang-bind")
  ("graph-matrix-test" "alexandria" "curry-compose-reader-macros"
   "graph" "graph-matrix" "metabang-bind" "stefil")
  ("graph-test" "alexandria" "curry-compose-reader-macros" "graph"
   "metabang-bind" "stefil")
  ("gravatar" "babel" "cl-json" "drakma" "md5" "puri")
  ("graylex" "alexandria" "cl-ppcre" "trivial-gray-streams")
  ("graylex-m4-example" "cl-heredoc" "graylex")
  ("green-threads" "cl-async-future" "cl-cont")
  ("group-by" "alexandria" "iterate")
  ("group-by-test" "group-by" "lisp-unit2") ("groupby")
  ("gsharp" "clim-listener" "cxml" "flexichain" "mcclim" "midi"
   "puri")
  ("gsll" "alexandria" "antik" "asdf-system-connections"
   "cffi-grovel" "cffi-libffi" "metabang-bind" "osicat"
   "trivial-garbage")
  ("gsll-tests" "gsll" "lisp-unit")
  ("gtfl" "cl-who" "ht-simple-ajax" "hunchentoot")
  ("gtk-cffi" "gtk-cffi-cell-renderer-pixbuf"
   "gtk-cffi-cell-renderer-text" "gtk-cffi-cell-renderer-toggle"
   "gtk-cffi-color-button" "gtk-cffi-combo-box" "gtk-cffi-eventbox"
   "gtk-cffi-file-chooser-button" "gtk-cffi-file-chooser-dialog"
   "gtk-cffi-frame" "gtk-cffi-image" "gtk-cffi-info-bar"
   "gtk-cffi-label" "gtk-cffi-list-store" "gtk-cffi-menu"
   "gtk-cffi-menu-bar" "gtk-cffi-notebook" "gtk-cffi-paned"
   "gtk-cffi-progress-bar" "gtk-cffi-scale"
   "gtk-cffi-scrolled-window" "gtk-cffi-spin-button"
   "gtk-cffi-status-icon" "gtk-cffi-statusbar" "gtk-cffi-table"
   "gtk-cffi-text-view" "gtk-cffi-toolbar"
   "gtk-cffi-tree-model-filter" "gtk-cffi-tree-view")
  ("gtk-cffi-bin" "gtk-cffi-container")
  ("gtk-cffi-box" "gtk-cffi-container")
  ("gtk-cffi-button" "gtk-cffi-misc" "gtk-cffi-widget")
  ("gtk-cffi-buttonbox" "gtk-cffi-box")
  ("gtk-cffi-cell-layout" "gtk-cffi-cell-renderer" "gtk-cffi-core")
  ("gtk-cffi-cell-renderer" "gtk-cffi-core")
  ("gtk-cffi-cell-renderer-pixbuf" "gtk-cffi-cell-renderer")
  ("gtk-cffi-cell-renderer-text" "gtk-cffi-cell-renderer")
  ("gtk-cffi-cell-renderer-toggle" "gtk-cffi-cell-renderer")
  ("gtk-cffi-color-button" "gtk-cffi-button")
  ("gtk-cffi-combo-box" "gtk-cffi-bin" "gtk-cffi-entry"
   "gtk-cffi-range")
  ("gtk-cffi-container" "gtk-cffi-widget")
  ("gtk-cffi-core" "gdk-cffi" "gio-cffi" "gtk-cffi-utils")
  ("gtk-cffi-dialog" "gtk-cffi-hbuttonbox" "gtk-cffi-vbox"
   "gtk-cffi-window")
  ("gtk-cffi-entry" "gtk-cffi-image")
  ("gtk-cffi-eventbox" "gtk-cffi-box")
  ("gtk-cffi-file-chooser" "gtk-cffi-core")
  ("gtk-cffi-file-chooser-button" "gtk-cffi-file-chooser"
   "gtk-cffi-hbox")
  ("gtk-cffi-file-chooser-dialog" "gtk-cffi-dialog"
   "gtk-cffi-file-chooser")
  ("gtk-cffi-frame" "gtk-cffi-bin") ("gtk-cffi-hbox" "gtk-cffi-box")
  ("gtk-cffi-hbuttonbox" "gtk-cffi-buttonbox")
  ("gtk-cffi-image" "gtk-cffi-misc")
  ("gtk-cffi-info-bar" "gtk-cffi-box" "gtk-cffi-message-dialog")
  ("gtk-cffi-label" "gtk-cffi-misc")
  ("gtk-cffi-list-store" "gtk-cffi-tree-model")
  ("gtk-cffi-menu" "gtk-cffi-menu-shell")
  ("gtk-cffi-menu-bar" "gtk-cffi-menu-shell")
  ("gtk-cffi-menu-shell" "gtk-cffi-container")
  ("gtk-cffi-message-dialog" "gtk-cffi-dialog")
  ("gtk-cffi-misc" "gtk-cffi-widget")
  ("gtk-cffi-notebook" "gtk-cffi-container")
  ("gtk-cffi-paned" "gtk-cffi-container")
  ("gtk-cffi-progress-bar" "gtk-cffi-widget")
  ("gtk-cffi-range" "gtk-cffi-bin" "gtk-cffi-tree-model")
  ("gtk-cffi-scale" "gtk-cffi-range")
  ("gtk-cffi-scrolled-window" "gtk-cffi-bin")
  ("gtk-cffi-spin-button" "gtk-cffi-entry")
  ("gtk-cffi-spinner" "gtk-cffi-widget")
  ("gtk-cffi-status-icon" "gtk-cffi-core" "gtk-cffi-image")
  ("gtk-cffi-statusbar" "gtk-cffi-hbox")
  ("gtk-cffi-table" "gtk-cffi-container")
  ("gtk-cffi-text-buffer" "gtk-cffi-core")
  ("gtk-cffi-text-view" "gtk-cffi-text-buffer")
  ("gtk-cffi-tool-shell" "gtk-cffi-container")
  ("gtk-cffi-toolbar" "gtk-cffi-tool-shell")
  ("gtk-cffi-tree-model" "gtk-cffi-core")
  ("gtk-cffi-tree-model-filter" "gtk-cffi-tree-model")
  ("gtk-cffi-tree-selection" "gtk-cffi-tree-model")
  ("gtk-cffi-tree-view" "gtk-cffi-tree-selection"
   "gtk-cffi-tree-view-column")
  ("gtk-cffi-tree-view-column" "gtk-cffi-cell-layout"
   "gtk-cffi-cell-renderer" "gtk-cffi-widget")
  ("gtk-cffi-utils" "alexandria" "cffi" "iterate")
  ("gtk-cffi-vbox" "gtk-cffi-box")
  ("gtk-cffi-widget" "gtk-cffi-core")
  ("gtk-cffi-window" "gtk-cffi-bin")
  ("gtk-ffi" "bordeaux-threads" "cells" "cffi" "pod-utils"
   "trivial-features" "utils-kt")
  ("gwl" "aserve" "bordeaux-threads" "glisp")
  ("gwl-graphics" "geom-base" "gwl")
  ("gzip-stream" "flexi-streams" "salza2" "trivial-gray-streams")
  ("halftone" "bordeaux-threads" "qtcore" "qtgui" "qtools" "qtopengl"
   "simple-tasks" "uiop" "verbose")
  ("hash-set" "alexandria" "fiveam" "optima")
  ("hdf5-cffi" "cffi" "cffi-grovel") ("hdf5-examples" "hdf5-cffi")
  ("helambdap" "cl-fad" "split-sequence" "xhtmlambda")
  ("hemlock.base" "alexandria" "bordeaux-threads" "cl-ppcre"
   "command-line-arguments" "conium" "iolib" "iterate" "osicat"
   "prepl" "trivial-gray-streams")
  ("hemlock.clx" "clx" "hemlock.base")
  ("hemlock.qt" "hemlock.base" "qt" "qt-repl")
  ("hemlock.tty" "hemlock.base") ("hermetic" "cl-pass" "clack")
  ("hh-aws" "cl-base64" "drakma" "ironclad" "puri" "s-xml")
  ("hh-aws-tests" "hh-aws" "lisp-unit" "uuid") ("hh-redblack")
  ("hh-redblack-tests" "hh-redblack" "lisp-unit")
  ("hh-web" "bordeaux-threads" "cl-base64" "cl-fad" "cl-ppcre"
   "drakma" "hunchentoot" "ironclad" "local-time" "log5"
   "parenscript" "trivial-backtrace" "uuid" "vecto")
  ("hinge" "alexandria" "arnesi" "bordeaux-threads" "closer-mop" "ev"
   "iolib" "log5" "uuid" "zmq")
  ("hl7-client" "usocket") ("hl7-parser")
  ("hompack" "blas-hompack" "f2cl") ("hompack-test-mainf" "hompack")
  ("hompack-test-mainp" "hompack") ("hompack-test-mains" "hompack")
  ("hspell" "cffi" "trivial-garbage")
  ("ht-simple-ajax" "hunchentoot") ("html-encode")
  ("html-entities" "cl-ppcre")
  ("html-entities-tests" "fiveam" "html-entities")
  ("html-match" "cl-ppcre" "unit-test")
  ("html-match.test" "html-match" "unit-test")
  ("html-sugar" "aserve" "sclf" "uffi" "webactions")
  ("html-template") ("htmlgen" "acl-compat")
  ("http-body" "babel" "cl-ppcre" "cl-utilities" "fast-http"
   "flexi-streams" "jonathan" "quri" "trivial-gray-streams")
  ("http-body-test" "cl-ppcre" "flexi-streams" "http-body" "prove"
   "prove-asdf" "trivial-types" "trivial-utf-8")
  ("http-parse" "babel" "cl-ppcre")
  ("http-parse-test" "babel" "eos" "http-parse") ("http-services")
  ("hu.dwim.asdf" "asdf" "uiop")
  ("hu.dwim.asdf.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation")
  ("hu.dwim.common" "alexandria" "anaphora" "closer-mop"
   "hu.dwim.asdf" "hu.dwim.common-lisp" "iterate" "metabang-bind")
  ("hu.dwim.common-lisp" "hu.dwim.asdf")
  ("hu.dwim.common-lisp.documentation" "hu.dwim.asdf"
   "hu.dwim.common-lisp" "hu.dwim.presentation")
  ("hu.dwim.common.documentation" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.presentation")
  ("hu.dwim.computed-class" "hu.dwim.asdf"
   "hu.dwim.def+hu.dwim.common" "hu.dwim.defclass-star+hu.dwim.def"
   "hu.dwim.syntax-sugar" "hu.dwim.util.mop")
  ("hu.dwim.computed-class+hu.dwim.logger" "hu.dwim.asdf"
   "hu.dwim.computed-class" "hu.dwim.logger")
  ("hu.dwim.computed-class+swank" "hu.dwim.asdf"
   "hu.dwim.computed-class" "swank")
  ("hu.dwim.computed-class.documentation" "hu.dwim.asdf"
   "hu.dwim.computed-class.test" "hu.dwim.presentation")
  ("hu.dwim.computed-class.test" "hu.dwim.asdf"
   "hu.dwim.computed-class+hu.dwim.logger"
   "hu.dwim.stefil+hu.dwim.def")
  ("hu.dwim.debug" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.def+swank" "hu.dwim.defclass-star" "hu.dwim.util"
   "hu.dwim.walker" "swank")
  ("hu.dwim.debug.documentation" "hu.dwim.asdf" "hu.dwim.debug.test"
   "hu.dwim.presentation")
  ("hu.dwim.debug.test" "hu.dwim.asdf" "hu.dwim.debug"
   "hu.dwim.stefil+hu.dwim.def+swank")
  ("hu.dwim.def" "alexandria" "anaphora" "hu.dwim.asdf" "iterate"
   "metabang-bind")
  ("hu.dwim.def+cl-l10n" "cl-l10n" "hu.dwim.asdf" "hu.dwim.def")
  ("hu.dwim.def+contextl" "contextl" "hu.dwim.asdf" "hu.dwim.def")
  ("hu.dwim.def+hu.dwim.common" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.def")
  ("hu.dwim.def+hu.dwim.delico" "hu.dwim.asdf" "hu.dwim.def"
   "hu.dwim.delico")
  ("hu.dwim.def+swank" "hu.dwim.asdf" "hu.dwim.def" "swank")
  ("hu.dwim.def.documentation" "hu.dwim.asdf" "hu.dwim.def.test"
   "hu.dwim.presentation")
  ("hu.dwim.def.namespace" "bordeaux-threads" "hu.dwim.asdf"
   "hu.dwim.def" "hu.dwim.util" "trivial-garbage")
  ("hu.dwim.def.test" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.stefil+hu.dwim.def" "optima")
  ("hu.dwim.defclass-star" "hu.dwim.asdf")
  ("hu.dwim.defclass-star+contextl" "contextl" "hu.dwim.asdf"
   "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.asdf" "hu.dwim.def"
   "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star+hu.dwim.def+contextl" "hu.dwim.asdf"
   "hu.dwim.defclass-star+contextl"
   "hu.dwim.defclass-star+hu.dwim.def")
  ("hu.dwim.defclass-star+swank" "hu.dwim.asdf"
   "hu.dwim.defclass-star" "swank")
  ("hu.dwim.defclass-star.documentation" "hu.dwim.asdf"
   "hu.dwim.defclass-star.test" "hu.dwim.presentation")
  ("hu.dwim.defclass-star.test" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.defclass-star" "hu.dwim.stefil+hu.dwim.def+swank")
  ("hu.dwim.delico" "contextl" "hu.dwim.asdf"
   "hu.dwim.def+hu.dwim.common" "hu.dwim.walker")
  ("hu.dwim.delico.documentation" "hu.dwim.asdf"
   "hu.dwim.delico.test" "hu.dwim.presentation"
   "hu.dwim.walker.documentation")
  ("hu.dwim.delico.test" "hu.dwim.asdf" "hu.dwim.def"
   "hu.dwim.delico" "hu.dwim.stefil+hu.dwim.def+swank" "hu.dwim.util"
   "hu.dwim.util.temporary-files")
  ("hu.dwim.graphviz" "cffi" "hu.dwim.asdf" "metabang-bind")
  ("hu.dwim.logger" "bordeaux-threads" "hu.dwim.asdf"
   "hu.dwim.def+hu.dwim.common" "hu.dwim.def.namespace"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.util"
   "hu.dwim.util.threads" "local-time")
  ("hu.dwim.logger+iolib" "hu.dwim.asdf" "hu.dwim.logger"
   "hu.dwim.util+iolib")
  ("hu.dwim.logger+swank" "hu.dwim.asdf" "hu.dwim.logger" "swank")
  ("hu.dwim.logger.documentation" "hu.dwim.asdf"
   "hu.dwim.logger.test" "hu.dwim.presentation")
  ("hu.dwim.logger.test" "hu.dwim.asdf" "hu.dwim.logger"
   "hu.dwim.stefil+hu.dwim.def+swank")
  ("hu.dwim.partial-eval" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.def" "hu.dwim.defclass-star+hu.dwim.def+contextl"
   "hu.dwim.logger" "hu.dwim.syntax-sugar" "hu.dwim.util"
   "hu.dwim.util.source" "hu.dwim.walker" "swank")
  ("hu.dwim.partial-eval.documentation" "hu.dwim.asdf"
   "hu.dwim.partial-eval.test" "hu.dwim.presentation")
  ("hu.dwim.partial-eval.test" "hu.dwim.asdf" "hu.dwim.partial-eval"
   "hu.dwim.stefil+hu.dwim.def+swank" "hu.dwim.util.mop")
  ("hu.dwim.perec" "babel" "cl-containers" "cl-ppcre" "contextl"
   "hu.dwim.asdf" "hu.dwim.common" "hu.dwim.computed-class"
   "hu.dwim.def+contextl" "hu.dwim.def+hu.dwim.common"
   "hu.dwim.def+hu.dwim.delico" "hu.dwim.defclass-star+hu.dwim.def"
   "hu.dwim.logger" "hu.dwim.rdbms" "hu.dwim.serializer"
   "hu.dwim.syntax-sugar+hu.dwim.walker" "hu.dwim.util.mop"
   "hu.dwim.walker" "ironclad" "local-time" "metacopy-with-contextl"
   "parse-number")
  ("hu.dwim.perec+hu.dwim.quasi-quote.xml" "hu.dwim.asdf"
   "hu.dwim.perec" "hu.dwim.quasi-quote.xml")
  ("hu.dwim.perec+iolib" "hu.dwim.asdf" "hu.dwim.perec"
   "iolib.sockets")
  ("hu.dwim.perec+swank" "hu.dwim.asdf" "hu.dwim.perec" "swank")
  ("hu.dwim.perec.all" "hu.dwim.asdf" "hu.dwim.perec.oracle"
   "hu.dwim.perec.postgresql" "hu.dwim.perec.sqlite")
  ("hu.dwim.perec.all.test" "hu.dwim.asdf"
   "hu.dwim.perec.oracle.test" "hu.dwim.perec.postgresql.test"
   "hu.dwim.perec.sqlite.test")
  ("hu.dwim.perec.documentation" "hu.dwim.asdf"
   "hu.dwim.perec.all.test" "hu.dwim.presentation")
  ("hu.dwim.perec.oracle" "hu.dwim.asdf" "hu.dwim.perec"
   "hu.dwim.rdbms.oracle")
  ("hu.dwim.perec.oracle.test" "hu.dwim.asdf" "hu.dwim.perec.oracle"
   "hu.dwim.perec.test")
  ("hu.dwim.perec.postgresql" "hu.dwim.asdf" "hu.dwim.perec"
   "hu.dwim.rdbms.postgresql")
  ("hu.dwim.perec.postgresql.test" "hu.dwim.asdf"
   "hu.dwim.perec.postgresql" "hu.dwim.perec.test")
  ("hu.dwim.perec.sqlite" "hu.dwim.asdf" "hu.dwim.perec"
   "hu.dwim.rdbms.sqlite")
  ("hu.dwim.perec.sqlite.test" "hu.dwim.asdf" "hu.dwim.perec.sqlite"
   "hu.dwim.perec.test")
  ("hu.dwim.perec.test" "hu.dwim.asdf"
   "hu.dwim.perec+hu.dwim.quasi-quote.xml" "hu.dwim.perec+iolib"
   "hu.dwim.perec+swank" "hu.dwim.util.test")
  ("hu.dwim.presentation" "cl-graph+hu.dwim.graphviz" "contextl"
   "hu.dwim.asdf" "hu.dwim.def+contextl" "hu.dwim.logger"
   "hu.dwim.stefil+hu.dwim.def" "hu.dwim.util.authorization"
   "hu.dwim.util.linear-mapping" "hu.dwim.util.source"
   "hu.dwim.util.standard-process" "hu.dwim.web-server.application"
   "iolib" "moptilities")
  ("hu.dwim.presentation+cl-graph+cl-typesetting" "cl-graph"
   "hu.dwim.asdf" "hu.dwim.presentation+cl-typesetting")
  ("hu.dwim.presentation+cl-typesetting" "cl-typesetting"
   "hu.dwim.asdf" "hu.dwim.presentation")
  ("hu.dwim.presentation+hu.dwim.stefil" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.stefil")
  ("hu.dwim.presentation+hu.dwim.web-server" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.web-server")
  ("hu.dwim.quasi-quote" "babel" "babel-streams" "hu.dwim.asdf"
   "hu.dwim.common" "hu.dwim.defclass-star+hu.dwim.def"
   "hu.dwim.syntax-sugar+hu.dwim.walker" "hu.dwim.util"
   "hu.dwim.walker")
  ("hu.dwim.quasi-quote.css" "hu.dwim.asdf" "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.quasi-quote.test")
  ("hu.dwim.quasi-quote.js" "cl-ppcre" "hu.dwim.asdf"
   "hu.dwim.quasi-quote" "hu.dwim.util.temporary-files"
   "hu.dwim.walker")
  ("hu.dwim.quasi-quote.test" "cxml" "hu.dwim.asdf"
   "hu.dwim.quasi-quote" "hu.dwim.quasi-quote.css"
   "hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js"
   "hu.dwim.stefil+hu.dwim.def+swank" "parse-number" "trivial-shell")
  ("hu.dwim.quasi-quote.xml" "hu.dwim.asdf" "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.xml+cxml" "cxml" "hu.dwim.asdf"
   "hu.dwim.quasi-quote.xml")
  ("hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js" "hu.dwim.asdf"
   "hu.dwim.quasi-quote.js" "hu.dwim.quasi-quote.xml")
  ("hu.dwim.rdbms" "babel" "hu.dwim.asdf"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.logger"
   "hu.dwim.syntax-sugar+hu.dwim.walker" "hu.dwim.util"
   "hu.dwim.util.error-handling" "hu.dwim.walker" "ironclad"
   "local-time")
  ("hu.dwim.rdbms.all" "hu.dwim.asdf" "hu.dwim.rdbms.oracle"
   "hu.dwim.rdbms.postgresql" "hu.dwim.rdbms.sqlite")
  ("hu.dwim.rdbms.all.test" "hu.dwim.asdf"
   "hu.dwim.rdbms.oracle.test" "hu.dwim.rdbms.postgresql.test"
   "hu.dwim.rdbms.sqlite.test")
  ("hu.dwim.rdbms.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.rdbms.all.test")
  ("hu.dwim.rdbms.oracle" "hu.dwim.asdf" "hu.dwim.rdbms"
   "verrazano-runtime")
  ("hu.dwim.rdbms.oracle.test" "hu.dwim.asdf" "hu.dwim.rdbms.oracle"
   "hu.dwim.rdbms.test")
  ("hu.dwim.rdbms.postgresql" "cl-postgres+local-time" "hu.dwim.asdf"
   "hu.dwim.rdbms")
  ("hu.dwim.rdbms.postgresql.test" "hu.dwim.asdf"
   "hu.dwim.rdbms.postgresql" "hu.dwim.rdbms.test")
  ("hu.dwim.rdbms.sqlite" "hu.dwim.asdf" "hu.dwim.rdbms"
   "verrazano-runtime")
  ("hu.dwim.rdbms.sqlite.test" "hu.dwim.asdf" "hu.dwim.rdbms.sqlite"
   "hu.dwim.rdbms.test")
  ("hu.dwim.rdbms.test" "hu.dwim.asdf" "hu.dwim.rdbms"
   "hu.dwim.stefil+hu.dwim.def+swank")
  ("hu.dwim.reiterate" "alexandria" "anaphora" "hu.dwim.asdf"
   "hu.dwim.common-lisp" "hu.dwim.def" "hu.dwim.defclass-star"
   "hu.dwim.syntax-sugar+hu.dwim.walker" "hu.dwim.util"
   "metabang-bind")
  ("hu.dwim.reiterate+hu.dwim.logger" "hu.dwim.asdf" "hu.dwim.logger"
   "hu.dwim.reiterate")
  ("hu.dwim.reiterate.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.reiterate.test")
  ("hu.dwim.reiterate.test" "hu.dwim.asdf" "hu.dwim.debug"
   "hu.dwim.reiterate+hu.dwim.logger"
   "hu.dwim.stefil+hu.dwim.def+swank")
  ("hu.dwim.serializer" "babel" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.def" "hu.dwim.syntax-sugar" "hu.dwim.util"
   "hu.dwim.util.mop")
  ("hu.dwim.serializer.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.serializer.test")
  ("hu.dwim.serializer.test" "hu.dwim.asdf" "hu.dwim.serializer"
   "hu.dwim.stefil+hu.dwim.def+swank")
  ("hu.dwim.stefil" "alexandria" "hu.dwim.asdf")
  ("hu.dwim.stefil+hu.dwim.def" "hu.dwim.asdf" "hu.dwim.def"
   "hu.dwim.stefil")
  ("hu.dwim.stefil+hu.dwim.def+swank" "hu.dwim.asdf"
   "hu.dwim.def+swank" "hu.dwim.stefil+hu.dwim.def"
   "hu.dwim.stefil+swank")
  ("hu.dwim.stefil+swank" "hu.dwim.asdf" "hu.dwim.stefil" "swank")
  ("hu.dwim.stefil.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.stefil.test")
  ("hu.dwim.stefil.test" "hu.dwim.asdf" "hu.dwim.stefil")
  ("hu.dwim.syntax-sugar" "hu.dwim.asdf" "hu.dwim.common")
  ("hu.dwim.syntax-sugar+hu.dwim.walker" "hu.dwim.asdf"
   "hu.dwim.syntax-sugar" "hu.dwim.walker")
  ("hu.dwim.syntax-sugar.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.syntax-sugar.test")
  ("hu.dwim.syntax-sugar.test" "hu.dwim.asdf"
   "hu.dwim.stefil+hu.dwim.def+swank"
   "hu.dwim.syntax-sugar+hu.dwim.walker"
   "hu.dwim.syntax-sugar.unicode")
  ("hu.dwim.syntax-sugar.unicode" "hu.dwim.asdf"
   "hu.dwim.syntax-sugar")
  ("hu.dwim.uri" "babel" "cl-ppcre" "hu.dwim.asdf" "hu.dwim.util"
   "iolib")
  ("hu.dwim.uri.test" "hu.dwim.asdf"
   "hu.dwim.stefil+hu.dwim.def+swank" "hu.dwim.uri" "hu.dwim.util")
  ("hu.dwim.util" "hu.dwim.asdf" "hu.dwim.def+hu.dwim.common"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.syntax-sugar")
  ("hu.dwim.util+iolib" "hu.dwim.asdf" "hu.dwim.util" "iolib")
  ("hu.dwim.util.authorization" "hu.dwim.asdf"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.logger"
   "hu.dwim.partial-eval" "hu.dwim.util" "hu.dwim.walker")
  ("hu.dwim.util.documentation" "hu.dwim.asdf" "hu.dwim.presentation"
   "hu.dwim.stefil+hu.dwim.def+swank")
  ("hu.dwim.util.error-handling" "hu.dwim.asdf"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.logger"
   "hu.dwim.util")
  ("hu.dwim.util.error-handling+swank" "hu.dwim.asdf"
   "hu.dwim.util.error-handling" "swank")
  ("hu.dwim.util.finite-state-machine" "hu.dwim.asdf"
   "hu.dwim.def.namespace" "hu.dwim.defclass-star+hu.dwim.def"
   "hu.dwim.util")
  ("hu.dwim.util.flexml" "cl-ppcre" "cxml" "hu.dwim.asdf"
   "hu.dwim.def" "hu.dwim.def.namespace"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.util")
  ("hu.dwim.util.i18n" "cl-l10n" "hu.dwim.asdf" "hu.dwim.util")
  ("hu.dwim.util.linear-mapping" "bordeaux-threads" "hu.dwim.asdf"
   "hu.dwim.util")
  ("hu.dwim.util.mop" "closer-mop" "hu.dwim.asdf" "hu.dwim.util")
  ("hu.dwim.util.production" "cl-fad" "command-line-arguments"
   "hu.dwim.asdf" "hu.dwim.logger" "hu.dwim.perec.postgresql"
   "hu.dwim.util+iolib" "hu.dwim.util.error-handling"
   "hu.dwim.util.temporary-files" "hu.dwim.web-server.application"
   "iolib")
  ("hu.dwim.util.production+swank" "hu.dwim.asdf"
   "hu.dwim.util.production" "swank")
  ("hu.dwim.util.soap" "babel" "babel-streams" "cxml" "drakma"
   "hu.dwim.asdf" "hu.dwim.defclass-star+hu.dwim.def"
   "hu.dwim.logger" "hu.dwim.quasi-quote.xml" "hu.dwim.util"
   "hu.dwim.util.flexml")
  ("hu.dwim.util.source" "hu.dwim.asdf" "hu.dwim.def+hu.dwim.common"
   "hu.dwim.syntax-sugar" "hu.dwim.util" "swank")
  ("hu.dwim.util.standard-process" "hu.dwim.asdf"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.delico"
   "hu.dwim.logger" "hu.dwim.util.finite-state-machine")
  ("hu.dwim.util.temporary-files" "hu.dwim.asdf" "hu.dwim.util+iolib"
   "iolib")
  ("hu.dwim.util.test" "hu.dwim.asdf"
   "hu.dwim.stefil+hu.dwim.def+swank" "hu.dwim.util"
   "hu.dwim.util.error-handling" "hu.dwim.util.error-handling+swank"
   "hu.dwim.util.finite-state-machine" "hu.dwim.util.i18n"
   "hu.dwim.util.mop" "hu.dwim.util.production+swank"
   "hu.dwim.util.soap" "hu.dwim.util.source"
   "hu.dwim.util.standard-process" "hu.dwim.util.threads"
   "hu.dwim.util.worker-group")
  ("hu.dwim.util.threads" "bordeaux-threads" "hu.dwim.asdf"
   "hu.dwim.def.namespace" "hu.dwim.util")
  ("hu.dwim.util.worker-group" "bordeaux-threads" "hu.dwim.asdf"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.logger"
   "hu.dwim.util.error-handling")
  ("hu.dwim.util.zlib" "cffi" "hu.dwim.asdf"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.logger"
   "hu.dwim.util")
  ("hu.dwim.walker" "alexandria" "anaphora" "closer-mop" "contextl"
   "hu.dwim.asdf" "hu.dwim.common-lisp" "hu.dwim.def+contextl"
   "hu.dwim.defclass-star+hu.dwim.def" "hu.dwim.util"
   "metabang-bind")
  ("hu.dwim.walker.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.walker.test")
  ("hu.dwim.walker.test" "hu.dwim.asdf" "hu.dwim.stefil+hu.dwim.def"
   "hu.dwim.stefil+swank" "hu.dwim.util.temporary-files"
   "hu.dwim.walker")
  ("hu.dwim.web-server" "babel" "babel-streams" "bordeaux-threads"
   "cffi" "cl+ssl" "cl-fad" "hu.dwim.asdf" "hu.dwim.common"
   "hu.dwim.computed-class" "hu.dwim.def+cl-l10n"
   "hu.dwim.def+contextl" "hu.dwim.def+hu.dwim.delico"
   "hu.dwim.def.namespace" "hu.dwim.logger+iolib"
   "hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js"
   "hu.dwim.syntax-sugar" "hu.dwim.uri" "hu.dwim.util.error-handling"
   "hu.dwim.util.temporary-files" "hu.dwim.util.zlib" "iolib"
   "local-time" "parse-number" "rfc2109" "rfc2388-binary" "swank")
  ("hu.dwim.web-server+swank" "hu.dwim.asdf" "hu.dwim.def+swank"
   "hu.dwim.web-server")
  ("hu.dwim.web-server.application" "hu.dwim.asdf"
   "hu.dwim.web-server")
  ("hu.dwim.web-server.application+hu.dwim.perec" "hu.dwim.asdf"
   "hu.dwim.perec" "hu.dwim.web-server.application")
  ("hu.dwim.web-server.application.test" "hu.dwim.asdf"
   "hu.dwim.web-server.application" "hu.dwim.web-server.test")
  ("hu.dwim.web-server.documentation" "hu.dwim.asdf"
   "hu.dwim.presentation" "hu.dwim.web-server.test")
  ("hu.dwim.web-server.test" "drakma" "hu.dwim.asdf"
   "hu.dwim.computed-class+hu.dwim.logger"
   "hu.dwim.stefil+hu.dwim.def+swank" "hu.dwim.web-server"
   "hu.dwim.web-server+swank")
  ("hu.dwim.web-server.websocket" "cl-base64" "hu.dwim.asdf"
   "hu.dwim.web-server" "ironclad")
  ("humbler" "cl-ppcre" "closer-mop" "local-time" "south"
   "trivial-mimes" "yason")
  ("hunchensocket" "alexandria" "bordeaux-threads" "chunga" "cl-fad"
   "flexi-streams" "hunchentoot" "ironclad" "trivial-backtrace"
   "trivial-utf-8")
  ("hunchentoot" "bordeaux-threads" "chunga" "cl+ssl" "cl-base64"
   "cl-fad" "cl-ppcre" "flexi-streams" "md5" "rfc2388"
   "trivial-backtrace" "usocket")
  ("hunchentoot-auth" "bordeaux-threads" "cl-store" "cl-who"
   "hunchentoot")
  ("hunchentoot-cgi" "hunchentoot" "puri")
  ("hunchentoot-dev" "cxml-stp" "hunchentoot" "hunchentoot-test"
   "swank" "xpath")
  ("hunchentoot-single-signon" "cl-base64" "cl-gss" "hunchentoot"
   "split-sequence")
  ("hunchentoot-test" "cl-ppcre" "cl-who" "drakma" "hunchentoot")
  ("hyperluminal-mem" "cffi" "osicat" "stmx" "swap-bytes"
   "trivial-features")
  ("hyperluminal-mem-test" "fiveam" "hyperluminal-mem" "log4cl")
  ("hyperobject" "clsql" "kmrcl")
  ("hyperobject-tests" "hyperobject" "rt") ("iconv" "cffi" "iolib")
  ("idna" "split-sequence") ("ie3fp") ("ieee-floats")
  ("ieee-floats-tests" "eos" "ieee-floats")
  ("image" "flexi-streams" "gzip-stream" "skippy" "zpng")
  ("imago" "zlib")
  ("immutable-struct" "alexandria" "closer-mop" "trivia")
  ("incf-cl" "cl-ppcre")
  ("incf-cl-test" "hu.dwim.stefil" "hu.dwim.stefil+swank" "incf-cl")
  ("incognito-keywords" "enhanced-eval-when" "map-bind")
  ("incongruent-methods" "closer-mop")
  ("inferior-shell" "alexandria" "asdf" "fare-mop"
   "fare-quasiquote-extras" "fare-utils" "optima")
  ("inferior-shell/test" "hu.dwim.stefil" "inferior-shell") ("infix")
  ("infix-dollar-reader" "cl-syntax")
  ("infix-dollar-reader-test" "infix-dollar-reader" "rt")
  ("inner-conditional" "alexandria" "cl-syntax-annot" "iterate"
   "optima")
  ("inner-conditional-test" "cl-test-more" "inner-conditional")
  ("inotify" "cffi" "cffi-grovel" "iolib")
  ("integral" "alexandria" "cl-ppcre" "cl-syntax-annot"
   "clos-fixtures" "closer-mop" "dbi" "group-by" "iterate"
   "split-sequence" "sxql" "trivial-types")
  ("integral-rest" "alexandria" "cl-inflector" "closer-mop"
   "integral" "jonathan" "map-set" "ningle")
  ("integral-rest-test" "integral" "integral-rest" "prove"
   "prove-asdf")
  ("integral-test" "integral" "local-time" "prove" "prove-asdf"
   "split-sequence" "uiop")
  ("intel-hex") ("intel-hex-test" "intel-hex" "prove" "prove-asdf")
  ("intercom" "alexandria" "bordeaux-threads" "hunchentoot" "jsown"
   "split-sequence")
  ("intercom-examples" "intercom" "jsown") ("interface" "alexandria")
  ("interfaces-test-implementation" "modularize"
   "modularize-interfaces")
  ("introspect-environment")
  ("introspect-environment-test" "fiveam" "introspect-environment")
  ("iolib" "alexandria" "babel" "bordeaux-threads" "cffi" "idna"
   "split-sequence" "swap-bytes" "trivial-features" "uiop")
  ("iolib-grovel" "alexandria" "cffi" "uiop")
  ("iolib-tests" "alexandria" "cffi" "fiveam" "iolib"
   "split-sequence" "trivial-features" "uiop")
  ("iolib.asdf" "alexandria")
  ("iolib.base" "alexandria" "split-sequence")
  ("iolib.common-lisp" "alexandria") ("iolib.conf" "alexandria")
  ("iolib.examples" "alexandria" "bordeaux-threads" "iolib"
   "split-sequence")
  ("iolib.multiplex" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib.os" "alexandria" "cffi" "split-sequence" "trivial-features"
   "uiop")
  ("iolib.pathnames" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib.sockets" "alexandria" "babel" "bordeaux-threads" "cffi"
   "idna" "split-sequence" "swap-bytes" "trivial-features" "uiop")
  ("iolib.streams" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib.syscalls" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib.trivial-sockets" "alexandria" "babel" "bordeaux-threads"
   "cffi" "idna" "split-sequence" "swap-bytes" "trivial-features"
   "uiop")
  ("iolib/asdf" "alexandria")
  ("iolib/base" "alexandria" "split-sequence")
  ("iolib/common-lisp" "alexandria") ("iolib/conf" "alexandria")
  ("iolib/examples" "alexandria" "bordeaux-threads" "iolib"
   "split-sequence")
  ("iolib/grovel" "alexandria" "cffi" "uiop")
  ("iolib/multiplex" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib/os" "alexandria" "cffi" "split-sequence" "trivial-features"
   "uiop")
  ("iolib/pathnames" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib/sockets" "alexandria" "babel" "bordeaux-threads" "cffi"
   "idna" "split-sequence" "swap-bytes" "trivial-features" "uiop")
  ("iolib/streams" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib/syscalls" "alexandria" "cffi" "split-sequence"
   "trivial-features" "uiop")
  ("iolib/tests" "alexandria" "cffi" "fiveam" "iolib"
   "split-sequence" "trivial-features" "uiop")
  ("iolib/trivial-sockets" "alexandria" "babel" "bordeaux-threads"
   "cffi" "idna" "split-sequence" "swap-bytes" "trivial-features"
   "uiop")
  ("iolib/zstreams" "alexandria" "bordeaux-threads" "cffi"
   "split-sequence" "trivial-features" "uiop")
  ("ip-interfaces" "cffi") ("irc-logger" "cl-irc" "cl-ppcre")
  ("ironclad" "nibbles") ("ironclad-tests" "ironclad")
  ("ironclad-text" "flexi-streams" "ironclad") ("iterate")
  ("iterate-clsql" "clsql" "iterate") ("iterate-pg" "iterate" "pg")
  ("iterate-tests" "iterate")
  ("ixf" "alexandria" "babel" "cl-ppcre" "ieee-floats" "local-time"
   "md5" "split-sequence")
  ("jenkins.api" "alexandria" "cl-json" "cl-ppcre" "closer-mop"
   "drakma" "iterate" "let-plus" "more-conditions" "puri"
   "split-sequence" "xml.location")
  ("jonathan" "babel" "cl-annot" "cl-ppcre" "cl-syntax"
   "cl-syntax-annot" "fast-io" "proc-parse" "trivial-types")
  ("jonathan-test" "jonathan" "prove" "prove-asdf")
  ("jp-numeral" "alexandria" "babel")
  ("jpl-queues" "bordeaux-threads" "jpl-util") ("jpl-util")
  ("js-parser" "cl-ppcre") ("js-parser-tests" "js-parser")
  ("json-responses" "cl-json" "hunchentoot")
  ("json-responses-test" "fiveam" "json-responses") ("json-streams")
  ("json-streams-tests" "cl-quickcheck" "flexi-streams"
   "json-streams")
  ("json-template") ("jsown") ("jsown-tests" "fiveam" "jsown")
  ("jwacs" "cl-ppcre") ("jwacs-tests" "jwacs") ("kanren-trs")
  ("kanren-trs-test" "kanren-trs")
  ("kebab" "alexandria" "cl-interpol" "cl-ppcre" "split-sequence")
  ("kebab-test" "kebab" "prove" "prove-asdf") ("kenzo")
  ("kenzo-test" "fiveam" "kenzo") ("kl-verify" "image") ("km")
  ("kmrcl") ("kmrcl-tests" "kmrcl" "rt") ("l-math")
  ("lack" "lack-component" "lack-util") ("lack-component")
  ("lack-middleware-accesslog" "lack-util" "local-time")
  ("lack-middleware-auth-basic" "cl-base64" "split-sequence")
  ("lack-middleware-backtrace" "uiop")
  ("lack-middleware-csrf" "lack-request" "lack-util")
  ("lack-middleware-mount" "lack-component")
  ("lack-middleware-session" "cl-ppcre" "lack-request"
   "lack-response" "lack-util")
  ("lack-middleware-static" "alexandria" "cl-fad" "local-time"
   "trivial-mimes")
  ("lack-request" "cl-ppcre" "http-body" "quri")
  ("lack-response" "local-time" "quri")
  ("lack-session-store-dbi" "cl-base64" "dbi"
   "lack-middleware-session" "marshal")
  ("lack-test" "flexi-streams" "lack" "prove" "quri")
  ("lack-util" "alexandria" "ironclad") ("lambda-fiddle")
  ("lambda-gtk" "cffi") ("lambda-gtk-examples" "lambda-gtk")
  ("lambda-reader" "named-readtables")
  ("lambda-reader-8bit" "asdf" "asdf-encodings" "named-readtables")
  ("lambdalite" "bordeaux-threads" "wu-sugar")
  ("langutils" "s-xml-rpc" "stdutils")
  ("lapack" "f2cl" "lapack-complex" "lapack-package" "lapack-real")
  ("lapack-complex" "blas-complex" "lapack-real")
  ("lapack-package" "blas-package")
  ("lapack-real" "blas-real" "lapack-package")
  ("lapack-tests" "lapack" "rt")
  ("lass" "cl-base64" "trivial-indent" "trivial-mimes")
  ("lassie" "fsvd")
  ("latex-table" "alexandria" "anaphora" "array-operations"
   "let-plus")
  ("ledger" "gwl") ("leech" "aserve" "unit-test")
  ("legion" "bordeaux-threads" "cl-speedy-queue" "vom")
  ("legion-test" "legion" "local-time" "prove" "prove-asdf")
  ("let-over-lambda" "cl-ppcre" "named-readtables")
  ("let-plus" "alexandria" "anaphora")
  ("let-plus-tests" "let-plus" "lift") ("letrec" "alexandria")
  ("lev" "cffi") ("levenshtein") ("lexer" "regex")
  ("lfarm-admin" "lfarm-common" "usocket")
  ("lfarm-client" "lfarm-common" "lparallel" "usocket")
  ("lfarm-common" "alexandria" "bordeaux-threads" "cl-store"
   "flexi-streams" "usocket")
  ("lfarm-gss" "cl-gss" "lfarm-common" "trivial-gray-streams")
  ("lfarm-launcher" "external-program" "lfarm-admin" "lfarm-server")
  ("lfarm-server" "lfarm-common" "usocket")
  ("lfarm-ssl" "cl+ssl" "lfarm-common")
  ("lfarm-test" "lfarm-admin" "lfarm-client" "lfarm-launcher"
   "lfarm-server")
  ("lhstats")
  ("libssh2" "babel" "cffi" "cffi-grovel" "cl-fad" "hu.dwim.logger"
   "split-sequence" "trivial-gray-streams" "usocket")
  ("libssh2.test" "hu.dwim.stefil" "libssh2")
  ("libusb-ffi" "antik" "cffi" "cffi-grovel") ("lift")
  ("lift-and-metatilities" "lift" "metatilities-base")
  ("lift-documentation" "lift") ("lift-test" "lift") ("lil")
  ("lil/test") ("lime" "alexandria" "swank-protocol" "trivial-types")
  ("lime-example" "bordeaux-threads" "lime")
  ("lime-test" "alexandria" "external-program" "fiveam" "lime")
  ("linedit" "alexandria" "madeira-port" "osicat" "terminfo" "uffi")
  ("linewise-template" "cl-ppcre") ("lisa")
  ("lisp-executable" "alexandria")
  ("lisp-executable-example" "lisp-executable")
  ("lisp-executable-tests" "lisp-executable" "lisp-unit")
  ("lisp-interface-library" "lil") ("lisp-invocation")
  ("lisp-invocation/all" "lisp-invocation")
  ("lisp-matrix" "cffi" "cl-utilities" "ffa" "lift"
   "org.middleangle.cl-blapack"
   "org.middleangle.foreign-numeric-vector" "xarray")
  ("lisp-namespace" "alexandria" "fare-quasiquote-extras" "optima")
  ("lisp-namespace.test" "fiveam" "lisp-namespace") ("lisp-unit")
  ("lisp-unit2" "alexandria" "cl-interpol" "iterate" "symbol-munger")
  ("lisp-unit2-test" "lisp-unit2")
  ("lispbuilder-lexer" "lispbuilder-regex")
  ("lispbuilder-net" "cffi" "lispbuilder-net-cffi")
  ("lispbuilder-net-cffi" "cffi") ("lispbuilder-opengl-1-1" "cffi")
  ("lispbuilder-opengl-examples" "cffi" "lispbuilder-opengl-1-1"
   "lispbuilder-sdl")
  ("lispbuilder-regex")
  ("lispbuilder-sdl" "cffi" "lispbuilder-sdl-assets"
   "lispbuilder-sdl-base" "trivial-garbage")
  ("lispbuilder-sdl-assets")
  ("lispbuilder-sdl-base" "cffi" "lispbuilder-sdl-cffi")
  ("lispbuilder-sdl-binaries")
  ("lispbuilder-sdl-cffi" "cffi" "lispbuilder-sdl-binaries")
  ("lispbuilder-sdl-cl-vectors" "cl-aa-misc" "cl-paths-ttf"
   "cl-vectors" "lispbuilder-sdl" "zpb-ttf")
  ("lispbuilder-sdl-cl-vectors-examples"
   "lispbuilder-sdl-cl-vectors")
  ("lispbuilder-sdl-examples" "lispbuilder-sdl")
  ("lispbuilder-sdl-gfx" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-gfx-cffi")
  ("lispbuilder-sdl-gfx-binaries")
  ("lispbuilder-sdl-gfx-cffi" "cffi" "lispbuilder-sdl")
  ("lispbuilder-sdl-gfx-examples" "lispbuilder-sdl-gfx")
  ("lispbuilder-sdl-image" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-image-cffi")
  ("lispbuilder-sdl-image-binaries")
  ("lispbuilder-sdl-image-cffi" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-image-binaries")
  ("lispbuilder-sdl-image-examples" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-image")
  ("lispbuilder-sdl-mixer" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-mixer-cffi")
  ("lispbuilder-sdl-mixer-binaries")
  ("lispbuilder-sdl-mixer-cffi" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-mixer-binaries")
  ("lispbuilder-sdl-mixer-examples" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-mixer")
  ("lispbuilder-sdl-ttf" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-ttf-cffi")
  ("lispbuilder-sdl-ttf-binaries")
  ("lispbuilder-sdl-ttf-cffi" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-ttf-binaries")
  ("lispbuilder-sdl-ttf-examples" "cffi" "lispbuilder-sdl"
   "lispbuilder-sdl-ttf")
  ("lispbuilder-sdl-vecto" "lispbuilder-sdl"
   "lispbuilder-sdl-cl-vectors" "vecto")
  ("lispbuilder-sdl-vecto-examples" "lispbuilder-sdl-vecto")
  ("lispbuilder-windows" "cffi") ("lispbuilder-yacc")
  ("list-of" "asdf-finalizers") ("listoflist" "clunit" "xarray")
  ("lla" "alexandria" "anaphora" "cffi" "cl-num-utils" "cl-slice"
   "let-plus")
  ("lla-tests" "clunit" "lla")
  ("llvm" "cffi" "cffi-grovel" "cl-ppcre" "split-sequence"
   "trivial-features" "trivial-shell")
  ("lml") ("lml-tests" "lml" "rt") ("lml2" "kmrcl")
  ("lml2-tests" "lml2" "rt") ("local-package-aliases")
  ("local-time" "cl-fad")
  ("local-time-duration" "alexandria" "esrap" "local-time")
  ("local-time.test" "local-time" "stefil")
  ("log4cl" "bordeaux-threads") ("log4cl-examples" "log4cl" "swank")
  ("log4cl-test" "log4cl" "stefil") ("log4slime" "log4cl" "swank")
  ("log5")
  ("lol-re" "alexandria" "cl-interpol" "cl-ppcre"
   "cl-read-macro-tokens" "defmacro-enhance" "hu.dwim.walker"
   "iterate" "named-readtables")
  ("lol-re-tests" "fiveam" "iterate" "lol-re")
  ("lowlight" "alexandria" "cl-ppcre" "cl-who" "graylex" "spinneret"
   "yacc")
  ("lowlight.doc" "cl-gendoc" "lowlight" "lowlight.tests")
  ("lowlight.old" "alexandria" "cl-ppcre" "cl-who" "spinneret")
  ("lowlight.tests" "fiveam" "lowlight")
  ("lparallel" "alexandria" "bordeaux-threads")
  ("lparallel-bench" "lparallel" "trivial-garbage")
  ("lparallel-test" "lparallel")
  ("lquery" "array-utils" "clss" "plump")
  ("lquery-test" "fiveam" "lquery") ("lracer")
  ("lredis" "babel" "babel-streams" "usocket") ("ltk")
  ("ltk-mw" "ltk") ("ltk-remote" "ltk")
  ("lucene-in-action-tests" "lift" "montezuma")
  ("lucerne" "alexandria" "cl-annot" "cl-mustache" "clack"
   "clack-errors" "clack-v1-compat" "djula" "local-time" "log4cl"
   "myway" "trivial-types")
  ("lucerne-auth" "cl-pass" "lucerne")
  ("lucerne-hello-world" "lucerne")
  ("lucerne-test" "drakma" "fiveam" "lucerne" "lucerne-hello-world"
   "lucerne-utweet")
  ("lucerne-utweet" "avatar-api" "local-time" "lucerne"
   "lucerne-auth")
  ("lw-compat")
  ("m2cl" "babel" "cl-json" "cl-ppcre" "flexi-streams" "salza2"
   "zmq")
  ("m2cl-examples" "bordeaux-threads" "m2cl")
  ("m2cl-test" "fiveam" "m2cl") ("mach-par") ("macro-level")
  ("macroexpand-dammit") ("madeira-port")
  ("madeira-port-tests" "eos" "madeira-port")
  ("magicffi" "cffi" "cffi-grovel" "cl-ppcre")
  ("magicffi-test" "alexandria" "magicffi")
  ("mailbox" "bordeaux-threads") ("make-hash")
  ("make-hash-tests" "fiveam" "make-hash")
  ("manardb" "alexandria" "cl-irregsexp" "closer-mop" "iterate"
   "osicat")
  ("manardb-test" "manardb" "stefil")
  ("manifest" "alexandria" "closer-mop" "monkeylib-html" "puri"
   "split-sequence" "toot")
  ("map-bind") ("map-set") ("marching-cubes")
  ("marching-cubes-example" "marching-cubes")
  ("marching-cubes-test" "cl-test-more" "marching-cubes") ("marshal")
  ("math-high" "antik" "gsll")
  ("math-high-tests" "lisp-unit" "math-high")
  ("mathkit" "alexandria") ("mcclim" "clim-looks")
  ("mcclim-freetype" "clim-clx" "mcclim")
  ("mcclim-gif-bitmaps" "mcclim" "skippy")
  ("mcclim-jpeg-bitmaps" "cl-jpeg" "mcclim")
  ("mcclim-png-bitmaps" "mcclim" "png-read")
  ("mcclim-tiff-bitmaps" "mcclim" "retrospectiff")
  ("mcclim-tree-with-cross-edges" "mcclim")
  ("mcclim-truetype" "cl-aa" "cl-paths-ttf" "cl-vectors" "clim-clx"
   "mcclim" "zpb-ttf")
  ("md5") ("media-types" "alexandria" "cl-ppcre" "serapeum")
  ("media-types-tests" "fiveam" "media-types")
  ("mel-base" "cl+ssl" "flexi-streams" "usocket") ("memoize")
  ("message-oo") ("meta" "named-readtables") ("meta-sexp")
  ("metabang-bind") ("metabang-bind-test" "lift" "metabang-bind")
  ("metacopy" "moptilities") ("metacopy-test" "lift" "metacopy")
  ("metacopy-test-with-contextl" "lift" "metacopy-test"
   "metacopy-with-contextl")
  ("metacopy-with-contextl" "contextl" "metacopy") ("metafs" "sclf")
  ("metap" "closer-mop") ("metap-test" "fiveam" "metap")
  ("metatilities" "asdf-system-connections" "cl-containers"
   "metabang-bind" "metatilities-base" "moptilities")
  ("metatilities-base") ("metatilities-test" "lift" "metatilities")
  ("method-combination-utilities" "closer-mop")
  ("method-combination-utilities.tests" "fiveam"
   "method-combination-utilities")
  ("method-versions") ("mexpr" "alexandria" "cl-syntax")
  ("mexpr-tests" "mexpr" "named-readtables" "should-test")
  ("mgl" "alexandria" "array-operations" "closer-mop" "ieee-floats"
   "lla" "mgl-gnuplot")
  ("mgl-example" "cl-ppcre" "mgl")
  ("mgl-gnuplot" "alexandria" "external-program")
  ("mgl-pax" "3bmd" "3bmd-ext-code-blocks" "alexandria" "babel"
   "cl-fad" "colorize" "ironclad" "named-readtables"
   "pythonic-string-reader" "swank")
  ("mgl-pax-test" "mgl-pax") ("mgl-test" "mgl")
  ("mgl-visuals" "cl-dot" "cl-ppcre" "mgl") ("micmac" "mgl-pax")
  ("micmac-test" "micmac") ("midi") ("mime4cl" "npg" "sclf")
  ("mime4cl-tests" "mime4cl") ("minheap")
  ("minheap-tests" "lisp-unit" "minheap") ("minpack" "f2cl")
  ("minpack-tests-hybrd" "minpack") ("minpack-tests-lmdif" "minpack")
  ("misc-extensions")
  ("mixalot" "alexandria" "bordeaux-threads" "cffi")
  ("mixalot-flac" "cffi" "flac" "mixalot")
  ("mixalot-mp3" "cffi" "mixalot" "mpg123-ffi")
  ("mixalot-vorbis" "cffi" "mixalot" "vorbisfile-ffi")
  ("mk-string-metrics")
  ("mk-string-metrics-tests" "mk-string-metrics") ("mlep")
  ("mlep-add" "cffi" "cl-num-utils" "lla" "mlep")
  ("mnst-relay" "asdf-nst" "nst" "nst-selftest-utils")
  ("modf" "alexandria" "closer-mop" "iterate")
  ("modf-fset" "fset" "modf")
  ("modf-fset-test" "modf" "modf-fset" "stefil")
  ("modf-test" "iterate" "modf" "stefil") ("modlisp" "kmrcl")
  ("modularize")
  ("modularize-hooks" "closer-mop" "lambda-fiddle" "modularize"
   "trivial-arguments")
  ("modularize-interfaces" "lambda-fiddle" "modularize"
   "trivial-arguments" "trivial-indent")
  ("modularize-test-module" "modularize") ("module-manager")
  ("module-manager-user")
  ("monkeylib-html" "com.gigamonkeys.macro-utilities"
   "com.gigamonkeys.pathnames" "com.gigamonkeys.test-framework"
   "com.gigamonkeys.utilities" "monkeylib-text-languages"
   "monkeylib-text-output")
  ("monkeylib-markup-html" "alexandria"
   "com.gigamonkeys.macro-utilities" "com.gigamonkeys.markup"
   "com.gigamonkeys.utilities" "monkeylib-html")
  ("monkeylib-text-languages" "com.gigamonkeys.macro-utilities"
   "monkeylib-text-output")
  ("monkeylib-text-output" "com.gigamonkeys.macro-utilities"
   "com.gigamonkeys.pathnames" "com.gigamonkeys.test-framework"
   "com.gigamonkeys.utilities")
  ("montezuma" "babel" "cl-fad" "cl-ppcre")
  ("montezuma-indexfiles" "cl-fad" "montezuma")
  ("montezuma-tests" "montezuma" "trivial-timeout") ("mop-utils")
  ("moptilities" "closer-mop")
  ("moptilities-test" "lift" "moptilities")
  ("more-conditions" "alexandria" "closer-mop")
  ("more-conditions-test" "fiveam" "let-plus" "more-conditions")
  ("mpc") ("mpg123-ffi" "cffi")
  ("mssql" "cffi" "garbage-pools" "iterate" "parse-number")
  ("mt19937") ("mtlisp" "acl-compat") ("multinode")
  ("multiple-value-variants" "enhanced-multiple-value-bind"
   "map-bind" "positional-lambda")
  ("multival-plist" "alexandria" "cl-annot" "cl-syntax-annot"
   "trivial-types")
  ("multival-plist-test" "cl-test-more" "multival-plist")
  ("mw-equiv")
  ("myway" "alexandria" "cl-ppcre" "cl-utilities" "map-set" "quri")
  ("myway-test" "myway" "prove" "prove-asdf")
  ("myweb" "bordeaux-threads" "cl-log" "local-time" "trivial-utf-8"
   "usocket")
  ("named-readtables")
  ("named-readtables-doc" "mgl-pax" "named-readtables")
  ("named-readtables-test" "named-readtables") ("napa-fft3")
  ("net-telent-date")
  ("net.didierverna.clon" "net.didierverna.clon.core"
   "net.didierverna.clon.setup" "net.didierverna.clon.termio")
  ("net.didierverna.clon.core" "net.didierverna.clon.setup")
  ("net.didierverna.clon.setup")
  ("net.didierverna.clon.setup/termio" "net.didierverna.clon.setup")
  ("net.didierverna.clon.termio" "cffi" "net.didierverna.clon.core"
   "net.didierverna.clon.setup")
  ("net.didierverna.declt" "net.didierverna.declt.core"
   "net.didierverna.declt.setup")
  ("net.didierverna.declt.core" "net.didierverna.declt.setup")
  ("net.didierverna.declt.setup") ("net4cl" "sclf")
  ("network-streaming")
  ("neutral" "alexandria" "cl-ppcre" "curry-compose-reader-macros"
   "metabang-bind" "software-evolution"
   "software-evolution-command-line" "software-evolution-utility"
   "split-sequence")
  ("new-op") ("nibbles") ("nibbles-tests" "nibbles")
  ("ningle" "alexandria" "cl-syntax-annot" "lack-component"
   "lack-request" "lack-response" "myway")
  ("ningle-test" "babel" "clack-test" "drakma" "ningle" "prove"
   "yason")
  ("npg") ("nsort" "prove") ("nst" "closer-mop" "defdoc")
  ("nst-manual-tests" "asdf-nst" "nst" "nst-selftest-utils")
  ("nst-meta-tests" "asdf-nst" "nst" "nst-selftest-utils")
  ("nst-mop-utils" "closer-mop" "nst") ("nst-selftest-utils" "nst")
  ("nst-simple-tests" "asdf-nst" "nst" "nst-selftest-utils")
  ("nst-test" "asdf-nst" "nst" "nst-meta-tests" "nst-simple-tests")
  ("nst-test-jenkins" "asdf-nst" "nst" "nst-test")
  ("nuclblog" "bordeaux-threads" "cl-markdown" "cl-store" "cl-who"
   "hunchentoot" "hunchentoot-auth" "md5")
  ("nxt" "babel" "cffi" "static-vectors")
  ("nxt-proxy" "nxt" "usocket")
  ("odd-streams" "trivial-gray-streams")
  ("odd-streams-test" "odd-streams")
  ("odesk" "alexandria" "cl-ppcre" "drakma" "iterate" "md5"
   "split-sequence")
  ("ods4cl" "sclf") ("oe-encode" "babel")
  ("oe-encode-test" "clunit" "oe-encode")
  ("open-vrp" "alexandria" "fiveam" "open-vrp-lib" "vecto")
  ("open-vrp-lib" "alexandria" "cl-fad" "fiveam" "vecto")
  ("opticl" "alexandria" "cl-jpeg" "png-read" "retrospectiff"
   "skippy" "zpng")
  ("opticl-doc" "alexandria" "cl-markdown" "opticl")
  ("optima" "alexandria" "closer-mop")
  ("optima.ppcre" "alexandria" "cl-ppcre" "optima")
  ("optima.test" "eos" "optima" "optima.ppcre")
  ("or-glpk" "cffi" "cl-opsresearch")
  ("or-gsl" "cffi" "cl-opsresearch")
  ("or-test" "cl-opsresearch" "fiveam" "or-glpk" "or-gsl")
  ("org-davep-dict" "acl-compat" "cl-ppcre" "split-sequence")
  ("org-davep-dictrepl" "org-davep-dict") ("org-sampler" "iterate")
  ("org.middleangle.cl-blapack" "cffi"
   "org.middleangle.foreign-numeric-vector")
  ("org.middleangle.cl-blapack-examples"
   "org.middleangle.cl-blapack")
  ("org.middleangle.cl-blapack-gen" "cffi"
   "org.middleangle.foreign-numeric-vector")
  ("org.middleangle.foreign-numeric-vector" "cffi" "iterate")
  ("os-interface") ("osc")
  ("osicat" "alexandria" "cffi" "cffi-grovel" "trivial-features")
  ("osicat-tests" "osicat" "rt") ("pack" "alexandria" "ieee-floats")
  ("package-renaming" "alexandria")
  ("package-renaming-test" "hu.dwim.stefil" "package-renaming")
  ("packet" "ieee-floats") ("paiprolog") ("pal" "cffi")
  ("pango" "cffi" "cl-cairo2") ("par-eval" "cl-mpi")
  ("parameterized-function" "interface")
  ("paren-files" "parenscript")
  ("paren-util" "paren-files" "parenscript")
  ("parenscript" "anaphora" "cl-ppcre" "named-readtables")
  ("parenscript-classic")
  ("parenscript.test" "cl-js" "eos" "parenscript")
  ("parse-declarations-1.0") ("parse-float" "alexandria")
  ("parse-float-tests" "lisp-unit" "parse-float") ("parse-js")
  ("parse-number")
  ("parse-number-range" "cartesian-product-switch"
   "enhanced-multiple-value-bind" "map-bind")
  ("parse-number-tests" "parse-number")
  ("parse-rgb" "cl-ppcre" "tcod") ("parseltongue" "lisp-unit")
  ("parser-combinators" "alexandria" "iterate")
  ("parser-combinators-cl-ppcre" "alexandria" "cl-ppcre" "iterate"
   "parser-combinators")
  ("parser-combinators-debug" "cl-containers" "parser-combinators")
  ("parser-combinators-tests" "alexandria" "hu.dwim.stefil" "infix"
   "iterate" "parser-combinators")
  ("patron" "bordeaux-threads")
  ("pcall" "bordeaux-threads" "pcall-queue")
  ("pcall-queue" "bordeaux-threads") ("pcall-tests" "fiveam" "pcall")
  ("pcl-unit-test" "standard-cl")
  ("percent-encoding" "anaphora" "babel")
  ("percent-encoding-test" "fiveam" "percent-encoding")
  ("perfpiece" "alexandria" "cffi") ("periodic-table")
  ("periods" "local-time") ("periods-series" "periods" "series")
  ("perlre" "cl-interpol" "cl-ppcre" "let-over-lambda" "prove")
  ("persistent-tables" "lisp-unit" "random-access-lists")
  ("persistent-variables")
  ("persistent-variables.test" "persistent-variables")
  ("petit.package-utils") ("petit.string-utils")
  ("petit.string-utils-test" "petit.string-utils" "rt")
  ("pettomato-deque")
  ("pettomato-deque-tests" "fiveam" "pettomato-deque")
  ("pettomato-indexed-priority-queue")
  ("pettomato-indexed-priority-queue-tests" "fiveam"
   "pettomato-indexed-priority-queue")
  ("pfft" "fft" "pcall") ("pg")
  ("pgloader" "abnf" "alexandria" "cl-base64" "cl-csv" "cl-fad"
   "cl-log" "cl-markdown" "cl-postgres" "command-line-arguments"
   "db3" "drakma" "esrap" "flexi-streams" "ixf" "local-time"
   "lparallel" "metabang-bind" "mssql" "postmodern" "py-configparser"
   "qmynd" "simple-date" "split-sequence" "sqlite"
   "trivial-backtrace" "uiop" "usocket" "uuid")
  ("ph-maths") ("phonon" "qtools") ("pileup" "alexandria")
  ("pileup-tests" "hu.dwim.stefil" "pileup") ("pipes") ("piping")
  ("pithy-xml")
  ("place-modifiers" "cartesian-product-switch" "map-bind")
  ("place-utils") ("plain-odbc" "cffi")
  ("plain-odbc-with-libs" "plain-odbc")
  ("planks" "babel" "bordeaux-threads" "closer-mop" "ironclad"
   "rucksack" "trivial-garbage")
  ("plokami" "cffi") ("plplot-examples" "cl-plplot" "png")
  ("plump" "plump-dom" "plump-lexer" "plump-parser")
  ("plump-bundle" "babel" "closer-mop" "fast-io" "plump-dom")
  ("plump-dom" "array-utils") ("plump-lexer")
  ("plump-parser" "plump-dom" "plump-lexer" "trivial-indent")
  ("plump-sexp" "plump") ("plump-tex" "cl-ppcre" "plump")
  ("plump-tex-test" "fiveam" "plump-tex")
  ("png" "cffi" "cffi-grovel") ("png-read" "babel" "chipz" "iterate")
  ("png-test" "png") ("pod-utils") ("policy-cond")
  ("polling-functions") ("pooler") ("portable-sockets")
  ("portable-sockets-test") ("portable-threads-test")
  ("positional-lambda" "map-bind")
  ("postmodern" "bordeaux-threads" "cl-postgres" "closer-mop"
   "s-sql")
  ("postmodern-tests" "eos" "postmodern" "simple-date"
   "simple-date-postgres-glue")
  ("postoffice" "acl-compat")
  ("pounds" "babel" "bordeaux-threads" "cffi" "nibbles"
   "trivial-gray-streams")
  ("pp-toml" "alexandria" "cl-ppcre" "esrap" "generic-comparability"
   "local-time" "parse-number" "split-sequence")
  ("pp-toml-tests" "alexandria" "cl-ppcre" "esrap" "fiveam"
   "generic-comparability" "local-time" "parse-number" "pp-toml"
   "split-sequence")
  ("prepl" "bordeaux-threads" "closer-mop" "conium" "iterate"
   "named-readtables")
  ("pretty-function") ("printv") ("priority-queue")
  ("proc-parse" "alexandria" "babel")
  ("proc-parse-test" "proc-parse" "prove" "prove-asdf")
  ("projectured.demo" "hu.dwim.asdf" "hu.dwim.web-server"
   "projectured.sdl.test")
  ("projectured.product" "hu.dwim.asdf" "hu.dwim.web-server"
   "projectured.sdl")
  ("projectured.sdl" "hu.dwim.asdf" "lispbuilder-sdl-gfx"
   "lispbuilder-sdl-image" "lispbuilder-sdl-ttf" "projectured")
  ("projectured.sdl.test" "hu.dwim.asdf" "projectured.sdl"
   "projectured.test")
  ("protobuf" "com.google.base" "varint")
  ("prove" "alexandria" "cl-ansi-text" "cl-ppcre") ("prove-asdf")
  ("psgraph") ("ptester") ("puri") ("puri-tests" "ptester" "puri")
  ("purl" "mpc" "percent-encoding")
  ("py-configparser" "parse-number")
  ("py-configvalidator" "alexandria" "cl-utilities"
   "py-configparser")
  ("pythonic-string-reader" "named-readtables")
  ("pzmq" "cffi" "cffi-grovel") ("pzmq-compat" "pzmq")
  ("pzmq-examples" "bordeaux-threads" "iterate" "local-time" "pzmq"
   "split-sequence")
  ("pzmq-test" "bordeaux-threads" "fiveam" "let-plus" "pzmq")
  ("q+" "qtools") ("qbook" "arnesi" "cl-ppcre" "iterate" "yaclml")
  ("qimageblitz" "qtools") ("qlot")
  ("qlot-install" "alexandria" "archive" "cl-fad" "cl-ppcre" "clack"
   "clack-handler-hunchentoot" "drakma" "function-cache"
   "gzip-stream" "ironclad" "iterate" "salza2" "split-sequence"
   "uiop" "usocket" "yason")
  ("qlot-test" "cl-fad" "prove" "prove-asdf" "qlot" "qlot-install")
  ("qmynd" "babel" "chipz" "cl+ssl" "flexi-streams" "ironclad"
   "list-of" "salza2" "trivial-gray-streams" "usocket")
  ("qmynd-test" "babel" "flexi-streams" "qmynd") ("qsci" "qtools")
  ("qt" "alexandria" "cffi" "cl-ppcre" "closer-mop" "iterate"
   "named-readtables" "trivial-features" "trivial-garbage")
  ("qt-lib-generator" "cl-ppcre" "trivial-features")
  ("qt-libs" "cffi" "cl-ppcre" "qt-lib-generator" "trivial-features")
  ("qt-repl" "bordeaux-threads" "qt")
  ("qt-test" "alexandria" "iterate" "qt" "rt" "trivial-garbage")
  ("qt-tutorial" "qt") ("qt3support" "qtools") ("qtcore" "qtools")
  ("qtdbus" "qtools") ("qtdeclarative" "qtools") ("qtgui" "qtools")
  ("qthelp" "qtools") ("qtnetwork" "qtools")
  ("qtools" "cl-ppcre" "closer-mop" "form-fiddle" "named-readtables"
   "qt-libs" "trivial-garbage" "trivial-indent"
   "trivial-main-thread")
  ("qtools-evaluator" "cl-ppcre" "qtcore" "qtgui" "qtools"
   "trivial-gray-streams")
  ("qtools-game" "closer-mop" "qtcore" "qtgui" "qtools" "qtopengl")
  ("qtools-helloworld" "qtcore" "qtgui" "qtools")
  ("qtools-keychord-editor" "qtools")
  ("qtools-melody" "phonon" "qtcore" "qtgui" "qtools")
  ("qtools-opengl" "cl-opengl" "qtcore" "qtgui" "qtools" "qtopengl")
  ("qtools-titter" "chirp" "qtcore" "qtgui" "qtools")
  ("qtopengl" "qtools") ("qtscript" "qtools") ("qtsql" "qtools")
  ("qtsvg" "qtools") ("qttest" "qtools") ("qtuitools" "qtools")
  ("qtwebkit" "qtools") ("qtxml" "qtools") ("qtxmlpatterns" "qtools")
  ("quadpack" "f2cl" "mach-par") ("quadpack-tests" "quadpack" "rt")
  ("quadtree") ("quadtree-test" "prove" "prove-asdf" "quadtree")
  ("quasiquote-2.0" "iterate")
  ("quasiquote-2.0-tests" "fiveam" "quasiquote-2.0")
  ("query-fs" "bordeaux-threads" "cl-fuse" "cl-fuse-meta-fs"
   "cl-ppcre" "command-line-arguments" "iterate" "trivial-backtrace")
  ("queue") ("queues")
  ("queues.priority-cqueue" "bordeaux-threads" "queues"
   "queues.priority-queue")
  ("queues.priority-queue" "queues")
  ("queues.simple-cqueue" "bordeaux-threads" "queues"
   "queues.simple-queue")
  ("queues.simple-queue" "queues") ("quickapp")
  ("quicklisp-slime-helper" "alexandria" "swank")
  ("quickproject" "cl-fad" "html-template")
  ("quicksearch" "alexandria" "anaphora" "bordeaux-threads"
   "cl-ppcre" "do-urlencode" "drakma" "flexi-streams" "html-entities"
   "iterate" "yason")
  ("quickutil" "quickutil-client")
  ("quickutil-client" "cl-fad" "quickutil-client-management"
   "quickutil-utilities")
  ("quickutil-client-management" "trivial-garbage")
  ("quickutil-server" "cl-fad" "cl-markdown" "cl-ppcre" "cl-syntax"
   "cl-syntax-annot" "clack-middleware-csrf" "closure-template" "dbi"
   "multival-plist" "ningle" "quickutil-utilities" "trivial-shell"
   "yason")
  ("quickutil-utilities" "cl-heredoc")
  ("quickutil-utilities-test" "quickutil-client" "quickutil-server")
  ("quid-pro-quo" "alexandria" "asdf-system-connections" "closer-mop"
   "method-combination-utilities")
  ("quid-pro-quo-tests" "fiveam" "quid-pro-quo") ("quine-mccluskey")
  ("quri" "alexandria" "babel" "cl-utilities" "split-sequence")
  ("quri-test" "prove" "prove-asdf" "quri") ("quux-time")
  ("racer" "aserve" "deflate" "flexi-streams")
  ("random" "com.google.base") ("random-access-lists" "lisp-unit")
  ("random-test" "hu.dwim.stefil" "random")
  ("ratify" "cl-ppcre" "local-time" "parse-float")
  ("rcl" "bordeaux-threads" "cffi" "trivial-garbage")
  ("rcl-test" "fiveam" "rcl") ("read-csv")
  ("read-csv.test" "read-csv") ("readable") ("reader-interception")
  ("reader-interception-test" "fare-utils" "hu.dwim.stefil"
   "reader-interception")
  ("rectangle-packing") ("recur")
  ("recursive-regex" "alexandria" "anaphora" "cl-interpol" "cl-ppcre"
   "iterate" "symbol-munger")
  ("recursive-regex-test" "lisp-unit" "recursive-regex") ("regex")
  ("regression" "lift" "surf" "tasty")
  ("repair" "alexandria" "bordeaux-threads" "cl-store"
   "curry-compose-reader-macros" "metabang-bind" "software-evolution"
   "software-evolution-command-line" "software-evolution-utility"
   "split-sequence")
  ("repl-utilities")
  ("restas" "alexandria" "bordeaux-threads" "cffi" "data-sift"
   "hunchentoot" "routes")
  ("restas-directory-publisher" "closure-template" "local-time"
   "restas")
  ("restas-doc" "restas" "restas-directory-publisher" "sphinx")
  ("restas.file-publisher" "cl-fad" "restas")
  ("restful" "alexandria" "cl-ppcre" "closer-mop" "hunchentoot"
   "jonathan")
  ("restful-test" "drakma" "prove" "prove-asdf" "restful")
  ("retrospectiff" "com.gigamonkeys.binary-data" "flexi-streams"
   "ieee-floats")
  ("reversi") ("rfc2109" "split-sequence") ("rfc2388")
  ("rfc2388-binary") ("rfc3339-timestamp" "yacc")
  ("rfc3339-timestamp-test" "lisp-unit" "rfc3339-timestamp")
  ("rlc" "kmrcl") ("robot" "gwl-graphics")
  ("rock" "anaphora" "asdf" "trivial-download" "trivial-extract"
   "trivial-types")
  ("rock-test" "fiveam" "rock")
  ("rock-web" "3bmd" "3bmd-ext-code-blocks"
   "3bmd-ext-definition-lists" "cl-markup" "lass" "rock")
  ("romreader") ("routes" "iterate" "puri" "split-sequence")
  ("routes-test" "lift" "routes")
  ("rpc4cl" "babel" "cl-ppcre" "cxml" "drakma" "parse-number"
   "rfc3339-timestamp" "trivial-timeout")
  ("rpc4cl-test" "hunchentoot" "lisp-unit" "rpc4cl")
  ("rpm" "cl-ppcre" "fare-utils" "inferior-shell" "lambda-reader")
  ("rss" "aserve" "kmrcl" "xmls") ("rt") ("rucksack")
  ("rucksack-test" "rucksack") ("rutils" "named-readtables")
  ("rutils-test" "rutils" "should-test")
  ("rutilsx" "named-readtables" "rutils")
  ("ryeboy" "alexandria" "protobuf" "prove-asdf" "usocket")
  ("ryeboy-test" "prove" "prove-asdf" "ryeboy") ("s-base64")
  ("s-dot")
  ("s-http-client" "chipz" "puri" "s-base64" "s-sysdeps" "s-utils")
  ("s-http-server" "puri" "s-base64" "s-sysdeps" "s-utils" "salza2")
  ("s-protobuf" "cffi") ("s-sql" "cl-postgres") ("s-sysdeps")
  ("s-utils") ("s-xml") ("s-xml-rpc" "s-xml")
  ("s-xml.examples" "s-xml") ("s-xml.test" "s-xml") ("salza2")
  ("sane" "cffi" "iterate" "trivial-gray-streams")
  ("sanitize" "cl-libxml2") ("sanitize-test" "eos" "sanitize")
  ("sapaclisp") ("sb-cga" "alexandria" "madeira-port") ("sb-fastcgi")
  ("sb-vector-io")
  ("scalpl" "anaphora" "chanl" "cl-base64" "cl-json" "dbi" "drakma"
   "ironclad" "local-time" "method-combination-utilities"
   "parse-float" "string-case")
  ("scalpl.mpex" "rss" "scalpl" "split-sequence") ("sclf")
  ("screamer")
  ("screamer-tests" "hu.dwim.stefil" "iterate" "screamer")
  ("scriba" "common-doc-plump" "esrap" "plump-sexp")
  ("scriba-test" "fiveam" "scriba")
  ("scribble" "fare-memoization" "fare-quasiquote-readtable"
   "fare-utils" "meta")
  ("scribble/test" "babel" "hu.dwim.stefil" "scribble")
  ("scriptl" "alexandria" "bordeaux-threads" "iolib" "osicat"
   "trivial-backtrace" "trivial-gray-streams" "trivial-utf-8")
  ("scriptl-examples" "scriptl")
  ("scriptl-util" "cl-ppcre" "scriptl")
  ("sdl2" "alexandria" "cl-autowrap" "cl-opengl" "cl-plus-c"
   "cl-ppcre" "trivial-channels" "trivial-garbage")
  ("sdl2-examples" "sdl2")
  ("sdl2kit" "alexandria" "defpackage-plus" "sdl2")
  ("sdl2kit-examples" "alexandria" "defpackage-plus" "glkit"
   "mathkit" "sdl2kit")
  ("secret-values") ("secure-random" "cl+ssl")
  ("selenium" "cl-ppcre" "cxml" "drakma" "puri" "split-sequence")
  ("sequence-iterators" "parse-declarations-1.0")
  ("sequence-iterators-test" "sequence-iterators")
  ("serapeum" "alexandria" "bordeaux-threads"
   "fare-quasiquote-extras" "introspect-environment"
   "named-readtables" "optima" "parse-declarations-1.0"
   "parse-number" "split-sequence" "string-case" "trivial-garbage"
   "uiop")
  ("serapeum-tests" "fiveam" "serapeum") ("series")
  ("series-tests" "series") ("session-token" "cl-isaac")
  ("sexml" "alexandria" "cl-ppcre" "contextl" "cxml"
   "macroexpand-dammit")
  ("sexml-objects" "sexml") ("sha3") ("shadchen") ("sheeple")
  ("sheeple-tests" "eos" "sheeple")
  ("shellpool" "bordeaux-threads" "bt-semaphore" "cl-fad"
   "trivial-features")
  ("shelly" "babel" "bordeaux-threads" "cl-fad" "local-time"
   "split-sequence" "trivial-signal" "uiop")
  ("shelly-test" "cl-test-more" "shelly")
  ("should-test" "cl-ppcre" "local-time" "osicat" "rutilsx")
  ("shuffletron" "mixalot" "mixalot-flac" "mixalot-mp3"
   "mixalot-vorbis" "osicat")
  ("simple-blog" "weblocks")
  ("simple-currency" "cl-store" "drakma" "simple-date"
   "split-sequence" "xmls")
  ("simple-date")
  ("simple-date-postgres-glue" "cl-postgres" "simple-date")
  ("simple-date-tests" "eos" "simple-date")
  ("simple-date-time" "cl-ppcre")
  ("simple-finalizer" "cffi" "trivial-garbage") ("simple-rgb")
  ("simple-tasks" "bordeaux-threads" "dissect")
  ("simpsamp" "jpl-util") ("single-threaded-ccl")
  ("singleton-classes" "closer-mop")
  ("sip-hash" "com.google.base" "nibbles")
  ("sip-hash-test" "hu.dwim.stefil" "sip-hash") ("skippy")
  ("smackjack" "alexandria" "cl-containers" "cl-json" "hunchentoot"
   "parenscript")
  ("smackjack-demo" "cl-containers" "cl-who" "local-time"
   "smackjack")
  ("smtp4cl" "mime4cl" "net4cl") ("smug" "asdf-package-system")
  ("snakes" "cl-cont" "cl-utilities" "closer-mop" "fiveam" "iterate")
  ("snappy" "com.google.base" "nibbles" "varint")
  ("snappy-test" "acm-random" "hu.dwim.stefil" "nibbles" "snappy")
  ("snmp" "ironclad" "trivial-gray-streams" "usocket")
  ("snmp-server" "snmp") ("snmp-test" "snmp" "snmp-server")
  ("softdrink" "lass" "lquery")
  ("software-evolution" "alexandria" "cl-ppcre"
   "curry-compose-reader-macros" "elf" "metabang-bind"
   "software-evolution-utility" "split-sequence")
  ("software-evolution-command-line" "metabang-bind")
  ("software-evolution-test" "alexandria"
   "curry-compose-reader-macros" "metabang-bind" "software-evolution"
   "software-evolution-utility" "stefil")
  ("software-evolution-utility" "alexandria" "cl-ppcre" "cl-store"
   "curry-compose-reader-macros" "metabang-bind" "split-sequence"
   "trivial-shell")
  ("soundex") ("south" "cl-ppcre" "drakma" "ironclad" "uuid")
  ("sparseset") ("spartns") ("spartns-test" "spartns")
  ("spatial-trees")
  ("spatial-trees.nns" "alexandria" "iterate" "optima"
   "spatial-trees")
  ("spatial-trees.nns.test" "alexandria" "fiveam" "iterate" "optima"
   "spatial-trees" "spatial-trees.nns")
  ("spatial-trees.test" "fiveam" "spatial-trees")
  ("spellcheck" "alexandria" "cl-ppcre")
  ("sphinx" "cl-fad" "closure-template" "colorize" "docutils")
  ("spinneret" "alexandria" "cl-markdown" "cl-ppcre" "parenscript")
  ("spinneret-tests" "fiveam" "spinneret") ("split-sequence")
  ("split-sequence-tests" "fiveam" "split-sequence")
  ("sqlite" "cffi" "iterate") ("squirl")
  ("squirl.demo" "cl-glu" "cl-glut" "cl-opengl" "squirl") ("st-json")
  ("standard-cl")
  ("staple" "3bmd" "3bmd-ext-code-blocks" "cl-ppcre" "clip"
   "closer-mop" "trivial-arguments")
  ("staple-server" "hunchentoot" "staple")
  ("static-vectors" "alexandria" "cffi" "cffi-grovel")
  ("static-vectors/test" "fiveam" "static-vectors")
  ("stdutils" "cl-fad" "cl-ppcre")
  ("stefil" "alexandria" "iterate" "metabang-bind" "swank")
  ("stefil-test" "stefil") ("stem")
  ("stmx" "alexandria" "bordeaux-threads" "closer-mop" "log4cl"
   "trivial-garbage")
  ("stmx.test" "bordeaux-threads" "fiveam" "log4cl" "stmx")
  ("stp-query" "alexandria" "anaphora" "arnesi" "cl-interpol"
   "cl-ppcre" "cxml-stp" "iterate" "split-sequence" "symbol-munger"
   "yacc")
  ("streaming") ("string-case") ("string-escape")
  ("stringprep" "alexandria" "anaphora" "cl-ppcre")
  ("stringprep-test" "fiveam" "stringprep")
  ("stump-touchy-mode-line" "defmacro-enhance" "iterate" "stumpwm")
  ("stumpwm" "cl-ppcre" "clx")
  ("submarine" "iterate" "mop-utils" "postmodern")
  ("surf" "geom-base") ("swank")
  ("swank-client" "bordeaux-threads" "com.google.base" "swank"
   "usocket")
  ("swank-client-test" "bordeaux-threads" "hu.dwim.stefil" "swank"
   "swank-client")
  ("swank-crew" "bordeaux-threads" "com.google.base"
   "com.google.flag" "osicat" "swank-client")
  ("swank-crew-test" "hu.dwim.stefil" "swank-crew")
  ("swank-protocol" "swank" "usocket")
  ("swap-bytes" "trivial-features")
  ("swap-bytes/test" "fiveam" "swap-bytes")
  ("sxql" "alexandria" "cl-syntax-annot" "iterate" "optima"
   "split-sequence" "trivial-types")
  ("sxql-test" "prove" "prove-asdf" "sxql")
  ("symbol-munger" "alexandria" "iterate")
  ("symbol-munger-test" "lisp-unit2" "symbol-munger")
  ("symbol-namespaces" "map-bind") ("synonyms")
  ("t-clack-handler-fcgi" "clack-test" "prove-asdf")
  ("t-clack-handler-hunchentoot" "clack-handler-hunchentoot"
   "clack-test" "prove-asdf")
  ("t-clack-handler-toot" "clack-handler-toot" "clack-test"
   "prove-asdf")
  ("t-clack-handler-wookie" "clack-test" "prove-asdf")
  ("t-clack-middleware-auth-basic" "clack"
   "clack-middleware-auth-basic" "clack-test" "drakma" "prove"
   "prove-asdf")
  ("t-clack-middleware-csrf" "clack" "clack-middleware-csrf"
   "clack-test" "drakma" "prove" "prove-asdf")
  ("t-clack-v1-compat" "clack-test" "clack-v1-compat" "drakma"
   "prove" "prove-asdf")
  ("t-lack" "clack" "clack-v1-compat" "lack" "prove" "prove-asdf")
  ("t-lack-component" "lack-component" "lack-test" "prove"
   "prove-asdf")
  ("t-lack-middleware-accesslog" "lack" "lack-test" "prove"
   "prove-asdf" "split-sequence")
  ("t-lack-middleware-auth-basic" "alexandria" "cl-base64" "lack"
   "lack-middleware-auth-basic" "lack-test" "prove" "prove-asdf")
  ("t-lack-middleware-backtrace" "alexandria" "lack" "lack-test"
   "prove" "prove-asdf")
  ("t-lack-middleware-csrf" "cl-ppcre" "lack" "lack-middleware-csrf"
   "lack-request" "lack-test" "prove" "prove-asdf")
  ("t-lack-middleware-mount" "lack" "lack-component"
   "lack-middleware-mount" "lack-test" "prove" "prove-asdf")
  ("t-lack-middleware-session" "lack" "lack-test" "prove"
   "prove-asdf")
  ("t-lack-middleware-static" "alexandria" "lack" "lack-test" "prove"
   "prove-asdf")
  ("t-lack-session-store-dbi" "dbi" "lack" "lack-session-store-dbi"
   "lack-test" "prove" "prove-asdf")
  ("t-lack-util" "lack-test" "lack-util" "prove" "prove-asdf")
  ("ta2" "gwl-graphics") ("tagger" "closer-mop" "fiveam")
  ("talcl" "alexandria" "buildnode" "cl-ppcre" "cxml" "iterate"
   "symbol-munger")
  ("talcl-examples" "buildnode-xhtml" "talcl")
  ("talcl-speed-tests" "buildnode-xhtml" "lisp-unit2" "talcl"
   "talcl-examples")
  ("talcl-test" "buildnode-xhtml" "lisp-unit2" "talcl")
  ("tap-unit-test") ("tasty" "gwl-graphics" "tree")
  ("tbnl" "cl-base64" "cl-ppcre" "kmrcl" "md5" "rfc2388"
   "url-rewrite")
  ("tcod" "cffi" "cffi-libffi" "defstar")
  ("teepeedee2" "alexandria" "cffi" "cl-cont" "cl-fad" "cl-irregsexp"
   "iterate" "parenscript" "trivial-backtrace" "trivial-garbage")
  ("teepeedee2-test" "fiveam" "teepeedee2") ("telnetlib" "cl-ppcre")
  ("template" "alexandria" "parameterized-function")
  ("temporal-functions" "macroexpand-dammit")
  ("temporary-file" "alexandria" "bordeaux-threads" "cl-fad"
   "cl-ppcre" "unit-test")
  ("terminfo")
  ("test-gtk" "cells-gtk" "cl-glu" "cl-glut" "cl-opengl")
  ("test-harness") ("test-serial-system")
  ("test.eager-future2" "eager-future2" "eos")
  ("test.vas-string-metrics" "vas-string-metrics")
  ("testbild" "cl-ppcre" "graylex")
  ("testbild-test" "alexandria" "cl-heredoc" "testbild"
   "trivial-gray-streams")
  ("text-query") ("thnappy" "cffi") ("thopter" "blackthorn")
  ("thorn" "common-doc") ("thorn-doc" "thorn")
  ("thorn-test" "fiveam" "thorn" "thorn-doc")
  ("thread-pool" "arnesi" "bordeaux-threads")
  ("thread.comm.rendezvous" "bordeaux-threads" "cl-annot")
  ("thread.comm.rendezvous.test" "cl-test-more"
   "thread.comm.rendezvous")
  ("tiff4cl" "ie3fp") ("time-interval" "cl-ppcre" "local-time")
  ("tinaa" "anaphora" "asdf-system-connections" "cl-containers"
   "cl-graph" "defsystem-compatibility" "dynamic-classes" "lml2"
   "metatilities" "trivial-shell")
  ("tinaa-and-cl-markdown" "cl-markdown" "tinaa")
  ("tinaa-test" "lift" "tinaa") ("toadstool" "closer-mop")
  ("toadstool-tests" "stefil" "toadstool") ("toms419" "f2cl")
  ("toms419-test" "toms419") ("toms717" "f2cl")
  ("toms717-tests" "toms717")
  ("toot" "alexandria" "bordeaux-threads" "chunga" "cl+ssl"
   "cl-base64" "cl-fad" "cl-ppcre" "flexi-streams" "md5" "puri"
   "trivial-backtrace" "usocket")
  ("torta" "gordon")
  ("towers" "alexandria" "cl-glu" "cl-glut" "cl-opengl")
  ("track-best") ("track-best-tests" "nst" "track-best")
  ("translators" "gwl")
  ("transparent-wrap" "fare-quasiquote-extras" "named-readtables"
   "optima" "trivial-arguments")
  ("transparent-wrap-test" "alexandria" "stefil" "transparent-wrap")
  ("tree" "gwl-graphics") ("treedb" "cl-json")
  ("treedb.doc" "cl-gendoc" "treedb" "treedb.tests")
  ("treedb.tests" "fiveam" "treedb") ("trees")
  ("trivia" "trivia.level2")
  ("trivia.balland2006" "alexandria" "iterate" "trivia" "type-i")
  ("trivia.balland2006.enabled" "trivia.balland2006")
  ("trivia.balland2006.enabled.test" "trivia.balland2006.enabled")
  ("trivia.balland2006.test" "fiveam" "trivia.balland2006" "type-r")
  ("trivia.benchmark" "iterate" "optima" "trivia"
   "trivia.balland2006")
  ("trivia.benchmark.run" "trivia.benchmark")
  ("trivia.level0" "alexandria")
  ("trivia.level0.test" "fiveam" "trivia.level0")
  ("trivia.level1" "trivia.level0")
  ("trivia.level1.test" "fiveam" "trivia.level1")
  ("trivia.level2" "closer-mop" "lisp-namespace" "trivia.level1")
  ("trivia.level2.test" "fiveam" "trivia.level2")
  ("trivia.ppcre" "cl-ppcre" "trivia.level2")
  ("trivia.ppcre.test" "fiveam" "trivia.ppcre") ("trivial-arguments")
  ("trivial-backtrace")
  ("trivial-backtrace-test" "lift" "trivial-backtrace")
  ("trivial-benchmark")
  ("trivial-bit-streams" "trivial-gray-streams")
  ("trivial-channels" "bordeaux-threads" "trivial-timeout")
  ("trivial-debug-console" "cffi") ("trivial-download" "drakma")
  ("trivial-download-test" "clack" "clack-v1-compat" "fiveam"
   "trivial-download")
  ("trivial-dump-core")
  ("trivial-extract" "archive" "cl-fad" "deflate" "zip")
  ("trivial-extract-test" "cl-fad" "fiveam" "trivial-download"
   "trivial-extract")
  ("trivial-features")
  ("trivial-features-tests" "alexandria" "cffi" "cffi-grovel" "rt"
   "trivial-features")
  ("trivial-garbage")
  ("trivial-garbage-tests" "rt" "trivial-garbage")
  ("trivial-gray-streams")
  ("trivial-gray-streams-test" "trivial-gray-streams")
  ("trivial-http" "usocket")
  ("trivial-http-test" "lift" "trivial-http") ("trivial-indent")
  ("trivial-irc" "cl-ppcre" "split-sequence" "usocket")
  ("trivial-irc-echobot" "trivial-irc")
  ("trivial-lazy" "bordeaux-threads")
  ("trivial-ldap" "cl+ssl" "usocket" "yacc")
  ("trivial-main-thread" "bordeaux-threads" "simple-tasks"
   "trivial-features")
  ("trivial-mimes") ("trivial-octet-streams")
  ("trivial-raw-io" "alexandria") ("trivial-shell")
  ("trivial-shell-test" "lift" "trivial-shell")
  ("trivial-signal" "bordeaux-threads" "cffi" "cffi-grovel")
  ("trivial-tco") ("trivial-tco-test" "clunit" "trivial-tco")
  ("trivial-thumbnail") ("trivial-timeout") ("trivial-timers")
  ("trivial-types") ("trivial-update") ("trivial-utf-8")
  ("trivial-utf-8-tests" "trivial-utf-8")
  ("trivialib.type-unify" "alexandria" "introspect-environment"
   "trivia" "type-r")
  ("trivialib.type-unify.test" "fiveam" "trivialib.type-unify")
  ("tutorial-example") ("twfy" "cl-json" "drakma")
  ("twitter-elephant-driver" "cl-twitter" "elephant")
  ("twitter-mongodb-driver" "cl-mongo" "cl-twitter")
  ("type-i" "alexandria" "introspect-environment" "lisp-namespace"
   "trivia")
  ("type-i.test" "fiveam" "type-i") ("type-r" "alexandria" "trivia")
  ("type-r.test" "fiveam" "type-r")
  ("ucw" "cl-ppcre" "closer-mop" "ucw-core")
  ("ucw-core" "arnesi" "bordeaux-threads" "cl-fad" "closer-mop"
   "iterate" "local-time" "net-telent-date" "rfc2109" "swank"
   "trivial-garbage" "usocket" "yaclml")
  ("ucw-core.test" "arnesi" "cxml" "drakma" "iterate" "stefil"
   "ucw-core")
  ("ucw.examples" "ucw" "ucw-core")
  ("ucw.httpd" "cl-ppcre" "puri" "rfc2388-binary" "ucw-core")
  ("ucw.iolib" "cl-ppcre" "iolib.sockets" "puri" "rfc2388-binary"
   "ucw-core")
  ("ucw.manual-examples" "ucw" "ucw-core")
  ("ucw.mod-lisp" "iolib.sockets" "ucw-core" "ucw.httpd") ("uffi")
  ("uffi-tests" "uffi") ("uiop")
  ("umlisp" "clsql" "clsql-mysql" "hyperobject" "kmrcl")
  ("umlisp-orf" "clsql" "clsql-postgresql-socket" "hyperobject"
   "kmrcl")
  ("umlisp-tests" "rt" "umlisp")
  ("unicly" "ironclad" "split-sequence") ("unifgram" "paiprolog")
  ("unit-formulas" "alexandria" "iterate") ("unit-test")
  ("universal-config" "cl-ppcre" "parse-float") ("unix-options")
  ("unix-opts") ("unix-opts-tests" "unix-opts")
  ("uri-template" "cl-ppcre" "flexi-streams" "named-readtables")
  ("uri-template.test" "eos" "uri-template") ("url-rewrite")
  ("userial" "contextl" "ieee-floats" "trivial-utf-8")
  ("userial-tests" "nst" "userial") ("usocket")
  ("usocket-test" "rt" "usocket") ("usocket-udp" "usocket")
  ("utilities.binary-dump" "alexandria" "let-plus" "nibbles")
  ("utilities.binary-dump-test" "alexandria" "fiveam" "let-plus"
   "nibbles" "split-sequence" "utilities.binary-dump")
  ("utilities.print-items" "alexandria" "iterate" "let-plus")
  ("utilities.print-items-test" "eos" "utilities.print-items")
  ("utilities.print-tree" "alexandria") ("utils-kt") ("utm")
  ("uuid" "ironclad" "trivial-utf-8")
  ("variates-and-metacopy" "cl-variates" "metacopy")
  ("varint" "com.google.base" "nibbles")
  ("varint-test" "hu.dwim.stefil" "varint")
  ("varjo" "alexandria" "cl-ppcre" "named-readtables"
   "split-sequence" "vas-string-metrics")
  ("vas-string-metrics") ("vecto" "cl-vectors" "zpb-ttf" "zpng")
  ("verbose" "bordeaux-threads" "clon" "dissect" "local-time"
   "piping" "split-sequence")
  ("verrazano" "alexandria" "cffi" "cl-ppcre" "closer-mop" "cxml"
   "iterate" "metabang-bind" "parse-number" "trivial-shell")
  ("verrazano-runtime" "cffi")
  ("vertex" "common-doc" "common-doc-plump" "plump-tex")
  ("vertex-test" "fiveam" "vertex")
  ("vgplot" "cl-fad" "cl-ppcre" "ltk")
  ("vgplot-test" "lisp-unit" "vgplot") ("vom")
  ("vorbisfile-ffi" "cffi" "cffi-grovel")
  ("web-crawler-tests" "cl-interpol" "cl-web-crawler" "fiveam")
  ("webactions" "acl-compat" "aserve" "htmlgen")
  ("weblocks" "anaphora" "babel" "bordeaux-threads" "cl-cont"
   "cl-fad" "cl-json" "cl-ppcre" "cl-who" "closer-mop" "f-underscore"
   "html-template" "hunchentoot" "metatilities" "optima"
   "parenscript" "parse-number" "pretty-function" "puri" "salza2"
   "split-sequence" "trivial-backtrace" "trivial-timeout"
   "weblocks-stores" "weblocks-util")
  ("weblocks-clsql" "closer-mop" "clsql" "clsql-fluid" "metatilities"
   "weblocks-stores" "weblocks-util")
  ("weblocks-clsql-demo" "metatilities" "weblocks" "weblocks-clsql"
   "weblocks-stores")
  ("weblocks-custom" "trivial-garbage" "weblocks" "weblocks-memory"
   "weblocks-stores")
  ("weblocks-demo" "metatilities" "weblocks")
  ("weblocks-demo-popover" "metatilities" "weblocks"
   "weblocks-yarek")
  ("weblocks-elephant" "closer-mop" "elephant" "metatilities"
   "weblocks" "weblocks-memory" "weblocks-stores")
  ("weblocks-elephant-demo" "metatilities" "weblocks-elephant")
  ("weblocks-memory" "cl-ppcre" "metatilities" "weblocks-stores")
  ("weblocks-montezuma" "montezuma" "weblocks-stores")
  ("weblocks-perec" "hu.dwim.perec" "weblocks-stores")
  ("weblocks-postmodern" "postmodern" "weblocks" "weblocks-stores")
  ("weblocks-prevalence" "bordeaux-threads" "cl-ppcre"
   "cl-prevalence" "metatilities" "weblocks-memory"
   "weblocks-stores")
  ("weblocks-s11" "arnesi" "weblocks")
  ("weblocks-scripts" "cl-fad" "cl-ppcre")
  ("weblocks-store-test" "f-underscore" "lift" "weblocks"
   "weblocks-memory" "weblocks-test")
  ("weblocks-stores" "closer-mop" "metatilities" "weblocks-util")
  ("weblocks-test" "anaphora" "closer-mop" "f-underscore" "lift"
   "metatilities" "weblocks" "weblocks-stores")
  ("weblocks-tree-widget" "alexandria" "weblocks" "yaclml")
  ("weblocks-util" "anaphora" "bordeaux-threads" "cl-cont" "cl-fad"
   "cl-json" "cl-ppcre" "cl-who" "closer-mop" "f-underscore"
   "html-template" "hunchentoot" "ironclad" "metatilities" "optima"
   "parenscript" "parse-number" "pretty-function" "puri" "salza2"
   "trivial-backtrace" "trivial-timeout")
  ("weblocks-utils" "alexandria" "arnesi" "cl-fad" "cl-json"
   "cl-tidy" "clache" "drakma" "uiop" "weblocks" "weblocks-custom"
   "weblocks-stores" "weblocks-tree-widget")
  ("weblocks-yarek" "weblocks") ("weblocks-yui" "weblocks")
  ("websocket-driver" "alexandria" "babel" "bordeaux-threads"
   "cl-async" "cl-async-future" "cl-base64" "cl-ppcre" "cl-reexport"
   "cl-syntax-annot" "event-emitter" "fast-io" "iolib" "ironclad"
   "puri")
  ("weft" "bordeaux-threads" "log4cl" "trivial-timeout" "usocket")
  ("wilbur" "usocket") ("wire-world" "gwl-graphics")
  ("with-c-syntax" "alexandria" "yacc")
  ("wkb" "flexi-streams" "ieee-floats" "jpl-util")
  ("woo" "alexandria" "cffi" "cffi-grovel" "fast-http" "fast-io"
   "flexi-streams" "lev" "local-time" "quri" "split-sequence"
   "static-vectors" "swap-bytes" "trivial-utf-8" "vom")
  ("woo-test" "clack-test" "prove-asdf" "woo")
  ("wookie" "alexandria" "babel" "blackbird" "chunga" "cl-async"
   "cl-async-ssl" "cl-fad" "cl-ppcre" "do-urlencode" "fast-http"
   "fast-io" "quri" "vom")
  ("workout-timer" "asdf" "command-line-arguments" "local-time"
   "mixalot" "mixalot-vorbis")
  ("wu-decimal") ("wu-sugar")
  ("wuwei" "aserve" "cl-json" "drakma" "ironclad" "mtlisp")
  ("wuwei-examples" "drakma" "wuwei") ("x.fdatatypes")
  ("x.fdatatypes-iterate" "iterate" "x.fdatatypes" "x.let-star")
  ("x.let-star")
  ("xarray" "anaphora" "cl-utilities" "iterate" "metabang-bind")
  ("xarray-test" "lift" "xarray") ("xecto") ("xembed" "clx")
  ("xfactory" "cl-libxml2") ("xfactory-test" "lift" "xfactory")
  ("xhtmlambda" "cl-unicode") ("xhtmlgen" "cxml")
  ("xhtmlgen-test" "rt" "xhtmlgen") ("xkeyboard" "clx")
  ("xkeyboard-test" "xkeyboard") ("xlunit") ("xlunit-tests" "xlunit")
  ("xml-emitter" "cl-utilities") ("xml-mop" "closer-mop" "s-xml")
  ("xml-render" "cl-typesetting" "xmls")
  ("xml.location" "alexandria" "closer-mop" "cxml-stp" "iterate"
   "let-plus" "more-conditions" "split-sequence" "xpath")
  ("xml.location-and-local-time" "local-time" "xml.location")
  ("xml.location-test" "lift" "xml.location") ("xmls")
  ("xmls-tools" "xmls") ("xmls/test" "xmls")
  ("xoverlay" "cl-libxml2")
  ("xpath" "cl-ppcre" "cxml" "parse-number" "yacc") ("xptest")
  ("xsubseq") ("xsubseq-test" "prove" "prove-asdf" "xsubseq")
  ("xuriella" "closure-html" "cxml" "cxml-stp" "split-sequence"
   "xpath")
  ("yacc") ("yaclanapht" "defmacro-enhance" "iterate")
  ("yaclanapht-test" "rt" "yaclanapht") ("yaclml" "arnesi" "iterate")
  ("yadd" "cl-html-parse" "gwl-graphics")
  ("yason" "alexandria" "trivial-gray-streams")
  ("zaws" "cl-base64" "drakma" "flexi-streams" "ironclad")
  ("zaws-xml" "cxml") ("zcdb")
  ("zeromq" "cffi" "cffi-grovel" "trivial-garbage")
  ("zeromq.tests" "bordeaux-threads" "fiveam" "zeromq")
  ("zip" "babel" "cl-fad" "salza2" "trivial-gray-streams") ("zlib")
  ("zmq" "bordeaux-threads" "cffi" "cffi-grovel" "trivial-features")
  ("zmq-examples" "bordeaux-threads" "zmq")
  ("zmq-test" "bordeaux-threads" "fiveam" "zmq") ("zpb-exif")
  ("zpb-ttf") ("zpng" "salza2")
  ("zs3" "cl-base64" "cxml" "drakma" "ironclad" "puri")
  ("zsort" "alexandria"))
 :SYSTEM->PROJECT
 (("1am" . "1am") ("3b-swf" . "3b-swf") ("3b-swf-swc" . "3b-swf")
  ("3bmd" . "3bmd") ("3bmd-ext-code-blocks" . "3bmd")
  ("3bmd-ext-definition-lists" . "3bmd") ("3bmd-ext-tables" . "3bmd")
  ("3bmd-ext-wiki-links" . "3bmd")
  ("a-cl-cairo2-loader" . "cl-cairo2") ("able" . "able")
  ("abnf" . "cl-abnf") ("abort-ks-execution-example" . "gbbopen")
  ("abstract-classes" . "cl-abstract-classes") ("access" . "access")
  ("access-test" . "access") ("acl-compat" . "portableaserve")
  ("acm-random" . "random") ("acm-random-test" . "random")
  ("advanced-readtable" . "advanced-readtable")
  ("adw-charting" . "adw-charting")
  ("adw-charting-google" . "adw-charting")
  ("adw-charting-vecto" . "adw-charting")
  ("agenda-shell" . "gbbopen") ("agenda-shell-user" . "gbbopen")
  ("alexandria" . "alexandria") ("alexandria-tests" . "alexandria")
  ("algebraic-data-library" . "algebraic-data-library")
  ("amazon-ecs" . "amazon-ecs") ("anaphora" . "anaphora")
  ("anaphora-test" . "anaphora")
  ("anaphoric-variants" . "anaphoric-variants") ("antik" . "antik")
  ("antik-tests" . "antik") ("apply-argv" . "apply-argv")
  ("apply-argv-tests" . "apply-argv") ("arc-compat" . "arc-compat")
  ("architecture.service-provider" . "architecture.service-provider")
  ("architecture.service-provider-and-hooks"
   . "architecture.service-provider")
  ("architecture.service-provider-and-hooks-test"
   . "architecture.service-provider")
  ("architecture.service-provider-test"
   . "architecture.service-provider")
  ("archive" . "archive") ("arnesi" . "arnesi")
  ("arnesi.cl-ppcre-extras" . "arnesi")
  ("arnesi.slime-extras" . "arnesi")
  ("array-operations" . "array-operations")
  ("array-operations-tests" . "array-operations")
  ("array-utils" . "array-utils")
  ("array-utils-test" . "array-utils")
  ("arrow-macros" . "arrow-macros")
  ("arrow-macros-test" . "arrow-macros")
  ("ascii-strings" . "cl-string-match") ("asdf-defdoc" . "nst")
  ("asdf-dependency-grovel" . "asdf-dependency-grovel")
  ("asdf-driver" . "uiop") ("asdf-encodings" . "asdf-encodings")
  ("asdf-encodings/test" . "asdf-encodings")
  ("asdf-finalizers" . "asdf-finalizers")
  ("asdf-finalizers-test" . "asdf-finalizers")
  ("asdf-finalizers-test/1" . "asdf-finalizers")
  ("asdf-linguist" . "asdf-linguist")
  ("asdf-linguist-test" . "asdf-linguist") ("asdf-nst" . "nst")
  ("asdf-package-system" . "asdf-package-system")
  ("asdf-system-connections" . "asdf-system-connections")
  ("aserve" . "portableaserve") ("asn.1" . "asn.1")
  ("asteroids" . "asteroids") ("atdoc" . "atdoc")
  ("automaton" . "mcclim") ("avatar-api" . "avatar-api")
  ("avatar-api-test" . "avatar-api") ("aws-sign4" . "aws-sign4")
  ("aws-sign4-example" . "aws-sign4")
  ("aws-sign4-tests" . "aws-sign4") ("ayah-captcha" . "ayah-captcha")
  ("ayah-captcha-demo" . "ayah-captcha") ("babel" . "babel")
  ("babel-streams" . "babel") ("babel-tests" . "babel")
  ("backports" . "backports") ("backports-test" . "backports")
  ("bacteria" . "cl-bacteria") ("bacteria.js" . "cl-bacteria")
  ("base" . "gendl") ("basic-binary-ipc" . "basic-binary-ipc")
  ("basic-binary-ipc-tests" . "basic-binary-ipc") ("beirc" . "beirc")
  ("bencode" . "cl-bencode") ("bencode-test" . "cl-bencode")
  ("bermuda" . "pal") ("bert" . "cl-bert") ("bibtex" . "cl-bibtex")
  ("big-string" . "big-string") ("binary-lass" . "lass")
  ("binary-types" . "binary-types") ("binascii" . "binascii")
  ("binascii-tests" . "binascii") ("binfix" . "binfix")
  ("binge" . "binge") ("binge-tests" . "binge")
  ("binomial-heap" . "binomial-heap") ("birch" . "birch")
  ("birch.test" . "birch") ("bit-smasher" . "bit-smasher")
  ("bitfield-schema" . "bitfield-schema") ("bk-tree" . "bk-tree")
  ("bknr.data.impex" . "bknr-datastore")
  ("bknr.datastore" . "bknr-datastore")
  ("bknr.impex" . "bknr-datastore")
  ("bknr.indices" . "bknr-datastore") ("bknr.modules" . "bknr-web")
  ("bknr.skip-list" . "bknr-datastore")
  ("bknr.skip-list.test" . "bknr-datastore")
  ("bknr.utils" . "bknr-datastore") ("bknr.web" . "bknr-web")
  ("bknr.xml" . "bknr-datastore") ("black-tie" . "black-tie")
  ("blackbird" . "blackbird") ("blackbird-test" . "blackbird")
  ("blackthorn" . "blackthorn-engine")
  ("blackthorn-collision-test" . "blackthorn-engine")
  ("blackthorn-stress-test" . "blackthorn-engine")
  ("blas-complex" . "f2cl") ("blas-hompack" . "f2cl")
  ("blas-package" . "f2cl") ("blas-real" . "f2cl")
  ("blocks-world" . "atdoc") ("bordeaux-fft" . "bordeaux-fft")
  ("bordeaux-threads" . "bordeaux-threads")
  ("bordeaux-threads-test" . "bordeaux-threads")
  ("bourbaki" . "bourbaki") ("bt-semaphore" . "bt-semaphore")
  ("bt-semaphore-test" . "bt-semaphore") ("btrie" . "btrie")
  ("btrie-tests" . "btrie")
  ("bubble-operator-upwards" . "bubble-operator-upwards")
  ("buffalo" . "buffalo") ("buildapp" . "buildapp")
  ("buildnode" . "buildnode") ("buildnode-excel" . "buildnode")
  ("buildnode-html5" . "buildnode") ("buildnode-kml" . "buildnode")
  ("buildnode-test" . "buildnode") ("buildnode-xhtml" . "buildnode")
  ("buildnode-xul" . "buildnode")
  ("bunnyslayer" . "blackthorn-engine")
  ("burgled-batteries" . "burgled-batteries")
  ("burgled-batteries-tests" . "burgled-batteries")
  ("burgled-batteries.syntax" . "burgled-batteries.syntax")
  ("burgled-batteries.syntax-test" . "burgled-batteries.syntax")
  ("bus" . "gendl") ("bytecurry.asdf-ext" . "bytecurry.asdf-ext")
  ("bytecurry.mocks" . "bytecurry.mocks")
  ("bytecurry.mocks/test" . "bytecurry.mocks")
  ("calispel" . "calispel") ("cambl" . "cambl")
  ("cambl-test" . "cambl") ("caramel" . "caramel")
  ("carrier" . "carrier")
  ("cartesian-product-switch" . "cartesian-product-switch")
  ("caveman" . "caveman")
  ("caveman-middleware-dbimanager" . "caveman")
  ("caveman-test" . "caveman") ("caveman2" . "caveman")
  ("caveman2-db" . "caveman") ("caveman2-test" . "caveman")
  ("cells" . "cells") ("cells-gtk" . "cells-gtk3")
  ("ceramic" . "ceramic") ("ceramic-hello-world" . "ceramic")
  ("ceramic-ipc-example" . "ceramic")
  ("ceramic-test-app" . "ceramic") ("cerberus" . "cerberus")
  ("cerberus-kdc" . "cerberus") ("cffi" . "cffi")
  ("cffi-examples" . "cffi") ("cffi-grovel" . "cffi")
  ("cffi-libffi" . "cffi") ("cffi-objects" . "cffi-objects")
  ("cffi-objects.tests" . "cffi-objects") ("cffi-tests" . "cffi")
  ("cffi-uffi-compat" . "cffi") ("changed-stream" . "changed-stream")
  ("changed-stream.test" . "changed-stream") ("chanl" . "chanl")
  ("chanl.examples" . "chanl") ("chanl.tests" . "chanl")
  ("cheat-js" . "cheat-js") ("check-it" . "check-it")
  ("check-it-test" . "check-it") ("checkl" . "checkl")
  ("checkl-docs" . "checkl") ("checkl-test" . "checkl")
  ("chemical-compounds" . "chemical-compounds")
  ("chillax" . "chillax") ("chillax.core" . "chillax")
  ("chillax.jsown" . "chillax") ("chillax.view-server" . "chillax")
  ("chillax.yason" . "chillax") ("chipz" . "chipz")
  ("chirp" . "chirp")
  ("chrome-native-messaging" . "chrome-native-messaging")
  ("chronicity" . "chronicity") ("chronicity-test" . "chronicity")
  ("chtml-matcher" . "chtml-matcher") ("chunga" . "chunga")
  ("circular-streams" . "circular-streams")
  ("circular-streams-test" . "circular-streams")
  ("city-hash" . "city-hash") ("city-hash-test" . "city-hash")
  ("cl+ssl" . "cl+ssl") ("cl-6502" . "cl-6502")
  ("cl-6502-tests" . "cl-6502") ("cl-aa" . "cl-vectors")
  ("cl-aa-misc" . "cl-vectors") ("cl-acronyms" . "cl-acronyms")
  ("cl-actors" . "common-lisp-actors") ("cl-alc" . "cl-openal")
  ("cl-algebraic-data-type" . "cl-algebraic-data-type")
  ("cl-alut" . "cl-openal") ("cl-ana" . "cl-ana")
  ("cl-ana.binary-tree" . "cl-ana") ("cl-ana.calculus" . "cl-ana")
  ("cl-ana.clos-utils" . "cl-ana") ("cl-ana.csv-table" . "cl-ana")
  ("cl-ana.error-propogation" . "cl-ana")
  ("cl-ana.file-utils" . "cl-ana") ("cl-ana.fitting" . "cl-ana")
  ("cl-ana.functional-utils" . "cl-ana")
  ("cl-ana.generic-math" . "cl-ana")
  ("cl-ana.gnuplot-interface" . "cl-ana")
  ("cl-ana.gsl-cffi" . "cl-ana")
  ("cl-ana.hash-table-utils" . "cl-ana")
  ("cl-ana.hdf-cffi" . "cl-ana") ("cl-ana.hdf-table" . "cl-ana")
  ("cl-ana.hdf-typespec" . "cl-ana") ("cl-ana.hdf-utils" . "cl-ana")
  ("cl-ana.histogram" . "cl-ana") ("cl-ana.int-char" . "cl-ana")
  ("cl-ana.linear-algebra" . "cl-ana")
  ("cl-ana.list-utils" . "cl-ana") ("cl-ana.lorentz" . "cl-ana")
  ("cl-ana.macro-utils" . "cl-ana") ("cl-ana.makeres" . "cl-ana")
  ("cl-ana.makeres-block" . "cl-ana")
  ("cl-ana.makeres-branch" . "cl-ana")
  ("cl-ana.makeres-graphviz" . "cl-ana")
  ("cl-ana.makeres-macro" . "cl-ana")
  ("cl-ana.makeres-progress" . "cl-ana")
  ("cl-ana.makeres-table" . "cl-ana") ("cl-ana.map" . "cl-ana")
  ("cl-ana.math-functions" . "cl-ana")
  ("cl-ana.memoization" . "cl-ana")
  ("cl-ana.ntuple-table" . "cl-ana")
  ("cl-ana.package-utils" . "cl-ana")
  ("cl-ana.pathname-utils" . "cl-ana") ("cl-ana.plotting" . "cl-ana")
  ("cl-ana.quantity" . "cl-ana") ("cl-ana.reusable-table" . "cl-ana")
  ("cl-ana.serialization" . "cl-ana")
  ("cl-ana.statistics" . "cl-ana") ("cl-ana.string-utils" . "cl-ana")
  ("cl-ana.symbol-utils" . "cl-ana") ("cl-ana.table" . "cl-ana")
  ("cl-ana.table-utils" . "cl-ana")
  ("cl-ana.table-viewing" . "cl-ana") ("cl-ana.tensor" . "cl-ana")
  ("cl-ana.typed-table" . "cl-ana") ("cl-ana.typespec" . "cl-ana")
  ("cl-android" . "cl-sl4a") ("cl-annot" . "cl-annot")
  ("cl-anonfun" . "cl-anonfun") ("cl-ansi-term" . "cl-ansi-term")
  ("cl-ansi-text" . "cl-ansi-text")
  ("cl-ansi-text-test" . "cl-ansi-text")
  ("cl-apple-plist" . "cl-apple-plist")
  ("cl-arff-parser" . "cl-arff-parser") ("cl-arrows" . "cl-arrows")
  ("cl-async" . "cl-async") ("cl-async-base" . "cl-async")
  ("cl-async-future" . "cl-async-future")
  ("cl-async-repl" . "cl-async") ("cl-async-ssl" . "cl-async")
  ("cl-async-test" . "cl-async") ("cl-async-util" . "cl-async")
  ("cl-authorize-net" . "cl-creditcard")
  ("cl-authorize-net-tests" . "cl-creditcard")
  ("cl-autorepo" . "cl-autorepo") ("cl-autowrap" . "cl-autowrap")
  ("cl-autowrap-test" . "cl-autowrap") ("cl-azure" . "cl-azure")
  ("cl-base32" . "cl-base32") ("cl-base32-tests" . "cl-base32")
  ("cl-base58" . "cl-base58") ("cl-base58-test" . "cl-base58")
  ("cl-base64" . "cl-base64") ("cl-base64-tests" . "cl-base64")
  ("cl-bayesnet" . "cl-bayesnet") ("cl-beanstalk" . "cl-beanstalk")
  ("cl-binary-file-0.4" . "cl-binary-file") ("cl-bloom" . "cl-bloom")
  ("cl-bplustree" . "cl-bplustree") ("cl-bson" . "cl-bson")
  ("cl-bson-test" . "cl-bson") ("cl-btree-0.5" . "cl-btree")
  ("cl-buchberger" . "cl-buchberger") ("cl-ca" . "cl-ca")
  ("cl-cairo2" . "cl-cairo2") ("cl-cairo2-demos" . "cl-cairo2")
  ("cl-cairo2-gtk2" . "cl-cairo2") ("cl-cairo2-xlib" . "cl-cairo2")
  ("cl-case-control" . "cl-case-control")
  ("cl-cffi-gtk" . "cl-cffi-gtk")
  ("cl-cffi-gtk-cairo" . "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-cairo" . "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-gdk" . "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-glib" . "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-gobject" . "cl-cffi-gtk")
  ("cl-cffi-gtk-demo-gtk" . "cl-cffi-gtk")
  ("cl-cffi-gtk-example-gtk" . "cl-cffi-gtk")
  ("cl-cffi-gtk-gdk" . "cl-cffi-gtk")
  ("cl-cffi-gtk-gdk-pixbuf" . "cl-cffi-gtk")
  ("cl-cffi-gtk-gio" . "cl-cffi-gtk")
  ("cl-cffi-gtk-glib" . "cl-cffi-gtk")
  ("cl-cffi-gtk-gobject" . "cl-cffi-gtk")
  ("cl-cffi-gtk-pango" . "cl-cffi-gtk") ("cl-charms" . "cl-charms")
  ("cl-charms-paint" . "cl-charms") ("cl-charms-timer" . "cl-charms")
  ("cl-cheshire-cat" . "cl-cheshire-cat") ("cl-cli" . "cl-cli")
  ("cl-colors" . "cl-colors") ("cl-colors-tests" . "cl-colors")
  ("cl-conspack" . "cl-conspack")
  ("cl-conspack-test" . "cl-conspack") ("cl-cont" . "cl-cont")
  ("cl-cont-test" . "cl-cont") ("cl-containers" . "cl-containers")
  ("cl-containers-test" . "cl-containers")
  ("cl-cookie" . "cl-cookie") ("cl-cookie-test" . "cl-cookie")
  ("cl-coroutine" . "cl-coroutine")
  ("cl-coroutine-test" . "cl-coroutine")
  ("cl-coveralls" . "cl-coveralls")
  ("cl-coveralls-test" . "cl-coveralls") ("cl-crc64" . "cl-crc64")
  ("cl-creditcard" . "cl-creditcard") ("cl-cron" . "cl-cron")
  ("cl-css" . "cl-css") ("cl-csv" . "cl-csv")
  ("cl-csv-clsql" . "cl-csv") ("cl-csv-data-table" . "cl-csv")
  ("cl-csv-test" . "cl-csv") ("cl-ctrnn" . "cl-ctrnn")
  ("cl-curlex" . "cl-curlex") ("cl-curlex-tests" . "cl-curlex")
  ("cl-custom-hash-table" . "cl-custom-hash-table")
  ("cl-custom-hash-table-test" . "cl-custom-hash-table")
  ("cl-data-frame" . "cl-data-frame")
  ("cl-data-frame-tests" . "cl-data-frame")
  ("cl-date-time-parser" . "cl-date-time-parser")
  ("cl-dbi" . "cl-dbi") ("cl-devil" . "cl-devil")
  ("cl-difflib" . "cl-difflib") ("cl-difflib-tests" . "cl-difflib")
  ("cl-dot" . "cl-dot") ("cl-dropbox" . "cl-dropbox")
  ("cl-dsl" . "cl-dsl") ("cl-dsl-tests" . "cl-dsl")
  ("cl-durian" . "cl-durian") ("cl-emacs-if" . "cl-emacs-if")
  ("cl-emb" . "cl-emb") ("cl-epoch" . "cl-epoch")
  ("cl-ewkb" . "cl-ewkb") ("cl-ewkb-tests" . "cl-ewkb")
  ("cl-factoring" . "cl-factoring")
  ("cl-factoring-test" . "cl-factoring") ("cl-fad" . "cl-fad")
  ("cl-fad-test" . "cl-fad") ("cl-fam" . "cl-fam")
  ("cl-fastcgi" . "cl-fastcgi") ("cl-fbclient" . "cl-fbclient")
  ("cl-flowd" . "cl-flowd") ("cl-fluiddb" . "cl-fluidinfo")
  ("cl-fluiddb-test" . "cl-fluidinfo")
  ("cl-fluidinfo" . "cl-fluidinfo") ("cl-freetype2" . "cl-freetype2")
  ("cl-freetype2-doc" . "cl-freetype2")
  ("cl-freetype2-tests" . "cl-freetype2")
  ("cl-fsnotify" . "cl-fsnotify") ("cl-ftp" . "cl-ftp")
  ("cl-fuse" . "cl-fuse") ("cl-fuse-meta-fs" . "cl-fuse-meta-fs")
  ("cl-gap-buffer" . "cl-gap-buffer") ("cl-gd" . "cl-gd")
  ("cl-gd-test" . "cl-gd") ("cl-gdata" . "cl-gdata")
  ("cl-gearman" . "cl-gearman") ("cl-gearman-test" . "cl-gearman")
  ("cl-gendoc" . "cl-gendoc") ("cl-gendoc-docs" . "cl-gendoc")
  ("cl-gene-searcher" . "cl-gene-searcher")
  ("cl-generic-arithmetic" . "cl-generic-arithmetic")
  ("cl-geocode" . "cl-geocode") ("cl-geoip" . "cl-geoip")
  ("cl-geometry" . "cl-geometry")
  ("cl-geometry-tests" . "cl-geometry") ("cl-gists" . "cl-gists")
  ("cl-gists-test" . "cl-gists") ("cl-git" . "cl-git")
  ("cl-git/tests" . "cl-git") ("cl-github-v3" . "cl-github-v3")
  ("cl-glfw" . "cl-glfw") ("cl-glfw-ftgl" . "cl-glfw")
  ("cl-glfw-glu" . "cl-glfw")
  ("cl-glfw-opengl-3dfx_multisample" . "cl-glfw")
  ("cl-glfw-opengl-3dfx_tbuffer" . "cl-glfw")
  ("cl-glfw-opengl-3dfx_texture_compression_fxt1" . "cl-glfw")
  ("cl-glfw-opengl-amd_blend_minmax_factor" . "cl-glfw")
  ("cl-glfw-opengl-amd_depth_clamp_separate" . "cl-glfw")
  ("cl-glfw-opengl-amd_draw_buffers_blend" . "cl-glfw")
  ("cl-glfw-opengl-amd_multi_draw_indirect" . "cl-glfw")
  ("cl-glfw-opengl-amd_name_gen_delete" . "cl-glfw")
  ("cl-glfw-opengl-amd_performance_monitor" . "cl-glfw")
  ("cl-glfw-opengl-amd_sample_positions" . "cl-glfw")
  ("cl-glfw-opengl-amd_seamless_cubemap_per_texture" . "cl-glfw")
  ("cl-glfw-opengl-amd_vertex_shader_tesselator" . "cl-glfw")
  ("cl-glfw-opengl-apple_aux_depth_stencil" . "cl-glfw")
  ("cl-glfw-opengl-apple_client_storage" . "cl-glfw")
  ("cl-glfw-opengl-apple_element_array" . "cl-glfw")
  ("cl-glfw-opengl-apple_fence" . "cl-glfw")
  ("cl-glfw-opengl-apple_float_pixels" . "cl-glfw")
  ("cl-glfw-opengl-apple_flush_buffer_range" . "cl-glfw")
  ("cl-glfw-opengl-apple_object_purgeable" . "cl-glfw")
  ("cl-glfw-opengl-apple_rgb_422" . "cl-glfw")
  ("cl-glfw-opengl-apple_row_bytes" . "cl-glfw")
  ("cl-glfw-opengl-apple_specular_vector" . "cl-glfw")
  ("cl-glfw-opengl-apple_texture_range" . "cl-glfw")
  ("cl-glfw-opengl-apple_transform_hint" . "cl-glfw")
  ("cl-glfw-opengl-apple_vertex_array_object" . "cl-glfw")
  ("cl-glfw-opengl-apple_vertex_array_range" . "cl-glfw")
  ("cl-glfw-opengl-apple_vertex_program_evaluators" . "cl-glfw")
  ("cl-glfw-opengl-apple_ycbcr_422" . "cl-glfw")
  ("cl-glfw-opengl-arb_blend_func_extended" . "cl-glfw")
  ("cl-glfw-opengl-arb_color_buffer_float" . "cl-glfw")
  ("cl-glfw-opengl-arb_copy_buffer" . "cl-glfw")
  ("cl-glfw-opengl-arb_depth_buffer_float" . "cl-glfw")
  ("cl-glfw-opengl-arb_depth_clamp" . "cl-glfw")
  ("cl-glfw-opengl-arb_depth_texture" . "cl-glfw")
  ("cl-glfw-opengl-arb_draw_buffers" . "cl-glfw")
  ("cl-glfw-opengl-arb_draw_buffers_blend" . "cl-glfw")
  ("cl-glfw-opengl-arb_draw_elements_base_vertex" . "cl-glfw")
  ("cl-glfw-opengl-arb_draw_indirect" . "cl-glfw")
  ("cl-glfw-opengl-arb_draw_instanced" . "cl-glfw")
  ("cl-glfw-opengl-arb_es2_compatibility" . "cl-glfw")
  ("cl-glfw-opengl-arb_fragment_program" . "cl-glfw")
  ("cl-glfw-opengl-arb_fragment_shader" . "cl-glfw")
  ("cl-glfw-opengl-arb_framebuffer_object" . "cl-glfw")
  ("cl-glfw-opengl-arb_framebuffer_object_deprecated" . "cl-glfw")
  ("cl-glfw-opengl-arb_framebuffer_srgb" . "cl-glfw")
  ("cl-glfw-opengl-arb_geometry_shader4" . "cl-glfw")
  ("cl-glfw-opengl-arb_get_program_binary" . "cl-glfw")
  ("cl-glfw-opengl-arb_gpu_shader5" . "cl-glfw")
  ("cl-glfw-opengl-arb_gpu_shader_fp64" . "cl-glfw")
  ("cl-glfw-opengl-arb_half_float_pixel" . "cl-glfw")
  ("cl-glfw-opengl-arb_half_float_vertex" . "cl-glfw")
  ("cl-glfw-opengl-arb_imaging" . "cl-glfw")
  ("cl-glfw-opengl-arb_imaging_deprecated" . "cl-glfw")
  ("cl-glfw-opengl-arb_instanced_arrays" . "cl-glfw")
  ("cl-glfw-opengl-arb_map_buffer_range" . "cl-glfw")
  ("cl-glfw-opengl-arb_matrix_palette" . "cl-glfw")
  ("cl-glfw-opengl-arb_multisample" . "cl-glfw")
  ("cl-glfw-opengl-arb_multitexture" . "cl-glfw")
  ("cl-glfw-opengl-arb_occlusion_query" . "cl-glfw")
  ("cl-glfw-opengl-arb_occlusion_query2" . "cl-glfw")
  ("cl-glfw-opengl-arb_pixel_buffer_object" . "cl-glfw")
  ("cl-glfw-opengl-arb_point_parameters" . "cl-glfw")
  ("cl-glfw-opengl-arb_point_sprite" . "cl-glfw")
  ("cl-glfw-opengl-arb_provoking_vertex" . "cl-glfw")
  ("cl-glfw-opengl-arb_robustness" . "cl-glfw")
  ("cl-glfw-opengl-arb_sample_shading" . "cl-glfw")
  ("cl-glfw-opengl-arb_sampler_objects" . "cl-glfw")
  ("cl-glfw-opengl-arb_seamless_cube_map" . "cl-glfw")
  ("cl-glfw-opengl-arb_separate_shader_objects" . "cl-glfw")
  ("cl-glfw-opengl-arb_shader_objects" . "cl-glfw")
  ("cl-glfw-opengl-arb_shader_subroutine" . "cl-glfw")
  ("cl-glfw-opengl-arb_shading_language_100" . "cl-glfw")
  ("cl-glfw-opengl-arb_shading_language_include" . "cl-glfw")
  ("cl-glfw-opengl-arb_shadow" . "cl-glfw")
  ("cl-glfw-opengl-arb_shadow_ambient" . "cl-glfw")
  ("cl-glfw-opengl-arb_tessellation_shader" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_border_clamp" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_buffer_object" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_buffer_object_rgb32" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_compression" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_compression_bptc" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_compression_rgtc" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_cube_map" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_cube_map_array" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_env_combine" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_env_dot3" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_float" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_gather" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_mirrored_repeat" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_multisample" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_rectangle" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_rg" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_rgb10_a2ui" . "cl-glfw")
  ("cl-glfw-opengl-arb_texture_swizzle" . "cl-glfw")
  ("cl-glfw-opengl-arb_timer_query" . "cl-glfw")
  ("cl-glfw-opengl-arb_transform_feedback2" . "cl-glfw")
  ("cl-glfw-opengl-arb_transpose_matrix" . "cl-glfw")
  ("cl-glfw-opengl-arb_uniform_buffer_object" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_array_bgra" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_array_object" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_attrib_64bit" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_blend" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_buffer_object" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_program" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_shader" . "cl-glfw")
  ("cl-glfw-opengl-arb_vertex_type_2_10_10_10_rev" . "cl-glfw")
  ("cl-glfw-opengl-arb_viewport_array" . "cl-glfw")
  ("cl-glfw-opengl-arb_window_pos" . "cl-glfw")
  ("cl-glfw-opengl-ati_draw_buffers" . "cl-glfw")
  ("cl-glfw-opengl-ati_element_array" . "cl-glfw")
  ("cl-glfw-opengl-ati_envmap_bumpmap" . "cl-glfw")
  ("cl-glfw-opengl-ati_fragment_shader" . "cl-glfw")
  ("cl-glfw-opengl-ati_map_object_buffer" . "cl-glfw")
  ("cl-glfw-opengl-ati_meminfo" . "cl-glfw")
  ("cl-glfw-opengl-ati_pixel_format_float" . "cl-glfw")
  ("cl-glfw-opengl-ati_pn_triangles" . "cl-glfw")
  ("cl-glfw-opengl-ati_separate_stencil" . "cl-glfw")
  ("cl-glfw-opengl-ati_text_fragment_shader" . "cl-glfw")
  ("cl-glfw-opengl-ati_texture_env_combine3" . "cl-glfw")
  ("cl-glfw-opengl-ati_texture_float" . "cl-glfw")
  ("cl-glfw-opengl-ati_texture_mirror_once" . "cl-glfw")
  ("cl-glfw-opengl-ati_vertex_array_object" . "cl-glfw")
  ("cl-glfw-opengl-ati_vertex_attrib_array_object" . "cl-glfw")
  ("cl-glfw-opengl-ati_vertex_streams" . "cl-glfw")
  ("cl-glfw-opengl-core" . "cl-glfw")
  ("cl-glfw-opengl-ext_422_pixels" . "cl-glfw")
  ("cl-glfw-opengl-ext_abgr" . "cl-glfw")
  ("cl-glfw-opengl-ext_bgra" . "cl-glfw")
  ("cl-glfw-opengl-ext_bindable_uniform" . "cl-glfw")
  ("cl-glfw-opengl-ext_blend_color" . "cl-glfw")
  ("cl-glfw-opengl-ext_blend_equation_separate" . "cl-glfw")
  ("cl-glfw-opengl-ext_blend_func_separate" . "cl-glfw")
  ("cl-glfw-opengl-ext_blend_minmax" . "cl-glfw")
  ("cl-glfw-opengl-ext_blend_subtract" . "cl-glfw")
  ("cl-glfw-opengl-ext_clip_volume_hint" . "cl-glfw")
  ("cl-glfw-opengl-ext_cmyka" . "cl-glfw")
  ("cl-glfw-opengl-ext_color_subtable" . "cl-glfw")
  ("cl-glfw-opengl-ext_compiled_vertex_array" . "cl-glfw")
  ("cl-glfw-opengl-ext_convolution" . "cl-glfw")
  ("cl-glfw-opengl-ext_coordinate_frame" . "cl-glfw")
  ("cl-glfw-opengl-ext_copy_texture" . "cl-glfw")
  ("cl-glfw-opengl-ext_cull_vertex" . "cl-glfw")
  ("cl-glfw-opengl-ext_depth_bounds_test" . "cl-glfw")
  ("cl-glfw-opengl-ext_direct_state_access" . "cl-glfw")
  ("cl-glfw-opengl-ext_draw_buffers2" . "cl-glfw")
  ("cl-glfw-opengl-ext_draw_instanced" . "cl-glfw")
  ("cl-glfw-opengl-ext_draw_range_elements" . "cl-glfw")
  ("cl-glfw-opengl-ext_fog_coord" . "cl-glfw")
  ("cl-glfw-opengl-ext_framebuffer_blit" . "cl-glfw")
  ("cl-glfw-opengl-ext_framebuffer_multisample" . "cl-glfw")
  ("cl-glfw-opengl-ext_framebuffer_object" . "cl-glfw")
  ("cl-glfw-opengl-ext_framebuffer_srgb" . "cl-glfw")
  ("cl-glfw-opengl-ext_geometry_shader4" . "cl-glfw")
  ("cl-glfw-opengl-ext_gpu_program_parameters" . "cl-glfw")
  ("cl-glfw-opengl-ext_gpu_shader4" . "cl-glfw")
  ("cl-glfw-opengl-ext_histogram" . "cl-glfw")
  ("cl-glfw-opengl-ext_index_array_formats" . "cl-glfw")
  ("cl-glfw-opengl-ext_index_func" . "cl-glfw")
  ("cl-glfw-opengl-ext_index_material" . "cl-glfw")
  ("cl-glfw-opengl-ext_light_texture" . "cl-glfw")
  ("cl-glfw-opengl-ext_multi_draw_arrays" . "cl-glfw")
  ("cl-glfw-opengl-ext_multisample" . "cl-glfw")
  ("cl-glfw-opengl-ext_packed_depth_stencil" . "cl-glfw")
  ("cl-glfw-opengl-ext_packed_float" . "cl-glfw")
  ("cl-glfw-opengl-ext_packed_pixels" . "cl-glfw")
  ("cl-glfw-opengl-ext_paletted_texture" . "cl-glfw")
  ("cl-glfw-opengl-ext_pixel_buffer_object" . "cl-glfw")
  ("cl-glfw-opengl-ext_pixel_transform" . "cl-glfw")
  ("cl-glfw-opengl-ext_point_parameters" . "cl-glfw")
  ("cl-glfw-opengl-ext_polygon_offset" . "cl-glfw")
  ("cl-glfw-opengl-ext_provoking_vertex" . "cl-glfw")
  ("cl-glfw-opengl-ext_secondary_color" . "cl-glfw")
  ("cl-glfw-opengl-ext_separate_shader_objects" . "cl-glfw")
  ("cl-glfw-opengl-ext_separate_specular_color" . "cl-glfw")
  ("cl-glfw-opengl-ext_shader_image_load_store" . "cl-glfw")
  ("cl-glfw-opengl-ext_stencil_clear_tag" . "cl-glfw")
  ("cl-glfw-opengl-ext_stencil_two_side" . "cl-glfw")
  ("cl-glfw-opengl-ext_stencil_wrap" . "cl-glfw")
  ("cl-glfw-opengl-ext_subtexture" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture3d" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_array" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_buffer_object" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_compression_latc" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_compression_rgtc" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_compression_s3tc" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_cube_map" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_env_combine" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_env_dot3" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_filter_anisotropic" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_integer" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_lod_bias" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_mirror_clamp" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_object" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_perturb_normal" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_shared_exponent" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_snorm" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_srgb" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_srgb_decode" . "cl-glfw")
  ("cl-glfw-opengl-ext_texture_swizzle" . "cl-glfw")
  ("cl-glfw-opengl-ext_timer_query" . "cl-glfw")
  ("cl-glfw-opengl-ext_transform_feedback" . "cl-glfw")
  ("cl-glfw-opengl-ext_vertex_array" . "cl-glfw")
  ("cl-glfw-opengl-ext_vertex_array_bgra" . "cl-glfw")
  ("cl-glfw-opengl-ext_vertex_attrib_64bit" . "cl-glfw")
  ("cl-glfw-opengl-ext_vertex_shader" . "cl-glfw")
  ("cl-glfw-opengl-ext_vertex_weighting" . "cl-glfw")
  ("cl-glfw-opengl-gremedy_frame_terminator" . "cl-glfw")
  ("cl-glfw-opengl-gremedy_string_marker" . "cl-glfw")
  ("cl-glfw-opengl-hp_convolution_border_modes" . "cl-glfw")
  ("cl-glfw-opengl-hp_image_transform" . "cl-glfw")
  ("cl-glfw-opengl-hp_occlusion_test" . "cl-glfw")
  ("cl-glfw-opengl-hp_texture_lighting" . "cl-glfw")
  ("cl-glfw-opengl-ibm_cull_vertex" . "cl-glfw")
  ("cl-glfw-opengl-ibm_multimode_draw_arrays" . "cl-glfw")
  ("cl-glfw-opengl-ibm_rasterpos_clip" . "cl-glfw")
  ("cl-glfw-opengl-ibm_texture_mirrored_repeat" . "cl-glfw")
  ("cl-glfw-opengl-ibm_vertex_array_lists" . "cl-glfw")
  ("cl-glfw-opengl-ingr_blend_func_separate" . "cl-glfw")
  ("cl-glfw-opengl-ingr_color_clamp" . "cl-glfw")
  ("cl-glfw-opengl-ingr_interlace_read" . "cl-glfw")
  ("cl-glfw-opengl-intel_parallel_arrays" . "cl-glfw")
  ("cl-glfw-opengl-mesa_pack_invert" . "cl-glfw")
  ("cl-glfw-opengl-mesa_packed_depth_stencil" . "cl-glfw")
  ("cl-glfw-opengl-mesa_program_debug" . "cl-glfw")
  ("cl-glfw-opengl-mesa_resize_buffers" . "cl-glfw")
  ("cl-glfw-opengl-mesa_shader_debug" . "cl-glfw")
  ("cl-glfw-opengl-mesa_trace" . "cl-glfw")
  ("cl-glfw-opengl-mesa_window_pos" . "cl-glfw")
  ("cl-glfw-opengl-mesa_ycbcr_texture" . "cl-glfw")
  ("cl-glfw-opengl-mesax_texture_stack" . "cl-glfw")
  ("cl-glfw-opengl-nv_conditional_render" . "cl-glfw")
  ("cl-glfw-opengl-nv_copy_depth_to_color" . "cl-glfw")
  ("cl-glfw-opengl-nv_copy_image" . "cl-glfw")
  ("cl-glfw-opengl-nv_depth_buffer_float" . "cl-glfw")
  ("cl-glfw-opengl-nv_depth_clamp" . "cl-glfw")
  ("cl-glfw-opengl-nv_evaluators" . "cl-glfw")
  ("cl-glfw-opengl-nv_explicit_multisample" . "cl-glfw")
  ("cl-glfw-opengl-nv_fence" . "cl-glfw")
  ("cl-glfw-opengl-nv_float_buffer" . "cl-glfw")
  ("cl-glfw-opengl-nv_fog_distance" . "cl-glfw")
  ("cl-glfw-opengl-nv_fragment_program" . "cl-glfw")
  ("cl-glfw-opengl-nv_fragment_program2" . "cl-glfw")
  ("cl-glfw-opengl-nv_framebuffer_multisample_coverage" . "cl-glfw")
  ("cl-glfw-opengl-nv_geometry_program4" . "cl-glfw")
  ("cl-glfw-opengl-nv_gpu_program4" . "cl-glfw")
  ("cl-glfw-opengl-nv_gpu_program5" . "cl-glfw")
  ("cl-glfw-opengl-nv_gpu_shader5" . "cl-glfw")
  ("cl-glfw-opengl-nv_half_float" . "cl-glfw")
  ("cl-glfw-opengl-nv_light_max_exponent" . "cl-glfw")
  ("cl-glfw-opengl-nv_multisample_coverage" . "cl-glfw")
  ("cl-glfw-opengl-nv_multisample_filter_hint" . "cl-glfw")
  ("cl-glfw-opengl-nv_occlusion_query" . "cl-glfw")
  ("cl-glfw-opengl-nv_packed_depth_stencil" . "cl-glfw")
  ("cl-glfw-opengl-nv_parameter_buffer_object" . "cl-glfw")
  ("cl-glfw-opengl-nv_pixel_data_range" . "cl-glfw")
  ("cl-glfw-opengl-nv_point_sprite" . "cl-glfw")
  ("cl-glfw-opengl-nv_present_video" . "cl-glfw")
  ("cl-glfw-opengl-nv_primitive_restart" . "cl-glfw")
  ("cl-glfw-opengl-nv_register_combiners" . "cl-glfw")
  ("cl-glfw-opengl-nv_register_combiners2" . "cl-glfw")
  ("cl-glfw-opengl-nv_shader_buffer_load" . "cl-glfw")
  ("cl-glfw-opengl-nv_shader_buffer_store" . "cl-glfw")
  ("cl-glfw-opengl-nv_tessellation_program5" . "cl-glfw")
  ("cl-glfw-opengl-nv_texgen_emboss" . "cl-glfw")
  ("cl-glfw-opengl-nv_texgen_reflection" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_barrier" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_env_combine4" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_expand_normal" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_multisample" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_rectangle" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_shader" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_shader2" . "cl-glfw")
  ("cl-glfw-opengl-nv_texture_shader3" . "cl-glfw")
  ("cl-glfw-opengl-nv_transform_feedback" . "cl-glfw")
  ("cl-glfw-opengl-nv_transform_feedback2" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_array_range" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_array_range2" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_attrib_integer_64bit" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_buffer_unified_memory" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_program" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_program2_option" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_program3" . "cl-glfw")
  ("cl-glfw-opengl-nv_vertex_program4" . "cl-glfw")
  ("cl-glfw-opengl-oes_read_format" . "cl-glfw")
  ("cl-glfw-opengl-oml_interlace" . "cl-glfw")
  ("cl-glfw-opengl-oml_resample" . "cl-glfw")
  ("cl-glfw-opengl-oml_subsample" . "cl-glfw")
  ("cl-glfw-opengl-pgi_misc_hints" . "cl-glfw")
  ("cl-glfw-opengl-pgi_vertex_hints" . "cl-glfw")
  ("cl-glfw-opengl-rend_screen_coordinates" . "cl-glfw")
  ("cl-glfw-opengl-s3_s3tc" . "cl-glfw")
  ("cl-glfw-opengl-sgi_color_table" . "cl-glfw")
  ("cl-glfw-opengl-sgi_depth_pass_instrument" . "cl-glfw")
  ("cl-glfw-opengl-sgis_detail_texture" . "cl-glfw")
  ("cl-glfw-opengl-sgis_fog_function" . "cl-glfw")
  ("cl-glfw-opengl-sgis_multisample" . "cl-glfw")
  ("cl-glfw-opengl-sgis_pixel_texture" . "cl-glfw")
  ("cl-glfw-opengl-sgis_point_parameters" . "cl-glfw")
  ("cl-glfw-opengl-sgis_sharpen_texture" . "cl-glfw")
  ("cl-glfw-opengl-sgis_texture4d" . "cl-glfw")
  ("cl-glfw-opengl-sgis_texture_color_mask" . "cl-glfw")
  ("cl-glfw-opengl-sgis_texture_filter4" . "cl-glfw")
  ("cl-glfw-opengl-sgis_texture_select" . "cl-glfw")
  ("cl-glfw-opengl-sgix_async" . "cl-glfw")
  ("cl-glfw-opengl-sgix_depth_texture" . "cl-glfw")
  ("cl-glfw-opengl-sgix_flush_raster" . "cl-glfw")
  ("cl-glfw-opengl-sgix_fog_scale" . "cl-glfw")
  ("cl-glfw-opengl-sgix_fragment_lighting" . "cl-glfw")
  ("cl-glfw-opengl-sgix_framezoom" . "cl-glfw")
  ("cl-glfw-opengl-sgix_igloo_interface" . "cl-glfw")
  ("cl-glfw-opengl-sgix_instruments" . "cl-glfw")
  ("cl-glfw-opengl-sgix_line_quality_hint" . "cl-glfw")
  ("cl-glfw-opengl-sgix_list_priority" . "cl-glfw")
  ("cl-glfw-opengl-sgix_pixel_texture" . "cl-glfw")
  ("cl-glfw-opengl-sgix_polynomial_ffd" . "cl-glfw")
  ("cl-glfw-opengl-sgix_reference_plane" . "cl-glfw")
  ("cl-glfw-opengl-sgix_resample" . "cl-glfw")
  ("cl-glfw-opengl-sgix_scalebias_hint" . "cl-glfw")
  ("cl-glfw-opengl-sgix_shadow" . "cl-glfw")
  ("cl-glfw-opengl-sgix_shadow_ambient" . "cl-glfw")
  ("cl-glfw-opengl-sgix_slim" . "cl-glfw")
  ("cl-glfw-opengl-sgix_sprite" . "cl-glfw")
  ("cl-glfw-opengl-sgix_tag_sample_buffer" . "cl-glfw")
  ("cl-glfw-opengl-sgix_texture_coordinate_clamp" . "cl-glfw")
  ("cl-glfw-opengl-sgix_texture_lod_bias" . "cl-glfw")
  ("cl-glfw-opengl-sgix_texture_multi_buffer" . "cl-glfw")
  ("cl-glfw-opengl-sgix_ycrcba" . "cl-glfw")
  ("cl-glfw-opengl-sun_convolution_border_modes" . "cl-glfw")
  ("cl-glfw-opengl-sun_global_alpha" . "cl-glfw")
  ("cl-glfw-opengl-sun_mesh_array" . "cl-glfw")
  ("cl-glfw-opengl-sun_slice_accum" . "cl-glfw")
  ("cl-glfw-opengl-sun_triangle_list" . "cl-glfw")
  ("cl-glfw-opengl-sun_vertex" . "cl-glfw")
  ("cl-glfw-opengl-sunx_constant_data" . "cl-glfw")
  ("cl-glfw-opengl-version_1_0" . "cl-glfw")
  ("cl-glfw-opengl-version_1_1" . "cl-glfw")
  ("cl-glfw-opengl-version_1_2" . "cl-glfw")
  ("cl-glfw-opengl-version_1_3" . "cl-glfw")
  ("cl-glfw-opengl-version_1_4" . "cl-glfw")
  ("cl-glfw-opengl-version_1_5" . "cl-glfw")
  ("cl-glfw-opengl-version_2_0" . "cl-glfw")
  ("cl-glfw-opengl-version_2_1" . "cl-glfw")
  ("cl-glfw-opengl-win_phong_shading" . "cl-glfw")
  ("cl-glfw-opengl-win_specular_fog" . "cl-glfw")
  ("cl-glfw-types" . "cl-glfw") ("cl-glfw3" . "cl-glfw3")
  ("cl-glfw3-examples" . "cl-glfw3") ("cl-glu" . "cl-opengl")
  ("cl-glut" . "cl-opengl") ("cl-glut-examples" . "cl-opengl")
  ("cl-gobject-introspection" . "cl-gobject-introspection")
  ("cl-gobject-introspection-test" . "cl-gobject-introspection")
  ("cl-gpu" . "cl-gpu") ("cl-gpu.buffers" . "cl-gpu")
  ("cl-gpu.core" . "cl-gpu") ("cl-gpu.test" . "cl-gpu")
  ("cl-grace" . "cl-grace") ("cl-graph" . "cl-graph")
  ("cl-graph+hu.dwim.graphviz" . "cl-graph")
  ("cl-graph-and-cl-mathstats" . "cl-graph")
  ("cl-graph-and-dynamic-classes" . "cl-graph")
  ("cl-graph-and-metacopy" . "cl-graph")
  ("cl-graph-and-moptilities" . "cl-graph")
  ("cl-graph-test" . "cl-graph") ("cl-growl" . "cl-growl")
  ("cl-gss" . "cl-gss") ("cl-gtk2-cairo" . "cl-gtk2")
  ("cl-gtk2-gdk" . "cl-gtk2") ("cl-gtk2-glib" . "cl-gtk2")
  ("cl-gtk2-gtk" . "cl-gtk2") ("cl-gtk2-pango" . "cl-gtk2")
  ("cl-haml" . "cl-haml") ("cl-haml-test" . "cl-haml")
  ("cl-hash-util" . "cl-hash-util") ("cl-heap" . "cl-heap")
  ("cl-heap-tests" . "cl-heap") ("cl-heredoc" . "cl-heredoc")
  ("cl-heredoc-test" . "cl-heredoc")
  ("cl-hooks" . "architecture.hooks")
  ("cl-hooks-test" . "architecture.hooks")
  ("cl-html-diff" . "cl-html-diff")
  ("cl-html-parse" . "cl-html-parse")
  ("cl-html5-parser" . "cl-html5-parser")
  ("cl-html5-parser-cxml" . "cl-html5-parser")
  ("cl-html5-parser-tests" . "cl-html5-parser") ("cl-hue" . "cl-hue")
  ("cl-i18n" . "cl-i18n") ("cl-ilu" . "cl-devil")
  ("cl-ilut" . "cl-devil") ("cl-indeterminism" . "cl-indeterminism")
  ("cl-indeterminism-tests" . "cl-indeterminism")
  ("cl-inflector" . "cl-inflector")
  ("cl-inflector-test" . "cl-inflector")
  ("cl-influxdb" . "cl-influxdb") ("cl-influxdb.doc" . "cl-influxdb")
  ("cl-influxdb.examples" . "cl-influxdb")
  ("cl-influxdb.examples-async" . "cl-influxdb")
  ("cl-inotify" . "cl-inotify") ("cl-intbytes" . "cl-intbytes")
  ("cl-intbytes-test" . "cl-intbytes")
  ("cl-interpol" . "cl-interpol")
  ("cl-interpol-test" . "cl-interpol") ("cl-irc" . "cl-irc")
  ("cl-irc-test" . "cl-irc") ("cl-irregsexp" . "cl-irregsexp")
  ("cl-irregsexp-test" . "cl-irregsexp") ("cl-isaac" . "cl-isaac")
  ("cl-jpeg" . "cl-jpeg") ("cl-js" . "js") ("cl-json" . "cl-json")
  ("cl-json.test" . "cl-json") ("cl-junit-xml" . "cl-junit-xml")
  ("cl-junit-xml.lisp-unit" . "cl-junit-xml")
  ("cl-junit-xml.lisp-unit2" . "cl-junit-xml")
  ("cl-junit-xml.test" . "cl-junit-xml")
  ("cl-kyoto-cabinet" . "cl-kyoto-cabinet") ("cl-l10n" . "cl-l10n")
  ("cl-l10n-cldr" . "cl-l10n-cldr") ("cl-l10n.test" . "cl-l10n")
  ("cl-larval" . "cl-larval") ("cl-lastfm" . "cl-lastfm")
  ("cl-lastfm-test" . "cl-lastfm") ("cl-launch" . "cl-launch")
  ("cl-ledger" . "cl-ledger") ("cl-lex" . "cl-lex")
  ("cl-liballegro" . "cl-liballegro")
  ("cl-libevent2" . "cl-libevent2")
  ("cl-libevent2-ssl" . "cl-libevent2")
  ("cl-liblinear" . "cl-libsvm") ("cl-libpuzzle" . "cl-libpuzzle")
  ("cl-libpuzzle-test" . "cl-libpuzzle") ("cl-libsvm" . "cl-libsvm")
  ("cl-libusb" . "cl-libusb") ("cl-libuv" . "cl-libuv")
  ("cl-libxml2" . "cl-libxml2") ("cl-libxml2-test" . "cl-libxml2")
  ("cl-libyaml" . "cl-libyaml") ("cl-libyaml-test" . "cl-libyaml")
  ("cl-lite" . "gendl") ("cl-locale" . "cl-locale")
  ("cl-locale-syntax" . "cl-locale")
  ("cl-locatives" . "cl-locatives") ("cl-log" . "cl-log")
  ("cl-log-test" . "cl-log") ("cl-logic" . "cl-logic")
  ("cl-ltsv" . "cl-ltsv") ("cl-ltsv-test" . "cl-ltsv")
  ("cl-m4" . "cl-m4") ("cl-m4-test" . "cl-m4")
  ("cl-markdown" . "cl-markdown")
  ("cl-markdown-comparisons" . "cl-markdown")
  ("cl-markdown-test" . "cl-markdown")
  ("cl-marklogic" . "cl-marklogic") ("cl-markup" . "cl-markup")
  ("cl-markup-test" . "cl-markup") ("cl-match" . "cl-match")
  ("cl-match-test" . "cl-match") ("cl-mathstats" . "cl-mathstats")
  ("cl-mathstats-test" . "cl-mathstats")
  ("cl-mechanize" . "cl-mechanize") ("cl-mediawiki" . "cl-mediawiki")
  ("cl-memcached" . "cl-memcached")
  ("cl-messagepack" . "cl-messagepack")
  ("cl-migrations" . "cl-migrations") ("cl-mime" . "cl-mime")
  ("cl-mock" . "cl-mock") ("cl-mock-basic" . "cl-mock")
  ("cl-mock-tests" . "cl-mock") ("cl-mock-tests-basic" . "cl-mock")
  ("cl-monad-macros" . "cl-monad-macros")
  ("cl-moneris" . "cl-moneris") ("cl-moneris-test" . "cl-moneris")
  ("cl-mongo" . "cl-mongo") ("cl-mongo-id" . "cl-mongo-id")
  ("cl-mop" . "cl-mop") ("cl-mpi" . "cl-mpi")
  ("cl-mpi-test" . "cl-mpi") ("cl-mtgnet" . "cl-mtgnet")
  ("cl-murmurhash" . "cl-murmurhash") ("cl-mustache" . "cl-mustache")
  ("cl-mustache-test" . "cl-mustache") ("cl-mw" . "cl-mw")
  ("cl-mw.examples.argument-processing" . "cl-mw")
  ("cl-mw.examples.hello-world" . "cl-mw")
  ("cl-mw.examples.higher-order" . "cl-mw")
  ("cl-mw.examples.monte-carlo-pi" . "cl-mw")
  ("cl-mw.examples.ping" . "cl-mw")
  ("cl-mw.examples.with-task-policy" . "cl-mw")
  ("cl-mysql" . "cl-mysql") ("cl-mysql-test" . "cl-mysql")
  ("cl-ncurses" . "cl-ncurses") ("cl-neo4j" . "cl-neo4j")
  ("cl-neo4j.tests" . "cl-neo4j")
  ("cl-netstring+" . "cl-netstring-plus")
  ("cl-netstrings" . "cl-netstrings") ("cl-ntriples" . "cl-ntriples")
  ("cl-num-utils" . "cl-num-utils")
  ("cl-num-utils-tests" . "cl-num-utils") ("cl-oauth" . "cl-oauth")
  ("cl-oauth.tests" . "cl-oauth") ("cl-olefs" . "cl-olefs")
  ("cl-one-time-passwords" . "cl-one-time-passwords")
  ("cl-one-time-passwords-test" . "cl-one-time-passwords")
  ("cl-oneliner" . "oneliner") ("cl-op" . "cl-op")
  ("cl-openal" . "cl-openal") ("cl-openal-examples" . "cl-openal")
  ("cl-opengl" . "cl-opengl") ("cl-openid" . "cl-openid")
  ("cl-openid.test" . "cl-openid")
  ("cl-openstack-client" . "cl-openstack-client")
  ("cl-openstack-client-test" . "cl-openstack-client")
  ("cl-opsresearch" . "cl-opsresearch")
  ("cl-org-mode" . "cl-org-mode")
  ("cl-package-locks" . "cl-package-locks")
  ("cl-parallel" . "cl-parallel") ("cl-pass" . "cl-pass")
  ("cl-pass-test" . "cl-pass") ("cl-paths" . "cl-vectors")
  ("cl-paths-ttf" . "cl-vectors") ("cl-pattern" . "cl-pattern")
  ("cl-pattern-benchmark" . "cl-pattern")
  ("cl-paymill" . "cl-paymill") ("cl-paypal" . "cl-paypal")
  ("cl-pdf" . "cl-pdf") ("cl-pdf-doc" . "cl-typesetting")
  ("cl-pdf-parser" . "cl-pdf")
  ("cl-performance-tuning-helper" . "cl-performance-tuning-helper")
  ("cl-performance-tuning-helper-test"
   . "cl-performance-tuning-helper")
  ("cl-permutation" . "cl-permutation")
  ("cl-permutation-examples" . "cl-permutation")
  ("cl-permutation-tests" . "cl-permutation")
  ("cl-photo" . "cl-photo") ("cl-photo-tests" . "cl-photo")
  ("cl-plplot" . "cl-plplot") ("cl-plumbing" . "cl-plumbing")
  ("cl-plumbing-test" . "cl-plumbing") ("cl-plus-c" . "cl-autowrap")
  ("cl-ply" . "cl-ply") ("cl-ply-test" . "cl-ply")
  ("cl-poker-eval" . "cl-poker-eval") ("cl-pop" . "cl-pop")
  ("cl-popen" . "cl-popen") ("cl-popen-test" . "cl-popen")
  ("cl-portaudio" . "cl-portaudio")
  ("cl-portaudio-doc" . "cl-portaudio")
  ("cl-portaudio-tests" . "cl-portaudio")
  ("cl-postgres" . "postmodern")
  ("cl-postgres+local-time" . "local-time")
  ("cl-postgres+local-time-duration" . "local-time-duration")
  ("cl-postgres-tests" . "postmodern") ("cl-ppcre" . "cl-ppcre")
  ("cl-ppcre-template" . "cl-unification")
  ("cl-ppcre-test" . "cl-ppcre") ("cl-ppcre-unicode" . "cl-ppcre")
  ("cl-ppcre-unicode-test" . "cl-ppcre")
  ("cl-prevalence" . "cl-prevalence")
  ("cl-prevalence-test" . "cl-prevalence")
  ("cl-primality" . "cl-primality")
  ("cl-primality-test" . "cl-primality")
  ("cl-prime-maker" . "cl-prime-maker") ("cl-proj" . "cl-proj")
  ("cl-project" . "cl-project") ("cl-project-test" . "cl-project")
  ("cl-protobufs" . "cl-protobufs")
  ("cl-protobufs-tests" . "cl-protobufs") ("cl-pslib" . "cl-pslib")
  ("cl-pslib-barcode" . "cl-pslib-barcode")
  ("cl-qprint" . "cl-qprint") ("cl-qrencode" . "cl-qrencode")
  ("cl-qrencode-test" . "cl-qrencode")
  ("cl-quakeinfo" . "cl-quakeinfo")
  ("cl-quickcheck" . "cl-quickcheck") ("cl-rabbit" . "cl-rabbit")
  ("cl-rabbit-tests" . "cl-rabbit") ("cl-randist" . "cl-randist")
  ("cl-random" . "cl-random") ("cl-random-tests" . "cl-random")
  ("cl-rdfxml" . "cl-rdfxml")
  ("cl-read-macro-tokens" . "cl-read-macro-tokens")
  ("cl-read-macro-tokens-tests" . "cl-read-macro-tokens")
  ("cl-readline" . "cl-readline") ("cl-recaptcha" . "cl-recaptcha")
  ("cl-reddit" . "cl-reddit") ("cl-redis" . "cl-redis")
  ("cl-redis-test" . "cl-redis") ("cl-reexport" . "cl-reexport")
  ("cl-reexport-test" . "cl-reexport")
  ("cl-rethinkdb" . "cl-rethinkdb")
  ("cl-rethinkdb-test" . "cl-rethinkdb")
  ("cl-rfc2047" . "cl-rfc2047") ("cl-rfc2047-test" . "cl-rfc2047")
  ("cl-riff" . "cl-riff") ("cl-rlimit" . "cl-rlimit")
  ("cl-rmath" . "cl-rmath") ("cl-rrd" . "cl-rrd")
  ("cl-rrt" . "cl-rrt") ("cl-rrt.benchmark" . "cl-rrt")
  ("cl-rrt.rtree" . "cl-rrt") ("cl-rrt.test" . "cl-rrt")
  ("cl-rsvg2" . "cl-rsvg2") ("cl-rsvg2-pixbuf" . "cl-rsvg2")
  ("cl-rsvg2-test" . "cl-rsvg2") ("cl-s3" . "cl-s3")
  ("cl-sam" . "cl-sam") ("cl-sam-test" . "cl-sam")
  ("cl-sasl" . "cl-sasl") ("cl-scribd" . "cl-scribd")
  ("cl-scripting" . "cl-scripting") ("cl-scrobbler" . "cl-scrobbler")
  ("cl-scrobbler-tests" . "cl-scrobbler")
  ("cl-secure-read" . "cl-secure-read")
  ("cl-secure-read-tests" . "cl-secure-read")
  ("cl-sendmail" . "cl-sendmail") ("cl-sentiment" . "cl-sentiment")
  ("cl-server-manager" . "cl-server-manager")
  ("cl-shellwords" . "cl-shellwords")
  ("cl-shellwords-test" . "cl-shellwords")
  ("cl-simple-concurrent-jobs" . "cl-simple-concurrent-jobs")
  ("cl-simple-table" . "cl-simple-table")
  ("cl-singleton-mixin" . "cl-singleton-mixin")
  ("cl-singleton-mixin-test" . "cl-singleton-mixin")
  ("cl-skip-list" . "cl-skip-list") ("cl-slice" . "cl-slice")
  ("cl-slice-tests" . "cl-slice") ("cl-slp" . "cl-slp")
  ("cl-slug" . "cl-slug") ("cl-slug-test" . "cl-slug")
  ("cl-smtp" . "cl-smtp") ("cl-sophia" . "cl-sophia")
  ("cl-sophia-test" . "cl-sophia") ("cl-soup" . "cl-webkit")
  ("cl-spark" . "cl-spark") ("cl-spark-test" . "cl-spark")
  ("cl-speedy-queue" . "cl-speedy-queue")
  ("cl-splicing-macro" . "cl-splicing-macro") ("cl-stm" . "cl-stm")
  ("cl-stm.test" . "cl-stm") ("cl-stomp" . "cl-stomp")
  ("cl-stopwatch" . "cl-stopwatch") ("cl-store" . "cl-store")
  ("cl-store-tests" . "cl-store") ("cl-strftime" . "cl-strftime")
  ("cl-strftime/tests" . "cl-strftime")
  ("cl-string-complete" . "cl-string-complete")
  ("cl-string-match" . "cl-string-match")
  ("cl-string-match-test" . "cl-string-match") ("cl-svg" . "cl-svg")
  ("cl-svm" . "cl-svm") ("cl-swap-file-0.5" . "cl-swap-file")
  ("cl-syntax" . "cl-syntax") ("cl-syntax-annot" . "cl-syntax")
  ("cl-syntax-anonfun" . "cl-syntax")
  ("cl-syntax-clsql" . "cl-syntax")
  ("cl-syntax-fare-quasiquote" . "cl-syntax")
  ("cl-syntax-interpol" . "cl-syntax")
  ("cl-syntax-markup" . "cl-syntax") ("cl-syslog" . "cl-syslog")
  ("cl-syslog-tests" . "cl-syslog") ("cl-table" . "cl-table")
  ("cl-template" . "cl-template")
  ("cl-template-tests" . "cl-template") ("cl-test-more" . "prove")
  ("cl-tga" . "cl-tga") ("cl-tidy" . "cl-tidy")
  ("cl-timing" . "gbbopen") ("cl-tk" . "cl-tk") ("cl-tld" . "cl-tld")
  ("cl-tokyo-cabinet" . "cl-tokyo-cabinet")
  ("cl-tokyo-cabinet-test" . "cl-tokyo-cabinet")
  ("cl-tulip-graph" . "cl-tulip-graph") ("cl-tuples" . "cl-tuples")
  ("cl-twit-repl" . "cl-twitter") ("cl-twitter" . "cl-twitter")
  ("cl-typesetting" . "cl-typesetting")
  ("cl-uglify-js" . "cl-uglify-js") ("cl-unicode" . "cl-unicode")
  ("cl-unicode/base" . "cl-unicode")
  ("cl-unicode/build" . "cl-unicode")
  ("cl-unicode/test" . "cl-unicode")
  ("cl-unification" . "cl-unification")
  ("cl-unification-lib" . "cl-unification")
  ("cl-unification-test" . "cl-unification")
  ("cl-utilities" . "cl-utilities") ("cl-v4l2" . "cl-v4l2")
  ("cl-variates" . "cl-variates") ("cl-vectors" . "cl-vectors")
  ("cl-virtualbox" . "cl-virtualbox") ("cl-voxelize" . "cl-voxelize")
  ("cl-voxelize-examples" . "cl-voxelize")
  ("cl-voxelize-test" . "cl-voxelize") ("cl-wal-0.4" . "cl-wal")
  ("cl-wav" . "cl-wav") ("cl-web-crawler" . "cl-web-crawler")
  ("cl-webdav" . "cl-webdav") ("cl-who" . "cl-who")
  ("cl-who-test" . "cl-who") ("cl-xkeysym" . "cl-xkeysym")
  ("cl-xmlspam" . "cl-xmlspam") ("cl-xmpp" . "cl-xmpp")
  ("cl-xmpp-sasl" . "cl-xmpp") ("cl-xmpp-tls" . "cl-xmpp")
  ("cl-xspf" . "cl-xspf") ("cl-xul" . "cl-xul")
  ("cl-xul-test" . "cl-xul") ("cl-yaclyaml" . "cl-yaclyaml")
  ("cl-yaclyaml-tests" . "cl-yaclyaml")
  ("cl-yahoo-finance" . "cl-yahoo-finance") ("cl-yaml" . "cl-yaml")
  ("cl-yaml-test" . "cl-yaml") ("cl4store" . "cl4store")
  ("cl4store-tests" . "cl4store") ("clache" . "clache")
  ("clache-test" . "clache") ("clack" . "clack")
  ("clack-errors" . "clack-errors")
  ("clack-errors-demo" . "clack-errors")
  ("clack-errors-test" . "clack-errors")
  ("clack-handler-fcgi" . "clack")
  ("clack-handler-hunchentoot" . "clack")
  ("clack-handler-toot" . "clack") ("clack-handler-woo" . "woo")
  ("clack-handler-wookie" . "clack")
  ("clack-middleware-auth-basic" . "clack")
  ("clack-middleware-clsql" . "clack")
  ("clack-middleware-csrf" . "clack")
  ("clack-middleware-dbi" . "clack")
  ("clack-middleware-oauth" . "clack")
  ("clack-middleware-postmodern" . "clack")
  ("clack-middleware-rucksack" . "clack")
  ("clack-session-store-dbi" . "clack") ("clack-test" . "clack")
  ("clack-v1-compat" . "clack") ("classimp" . "classimp")
  ("classimp-samples" . "classimp") ("clavatar" . "clavatar")
  ("clavier" . "clavier") ("clavier.test" . "clavier")
  ("clawk" . "clawk") ("clazy" . "clazy") ("clem" . "clem")
  ("clem-benchmark" . "clem") ("clem-test" . "clem")
  ("cleric" . "cleric") ("cleric-test" . "cleric")
  ("clesh" . "clesh") ("clesh-tests" . "clesh")
  ("cletris" . "cletris") ("cletris-network" . "cletris")
  ("clfswm" . "clfswm") ("clhs" . "clhs")
  ("cli-parser" . "cl-cli-parser") ("clickr" . "clickr")
  ("clim" . "mcclim") ("clim-basic" . "mcclim")
  ("clim-clx" . "mcclim") ("clim-core" . "mcclim")
  ("clim-examples" . "mcclim") ("clim-gtkairo" . "mcclim")
  ("clim-lisp" . "mcclim") ("clim-listener" . "mcclim")
  ("clim-looks" . "mcclim") ("clim-null" . "mcclim")
  ("clim-postscript" . "mcclim") ("clim-widgets" . "clim-widgets")
  ("climacs" . "climacs") ("climc" . "climc")
  ("climc-test" . "climc") ("climon" . "climon")
  ("climon-test" . "climon") ("clinch" . "clinch")
  ("clinch-cairo" . "clinch") ("clinch-freeimage" . "clinch")
  ("clinch-glfw" . "clinch") ("clinch-pango" . "clinch")
  ("clinch-sdl" . "clinch") ("clip" . "clip") ("clipper" . "clipper")
  ("clipper-test" . "clipper") ("clite" . "clite")
  ("clml.blas" . "clml") ("clml.blas.complex" . "clml")
  ("clml.blas.hompack" . "clml") ("clml.blas.real" . "clml")
  ("clml.lapack" . "clml") ("clml.lapack-real" . "clml")
  ("clml.statistics" . "clml") ("clml.statistics.rand" . "clml")
  ("clml.utility" . "clml") ("clnuplot" . "clnuplot")
  ("clobber" . "clobber") ("clod" . "clod")
  ("clods-export" . "clods-export") ("clon" . "clon")
  ("clon-test" . "clon") ("clonsigna" . "clonsigna")
  ("clos-diff" . "clos-diff") ("clos-fixtures" . "clos-fixtures")
  ("clos-fixtures-test" . "clos-fixtures")
  ("closer-mop" . "closer-mop") ("closure-common" . "closure-common")
  ("closure-html" . "closure-html")
  ("closure-template" . "cl-closure-template")
  ("closure-template-test" . "cl-closure-template") ("clot" . "clot")
  ("clouchdb" . "clouchdb") ("clouchdb-examples" . "clouchdb")
  ("clouseau" . "mcclim") ("clpmr" . "clpmr")
  ("clpython" . "cl-python") ("clpython.basic" . "cl-python")
  ("clpython.compiler" . "cl-python")
  ("clpython.contrib" . "cl-python") ("clpython.lib" . "cl-python")
  ("clpython.parser" . "cl-python")
  ("clpython.runtime" . "cl-python") ("clpython.test" . "cl-python")
  ("cls" . "common-lisp-stat") ("clsql" . "clsql")
  ("clsql-aodbc" . "clsql") ("clsql-cffi" . "clsql")
  ("clsql-fluid" . "clsql-fluid") ("clsql-helper" . "clsql-helper")
  ("clsql-helper-local-time" . "clsql-helper")
  ("clsql-helper-slot-coercer" . "clsql-helper")
  ("clsql-helper-slot-coercer-test" . "clsql-helper")
  ("clsql-helper-test" . "clsql-helper") ("clsql-mysql" . "clsql")
  ("clsql-odbc" . "clsql") ("clsql-orm" . "clsql-orm")
  ("clsql-postgresql" . "clsql")
  ("clsql-postgresql-socket" . "clsql")
  ("clsql-postgresql-socket3" . "clsql") ("clsql-sqlite" . "clsql")
  ("clsql-sqlite3" . "clsql") ("clsql-tests" . "clsql")
  ("clsql-uffi" . "clsql") ("clss" . "clss") ("clunit" . "clunit")
  ("clws" . "clws") ("clx" . "clx") ("clx-cursor" . "clx-cursor")
  ("clx-truetype" . "clx-truetype")
  ("clx-truetype-test" . "clx-truetype") ("cobstor" . "cobstor")
  ("cobstor-tests" . "cobstor") ("cocoahelper" . "lispbuilder")
  ("codata-recommended-values" . "codata-recommended-values")
  ("coleslaw" . "coleslaw") ("coleslaw-tests" . "coleslaw")
  ("collectors" . "collectors") ("collectors-test" . "collectors")
  ("colleen" . "colleen") ("colnew" . "f2cl")
  ("colnew-test-1" . "f2cl") ("colnew-test-2" . "f2cl")
  ("colnew-test-3" . "f2cl") ("colorize" . "colorize")
  ("com.clearly-useful.generic-collection-interface"
   . "com.clearly-useful.generic-collection-interface")
  ("com.clearly-useful.generic-collection-interface.test"
   . "com.clearly-useful.generic-collection-interface")
  ("com.clearly-useful.iterate+" . "com.clearly-useful.iterate-plus")
  ("com.clearly-useful.iterator-protocol"
   . "com.clearly-useful.iterator-protocol")
  ("com.clearly-useful.protocols" . "com.clearly-useful.protocols")
  ("com.dvlsoft.asdf-flv" . "asdf-flv")
  ("com.dvlsoft.rcfiles" . "cl-rcfiles")
  ("com.elbeno.curve" . "curve") ("com.elbeno.vector" . "vector")
  ("com.gigamonkeys.binary-data" . "monkeylib-binary-data")
  ("com.gigamonkeys.json" . "monkeylib-json")
  ("com.gigamonkeys.macro-utilities" . "monkeylib-macro-utilities")
  ("com.gigamonkeys.markup" . "monkeylib-markup")
  ("com.gigamonkeys.parser" . "monkeylib-parser")
  ("com.gigamonkeys.pathnames" . "monkeylib-pathnames")
  ("com.gigamonkeys.prose-diff" . "monkeylib-prose-diff")
  ("com.gigamonkeys.test-framework" . "monkeylib-test-framework")
  ("com.gigamonkeys.utilities" . "monkeylib-utilities")
  ("com.google.base" . "com.google.base")
  ("com.google.base-test" . "com.google.base")
  ("com.google.flag" . "lisp-gflags")
  ("com.google.flag-test" . "lisp-gflags")
  ("com.informatimago" . "com.informatimago")
  ("com.informatimago.clext" . "com.informatimago")
  ("com.informatimago.clext.association" . "com.informatimago")
  ("com.informatimago.clext.association.test" . "com.informatimago")
  ("com.informatimago.clext.run-program" . "com.informatimago")
  ("com.informatimago.clext.run-program.test" . "com.informatimago")
  ("com.informatimago.clext.test" . "com.informatimago")
  ("com.informatimago.clisp" . "com.informatimago")
  ("com.informatimago.clisp.test" . "com.informatimago")
  ("com.informatimago.clmisc" . "com.informatimago")
  ("com.informatimago.clmisc.test" . "com.informatimago")
  ("com.informatimago.common-lisp" . "com.informatimago")
  ("com.informatimago.common-lisp.apple-file" . "com.informatimago")
  ("com.informatimago.common-lisp.apple-file.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.arithmetic" . "com.informatimago")
  ("com.informatimago.common-lisp.arithmetic.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.bank" . "com.informatimago")
  ("com.informatimago.common-lisp.bank.test" . "com.informatimago")
  ("com.informatimago.common-lisp.cesarum" . "com.informatimago")
  ("com.informatimago.common-lisp.cesarum.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.csv" . "com.informatimago")
  ("com.informatimago.common-lisp.csv.test" . "com.informatimago")
  ("com.informatimago.common-lisp.data-encoding"
   . "com.informatimago")
  ("com.informatimago.common-lisp.data-encoding.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.diagram" . "com.informatimago")
  ("com.informatimago.common-lisp.diagram.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.ed" . "com.informatimago")
  ("com.informatimago.common-lisp.ed.test" . "com.informatimago")
  ("com.informatimago.common-lisp.graphviz" . "com.informatimago")
  ("com.informatimago.common-lisp.graphviz.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.heap" . "com.informatimago")
  ("com.informatimago.common-lisp.heap.test" . "com.informatimago")
  ("com.informatimago.common-lisp.html-base" . "com.informatimago")
  ("com.informatimago.common-lisp.html-base.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.html-generator"
   . "com.informatimago")
  ("com.informatimago.common-lisp.html-generator.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.html-parser" . "com.informatimago")
  ("com.informatimago.common-lisp.html-parser.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.http" . "com.informatimago")
  ("com.informatimago.common-lisp.http.test" . "com.informatimago")
  ("com.informatimago.common-lisp.interactive" . "com.informatimago")
  ("com.informatimago.common-lisp.interactive.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.invoice" . "com.informatimago")
  ("com.informatimago.common-lisp.invoice.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.lisp" . "com.informatimago")
  ("com.informatimago.common-lisp.lisp-reader" . "com.informatimago")
  ("com.informatimago.common-lisp.lisp-reader.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.lisp-sexp" . "com.informatimago")
  ("com.informatimago.common-lisp.lisp-sexp.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.lisp-text" . "com.informatimago")
  ("com.informatimago.common-lisp.lisp-text.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.lisp.ibcl" . "com.informatimago")
  ("com.informatimago.common-lisp.lisp.ibcl.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.lisp.stepper"
   . "com.informatimago")
  ("com.informatimago.common-lisp.lisp.stepper.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.lisp.test" . "com.informatimago")
  ("com.informatimago.common-lisp.parser" . "com.informatimago")
  ("com.informatimago.common-lisp.parser.test" . "com.informatimago")
  ("com.informatimago.common-lisp.picture" . "com.informatimago")
  ("com.informatimago.common-lisp.picture.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.regexp" . "com.informatimago")
  ("com.informatimago.common-lisp.regexp.test" . "com.informatimago")
  ("com.informatimago.common-lisp.rfc2822" . "com.informatimago")
  ("com.informatimago.common-lisp.rfc2822.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.rfc3548" . "com.informatimago")
  ("com.informatimago.common-lisp.rfc3548.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.scanner" . "com.informatimago")
  ("com.informatimago.common-lisp.scanner.test"
   . "com.informatimago")
  ("com.informatimago.common-lisp.telnet" . "com.informatimago")
  ("com.informatimago.common-lisp.telnet.test" . "com.informatimago")
  ("com.informatimago.common-lisp.test" . "com.informatimago")
  ("com.informatimago.common-lisp.unix" . "com.informatimago")
  ("com.informatimago.common-lisp.unix.test" . "com.informatimago")
  ("com.informatimago.editor" . "com.informatimago")
  ("com.informatimago.editor.test" . "com.informatimago")
  ("com.informatimago.future" . "com.informatimago")
  ("com.informatimago.future.empty" . "com.informatimago")
  ("com.informatimago.future.empty.test" . "com.informatimago")
  ("com.informatimago.future.test" . "com.informatimago")
  ("com.informatimago.languages" . "com.informatimago")
  ("com.informatimago.languages.c11" . "com.informatimago")
  ("com.informatimago.languages.cpp" . "com.informatimago")
  ("com.informatimago.languages.cpp.test" . "com.informatimago")
  ("com.informatimago.languages.cxx" . "com.informatimago")
  ("com.informatimago.languages.cxx.test" . "com.informatimago")
  ("com.informatimago.languages.linc" . "com.informatimago")
  ("com.informatimago.languages.linc.test" . "com.informatimago")
  ("com.informatimago.languages.lua" . "com.informatimago")
  ("com.informatimago.languages.lua.test" . "com.informatimago")
  ("com.informatimago.lispdoc" . "com.informatimago")
  ("com.informatimago.lispdoc.test" . "com.informatimago")
  ("com.informatimago.objcl" . "com.informatimago")
  ("com.informatimago.objcl.test" . "com.informatimago")
  ("com.informatimago.rdp" . "com.informatimago")
  ("com.informatimago.rdp.basic" . "com.informatimago")
  ("com.informatimago.rdp.basic.example" . "com.informatimago")
  ("com.informatimago.rdp.basic.example.test" . "com.informatimago")
  ("com.informatimago.rdp.basic.test" . "com.informatimago")
  ("com.informatimago.rdp.example" . "com.informatimago")
  ("com.informatimago.rdp.example.test" . "com.informatimago")
  ("com.informatimago.rdp.test" . "com.informatimago")
  ("com.informatimago.small-cl-pgms" . "com.informatimago")
  ("com.informatimago.small-cl-pgms.botihn" . "com.informatimago")
  ("com.informatimago.small-cl-pgms.brainfuck" . "com.informatimago")
  ("com.informatimago.small-cl-pgms.brainfuck.test"
   . "com.informatimago")
  ("com.informatimago.small-cl-pgms.life" . "com.informatimago")
  ("com.informatimago.small-cl-pgms.life.test" . "com.informatimago")
  ("com.informatimago.small-cl-pgms.quine" . "com.informatimago")
  ("com.informatimago.small-cl-pgms.quine.test"
   . "com.informatimago")
  ("com.informatimago.small-cl-pgms.test" . "com.informatimago")
  ("com.informatimago.small-cl-pgms.what-implementation"
   . "com.informatimago")
  ("com.informatimago.small-cl-pgms.what-implementation.test"
   . "com.informatimago")
  ("com.informatimago.susv3" . "com.informatimago")
  ("com.informatimago.susv3.test" . "com.informatimago")
  ("com.informatimago.test" . "com.informatimago")
  ("com.informatimago.tools" . "com.informatimago")
  ("com.informatimago.tools.check-asdf" . "com.informatimago")
  ("com.informatimago.tools.check-asdf.test" . "com.informatimago")
  ("com.informatimago.tools.make-depends" . "com.informatimago")
  ("com.informatimago.tools.make-depends.test" . "com.informatimago")
  ("com.informatimago.tools.manifest" . "com.informatimago")
  ("com.informatimago.tools.manifest.test" . "com.informatimago")
  ("com.informatimago.tools.pathname" . "com.informatimago")
  ("com.informatimago.tools.pathname.test" . "com.informatimago")
  ("com.informatimago.tools.quicklisp" . "com.informatimago")
  ("com.informatimago.tools.quicklisp.test" . "com.informatimago")
  ("com.informatimago.tools.reader-macro" . "com.informatimago")
  ("com.informatimago.tools.script" . "com.informatimago")
  ("com.informatimago.tools.script.test" . "com.informatimago")
  ("com.informatimago.tools.source" . "com.informatimago")
  ("com.informatimago.tools.source.test" . "com.informatimago")
  ("com.informatimago.tools.summary" . "com.informatimago")
  ("com.informatimago.tools.summary.test" . "com.informatimago")
  ("com.informatimago.tools.symbol" . "com.informatimago")
  ("com.informatimago.tools.symbol.test" . "com.informatimago")
  ("com.informatimago.tools.test" . "com.informatimago")
  ("com.informatimago.tools.thread" . "com.informatimago")
  ("com.informatimago.tools.try-systems" . "com.informatimago")
  ("com.informatimago.tools.undefmethod" . "com.informatimago")
  ("com.informatimago.tools.undefmethod.test" . "com.informatimago")
  ("com.informatimago.xcode" . "com.informatimago")
  ("com.informatimago.xcode.test" . "com.informatimago")
  ("com.ogamita.swig" . "com.informatimago")
  ("com.ogamita.swig.test" . "com.informatimago")
  ("command-line-arguments" . "command-line-arguments")
  ("common-doc" . "common-doc") ("common-doc-contrib" . "common-doc")
  ("common-doc-gnuplot" . "common-doc")
  ("common-doc-graphviz" . "common-doc")
  ("common-doc-include" . "common-doc")
  ("common-doc-plump" . "common-doc-plump")
  ("common-doc-plump-test" . "common-doc-plump")
  ("common-doc-split-paragraphs" . "common-doc")
  ("common-doc-test" . "common-doc")
  ("common-doc-tex" . "common-doc") ("common-html" . "common-html")
  ("common-html-test" . "common-html") ("comp-set" . "nst")
  ("computable-reals" . "computable-reals")
  ("conditional-commands" . "mcclim")
  ("conduit-packages" . "conduit-packages") ("conium" . "conium")
  ("consix" . "consix")
  ("containers-and-utilities" . "cl-containers")
  ("containers-moptilities" . "cl-containers")
  ("contextl" . "contextl") ("corona" . "corona")
  ("corona-test" . "corona") ("corona-web" . "corona")
  ("cqlcl" . "cqlcl") ("cqlcl-test" . "cqlcl") ("crane" . "crane")
  ("crane-test" . "crane") ("croatoan" . "croatoan")
  ("crypt" . "cl-crypt") ("crypto-shortcuts" . "crypto-shortcuts")
  ("csound" . "sqnc") ("css-lite" . "css-lite")
  ("css-selectors" . "css-selectors")
  ("css-selectors-simple-tree" . "css-selectors")
  ("css-selectors-stp" . "css-selectors")
  ("css-selectors-test" . "css-selectors")
  ("csv-parser" . "csv-parser") ("curly" . "curly")
  ("curly.test" . "curly")
  ("curry-compose-reader-macros" . "curry-compose-reader-macros")
  ("cxml" . "cxml") ("cxml-dom" . "cxml") ("cxml-klacks" . "cxml")
  ("cxml-rng" . "cxml-rng") ("cxml-rpc" . "cxml-rpc")
  ("cxml-stp" . "cxml-stp") ("cxml-stp-test" . "cxml-stp")
  ("cxml-test" . "cxml") ("cxml-xml" . "cxml") ("daemon" . "daemon")
  ("darts.lib.hashtree-test" . "dartsclhashtree")
  ("darts.lib.hashtrie" . "dartsclhashtree")
  ("darts.lib.message-pack" . "dartsclmessagepack")
  ("darts.lib.message-pack-test" . "dartsclmessagepack")
  ("darts.lib.sequence-metrics" . "dartsclsequencemetrics")
  ("darts.lib.wbtree" . "dartsclhashtree")
  ("data-format-validation" . "cl-data-format-validation")
  ("data-sift" . "data-sift") ("data-sift-test" . "data-sift")
  ("data-table" . "data-table") ("data-table-clsql" . "data-table")
  ("data-table-test" . "data-table") ("datafly" . "datafly")
  ("datafly-test" . "datafly") ("date-calc" . "date-calc")
  ("db3" . "cl-db3") ("dbd-mysql" . "cl-dbi")
  ("dbd-postgres" . "cl-dbi") ("dbd-sqlite3" . "cl-dbi")
  ("dbi" . "cl-dbi") ("dbi-test" . "cl-dbi") ("dbus" . "dbus")
  ("dcm" . "elephant") ("decimals" . "cl-decimals")
  ("defclass-std" . "defclass-std")
  ("defclass-std-test" . "defclass-std") ("defcontract" . "nst")
  ("defdoc" . "nst") ("defenum" . "defenum")
  ("deferred" . "deferred")
  ("define-json-expander" . "define-json-expander")
  ("deflate" . "deflate") ("defmacro-enhance" . "defmacro-enhance")
  ("defmacro-enhance-tests" . "defmacro-enhance")
  ("defmemo" . "defmemo") ("defmemo-test" . "defmemo")
  ("defpackage-plus" . "defpackage-plus") ("defrec" . "defrec")
  ("defstar" . "defstar")
  ("defsystem-compatibility" . "defsystem-compatibility")
  ("defsystem-compatibility-test" . "defsystem-compatibility")
  ("defvariant" . "defvariant") ("degree-symbol" . "antik")
  ("delorean" . "delorean") ("delorean-test" . "delorean")
  ("delta-debug" . "delta-debug") ("delta-debug-exe" . "delta-debug")
  ("delta-debug-test" . "delta-debug")
  ("deoxybyte-gzip" . "deoxybyte-gzip")
  ("deoxybyte-gzip-test" . "deoxybyte-gzip")
  ("deoxybyte-io" . "deoxybyte-io")
  ("deoxybyte-io-test" . "deoxybyte-io")
  ("deoxybyte-systems" . "deoxybyte-systems")
  ("deoxybyte-unix" . "deoxybyte-unix")
  ("deoxybyte-unix-test" . "deoxybyte-unix")
  ("deoxybyte-utilities" . "deoxybyte-utilities")
  ("deoxybyte-utilities-test" . "deoxybyte-utilities")
  ("descriptions" . "descriptions")
  ("descriptions-test" . "descriptions")
  ("descriptions.serialization" . "descriptions")
  ("descriptions.validation" . "descriptions")
  ("dexador" . "dexador") ("dexador-test" . "dexador")
  ("diff" . "diff") ("dissect" . "dissect") ("djula" . "djula")
  ("djula-demo" . "djula") ("djula-test" . "djula")
  ("dlist" . "dlist") ("dlist-test" . "dlist")
  ("do-urlencode" . "do-urlencode") ("docbrowser" . "docbrowser")
  ("docparser" . "docparser") ("docparser-test" . "docparser")
  ("docparser-test-system" . "docparser")
  ("documentation-template" . "documentation-template")
  ("docutils" . "cl-docutils") ("dom" . "gendl")
  ("donuts" . "donuts") ("doplus" . "doplus")
  ("doplus-fset" . "doplus") ("doplus-tests" . "doplus")
  ("double-metaphone" . "gbbopen") ("drakma" . "drakma")
  ("drakma-async" . "drakma-async") ("drakma-test" . "drakma")
  ("draw-cons-tree" . "draw-cons-tree") ("drei-mcclim" . "mcclim")
  ("drei-tests" . "mcclim") ("dso-lex" . "dso-lex")
  ("dso-util" . "dso-util") ("duologue" . "duologue")
  ("dweet" . "dweet") ("dyna" . "dyna") ("dyna-test" . "dyna")
  ("dynamic-classes" . "dynamic-classes")
  ("dynamic-classes-test" . "dynamic-classes")
  ("dynamic-collect" . "dynamic-collect")
  ("dynamic-mixins" . "dynamic-mixins") ("dynamic-wind" . "contextl")
  ("eager-future" . "eager-future")
  ("eager-future.test" . "eager-future")
  ("eager-future2" . "eager-future2")
  ("eazy-gnuplot" . "eazy-gnuplot")
  ("eazy-gnuplot.test" . "eazy-gnuplot")
  ("eazy-process" . "eazy-process")
  ("eazy-process.test" . "eazy-process")
  ("eazy-project" . "eazy-project")
  ("eazy-project.test" . "eazy-project") ("ec2" . "ec2")
  ("eco" . "eco") ("eco-test" . "eco") ("ele-bdb" . "elephant")
  ("ele-clp" . "elephant") ("ele-clsql" . "elephant")
  ("ele-postgresql" . "elephant") ("elephant" . "elephant")
  ("elephant-tests" . "elephant") ("elf" . "elf")
  ("elf-test" . "elf") ("enchant" . "cl-enchant")
  ("enchant-autoload" . "cl-enchant")
  ("enhanced-eval-when" . "enhanced-eval-when")
  ("enhanced-multiple-value-bind" . "enhanced-multiple-value-bind")
  ("enumerations" . "cl-enumeration") ("envy" . "envy")
  ("envy-test" . "envy") ("eos" . "eos") ("eos-tests" . "eos")
  ("epigraph" . "epigraph") ("epigraph-test" . "epigraph")
  ("epmd" . "cl-epmd") ("epmd-test" . "cl-epmd")
  ("equals" . "equals") ("erlang-term" . "cl-erlang-term")
  ("erlang-term-optima" . "cl-erlang-term")
  ("erlang-term-test" . "cl-erlang-term") ("ernestine" . "ernestine")
  ("ernestine-gui" . "ernestine") ("ernestine-tests" . "ernestine")
  ("erudite" . "erudite") ("erudite-test" . "erudite")
  ("esa" . "mcclim") ("esa-mcclim" . "mcclim")
  ("escalator" . "escalator") ("escalator-bench" . "escalator")
  ("esrap" . "esrap") ("esrap-liquid" . "esrap-liquid")
  ("esrap-liquid-tests" . "esrap-liquid") ("esrap-peg" . "esrap-peg")
  ("esrap-tests" . "esrap") ("ev" . "cl-ev")
  ("event-emitter" . "event-emitter")
  ("event-emitter-test" . "event-emitter")
  ("event-glue" . "event-glue") ("event-glue-test" . "event-glue")
  ("evol" . "evol") ("evol-test" . "evol")
  ("exponential-backoff" . "exponential-backoff")
  ("exscribe" . "exscribe") ("exscribe/typeset" . "exscribe")
  ("ext-blog" . "ext-blog") ("extended-reals" . "extended-reals")
  ("extensible-sequences" . "sequence-iterators")
  ("external-program" . "external-program")
  ("external-program-test" . "external-program")
  ("f-underscore" . "f-underscore") ("f2cl" . "f2cl")
  ("f2cl-lib" . "clml") ("fare-csv" . "fare-csv")
  ("fare-memoization" . "fare-memoization")
  ("fare-memoization/test" . "fare-memoization")
  ("fare-mop" . "fare-mop") ("fare-quasiquote" . "fare-quasiquote")
  ("fare-quasiquote-extras" . "fare-quasiquote")
  ("fare-quasiquote-optima" . "fare-quasiquote")
  ("fare-quasiquote-readtable" . "fare-quasiquote")
  ("fare-quasiquote-test" . "fare-quasiquote")
  ("fare-utils" . "fare-utils") ("fare-utils-test" . "fare-utils")
  ("fast-http" . "fast-http") ("fast-http-test" . "fast-http")
  ("fast-io" . "fast-io") ("fast-io-test" . "fast-io")
  ("femlisp" . "femlisp") ("femlisp-basic" . "femlisp")
  ("femlisp-matlisp" . "femlisp") ("femlisp-parallel" . "femlisp")
  ("ffa" . "ffa") ("fft" . "fft") ("fgraph" . "cl-fgraph")
  ("fiasco" . "fiasco") ("filtered-functions" . "filtered-functions")
  ("find-port" . "find-port") ("find-port-test" . "find-port")
  ("firephp" . "firephp") ("firephp-tests" . "firephp")
  ("fishpack" . "f2cl") ("fishpack-test-hstcrt" . "f2cl")
  ("fishpack-test-hstcsp" . "f2cl") ("fishpack-test-hstcyl" . "f2cl")
  ("fishpack-test-hstplr" . "f2cl") ("fishpack-test-hstssp" . "f2cl")
  ("fishpack-test-hwscrt" . "f2cl") ("fishpack-test-hwscsp" . "f2cl")
  ("fishpack-test-hwscyl" . "f2cl") ("fishpack-test-hwsplr" . "f2cl")
  ("fishpack-test-hwsssp" . "f2cl") ("fishpack-test-sepx4" . "f2cl")
  ("fiveam" . "fiveam") ("fiveam-test" . "fiveam")
  ("flac" . "mixalot") ("flexi-streams" . "flexi-streams")
  ("flexi-streams-test" . "flexi-streams")
  ("flexichain" . "flexichain") ("flexichain-doc" . "flexichain")
  ("floating-point" . "floating-point")
  ("floating-point-test" . "floating-point") ("fmarshal" . "marshal")
  ("fmarshal-test" . "marshal") ("fmt" . "fmt") ("fmt-test" . "fmt")
  ("fmt-time" . "fmt") ("fn" . "fn") ("folio" . "folio")
  ("folio.as" . "folio") ("folio.boxes" . "folio")
  ("folio.collections" . "folio") ("folio.functions" . "folio")
  ("folio2" . "folio2") ("folio2-as" . "folio2")
  ("folio2-as-syntax" . "folio2") ("folio2-as-tests" . "folio2")
  ("folio2-boxes" . "folio2") ("folio2-boxes-tests" . "folio2")
  ("folio2-functions" . "folio2")
  ("folio2-functions-syntax" . "folio2")
  ("folio2-functions-tests" . "folio2") ("folio2-make" . "folio2")
  ("folio2-make-tests" . "folio2") ("folio2-maps" . "folio2")
  ("folio2-maps-syntax" . "folio2") ("folio2-maps-tests" . "folio2")
  ("folio2-pairs" . "folio2") ("folio2-pairs-tests" . "folio2")
  ("folio2-sequences" . "folio2")
  ("folio2-sequences-syntax" . "folio2")
  ("folio2-sequences-tests" . "folio2") ("folio2-series" . "folio2")
  ("folio2-series-tests" . "folio2") ("folio2-taps" . "folio2")
  ("folio2-taps-tests" . "folio2") ("folio2-tests" . "folio2")
  ("fomus" . "fomus") ("fork-future" . "clml")
  ("form-fiddle" . "form-fiddle") ("formlets" . "formlets")
  ("formlets-test" . "formlets") ("fprog" . "cambl")
  ("fred" . "fred") ("freeimage" . "clinch") ("frpc" . "frpc")
  ("frpc-des" . "frpc") ("frpc-gss" . "frpc") ("frpcgen" . "frpc")
  ("fs-watcher" . "fs-watcher") ("fset" . "fset") ("fsvd" . "fsvd")
  ("ftp" . "cl-ftp") ("fucc-generator" . "fucc")
  ("fucc-parser" . "fucc") ("function-cache" . "function-cache")
  ("function-cache-clsql" . "function-cache")
  ("function-cache-test" . "function-cache")
  ("functional-geometry" . "mcclim") ("funds" . "funds")
  ("g-lib-cffi" . "gtk-cffi") ("g-object-cffi" . "gtk-cffi")
  ("gambol" . "cl-gambol") ("garbage-pools" . "garbage-pools")
  ("garbage-pools-test" . "garbage-pools") ("gbbopen" . "gbbopen")
  ("gbbopen-core" . "gbbopen") ("gbbopen-modules" . "gbbopen")
  ("gbbopen-test" . "gbbopen") ("gbbopen-tools" . "gbbopen")
  ("gbbopen-tools-test" . "gbbopen")
  ("gbbopen-tools-user" . "gbbopen") ("gbbopen-user" . "gbbopen")
  ("gcm" . "gcm") ("gdk-cffi" . "gtk-cffi") ("gendl" . "gendl")
  ("general-accumulator" . "cl-general-accumulator")
  ("generators" . "generators")
  ("generic-comparability" . "generic-comparability")
  ("generic-sequences" . "generic-sequences")
  ("generic-sequences-cont" . "generic-sequences")
  ("generic-sequences-iterate" . "generic-sequences")
  ("generic-sequences-stream" . "generic-sequences")
  ("generic-sequences-test" . "generic-sequences")
  ("genhash" . "genhash") ("geo" . "cl-geo") ("geom-base" . "gendl")
  ("getopt" . "getopt") ("getopt-tests" . "getopt")
  ("gettext" . "gettext") ("gettext-example" . "gettext")
  ("gettext-tests" . "gettext") ("gi-cffi" . "gtk-cffi")
  ("gio-cffi" . "gtk-cffi") ("glass" . "glass") ("glaw" . "glaw")
  ("glaw-examples" . "glaw") ("glaw-imago" . "glaw")
  ("glaw-sdl" . "glaw") ("glisp" . "gendl") ("glkit" . "glkit")
  ("glkit-examples" . "glkit") ("global-vars" . "global-vars")
  ("global-vars-test" . "global-vars") ("glop" . "glop")
  ("glop-test" . "glop") ("glu-tessellate" . "glu-tessellate")
  ("glyphs" . "glyphs") ("glyphs-test" . "glyphs")
  ("goatee-core" . "mcclim") ("gordon" . "gordon")
  ("graph" . "graph") ("graph-dot" . "graph")
  ("graph-json" . "graph") ("graph-matrix" . "graph")
  ("graph-matrix-test" . "graph") ("graph-test" . "graph")
  ("gravatar" . "cl-gravatar") ("graylex" . "graylex")
  ("graylex-m4-example" . "graylex")
  ("green-threads" . "green-threads") ("group-by" . "group-by")
  ("group-by-test" . "group-by") ("groupby" . "cl-groupby")
  ("gsharp" . "gsharp") ("gsll" . "gsll") ("gsll-tests" . "gsll")
  ("gtfl" . "gtfl") ("gtk-cffi" . "gtk-cffi")
  ("gtk-cffi-bin" . "gtk-cffi") ("gtk-cffi-box" . "gtk-cffi")
  ("gtk-cffi-button" . "gtk-cffi")
  ("gtk-cffi-buttonbox" . "gtk-cffi")
  ("gtk-cffi-cell-layout" . "gtk-cffi")
  ("gtk-cffi-cell-renderer" . "gtk-cffi")
  ("gtk-cffi-cell-renderer-pixbuf" . "gtk-cffi")
  ("gtk-cffi-cell-renderer-text" . "gtk-cffi")
  ("gtk-cffi-cell-renderer-toggle" . "gtk-cffi")
  ("gtk-cffi-color-button" . "gtk-cffi")
  ("gtk-cffi-combo-box" . "gtk-cffi")
  ("gtk-cffi-container" . "gtk-cffi") ("gtk-cffi-core" . "gtk-cffi")
  ("gtk-cffi-dialog" . "gtk-cffi") ("gtk-cffi-entry" . "gtk-cffi")
  ("gtk-cffi-eventbox" . "gtk-cffi")
  ("gtk-cffi-file-chooser" . "gtk-cffi")
  ("gtk-cffi-file-chooser-button" . "gtk-cffi")
  ("gtk-cffi-file-chooser-dialog" . "gtk-cffi")
  ("gtk-cffi-frame" . "gtk-cffi") ("gtk-cffi-hbox" . "gtk-cffi")
  ("gtk-cffi-hbuttonbox" . "gtk-cffi")
  ("gtk-cffi-image" . "gtk-cffi") ("gtk-cffi-info-bar" . "gtk-cffi")
  ("gtk-cffi-label" . "gtk-cffi")
  ("gtk-cffi-list-store" . "gtk-cffi") ("gtk-cffi-menu" . "gtk-cffi")
  ("gtk-cffi-menu-bar" . "gtk-cffi")
  ("gtk-cffi-menu-shell" . "gtk-cffi")
  ("gtk-cffi-message-dialog" . "gtk-cffi")
  ("gtk-cffi-misc" . "gtk-cffi") ("gtk-cffi-notebook" . "gtk-cffi")
  ("gtk-cffi-paned" . "gtk-cffi")
  ("gtk-cffi-progress-bar" . "gtk-cffi")
  ("gtk-cffi-range" . "gtk-cffi") ("gtk-cffi-scale" . "gtk-cffi")
  ("gtk-cffi-scrolled-window" . "gtk-cffi")
  ("gtk-cffi-spin-button" . "gtk-cffi")
  ("gtk-cffi-spinner" . "gtk-cffi")
  ("gtk-cffi-status-icon" . "gtk-cffi")
  ("gtk-cffi-statusbar" . "gtk-cffi") ("gtk-cffi-table" . "gtk-cffi")
  ("gtk-cffi-text-buffer" . "gtk-cffi")
  ("gtk-cffi-text-view" . "gtk-cffi")
  ("gtk-cffi-tool-shell" . "gtk-cffi")
  ("gtk-cffi-toolbar" . "gtk-cffi")
  ("gtk-cffi-tree-model" . "gtk-cffi")
  ("gtk-cffi-tree-model-filter" . "gtk-cffi")
  ("gtk-cffi-tree-selection" . "gtk-cffi")
  ("gtk-cffi-tree-view" . "gtk-cffi")
  ("gtk-cffi-tree-view-column" . "gtk-cffi")
  ("gtk-cffi-utils" . "gtk-cffi") ("gtk-cffi-vbox" . "gtk-cffi")
  ("gtk-cffi-widget" . "gtk-cffi") ("gtk-cffi-window" . "gtk-cffi")
  ("gtk-ffi" . "cells-gtk3") ("gwl" . "gendl")
  ("gwl-graphics" . "gendl") ("gzip-stream" . "gzip-stream")
  ("halftone" . "halftone") ("hash-set" . "hash-set")
  ("hdf5-cffi" . "hdf5-cffi") ("hdf5-examples" . "hdf5-cffi")
  ("helambdap" . "helambdap") ("hemlock.base" . "hemlock")
  ("hemlock.clx" . "hemlock") ("hemlock.qt" . "hemlock")
  ("hemlock.tty" . "hemlock") ("hermetic" . "hermetic")
  ("hh-aws" . "hh-aws") ("hh-aws-tests" . "hh-aws")
  ("hh-redblack" . "hh-redblack")
  ("hh-redblack-tests" . "hh-redblack") ("hh-web" . "hh-web")
  ("hinge" . "hinge") ("hl7-client" . "hl7-client")
  ("hl7-parser" . "hl7-parser") ("hompack" . "f2cl")
  ("hompack-test-mainf" . "f2cl") ("hompack-test-mainp" . "f2cl")
  ("hompack-test-mains" . "f2cl") ("hspell" . "hspell")
  ("ht-simple-ajax" . "ht-simple-ajax")
  ("html-encode" . "html-encode") ("html-entities" . "html-entities")
  ("html-entities-tests" . "html-entities")
  ("html-match" . "bknr-web") ("html-match.test" . "bknr-web")
  ("html-sugar" . "html-sugar") ("html-template" . "html-template")
  ("htmlgen" . "portableaserve") ("http-body" . "http-body")
  ("http-body-test" . "http-body") ("http-parse" . "http-parse")
  ("http-parse-test" . "http-parse") ("http-services" . "gbbopen")
  ("hu.dwim.asdf" . "hu.dwim.asdf")
  ("hu.dwim.asdf.documentation" . "hu.dwim.asdf")
  ("hu.dwim.common" . "hu.dwim.common")
  ("hu.dwim.common-lisp" . "hu.dwim.common-lisp")
  ("hu.dwim.common-lisp.documentation" . "hu.dwim.common-lisp")
  ("hu.dwim.common.documentation" . "hu.dwim.common")
  ("hu.dwim.computed-class" . "hu.dwim.computed-class")
  ("hu.dwim.computed-class+hu.dwim.logger"
   . "hu.dwim.computed-class")
  ("hu.dwim.computed-class+swank" . "hu.dwim.computed-class")
  ("hu.dwim.computed-class.documentation" . "hu.dwim.computed-class")
  ("hu.dwim.computed-class.test" . "hu.dwim.computed-class")
  ("hu.dwim.debug" . "hu.dwim.debug")
  ("hu.dwim.debug.documentation" . "hu.dwim.debug")
  ("hu.dwim.debug.test" . "hu.dwim.debug")
  ("hu.dwim.def" . "hu.dwim.def")
  ("hu.dwim.def+cl-l10n" . "hu.dwim.def")
  ("hu.dwim.def+contextl" . "hu.dwim.def")
  ("hu.dwim.def+hu.dwim.common" . "hu.dwim.def")
  ("hu.dwim.def+hu.dwim.delico" . "hu.dwim.def")
  ("hu.dwim.def+swank" . "hu.dwim.def")
  ("hu.dwim.def.documentation" . "hu.dwim.def")
  ("hu.dwim.def.namespace" . "hu.dwim.def")
  ("hu.dwim.def.test" . "hu.dwim.def")
  ("hu.dwim.defclass-star" . "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star+contextl" . "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star+hu.dwim.def" . "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star+hu.dwim.def+contextl"
   . "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star+swank" . "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star.documentation" . "hu.dwim.defclass-star")
  ("hu.dwim.defclass-star.test" . "hu.dwim.defclass-star")
  ("hu.dwim.delico" . "hu.dwim.delico")
  ("hu.dwim.delico.documentation" . "hu.dwim.delico")
  ("hu.dwim.delico.test" . "hu.dwim.delico")
  ("hu.dwim.graphviz" . "hu.dwim.graphviz")
  ("hu.dwim.logger" . "hu.dwim.logger")
  ("hu.dwim.logger+iolib" . "hu.dwim.logger")
  ("hu.dwim.logger+swank" . "hu.dwim.logger")
  ("hu.dwim.logger.documentation" . "hu.dwim.logger")
  ("hu.dwim.logger.test" . "hu.dwim.logger")
  ("hu.dwim.partial-eval" . "hu.dwim.partial-eval")
  ("hu.dwim.partial-eval.documentation" . "hu.dwim.partial-eval")
  ("hu.dwim.partial-eval.test" . "hu.dwim.partial-eval")
  ("hu.dwim.perec" . "hu.dwim.perec")
  ("hu.dwim.perec+hu.dwim.quasi-quote.xml" . "hu.dwim.perec")
  ("hu.dwim.perec+iolib" . "hu.dwim.perec")
  ("hu.dwim.perec+swank" . "hu.dwim.perec")
  ("hu.dwim.perec.all" . "hu.dwim.perec")
  ("hu.dwim.perec.all.test" . "hu.dwim.perec")
  ("hu.dwim.perec.documentation" . "hu.dwim.perec")
  ("hu.dwim.perec.oracle" . "hu.dwim.perec")
  ("hu.dwim.perec.oracle.test" . "hu.dwim.perec")
  ("hu.dwim.perec.postgresql" . "hu.dwim.perec")
  ("hu.dwim.perec.postgresql.test" . "hu.dwim.perec")
  ("hu.dwim.perec.sqlite" . "hu.dwim.perec")
  ("hu.dwim.perec.sqlite.test" . "hu.dwim.perec")
  ("hu.dwim.perec.test" . "hu.dwim.perec")
  ("hu.dwim.presentation" . "hu.dwim.presentation")
  ("hu.dwim.presentation+cl-graph+cl-typesetting"
   . "hu.dwim.presentation")
  ("hu.dwim.presentation+cl-typesetting" . "hu.dwim.presentation")
  ("hu.dwim.presentation+hu.dwim.stefil" . "hu.dwim.presentation")
  ("hu.dwim.presentation+hu.dwim.web-server"
   . "hu.dwim.presentation")
  ("hu.dwim.quasi-quote" . "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.css" . "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.documentation" . "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.js" . "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.test" . "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.xml" . "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.xml+cxml" . "hu.dwim.quasi-quote")
  ("hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js"
   . "hu.dwim.quasi-quote")
  ("hu.dwim.rdbms" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.all" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.all.test" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.documentation" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.oracle" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.oracle.test" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.postgresql" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.postgresql.test" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.sqlite" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.sqlite.test" . "hu.dwim.rdbms")
  ("hu.dwim.rdbms.test" . "hu.dwim.rdbms")
  ("hu.dwim.reiterate" . "hu.dwim.reiterate")
  ("hu.dwim.reiterate+hu.dwim.logger" . "hu.dwim.reiterate")
  ("hu.dwim.reiterate.documentation" . "hu.dwim.reiterate")
  ("hu.dwim.reiterate.test" . "hu.dwim.reiterate")
  ("hu.dwim.serializer" . "hu.dwim.serializer")
  ("hu.dwim.serializer.documentation" . "hu.dwim.serializer")
  ("hu.dwim.serializer.test" . "hu.dwim.serializer")
  ("hu.dwim.stefil" . "hu.dwim.stefil")
  ("hu.dwim.stefil+hu.dwim.def" . "hu.dwim.stefil")
  ("hu.dwim.stefil+hu.dwim.def+swank" . "hu.dwim.stefil")
  ("hu.dwim.stefil+swank" . "hu.dwim.stefil")
  ("hu.dwim.stefil.documentation" . "hu.dwim.stefil")
  ("hu.dwim.stefil.test" . "hu.dwim.stefil")
  ("hu.dwim.syntax-sugar" . "hu.dwim.syntax-sugar")
  ("hu.dwim.syntax-sugar+hu.dwim.walker" . "hu.dwim.syntax-sugar")
  ("hu.dwim.syntax-sugar.documentation" . "hu.dwim.syntax-sugar")
  ("hu.dwim.syntax-sugar.test" . "hu.dwim.syntax-sugar")
  ("hu.dwim.syntax-sugar.unicode" . "hu.dwim.syntax-sugar")
  ("hu.dwim.uri" . "hu.dwim.uri")
  ("hu.dwim.uri.test" . "hu.dwim.uri")
  ("hu.dwim.util" . "hu.dwim.util")
  ("hu.dwim.util+iolib" . "hu.dwim.util")
  ("hu.dwim.util.authorization" . "hu.dwim.util")
  ("hu.dwim.util.documentation" . "hu.dwim.util")
  ("hu.dwim.util.error-handling" . "hu.dwim.util")
  ("hu.dwim.util.error-handling+swank" . "hu.dwim.util")
  ("hu.dwim.util.finite-state-machine" . "hu.dwim.util")
  ("hu.dwim.util.flexml" . "hu.dwim.util")
  ("hu.dwim.util.i18n" . "hu.dwim.util")
  ("hu.dwim.util.linear-mapping" . "hu.dwim.util")
  ("hu.dwim.util.mop" . "hu.dwim.util")
  ("hu.dwim.util.production" . "hu.dwim.util")
  ("hu.dwim.util.production+swank" . "hu.dwim.util")
  ("hu.dwim.util.soap" . "hu.dwim.util")
  ("hu.dwim.util.source" . "hu.dwim.util")
  ("hu.dwim.util.standard-process" . "hu.dwim.util")
  ("hu.dwim.util.temporary-files" . "hu.dwim.util")
  ("hu.dwim.util.test" . "hu.dwim.util")
  ("hu.dwim.util.threads" . "hu.dwim.util")
  ("hu.dwim.util.worker-group" . "hu.dwim.util")
  ("hu.dwim.util.zlib" . "hu.dwim.util")
  ("hu.dwim.walker" . "hu.dwim.walker")
  ("hu.dwim.walker.documentation" . "hu.dwim.walker")
  ("hu.dwim.walker.test" . "hu.dwim.walker")
  ("hu.dwim.web-server" . "hu.dwim.web-server")
  ("hu.dwim.web-server+swank" . "hu.dwim.web-server")
  ("hu.dwim.web-server.application" . "hu.dwim.web-server")
  ("hu.dwim.web-server.application+hu.dwim.perec"
   . "hu.dwim.web-server")
  ("hu.dwim.web-server.application.test" . "hu.dwim.web-server")
  ("hu.dwim.web-server.documentation" . "hu.dwim.web-server")
  ("hu.dwim.web-server.test" . "hu.dwim.web-server")
  ("hu.dwim.web-server.websocket" . "hu.dwim.web-server")
  ("humbler" . "humbler") ("hunchensocket" . "hunchensocket")
  ("hunchentoot" . "hunchentoot")
  ("hunchentoot-auth" . "hunchentoot-auth")
  ("hunchentoot-cgi" . "hunchentoot-cgi")
  ("hunchentoot-dev" . "hunchentoot")
  ("hunchentoot-single-signon" . "hunchentoot-single-signon")
  ("hunchentoot-test" . "hunchentoot")
  ("hyperluminal-mem" . "hyperluminal-mem")
  ("hyperluminal-mem-test" . "hyperluminal-mem")
  ("hyperobject" . "hyperobject")
  ("hyperobject-tests" . "hyperobject") ("iconv" . "cl-iconv")
  ("idna" . "idna") ("ie3fp" . "ie3fp")
  ("ieee-floats" . "ieee-floats")
  ("ieee-floats-tests" . "ieee-floats") ("image" . "image")
  ("imago" . "imago") ("immutable-struct" . "immutable-struct")
  ("incf-cl" . "incf-cl") ("incf-cl-test" . "incf-cl")
  ("incognito-keywords" . "incognito-keywords")
  ("incongruent-methods" . "incongruent-methods")
  ("inferior-shell" . "inferior-shell")
  ("inferior-shell/test" . "inferior-shell") ("infix" . "femlisp")
  ("infix-dollar-reader" . "infix-dollar-reader")
  ("infix-dollar-reader-test" . "infix-dollar-reader")
  ("inner-conditional" . "inner-conditional")
  ("inner-conditional-test" . "inner-conditional")
  ("inotify" . "inotify") ("integral" . "integral")
  ("integral-rest" . "integral-rest")
  ("integral-rest-test" . "integral-rest")
  ("integral-test" . "integral") ("intel-hex" . "intel-hex")
  ("intel-hex-test" . "intel-hex") ("intercom" . "intercom")
  ("intercom-examples" . "intercom") ("interface" . "interface")
  ("interfaces-test-implementation" . "modularize-interfaces")
  ("introspect-environment" . "introspect-environment")
  ("introspect-environment-test" . "introspect-environment")
  ("iolib" . "iolib") ("iolib-grovel" . "iolib")
  ("iolib-tests" . "iolib") ("iolib.asdf" . "iolib")
  ("iolib.base" . "iolib") ("iolib.common-lisp" . "iolib")
  ("iolib.conf" . "iolib") ("iolib.examples" . "iolib")
  ("iolib.multiplex" . "iolib") ("iolib.os" . "iolib")
  ("iolib.pathnames" . "iolib") ("iolib.sockets" . "iolib")
  ("iolib.streams" . "iolib") ("iolib.syscalls" . "iolib")
  ("iolib.trivial-sockets" . "iolib") ("iolib/asdf" . "iolib")
  ("iolib/base" . "iolib") ("iolib/common-lisp" . "iolib")
  ("iolib/conf" . "iolib") ("iolib/examples" . "iolib")
  ("iolib/grovel" . "iolib") ("iolib/multiplex" . "iolib")
  ("iolib/os" . "iolib") ("iolib/pathnames" . "iolib")
  ("iolib/sockets" . "iolib") ("iolib/streams" . "iolib")
  ("iolib/syscalls" . "iolib") ("iolib/tests" . "iolib")
  ("iolib/trivial-sockets" . "iolib") ("iolib/zstreams" . "iolib")
  ("ip-interfaces" . "ip-interfaces") ("irc-logger" . "irc-logger")
  ("ironclad" . "ironclad") ("ironclad-tests" . "ironclad")
  ("ironclad-text" . "ironclad") ("iterate" . "iterate")
  ("iterate-clsql" . "iterate-clsql") ("iterate-pg" . "iterate")
  ("iterate-tests" . "iterate") ("ixf" . "cl-ixf")
  ("jenkins.api" . "jenkins") ("jonathan" . "jonathan")
  ("jonathan-test" . "jonathan") ("jp-numeral" . "jp-numeral")
  ("jpl-queues" . "jpl-queues") ("jpl-util" . "cl-jpl-util")
  ("js-parser" . "js-parser") ("js-parser-tests" . "js-parser")
  ("json-responses" . "json-responses")
  ("json-responses-test" . "json-responses")
  ("json-streams" . "json-streams")
  ("json-streams-tests" . "json-streams")
  ("json-template" . "cl-json-template") ("jsown" . "jsown")
  ("jsown-tests" . "jsown") ("jwacs" . "jwacs")
  ("jwacs-tests" . "jwacs") ("kanren-trs" . "cl-kanren-trs")
  ("kanren-trs-test" . "cl-kanren-trs") ("kebab" . "kebab")
  ("kebab-test" . "kebab") ("kenzo" . "kenzo")
  ("kenzo-test" . "kenzo") ("kl-verify" . "kl-verify") ("km" . "km")
  ("kmrcl" . "kmrcl") ("kmrcl-tests" . "kmrcl") ("l-math" . "l-math")
  ("lack" . "lack") ("lack-component" . "lack")
  ("lack-middleware-accesslog" . "lack")
  ("lack-middleware-auth-basic" . "lack")
  ("lack-middleware-backtrace" . "lack")
  ("lack-middleware-csrf" . "lack")
  ("lack-middleware-mount" . "lack")
  ("lack-middleware-session" . "lack")
  ("lack-middleware-static" . "lack") ("lack-request" . "lack")
  ("lack-response" . "lack") ("lack-session-store-dbi" . "lack")
  ("lack-test" . "lack") ("lack-util" . "lack")
  ("lambda-fiddle" . "lambda-fiddle") ("lambda-gtk" . "lambda-gtk")
  ("lambda-gtk-examples" . "lambda-gtk")
  ("lambda-reader" . "lambda-reader")
  ("lambda-reader-8bit" . "lambda-reader")
  ("lambdalite" . "lambdalite") ("langutils" . "cl-langutils")
  ("lapack" . "f2cl") ("lapack-complex" . "f2cl")
  ("lapack-package" . "f2cl") ("lapack-real" . "f2cl")
  ("lapack-tests" . "f2cl") ("lass" . "lass") ("lassie" . "lassie")
  ("latex-table" . "latex-table") ("ledger" . "gendl")
  ("leech" . "bknr-web") ("legion" . "legion")
  ("legion-test" . "legion") ("let-over-lambda" . "let-over-lambda")
  ("let-plus" . "let-plus") ("let-plus-tests" . "let-plus")
  ("letrec" . "letrec") ("lev" . "lev")
  ("levenshtein" . "levenshtein") ("lexer" . "cl-lexer")
  ("lfarm-admin" . "lfarm") ("lfarm-client" . "lfarm")
  ("lfarm-common" . "lfarm") ("lfarm-gss" . "lfarm")
  ("lfarm-launcher" . "lfarm") ("lfarm-server" . "lfarm")
  ("lfarm-ssl" . "lfarm") ("lfarm-test" . "lfarm")
  ("lhstats" . "lhstats") ("libssh2" . "cl-libssh2")
  ("libssh2.test" . "cl-libssh2") ("libusb-ffi" . "cl-libusb")
  ("lift" . "lift") ("lift-and-metatilities" . "metatilities")
  ("lift-documentation" . "lift") ("lift-test" . "lift")
  ("lil" . "lisp-interface-library")
  ("lil/test" . "lisp-interface-library") ("lime" . "lime")
  ("lime-example" . "lime") ("lime-test" . "lime")
  ("linedit" . "linedit") ("linewise-template" . "linewise-template")
  ("lisa" . "lisa") ("lisp-executable" . "lisp-executable")
  ("lisp-executable-example" . "lisp-executable")
  ("lisp-executable-tests" . "lisp-executable")
  ("lisp-interface-library" . "lisp-interface-library")
  ("lisp-invocation" . "lisp-invocation")
  ("lisp-invocation/all" . "lisp-invocation")
  ("lisp-matrix" . "lisp-matrix")
  ("lisp-namespace" . "lisp-namespace")
  ("lisp-namespace.test" . "lisp-namespace")
  ("lisp-unit" . "lisp-unit") ("lisp-unit2" . "lisp-unit2")
  ("lisp-unit2-test" . "lisp-unit2")
  ("lispbuilder-lexer" . "lispbuilder")
  ("lispbuilder-net" . "lispbuilder")
  ("lispbuilder-net-cffi" . "lispbuilder")
  ("lispbuilder-opengl-1-1" . "lispbuilder")
  ("lispbuilder-opengl-examples" . "lispbuilder")
  ("lispbuilder-regex" . "lispbuilder")
  ("lispbuilder-sdl" . "lispbuilder")
  ("lispbuilder-sdl-assets" . "lispbuilder")
  ("lispbuilder-sdl-base" . "lispbuilder")
  ("lispbuilder-sdl-binaries" . "lispbuilder")
  ("lispbuilder-sdl-cffi" . "lispbuilder")
  ("lispbuilder-sdl-cl-vectors" . "lispbuilder")
  ("lispbuilder-sdl-cl-vectors-examples" . "lispbuilder")
  ("lispbuilder-sdl-examples" . "lispbuilder")
  ("lispbuilder-sdl-gfx" . "lispbuilder")
  ("lispbuilder-sdl-gfx-binaries" . "lispbuilder")
  ("lispbuilder-sdl-gfx-cffi" . "lispbuilder")
  ("lispbuilder-sdl-gfx-examples" . "lispbuilder")
  ("lispbuilder-sdl-image" . "lispbuilder")
  ("lispbuilder-sdl-image-binaries" . "lispbuilder")
  ("lispbuilder-sdl-image-cffi" . "lispbuilder")
  ("lispbuilder-sdl-image-examples" . "lispbuilder")
  ("lispbuilder-sdl-mixer" . "lispbuilder")
  ("lispbuilder-sdl-mixer-binaries" . "lispbuilder")
  ("lispbuilder-sdl-mixer-cffi" . "lispbuilder")
  ("lispbuilder-sdl-mixer-examples" . "lispbuilder")
  ("lispbuilder-sdl-ttf" . "lispbuilder")
  ("lispbuilder-sdl-ttf-binaries" . "lispbuilder")
  ("lispbuilder-sdl-ttf-cffi" . "lispbuilder")
  ("lispbuilder-sdl-ttf-examples" . "lispbuilder")
  ("lispbuilder-sdl-vecto" . "lispbuilder")
  ("lispbuilder-sdl-vecto-examples" . "lispbuilder")
  ("lispbuilder-windows" . "lispbuilder")
  ("lispbuilder-yacc" . "lispbuilder")
  ("list-of" . "asdf-finalizers") ("listoflist" . "listoflist")
  ("lla" . "lla") ("lla-tests" . "lla") ("llvm" . "cl-llvm")
  ("lml" . "lml") ("lml-tests" . "lml") ("lml2" . "lml2")
  ("lml2-tests" . "lml2")
  ("local-package-aliases" . "local-package-aliases")
  ("local-time" . "local-time")
  ("local-time-duration" . "local-time-duration")
  ("local-time.test" . "local-time") ("log4cl" . "log4cl")
  ("log4cl-examples" . "log4cl") ("log4cl-test" . "log4cl")
  ("log4slime" . "log4cl") ("log5" . "log5") ("lol-re" . "lol-re")
  ("lol-re-tests" . "lol-re") ("lowlight" . "lowlight")
  ("lowlight.doc" . "lowlight") ("lowlight.old" . "lowlight")
  ("lowlight.tests" . "lowlight") ("lparallel" . "lparallel")
  ("lparallel-bench" . "lparallel") ("lparallel-test" . "lparallel")
  ("lquery" . "lquery") ("lquery-test" . "lquery")
  ("lracer" . "racer") ("lredis" . "lredis") ("ltk" . "ltk")
  ("ltk-mw" . "ltk") ("ltk-remote" . "ltk")
  ("lucene-in-action-tests" . "montezuma") ("lucerne" . "lucerne")
  ("lucerne-auth" . "lucerne") ("lucerne-hello-world" . "lucerne")
  ("lucerne-test" . "lucerne") ("lucerne-utweet" . "lucerne")
  ("lw-compat" . "lw-compat") ("m2cl" . "m2cl")
  ("m2cl-examples" . "m2cl") ("m2cl-test" . "m2cl")
  ("mach-par" . "f2cl") ("macro-level" . "macro-level")
  ("macroexpand-dammit" . "macroexpand-dammit")
  ("madeira-port" . "madeira-port")
  ("madeira-port-tests" . "madeira-port") ("magicffi" . "magicffi")
  ("magicffi-test" . "magicffi") ("mailbox" . "mailbox")
  ("make-hash" . "make-hash") ("make-hash-tests" . "make-hash")
  ("manardb" . "manardb") ("manardb-test" . "manardb")
  ("manifest" . "manifest") ("map-bind" . "map-bind")
  ("map-set" . "map-set") ("marching-cubes" . "marching-cubes")
  ("marching-cubes-example" . "marching-cubes")
  ("marching-cubes-test" . "marching-cubes")
  ("marshal" . "cl-marshal") ("math-high" . "antik")
  ("math-high-tests" . "antik") ("mathkit" . "mathkit")
  ("mcclim" . "mcclim") ("mcclim-freetype" . "mcclim")
  ("mcclim-gif-bitmaps" . "mcclim")
  ("mcclim-jpeg-bitmaps" . "mcclim")
  ("mcclim-png-bitmaps" . "mcclim")
  ("mcclim-tiff-bitmaps" . "mcclim")
  ("mcclim-tree-with-cross-edges" . "mcclim")
  ("mcclim-truetype" . "mcclim") ("md5" . "md5")
  ("media-types" . "media-types")
  ("media-types-tests" . "media-types") ("mel-base" . "mel-base")
  ("memoize" . "memoize") ("message-oo" . "message-oo")
  ("meta" . "meta") ("meta-sexp" . "meta-sexp")
  ("metabang-bind" . "metabang-bind")
  ("metabang-bind-test" . "metabang-bind") ("metacopy" . "metacopy")
  ("metacopy-test" . "metacopy")
  ("metacopy-test-with-contextl" . "metacopy")
  ("metacopy-with-contextl" . "metacopy") ("metafs" . "metafs")
  ("metap" . "metap") ("metap-test" . "metap")
  ("metatilities" . "metatilities")
  ("metatilities-base" . "metatilities-base")
  ("metatilities-test" . "metatilities")
  ("method-combination-utilities" . "method-combination-utilities")
  ("method-combination-utilities.tests"
   . "method-combination-utilities")
  ("method-versions" . "method-versions") ("mexpr" . "mexpr")
  ("mexpr-tests" . "mexpr") ("mgl" . "mgl") ("mgl-example" . "mgl")
  ("mgl-gnuplot" . "mgl") ("mgl-pax" . "mgl-pax")
  ("mgl-pax-test" . "mgl-pax") ("mgl-test" . "mgl")
  ("mgl-visuals" . "mgl") ("micmac" . "micmac")
  ("micmac-test" . "micmac") ("midi" . "midi")
  ("mime4cl" . "mime4cl") ("mime4cl-tests" . "mime4cl")
  ("minheap" . "minheap") ("minheap-tests" . "minheap")
  ("minpack" . "f2cl") ("minpack-tests-hybrd" . "f2cl")
  ("minpack-tests-lmdif" . "f2cl")
  ("misc-extensions" . "misc-extensions") ("mixalot" . "mixalot")
  ("mixalot-flac" . "mixalot") ("mixalot-mp3" . "mixalot")
  ("mixalot-vorbis" . "mixalot")
  ("mk-string-metrics" . "mk-string-metrics")
  ("mk-string-metrics-tests" . "mk-string-metrics")
  ("mlep" . "cl-mlep") ("mlep-add" . "cl-mlep")
  ("mnst-relay" . "nst") ("modf" . "modf")
  ("modf-fset" . "modf-fset") ("modf-fset-test" . "modf-fset")
  ("modf-test" . "modf") ("modlisp" . "cl-modlisp")
  ("modularize" . "modularize")
  ("modularize-hooks" . "modularize-hooks")
  ("modularize-interfaces" . "modularize-interfaces")
  ("modularize-test-module" . "modularize")
  ("module-manager" . "gbbopen") ("module-manager-user" . "gbbopen")
  ("monkeylib-html" . "monkeylib-html")
  ("monkeylib-markup-html" . "monkeylib-markup-html")
  ("monkeylib-text-languages" . "monkeylib-text-languages")
  ("monkeylib-text-output" . "monkeylib-text-output")
  ("montezuma" . "montezuma") ("montezuma-indexfiles" . "montezuma")
  ("montezuma-tests" . "montezuma") ("mop-utils" . "mop-utils")
  ("moptilities" . "moptilities")
  ("moptilities-test" . "moptilities")
  ("more-conditions" . "more-conditions")
  ("more-conditions-test" . "more-conditions") ("mpc" . "mpc")
  ("mpg123-ffi" . "mixalot") ("mssql" . "cl-mssql")
  ("mt19937" . "mt19937") ("mtlisp" . "mtlisp")
  ("multinode" . "gbbopen")
  ("multiple-value-variants" . "multiple-value-variants")
  ("multival-plist" . "multival-plist")
  ("multival-plist-test" . "multival-plist")
  ("mw-equiv" . "mw-equiv") ("myway" . "myway")
  ("myway-test" . "myway") ("myweb" . "myweb")
  ("named-readtables" . "named-readtables")
  ("named-readtables-doc" . "named-readtables")
  ("named-readtables-test" . "named-readtables")
  ("napa-fft3" . "napa-fft3") ("net-telent-date" . "net-telent-date")
  ("net.didierverna.clon" . "cl-clon")
  ("net.didierverna.clon.core" . "cl-clon")
  ("net.didierverna.clon.setup" . "cl-clon")
  ("net.didierverna.clon.setup/termio" . "cl-clon")
  ("net.didierverna.clon.termio" . "cl-clon")
  ("net.didierverna.declt" . "declt")
  ("net.didierverna.declt.core" . "declt")
  ("net.didierverna.declt.setup" . "declt") ("net4cl" . "net4cl")
  ("network-streaming" . "gbbopen")
  ("neutral" . "software-evolution") ("new-op" . "new-op")
  ("nibbles" . "nibbles") ("nibbles-tests" . "nibbles")
  ("ningle" . "ningle") ("ningle-test" . "ningle") ("npg" . "npg")
  ("nsort" . "nsort") ("nst" . "nst") ("nst-manual-tests" . "nst")
  ("nst-meta-tests" . "nst") ("nst-mop-utils" . "nst")
  ("nst-selftest-utils" . "nst") ("nst-simple-tests" . "nst")
  ("nst-test" . "nst") ("nst-test-jenkins" . "nst")
  ("nuclblog" . "nuclblog") ("nxt" . "cl-nxt")
  ("nxt-proxy" . "cl-nxt") ("odd-streams" . "odd-streams")
  ("odd-streams-test" . "odd-streams") ("odesk" . "cl-odesk")
  ("ods4cl" . "ods4cl") ("oe-encode" . "oe-encode")
  ("oe-encode-test" . "oe-encode") ("open-vrp" . "open-vrp")
  ("open-vrp-lib" . "open-vrp") ("opticl" . "opticl")
  ("opticl-doc" . "opticl") ("optima" . "optima")
  ("optima.ppcre" . "optima") ("optima.test" . "optima")
  ("or-glpk" . "cl-opsresearch") ("or-gsl" . "cl-opsresearch")
  ("or-test" . "cl-opsresearch")
  ("org-davep-dict" . "org-davep-dict")
  ("org-davep-dictrepl" . "org-davep-dictrepl")
  ("org-sampler" . "nst")
  ("org.middleangle.cl-blapack" . "cl-blapack")
  ("org.middleangle.cl-blapack-examples" . "cl-blapack")
  ("org.middleangle.cl-blapack-gen" . "cl-blapack")
  ("org.middleangle.foreign-numeric-vector" . "fnv")
  ("os-interface" . "gbbopen") ("osc" . "osc") ("osicat" . "osicat")
  ("osicat-tests" . "osicat") ("pack" . "pack")
  ("package-renaming" . "package-renaming")
  ("package-renaming-test" . "package-renaming")
  ("packet" . "packet") ("paiprolog" . "paiprolog") ("pal" . "pal")
  ("pango" . "clinch") ("par-eval" . "cl-mpi")
  ("parameterized-function" . "parameterized-function")
  ("paren-files" . "paren-files") ("paren-util" . "paren-util")
  ("parenscript" . "parenscript")
  ("parenscript-classic" . "parenscript-classic")
  ("parenscript.test" . "parenscript")
  ("parse-declarations-1.0" . "parse-declarations")
  ("parse-float" . "parse-float")
  ("parse-float-tests" . "parse-float") ("parse-js" . "parse-js")
  ("parse-number" . "parse-number")
  ("parse-number-range" . "parse-number-range")
  ("parse-number-tests" . "parse-number") ("parse-rgb" . "cl-tcod")
  ("parseltongue" . "parseltongue")
  ("parser-combinators" . "cl-parser-combinators")
  ("parser-combinators-cl-ppcre" . "cl-parser-combinators")
  ("parser-combinators-debug" . "cl-parser-combinators")
  ("parser-combinators-tests" . "cl-parser-combinators")
  ("patron" . "patron") ("pcall" . "pcall") ("pcall-queue" . "pcall")
  ("pcall-tests" . "pcall") ("pcl-unit-test" . "cl-match")
  ("percent-encoding" . "percent-encoding")
  ("percent-encoding-test" . "percent-encoding")
  ("perfpiece" . "perfpiece") ("periodic-table" . "periodic-table")
  ("periods" . "periods") ("periods-series" . "periods")
  ("perlre" . "perlre") ("persistent-tables" . "persistent-tables")
  ("persistent-variables" . "persistent-variables")
  ("persistent-variables.test" . "persistent-variables")
  ("petit.package-utils" . "petit.package-utils")
  ("petit.string-utils" . "petit.string-utils")
  ("petit.string-utils-test" . "petit.string-utils")
  ("pettomato-deque" . "pettomato-deque")
  ("pettomato-deque-tests" . "pettomato-deque")
  ("pettomato-indexed-priority-queue"
   . "pettomato-indexed-priority-queue")
  ("pettomato-indexed-priority-queue-tests"
   . "pettomato-indexed-priority-queue")
  ("pfft" . "fft") ("pg" . "pg") ("pgloader" . "pgloader")
  ("ph-maths" . "cells-gtk3") ("phonon" . "qtools")
  ("pileup" . "pileup") ("pileup-tests" . "pileup")
  ("pipes" . "pipes") ("piping" . "piping")
  ("pithy-xml" . "pithy-xml") ("place-modifiers" . "place-modifiers")
  ("place-utils" . "place-utils") ("plain-odbc" . "plain-odbc")
  ("plain-odbc-with-libs" . "plain-odbc") ("planks" . "planks")
  ("plokami" . "plokami") ("plplot-examples" . "cl-plplot")
  ("plump" . "plump") ("plump-bundle" . "plump-bundle")
  ("plump-dom" . "plump") ("plump-lexer" . "plump")
  ("plump-parser" . "plump") ("plump-sexp" . "plump-sexp")
  ("plump-tex" . "plump-tex") ("plump-tex-test" . "plump-tex")
  ("png" . "cl-png") ("png-read" . "png-read")
  ("png-test" . "cl-png") ("pod-utils" . "cells-gtk3")
  ("policy-cond" . "policy-cond") ("polling-functions" . "gbbopen")
  ("pooler" . "pooler") ("portable-sockets" . "gbbopen")
  ("portable-sockets-test" . "gbbopen")
  ("portable-threads-test" . "gbbopen")
  ("positional-lambda" . "positional-lambda")
  ("postmodern" . "postmodern") ("postmodern-tests" . "postmodern")
  ("postoffice" . "postoffice") ("pounds" . "pounds")
  ("pp-toml" . "pp-toml") ("pp-toml-tests" . "pp-toml")
  ("prepl" . "prepl") ("pretty-function" . "pretty-function")
  ("printv" . "printv") ("priority-queue" . "priority-queue")
  ("proc-parse" . "proc-parse") ("proc-parse-test" . "proc-parse")
  ("projectured.demo" . "projectured")
  ("projectured.product" . "projectured")
  ("projectured.sdl" . "projectured")
  ("projectured.sdl.test" . "projectured") ("protobuf" . "protobuf")
  ("prove" . "prove") ("prove-asdf" . "prove")
  ("psgraph" . "psgraph") ("ptester" . "ptester") ("puri" . "puri")
  ("puri-tests" . "puri") ("purl" . "purl")
  ("py-configparser" . "py-configparser")
  ("py-configvalidator" . "py-configvalidator")
  ("pythonic-string-reader" . "pythonic-string-reader")
  ("pzmq" . "pzmq") ("pzmq-compat" . "pzmq")
  ("pzmq-examples" . "pzmq") ("pzmq-test" . "pzmq") ("q+" . "qtools")
  ("qbook" . "qbook") ("qimageblitz" . "qtools") ("qlot" . "qlot")
  ("qlot-install" . "qlot") ("qlot-test" . "qlot")
  ("qmynd" . "qmynd") ("qmynd-test" . "qmynd") ("qsci" . "qtools")
  ("qt" . "commonqt") ("qt-lib-generator" . "qt-libs")
  ("qt-libs" . "qt-libs") ("qt-repl" . "commonqt")
  ("qt-test" . "commonqt") ("qt-tutorial" . "commonqt")
  ("qt3support" . "qtools") ("qtcore" . "qtools")
  ("qtdbus" . "qtools") ("qtdeclarative" . "qtools")
  ("qtgui" . "qtools") ("qthelp" . "qtools") ("qtnetwork" . "qtools")
  ("qtools" . "qtools") ("qtools-evaluator" . "qtools")
  ("qtools-game" . "qtools") ("qtools-helloworld" . "qtools")
  ("qtools-keychord-editor" . "qtools") ("qtools-melody" . "qtools")
  ("qtools-opengl" . "qtools") ("qtools-titter" . "qtools")
  ("qtopengl" . "qtools") ("qtscript" . "qtools")
  ("qtsql" . "qtools") ("qtsvg" . "qtools") ("qttest" . "qtools")
  ("qtuitools" . "qtools") ("qtwebkit" . "qtools")
  ("qtxml" . "qtools") ("qtxmlpatterns" . "qtools")
  ("quadpack" . "f2cl") ("quadpack-tests" . "f2cl")
  ("quadtree" . "quadtree") ("quadtree-test" . "quadtree")
  ("quasiquote-2.0" . "quasiquote-2.0")
  ("quasiquote-2.0-tests" . "quasiquote-2.0")
  ("query-fs" . "query-fs") ("queue" . "gbbopen")
  ("queues" . "queues") ("queues.priority-cqueue" . "queues")
  ("queues.priority-queue" . "queues")
  ("queues.simple-cqueue" . "queues")
  ("queues.simple-queue" . "queues") ("quickapp" . "quickapp")
  ("quicklisp-slime-helper" . "quicklisp-slime-helper")
  ("quickproject" . "quickproject") ("quicksearch" . "quicksearch")
  ("quickutil" . "quickutil") ("quickutil-client" . "quickutil")
  ("quickutil-client-management" . "quickutil")
  ("quickutil-server" . "quickutil")
  ("quickutil-utilities" . "quickutil")
  ("quickutil-utilities-test" . "quickutil")
  ("quid-pro-quo" . "quid-pro-quo")
  ("quid-pro-quo-tests" . "quid-pro-quo")
  ("quine-mccluskey" . "cl-logic") ("quri" . "quri")
  ("quri-test" . "quri") ("quux-time" . "quux-time")
  ("racer" . "racer") ("random" . "random")
  ("random-access-lists" . "random-access-lists")
  ("random-test" . "random") ("ratify" . "ratify") ("rcl" . "rcl")
  ("rcl-test" . "rcl") ("read-csv" . "read-csv")
  ("read-csv.test" . "read-csv") ("readable" . "readable")
  ("reader-interception" . "reader-interception")
  ("reader-interception-test" . "reader-interception")
  ("rectangle-packing" . "rectangle-packing") ("recur" . "recur")
  ("recursive-regex" . "recursive-regex")
  ("recursive-regex-test" . "recursive-regex") ("regex" . "regex")
  ("regression" . "gendl") ("repair" . "software-evolution")
  ("repl-utilities" . "repl-utilities") ("restas" . "restas")
  ("restas-directory-publisher" . "restas-directory-publisher")
  ("restas-doc" . "restas")
  ("restas.file-publisher" . "restas.file-publisher")
  ("restful" . "restful") ("restful-test" . "restful")
  ("retrospectiff" . "retrospectiff") ("reversi" . "reversi")
  ("rfc2109" . "rfc2109") ("rfc2388" . "rfc2388")
  ("rfc2388-binary" . "rfc2388-binary")
  ("rfc3339-timestamp" . "rfc3339-timestamp")
  ("rfc3339-timestamp-test" . "rfc3339-timestamp") ("rlc" . "rlc")
  ("robot" . "gendl") ("rock" . "rock") ("rock-test" . "rock")
  ("rock-web" . "rock") ("romreader" . "romreader")
  ("routes" . "cl-routes") ("routes-test" . "cl-routes")
  ("rpc4cl" . "rpc4cl") ("rpc4cl-test" . "rpc4cl") ("rpm" . "rpm")
  ("rss" . "cl-rss") ("rt" . "rt") ("rucksack" . "rucksack")
  ("rucksack-test" . "rucksack") ("rutils" . "rutils")
  ("rutils-test" . "rutils") ("rutilsx" . "rutils")
  ("ryeboy" . "ryeboy") ("ryeboy-test" . "ryeboy")
  ("s-base64" . "s-base64") ("s-dot" . "s-dot")
  ("s-http-client" . "s-http-client")
  ("s-http-server" . "s-http-server") ("s-protobuf" . "s-protobuf")
  ("s-sql" . "postmodern") ("s-sysdeps" . "s-sysdeps")
  ("s-utils" . "s-utils") ("s-xml" . "s-xml")
  ("s-xml-rpc" . "s-xml-rpc") ("s-xml.examples" . "s-xml")
  ("s-xml.test" . "s-xml") ("salza2" . "salza2") ("sane" . "cl-sane")
  ("sanitize" . "cl-sanitize") ("sanitize-test" . "cl-sanitize")
  ("sapaclisp" . "sapaclisp") ("sb-cga" . "sb-cga")
  ("sb-fastcgi" . "sb-fastcgi") ("sb-vector-io" . "sb-vector-io")
  ("scalpl" . "scalpl") ("scalpl.mpex" . "scalpl") ("sclf" . "sclf")
  ("screamer" . "screamer") ("screamer-tests" . "screamer")
  ("scriba" . "scriba") ("scriba-test" . "scriba")
  ("scribble" . "scribble") ("scribble/test" . "scribble")
  ("scriptl" . "scriptl") ("scriptl-examples" . "scriptl")
  ("scriptl-util" . "scriptl") ("sdl2" . "cl-sdl2")
  ("sdl2-examples" . "cl-sdl2") ("sdl2kit" . "sdl2kit")
  ("sdl2kit-examples" . "sdl2kit")
  ("secret-values" . "secret-values")
  ("secure-random" . "secure-random") ("selenium" . "cl-selenium")
  ("sequence-iterators" . "sequence-iterators")
  ("sequence-iterators-test" . "sequence-iterators")
  ("serapeum" . "serapeum") ("serapeum-tests" . "serapeum")
  ("series" . "series") ("series-tests" . "series")
  ("session-token" . "session-token") ("sexml" . "sexml")
  ("sexml-objects" . "sexml") ("sha3" . "sha3")
  ("shadchen" . "shadchen") ("sheeple" . "sheeple")
  ("sheeple-tests" . "sheeple") ("shellpool" . "shellpool")
  ("shelly" . "shelly") ("shelly-test" . "shelly")
  ("should-test" . "should-test") ("shuffletron" . "shuffletron")
  ("simple-blog" . "weblocks-examples")
  ("simple-currency" . "simple-currency")
  ("simple-date" . "postmodern")
  ("simple-date-postgres-glue" . "postmodern")
  ("simple-date-tests" . "postmodern")
  ("simple-date-time" . "simple-date-time")
  ("simple-finalizer" . "simple-finalizer")
  ("simple-rgb" . "simple-rgb") ("simple-tasks" . "simple-tasks")
  ("simpsamp" . "simpsamp")
  ("single-threaded-ccl" . "single-threaded-ccl")
  ("singleton-classes" . "cl-abstract-classes")
  ("sip-hash" . "sip-hash") ("sip-hash-test" . "sip-hash")
  ("skippy" . "skippy") ("smackjack" . "smackjack")
  ("smackjack-demo" . "smackjack") ("smtp4cl" . "smtp4cl")
  ("smug" . "smug") ("snakes" . "snakes") ("snappy" . "snappy")
  ("snappy-test" . "snappy") ("snmp" . "snmp")
  ("snmp-server" . "snmp") ("snmp-test" . "snmp")
  ("softdrink" . "softdrink")
  ("software-evolution" . "software-evolution")
  ("software-evolution-command-line" . "software-evolution")
  ("software-evolution-test" . "software-evolution")
  ("software-evolution-utility" . "software-evolution")
  ("soundex" . "soundex") ("south" . "south")
  ("sparseset" . "buffalo") ("spartns" . "spartns")
  ("spartns-test" . "spartns") ("spatial-trees" . "spatial-trees")
  ("spatial-trees.nns" . "spatial-trees")
  ("spatial-trees.nns.test" . "spatial-trees")
  ("spatial-trees.test" . "spatial-trees")
  ("spellcheck" . "spellcheck") ("sphinx" . "cl-sphinx")
  ("spinneret" . "spinneret") ("spinneret-tests" . "spinneret")
  ("split-sequence" . "split-sequence")
  ("split-sequence-tests" . "split-sequence")
  ("sqlite" . "cl-sqlite") ("squirl" . "squirl")
  ("squirl.demo" . "squirl") ("st-json" . "st-json")
  ("standard-cl" . "cl-match") ("staple" . "staple")
  ("staple-server" . "staple") ("static-vectors" . "static-vectors")
  ("static-vectors/test" . "static-vectors")
  ("stdutils" . "cl-stdutils") ("stefil" . "stefil")
  ("stefil-test" . "stefil") ("stem" . "stem") ("stmx" . "stmx")
  ("stmx.test" . "stmx") ("stp-query" . "stp-query")
  ("streaming" . "gbbopen") ("string-case" . "string-case")
  ("string-escape" . "string-escape") ("stringprep" . "stringprep")
  ("stringprep-test" . "stringprep")
  ("stump-touchy-mode-line" . "stump-touchy-mode-line")
  ("stumpwm" . "stumpwm") ("submarine" . "submarine")
  ("surf" . "gendl") ("swank" . "slime")
  ("swank-client" . "swank-client")
  ("swank-client-test" . "swank-client")
  ("swank-crew" . "swank-crew") ("swank-crew-test" . "swank-crew")
  ("swank-protocol" . "swank-protocol") ("swap-bytes" . "swap-bytes")
  ("swap-bytes/test" . "swap-bytes") ("sxql" . "sxql")
  ("sxql-test" . "sxql") ("symbol-munger" . "symbol-munger")
  ("symbol-munger-test" . "symbol-munger")
  ("symbol-namespaces" . "symbol-namespaces")
  ("synonyms" . "synonyms") ("t-clack-handler-fcgi" . "clack")
  ("t-clack-handler-hunchentoot" . "clack")
  ("t-clack-handler-toot" . "clack")
  ("t-clack-handler-wookie" . "clack")
  ("t-clack-middleware-auth-basic" . "clack")
  ("t-clack-middleware-csrf" . "clack")
  ("t-clack-v1-compat" . "clack") ("t-lack" . "lack")
  ("t-lack-component" . "lack")
  ("t-lack-middleware-accesslog" . "lack")
  ("t-lack-middleware-auth-basic" . "lack")
  ("t-lack-middleware-backtrace" . "lack")
  ("t-lack-middleware-csrf" . "lack")
  ("t-lack-middleware-mount" . "lack")
  ("t-lack-middleware-session" . "lack")
  ("t-lack-middleware-static" . "lack")
  ("t-lack-session-store-dbi" . "lack") ("t-lack-util" . "lack")
  ("ta2" . "gendl") ("tagger" . "tagger") ("talcl" . "talcl")
  ("talcl-examples" . "talcl") ("talcl-speed-tests" . "talcl")
  ("talcl-test" . "talcl") ("tap-unit-test" . "tap-unit-test")
  ("tasty" . "gendl") ("tbnl" . "tbnl") ("tcod" . "cl-tcod")
  ("teepeedee2" . "teepeedee2") ("teepeedee2-test" . "teepeedee2")
  ("telnetlib" . "telnetlib") ("template" . "template")
  ("temporal-functions" . "temporal-functions")
  ("temporary-file" . "temporary-file") ("terminfo" . "terminfo")
  ("test-gtk" . "cells-gtk3") ("test-harness" . "gbbopen")
  ("test-serial-system" . "asdf-dependency-grovel")
  ("test.eager-future2" . "eager-future2")
  ("test.vas-string-metrics" . "vas-string-metrics")
  ("testbild" . "testbild") ("testbild-test" . "testbild")
  ("text-query" . "text-query") ("thnappy" . "thnappy")
  ("thopter" . "blackthorn-engine") ("thorn" . "thorn")
  ("thorn-doc" . "thorn") ("thorn-test" . "thorn")
  ("thread-pool" . "thread-pool")
  ("thread.comm.rendezvous" . "thread.comm.rendezvous")
  ("thread.comm.rendezvous.test" . "thread.comm.rendezvous")
  ("tiff4cl" . "tiff4cl") ("time-interval" . "time-interval")
  ("tinaa" . "tinaa") ("tinaa-and-cl-markdown" . "tinaa")
  ("tinaa-test" . "tinaa") ("toadstool" . "toadstool")
  ("toadstool-tests" . "toadstool") ("toms419" . "f2cl")
  ("toms419-test" . "f2cl") ("toms717" . "f2cl")
  ("toms717-tests" . "f2cl") ("toot" . "toot") ("torta" . "torta")
  ("towers" . "towers") ("track-best" . "track-best")
  ("track-best-tests" . "track-best") ("translators" . "gendl")
  ("transparent-wrap" . "transparent-wrap")
  ("transparent-wrap-test" . "transparent-wrap") ("tree" . "gendl")
  ("treedb" . "treedb") ("treedb.doc" . "treedb")
  ("treedb.tests" . "treedb") ("trees" . "trees")
  ("trivia" . "trivia") ("trivia.balland2006" . "trivia.balland2006")
  ("trivia.balland2006.enabled" . "trivia.balland2006")
  ("trivia.balland2006.enabled.test" . "trivia.balland2006")
  ("trivia.balland2006.test" . "trivia.balland2006")
  ("trivia.benchmark" . "trivia") ("trivia.benchmark.run" . "trivia")
  ("trivia.level0" . "trivia") ("trivia.level0.test" . "trivia")
  ("trivia.level1" . "trivia") ("trivia.level1.test" . "trivia")
  ("trivia.level2" . "trivia") ("trivia.level2.test" . "trivia")
  ("trivia.ppcre" . "trivia") ("trivia.ppcre.test" . "trivia")
  ("trivial-arguments" . "trivial-arguments")
  ("trivial-backtrace" . "trivial-backtrace")
  ("trivial-backtrace-test" . "trivial-backtrace")
  ("trivial-benchmark" . "trivial-benchmark")
  ("trivial-bit-streams" . "trivial-bit-streams")
  ("trivial-channels" . "trivial-channels")
  ("trivial-debug-console" . "trivial-debug-console")
  ("trivial-download" . "trivial-download")
  ("trivial-download-test" . "trivial-download")
  ("trivial-dump-core" . "trivial-dump-core")
  ("trivial-extract" . "trivial-extract")
  ("trivial-extract-test" . "trivial-extract")
  ("trivial-features" . "trivial-features")
  ("trivial-features-tests" . "trivial-features")
  ("trivial-garbage" . "trivial-garbage")
  ("trivial-garbage-tests" . "trivial-garbage")
  ("trivial-gray-streams" . "trivial-gray-streams")
  ("trivial-gray-streams-test" . "trivial-gray-streams")
  ("trivial-http" . "trivial-http")
  ("trivial-http-test" . "trivial-http")
  ("trivial-indent" . "trivial-indent")
  ("trivial-irc" . "trivial-irc")
  ("trivial-irc-echobot" . "trivial-irc")
  ("trivial-lazy" . "trivial-lazy") ("trivial-ldap" . "trivial-ldap")
  ("trivial-main-thread" . "trivial-main-thread")
  ("trivial-mimes" . "trivial-mimes")
  ("trivial-octet-streams" . "trivial-octet-streams")
  ("trivial-raw-io" . "trivial-raw-io")
  ("trivial-shell" . "trivial-shell")
  ("trivial-shell-test" . "trivial-shell")
  ("trivial-signal" . "trivial-signal")
  ("trivial-tco" . "trivial-tco")
  ("trivial-tco-test" . "trivial-tco")
  ("trivial-thumbnail" . "trivial-thumbnail")
  ("trivial-timeout" . "trivial-timeout")
  ("trivial-timers" . "trivial-timers")
  ("trivial-types" . "trivial-types")
  ("trivial-update" . "trivial-update")
  ("trivial-utf-8" . "trivial-utf-8")
  ("trivial-utf-8-tests" . "trivial-utf-8")
  ("trivialib.type-unify" . "trivialib.type-unify")
  ("trivialib.type-unify.test" . "trivialib.type-unify")
  ("tutorial-example" . "gbbopen") ("twfy" . "twfy")
  ("twitter-elephant-driver" . "cl-twitter")
  ("twitter-mongodb-driver" . "cl-twitter") ("type-i" . "type-i")
  ("type-i.test" . "type-i") ("type-r" . "type-r")
  ("type-r.test" . "type-r") ("ucw" . "ucw") ("ucw-core" . "ucw")
  ("ucw-core.test" . "ucw") ("ucw.examples" . "ucw")
  ("ucw.httpd" . "ucw") ("ucw.iolib" . "ucw")
  ("ucw.manual-examples" . "ucw") ("ucw.mod-lisp" . "ucw")
  ("uffi" . "uffi") ("uffi-tests" . "uffi") ("uiop" . "uiop")
  ("umlisp" . "umlisp") ("umlisp-orf" . "umlisp-orf")
  ("umlisp-tests" . "umlisp") ("unicly" . "unicly")
  ("unifgram" . "paiprolog") ("unit-formulas" . "unit-formula")
  ("unit-test" . "unit-test")
  ("universal-config" . "universal-config")
  ("unix-options" . "unix-options") ("unix-opts" . "unix-opts")
  ("unix-opts-tests" . "unix-opts") ("uri-template" . "uri-template")
  ("uri-template.test" . "uri-template")
  ("url-rewrite" . "url-rewrite") ("userial" . "userial")
  ("userial-tests" . "userial") ("usocket" . "usocket")
  ("usocket-test" . "usocket") ("usocket-udp" . "usocket-udp")
  ("utilities.binary-dump" . "utilities.binary-dump")
  ("utilities.binary-dump-test" . "utilities.binary-dump")
  ("utilities.print-items" . "utilities.print-items")
  ("utilities.print-items-test" . "utilities.print-items")
  ("utilities.print-tree" . "utilities.print-tree")
  ("utils-kt" . "utils-kt") ("utm" . "utm") ("uuid" . "uuid")
  ("variates-and-metacopy" . "cl-variates") ("varint" . "protobuf")
  ("varint-test" . "protobuf") ("varjo" . "varjo")
  ("vas-string-metrics" . "vas-string-metrics") ("vecto" . "vecto")
  ("verbose" . "verbose") ("verrazano" . "verrazano")
  ("verrazano-runtime" . "verrazano") ("vertex" . "vertex")
  ("vertex-test" . "vertex") ("vgplot" . "vgplot")
  ("vgplot-test" . "vgplot") ("vom" . "vom")
  ("vorbisfile-ffi" . "mixalot")
  ("web-crawler-tests" . "cl-web-crawler")
  ("webactions" . "portableaserve") ("weblocks" . "weblocks")
  ("weblocks-clsql" . "weblocks-stores")
  ("weblocks-clsql-demo" . "weblocks-examples")
  ("weblocks-custom" . "weblocks-stores")
  ("weblocks-demo" . "weblocks-examples")
  ("weblocks-demo-popover" . "weblocks")
  ("weblocks-elephant" . "weblocks-stores")
  ("weblocks-elephant-demo" . "weblocks-examples")
  ("weblocks-memory" . "weblocks-stores")
  ("weblocks-montezuma" . "weblocks-stores")
  ("weblocks-perec" . "weblocks-stores")
  ("weblocks-postmodern" . "weblocks-stores")
  ("weblocks-prevalence" . "weblocks-stores")
  ("weblocks-s11" . "weblocks") ("weblocks-scripts" . "weblocks")
  ("weblocks-store-test" . "weblocks-stores")
  ("weblocks-stores" . "weblocks-stores")
  ("weblocks-test" . "weblocks")
  ("weblocks-tree-widget" . "weblocks-tree-widget")
  ("weblocks-util" . "weblocks")
  ("weblocks-utils" . "weblocks-utils")
  ("weblocks-yarek" . "weblocks") ("weblocks-yui" . "weblocks")
  ("websocket-driver" . "websocket-driver") ("weft" . "weft")
  ("wilbur" . "de.setf.wilbur") ("wire-world" . "gendl")
  ("with-c-syntax" . "with-c-syntax") ("wkb" . "cl-wkb")
  ("woo" . "woo") ("woo-test" . "woo") ("wookie" . "wookie")
  ("workout-timer" . "workout-timer") ("wu-decimal" . "wu-decimal")
  ("wu-sugar" . "wu-sugar") ("wuwei" . "wuwei")
  ("wuwei-examples" . "wuwei") ("x.fdatatypes" . "x.fdatatypes")
  ("x.fdatatypes-iterate" . "x.fdatatypes")
  ("x.let-star" . "x.let-star") ("xarray" . "xarray")
  ("xarray-test" . "xarray") ("xecto" . "xecto")
  ("xembed" . "clx-xembed") ("xfactory" . "cl-libxml2")
  ("xfactory-test" . "cl-libxml2") ("xhtmlambda" . "xhtmlambda")
  ("xhtmlgen" . "xhtmlgen") ("xhtmlgen-test" . "xhtmlgen")
  ("xkeyboard" . "clx-xkeyboard")
  ("xkeyboard-test" . "clx-xkeyboard") ("xlunit" . "xlunit")
  ("xlunit-tests" . "xlunit") ("xml-emitter" . "xml-emitter")
  ("xml-mop" . "xml-mop") ("xml-render" . "cl-typesetting")
  ("xml.location" . "xml.location")
  ("xml.location-and-local-time" . "xml.location")
  ("xml.location-test" . "xml.location") ("xmls" . "xmls")
  ("xmls-tools" . "xmls-tools") ("xmls/test" . "xmls")
  ("xoverlay" . "cl-libxml2") ("xpath" . "plexippus-xpath")
  ("xptest" . "xptest") ("xsubseq" . "xsubseq")
  ("xsubseq-test" . "xsubseq") ("xuriella" . "xuriella")
  ("yacc" . "cl-yacc") ("yaclanapht" . "yaclanapht")
  ("yaclanapht-test" . "yaclanapht") ("yaclml" . "yaclml")
  ("yadd" . "gendl") ("yason" . "yason") ("zaws" . "zaws")
  ("zaws-xml" . "zaws") ("zcdb" . "zcdb") ("zeromq" . "cl-zmq")
  ("zeromq.tests" . "cl-zmq") ("zip" . "zip") ("zlib" . "zlib")
  ("zmq" . "lisp-zmq") ("zmq-examples" . "lisp-zmq")
  ("zmq-test" . "lisp-zmq") ("zpb-exif" . "zpb-exif")
  ("zpb-ttf" . "zpb-ttf") ("zpng" . "zpng") ("zs3" . "zs3")
  ("zsort" . "zsort")))