;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem gigamonkey-distcompiler
  :components
  :description "Tool for generating Quicklisp dists."
  ((:file "packages")
   (:file "tarhash" :depends-on ("packages"))
   (:file "distcompiler" :depends-on ("packages" "tarhash")))
  :depends-on (:ironclad
               :com.gigamonkeys.pathnames
               :com.gigamonkeys.utilities
               :com.gigamonkeys.macro-utilities))