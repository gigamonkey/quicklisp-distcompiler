;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem gigamonkey-distcompiler
  :components
  ((:file "packages")
   (:file "tarhash" :depends-on ("packages"))
   (:file "distcompiler" :depends-on ("packages" "tarhash")))
  :depends-on (:ironclad))