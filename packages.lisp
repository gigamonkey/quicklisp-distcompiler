;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :gigamonkey-distcompiler
  (:use :common-lisp
        :com.gigamonkeys.pathnames
        :com.gigamonkeys.utilities
        :com.gigamonkeys.macro-utilities)
  (:export :compile-dist))

