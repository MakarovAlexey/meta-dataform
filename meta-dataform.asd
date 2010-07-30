;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:meta-dataform-asd
  (:use :cl :asdf))

(in-package :meta-dataform-asd)

(defsystem meta-dataform
    :name "meta-dataform"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "meta-dataform"
    :depends-on (:weblocks-memory)
    :components ((:file "meta-dataform")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("meta-dataform"))
		 (:module src
		  :components ((:file "dataform-fields")
			       (:file "private-store")
			       (:file "gridedit-class" :depends-on ("private-store"))
			       (:file "dataform-class" :depends-on ("gridedit-class"))
			       (:file "init-session" :depends-on ("dataform-fields" "dataform-class")))
		  :depends-on ("meta-dataform" conf))))

