
(defpackage #:meta-dataform
  (:use :cl :sb-mop :weblocks :weblocks-memory
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
		#:set-cookie #:set-cookie* #:cookie-in
		#:user-agent #:referer)
  (:export :standard-gridedit)
  (:documentation
   "A web application based on Weblocks."))

(in-package :meta-dataform)

(export '(start-meta-dataform stop-meta-dataform))

;; A macro that generates a class or this webapp

(defwebapp meta-dataform
    :prefix "/"
    :description "meta-dataform: A new application"
    :init-user-session 'meta-dataform::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t
    )

;; Top level start & stop scripts

(defun start-meta-dataform (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'meta-dataform))

(defun stop-meta-dataform ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'meta-dataform)
  (stop-weblocks))

(start-meta-dataform)

