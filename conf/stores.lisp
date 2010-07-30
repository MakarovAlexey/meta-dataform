
(in-package :meta-dataform)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *meta-dataform-store* :memory
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :meta-dataform)))

