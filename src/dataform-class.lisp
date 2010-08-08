;;понадобиться переопределить dataform-submit-action
;;допилить инициализацю объекта до совместимости с weblocks через (defmethod finalize-inheritace :after ((class dataform-class)) ...)
;;update-object-view-from-request создать аналог

(in-package meta-dataform)

(export '(standard-dataform))

(defclass my-form-view (form-view) ())

(defclass my-data-view (data-view) ())

;;(defmethod print-view-field-value (value presentation field (view my-data-view) widget obj &rest args)
;;  (print-view-field-value value (make-instance 'text-presentation) field view widget obj args))

(defclass dataform-class (widget-class my-form-view my-data-view) ;;т.о. слоты form-view становятся свойством класса
  ((constructor-fn :initarg :constructor-fn :accessor constructor-fn))
  (:default-initargs
   ;:persistp nil
    )
  (:documentation "Необходим для определения поведения"))

(defclass standard-dataform (dataform) ()
  (:default-initargs
     :ui-state :form)
  (:metaclass dataform-class))

(defmacro deform (name direct-superclasses direct-slots &optional creation-fn)
  `(let ((class-name (quote ,name))
	 (slots-initargs (mapcar (lambda (slot-initargs)
				   (etypecase slot-initargs
				     (symbol
				      (list :name slot-initargs))
				     (list
				      (list* :name (mapcar (lambda (el)
							     (if (symbolp el) el
								 (eval el)))
							   slot-initargs)))))
				 (quote ,direct-slots))))
     (setf (find-class class-name)
	   (make-instance 'dataform-class :name class-name
			  :direct-superclasses (mapcar #'find-class (quote ,direct-superclasses))
			  :direct-slots slots-initargs
			  :constructor-fn ,creation-fn))))

;;и теперь можно определить поведение
;; Следующие два метода определяют, что суперкласс метакласса "form-class", по умолчанию, будет класс "standard-form"
(defmethod initialize-instance :around ((class dataform-class) &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
	 thereis (subtypep class (find-class 'standard-dataform)))
      ;; 'standard-form is already one of the (indirect) superclasses
      (call-next-method)
      ;; 'standard-form is not one of the superclasses, so we have to add it
      (apply #'call-next-method class :direct-superclasses (append direct-superclasses (list (find-class 'standard-dataform))) initargs)))

(defmethod reinitialize-instance :around ((class dataform-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (if (loop for class in direct-superclasses thereis (subtypep class (find-class 'standard-dataform)))
	  (call-next-method)
	  (apply #'call-next-method class :direct-superclasses (append direct-superclasses
								       (list (find-class 'standard-dataform))) initargs))
      ;; if direct superclasses are not explicitly passed ;; we _must_ not change anything
      (call-next-method)))

(defun compute-constructor-fn (&rest classes)
  (dolist (class classes (error "Object constructor function must be present"))
    (let ((constructor (constructor-fn class)))
      (when (not (null constructor))
	(return constructor)))))

(defmethod initialize-instance :after ((class dataform-class) &key direct-superclasses constructor-fn &allow-other-keys)
  (when (null constructor-fn)
    (setf (constructor-fn class)
	  (apply #'compute-constructor-fn
		 (remove-if-not (lambda (class)
				  (subtypep class 'standard-dataform))
				direct-superclasses)))))

(defmethod reinitialize-instance :after ((class dataform-class) &key direct-superclasses constructor-fn &allow-other-keys)
  (when (null constructor-fn)
    (setf (constructor-fn class)
	  (apply #'compute-constructor-fn
		 (remove-if-not (lambda (class)
				  (subtypep class 'standard-dataform))
				direct-superclasses)))))

(defmethod initialize-instance :after ((instance standard-dataform) &key &allow-other-keys)
  (setf (dataform-form-view instance) (class-of instance)
	(dataform-data-view instance) (class-of instance)))

(defclass direct-form-field-definition (form-view-field data-view-field standard-direct-slot-definition) ()
  (:documentation "Хранит то, что передается из defview в полях как :present-as и :parse-as"))

(defclass effective-field-definition (form-view-field data-view-field standard-effective-slot-definition) ())

(defmethod direct-slot-definition-class ((class dataform-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-form-field-definition))

(defmethod initialize-instance :after ((instance direct-form-field-definition) &key name &allow-other-keys)
  (setf (view-field-slot-name instance) name))

(defun compute-standard-effective-slot-definition-initargs (class direct-slot-definitions)
  #+sbcl(sb-pcl::compute-effective-slot-definition-initargs class direct-slot-definitions)
  #-sbcl(not-yet-implemented))

(defun compute-form-view-field-initargs (direct-slot-definition)
  (loop for slot in (class-slots (class-of direct-slot-definition))
     for initarg = (first (slot-definition-initargs slot))
     for slot-name = (slot-definition-name slot)
     when (not (null (and initarg (slot-boundp direct-slot-definition slot-name)
			  (not (null (slot-value direct-slot-definition slot-name))))))
     append `(,initarg ,(slot-value direct-slot-definition slot-name))))

(defmethod compute-effective-slot-definition ((dataform dataform-class) slot-name direct-slot-definitions)
  :documentation "Необходимо доинициализировать метаобъект слота, поля переопределяются полностью и берется последнее опредление слота"
  (let ((dsd (first direct-slot-definitions)))
    (if (typep dsd 'direct-form-field-definition)
      (apply #'make-instance 'effective-field-definition
	     (append (compute-standard-effective-slot-definition-initargs dataform direct-slot-definitions)
		     (compute-form-view-field-initargs dsd)))
      (call-next-method))))

(defmethod compute-slots ((class dataform-class))
  (let ((slots (call-next-method)))
    (setf (view-fields class)
	  (remove-if-not (lambda (slot)
			   (typep slot 'effective-field-definition)) slots))
    slots))

(defmethod render-view-field-value (value (presentation checkbox-presentation) (field effective-field-definition) (view my-data-view) widget obj &rest args)
  (ecase (dataform-ui-state widget)
    (:data (apply #'render-view-field-value value (make-instance 'predicate-presentation) field view widget obj args))
    (:form (call-next-method))))

(defmethod render-view-field-value (value (presentation dropdown-presentation) (field effective-field-definition) (view my-data-view) widget obj &rest args)
  (ecase (dataform-ui-state widget)
    (:data (apply #'render-view-field-value value (make-instance 'text-presentation) field view widget obj args))
    (:form (call-next-method))))

(defmethod render-view-field-value (value (presentation input-presentation) (field effective-field-definition) (view my-data-view) widget obj &rest args)
  (ecase (dataform-ui-state widget)
    (:data (apply #'render-view-field-value value (make-instance 'text-presentation) field view widget obj args))
    (:form (call-next-method))))

(defmethod render-view-field-value (value (presentation radio-presentation) (field effective-field-definition) (view my-data-view) widget obj &rest args)
  (ecase (dataform-ui-state widget)
    (:data (apply #'render-view-field-value value (make-instance 'text-presentation) field view widget obj args))
    (:form (call-next-method))))

;;Так как объект создается после отправки данных формы на сервер, изменим поведение следующх функций
(defmethod render-widget-body ((obj standard-dataform) &rest args)
  (let ((data (if (or (slot-boundp obj 'data) (null (slot-value obj 'data)))
		  (dataform-data obj)
		  obj)))
    (apply #'render-dataform obj data args)))

(defmethod render-object-view-impl (obj view (widget standard-dataform) &rest args)
    (apply #'with-view-header view obj widget
	   (lambda (view obj &rest args)
	     (apply #'map-view-fields
		    (lambda (field-info)
		      (let ((field (field-info-field field-info))
			    (obj (field-info-object field-info)))
			(safe-apply (view-field-prefix-fn field) view field obj args)
			(apply #'render-view-field
			       field view widget (view-field-presentation field)
			       (when (not (null obj)) ;; when obj is nil we must skip obtaining value (the values of nil object is nil)
				 (obtain-view-field-value field obj))
			       obj
			       :field-info field-info
			       args)
			(safe-apply (view-field-suffix-fn field) view field obj args)))
		    view obj args))
	   args))

;;Написать свое представление, которое дополнит effective-field-definition  !!!!!!!!!!!!!!

;render-view-field-value < null > < password-presentation  > < form-view-field  > < form-view  > < t > < t > 
;render-view-field-value < t > < dropdown-presentation > < form-view-field > < form-view > < t > < t > 
;render-view-field-value < t > < password-presentation > < form-view-field > < form-view > < t > < t > 
;render-view-field-value < t > < radio-presentation > < form-view-field > < form-view > < t > < t > 
;render-view-field-value < t > < textarea-presentation > < form-view-field > < form-view > < t > < t > 
;render-view-field-value < t > < us-state-presentation > < form-view-field > < form-view > < t > < t > 

;render-view-field-value < null > < predicate-presentation > < t > < t > < t > < t > 
;render-view-field-value < t > < excerpt-presentation > < t > < t > < t > < t > 
;render-view-field-value < t > < image-presentation > < t > < t > < t > < t > 
;render-view-field-value < t > < paragraph-presentation > < t > < t > < t > < t > 
;render-view-field-value < t > < text-presentation > < t > < t > < t > < t > 
;render-view-field-value < t > < url-presentation > < t > < t > < t > < t > 

;(defmethod render-object-view-impl (obj view (widget standard-dataform))
;  (if (not (null obj)) (call-next-method)

;(in-package :weblocks)

;;; Пришлось слегка изменить поведение этой функции
(defun find-view (view &optional (signal-error-p t))
  "Finds a view. If 'view' is a symbol, looks up the appropriate view
object. If 'view' is a view object, simply returns it. Otherwise,
signals an error.

If 'view' is a list, finds a scaffold class by calling
'scaffold-class-name' and builds an appropriate scaffold view by
calling 'generate-scaffold-view' with the scaffold class name and the
second argument."
  (if (or (typep view 'meta-dataform::dataform-class)
	  (typep view 'meta-dataform::gridedit-class)) view ;; Если тип представления dataform-class или gridedit-class, то просто возвращаем объект
      (or (etypecase view
	    (list (generate-scaffold-view (make-instance (scaffold-class-name (first view)))
					  (find-class (second view))))
	    (symbol (gethash view weblocks::*views*))
	    (view view))
	  (when signal-error-p
	    (error "Cannot find view ~A" view)))))

(define-condition value-condition ()
  ((field :initarg :field :reader field)
   (error-msg :initarg :error-msg :reader error-msg)))

(define-condition value-not-parsed (value-condition) ())

(define-condition value-not-valid (value-condition) ())

(define-condition value-is-required (value-condition) ())

(defun validate-field (dataform field)
  (let ((slot-name (slot-definition-name field)))
    (when (form-view-field-required-p field)
      (error 'value-is-required :field field :error (weblocks::get-required-error-msg field)))
    (multiple-value-bind (valid error) (validate-form-view-field (slot-definition-name field)
								 dataform
								 field
								 (dataform-form-view dataform)
								 (when (slot-boundp dataform slot-name)
								   (slot-value dataform slot-name)))
      (when (not valid)
	(error 'value-not-valid :field field :error error)))))

(defun apply-value (dataform field)
  (let ((slot-name (slot-definition-name field)))
    (multiple-value-bind (parsedp presentp parsed-value)
	(parse-view-field-value (form-view-field-parser field)
				(request-parameter-for-presentation (attributize-name (format nil "~@[~A-~]~A" nil slot-name))
								    (view-field-presentation field))
				dataform
				(dataform-form-view dataform)
				field)
      (when (not parsedp)
	(error 'value-not-parsed :field field :error-msg (weblocks::get-required-error-msg field)))
      (if (not presentp)
	  (slot-makunbound dataform slot-name)
	  (setf (slot-value dataform slot-name) parsed-value)))
    (validate-field dataform field)))

(defun apply-values (dataform)
  (dolist (field (view-fields (dataform-form-view dataform)))
    (restart-case (apply-value dataform field)
      (skip-field () nil))))

(defun params-list (dataform)
  (loop for field in (view-fields (dataform-form-view dataform))
     for slot-name = (slot-definition-name field)
     when (slot-boundp dataform slot-name)
     append `(,(intern (string slot-name) :keyword) ,(slot-value dataform slot-name))))

(defun create-object (dataform)
  (let ((object (apply (constructor-fn (class-of dataform)) (params-list dataform))))
    (setf (dataform-data dataform) object)
    (persist-object (dataform-class-store dataform) object)))

;;if writer slot of field is unbound, (form-view-field-writer field) throws exception
(defmethod write-field-value (field obj dataform)
  (funcall (form-view-field-writer field) obj (slot-value dataform (view-field-slot-name field))))

(defmethod write-field-value (field (obj standard-object) dataform)
  (if (slot-boundp field 'weblocks::writer) (call-next-method)
      (let ((slot-name (view-field-slot-name field)))
	(setf (slot-value obj slot-name) (slot-value dataform slot-name)))))

(defmethod update-object (obj dataform)
  (loop for field in (view-fields (dataform-form-view dataform))
     for slot-name = (slot-definition-name field)
     if (slot-boundp dataform slot-name)
     do (write-field-value field obj dataform)
     else do (slot-makunbound obj slot-name)))

(defmethod dataform-submit-action ((dataform standard-dataform) obj &rest args)
  (declare (ignore args))
  (let ((errors nil))
    (flet ((store-error (c)
	     (push c errors)
	     (invoke-restart 'skip-field)))
      (handler-bind (('value-not-parsed #'store-error)
		     ('value-not-valid #'store-error))
	(apply-values dataform)));; подумать над переделкой, вынести накопление ошибок в apply-values, создать новое улсовие "some-fields-have-errors" перехватывая в dataform-submit-action сдлеать возврат ошибок или t
    (if (not (null errors))
	(loop for e in errors appending
	     `(,(field e) ,(error-msg e)))
	(progn (if (null obj)
		   (create-object dataform)
		   (update-object obj dataform))
	       t))))

;(defmethod dataform-submit-action ((dataform standard-dataform) obj &rest args)
;  (apply #'update-object-view-from-request obj dataform args))
  
;  (apply #'update-object-view-from-request obj dataform args)
;	 :class-store (dataform-class-store dataform) args))