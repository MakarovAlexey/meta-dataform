(in-package meta-dataform)

(export '(standard-gridedit))

(defclass my-table-view (table-view) ())

(defclass my-table-view-field (table-view-field) ())

;;(defmethod initialize-instance :after ((instance my-table-view-field) &key 

(defclass gridedit-class (widget-class my-table-view)
  ((item-dataform-class :initarg :dataform-class
			:accessor item-dataform-class-of
			:documentation "Хранит имя класса dataform, данный объект создается при выборе объекта в таблице")
   (direct-fields :initarg :direct-fields :initform nil
		  :reader direct-fields))
  (:documentation "Необходим для определения поведения"))

(defclass standard-gridedit (gridedit)
  ()
  (:metaclass gridedit-class))

(defmethod initialize-instance :around ((class gridedit-class) &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
	 thereis (subtypep class (find-class 'standard-gridedit)))
      ;; 'standard-form is already one of the (indirect) superclasses
      (call-next-method)
      ;; 'standard-form is not one of the superclasses, so we have to add it
      (apply #'call-next-method class :direct-superclasses (append direct-superclasses (list (find-class 'standard-gridedit))) initargs)))

(defmethod reinitialize-instance :around ((class gridedit-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (if (loop for class in direct-superclasses thereis (subtypep class (find-class 'standard-gridedit)))
	  (call-next-method)
	  (apply #'call-next-method class :direct-superclasses (append direct-superclasses
								       (list (find-class 'standard-gridedit))) initargs))
      ;; if direct superclasses are not explicitly passed ;; we _must_ not change anything
      (call-next-method)))

(defun compute-dataform-class (&rest classes-names)
  (let ((classes (remove-if-not (lambda (class)
				  (subtypep class 'standard-gridedit))
				(mapcar #'find-class classes-names))))
    (dolist (class classes (error "dataform-class must be present"))
      (let ((dataform-class (item-dataform-class-of class)))
	(when (not (null dataform-class))
	  (return dataform-class))))))

(defun compute-view-field (&rest direct-fields)
  (first direct-fields))

(defun compute-fields (direct-fields &rest direct-superclasses) ;;отрефакторить
  (let* ((fields (make-hash-table)))
    (dolist (field (apply #'append
			  direct-fields
			  (mapcar #'direct-fields
				  (remove-if-not (lambda (class)
						   (subtypep class 'standard-gridedit))
						 direct-superclasses))))
      (push field (gethash (view-field-slot-name field) fields)))
    (reverse (loop for direct-fields being the hash-values of fields using (hash-key key)
		collect (apply #'compute-view-field (reverse direct-fields))))))

(defmethod initialize-instance :after ((class gridedit-class) &key direct-superclasses direct-fields &allow-other-keys)
  (when (null (item-dataform-class-of class))
    (apply #'compute-dataform-class direct-superclasses))
  (setf (view-fields class) (apply #'compute-fields direct-fields direct-superclasses)))

(defmethod reinitialize-instance :after ((class gridedit-class) &key direct-superclasses direct-fields &allow-other-keys)
  (when (null (item-dataform-class-of class))
    (apply #'compute-dataform-class direct-superclasses))
  (setf (view-fields class) (apply #'compute-fields direct-fields direct-superclasses)))

;данный метод почему-то не срабатывает, вметсто него србабатывае мтеод класса dataseq
;(defmethod initialize-instance :after ((obj standard-gridedit) &key &allow-other-keys)
;  (declare (ignore initargs))
;  ;; Ensure data-class is specified
;;  (when (null (dataseq-data-class obj))
;;    (error "data-class must be specified to initialize a dataseq-based widget."))
;  ;; Ensure class-store is specified
;;  (setf (dataseq-class-store obj)
;;	(or (dataseq-class-store obj)
;;	    (class-store (dataseq-data-class obj))))
;  ;; Ensure pagination widget is instantiated
;  (unless (dataseq-pagination-widget obj)
;    (setf (dataseq-pagination-widget obj)
;	  (make-instance 'pagination
;			 :on-change (lambda (&rest args)
;				      (declare (ignore args))
;				      (dataseq-clear-selection obj)
;				      (mark-dirty obj))
;			 :on-error (dataseq-flash obj)
;			 :show-total-items-p nil
;			 :total-items (dataseq-data-count obj)))))

(defmethod initialize-instance :around ((object standard-gridedit) &key direct-fields &allow-other-keys)
  (setf (dataseq-view object) (class-of object))
  (call-next-method))

(defmacro defgrid (name direct-superclasses direct-columns &key data-list dataform-class &allow-other-keys)
  `(let* ((columns (mapcar (lambda (column-definition-args)
			     (apply #'make-instance 'my-table-view-field (mapcar (lambda (el)
										   (if (symbolp el) el
										       (eval el)))
										 column-definition-args)))
			   (quote ,direct-columns)))
	  (data-store (make-private-store ,data-list))
	  (class-name (quote ,name))
	  (initargs (list :name class-name
			  :direct-superclasses (mapcar #'find-class (quote ,direct-superclasses))
			  :direct-fields columns
			  :dataform-class (quote ,dataform-class)
			  :direct-default-initargs (list (list :data-class t (lambda () t))
							 (list :class-store data-store (lambda () data-store)))))
	  (class (find-class class-name nil)))
     (if (not (null class))
	 (apply #'reinitialize-instance class initargs)
	 (setf (find-class class-name)
	       (apply #'make-instance 'gridedit-class initargs)))))

(defmethod dataedit-create-new-item-widget ((grid standard-gridedit))
  (make-instance (item-dataform-class-of (class-of grid))
		 :class-store (dataseq-class-store grid)
		 :ui-state :form
		 :on-cancel (lambda (obj)
			      (declare (ignore obj))
			      (dataedit-reset-state grid)
			      (throw 'annihilate-dataform nil))
		 :on-success (lambda (obj)
			       (safe-funcall (dataedit-on-add-item grid) grid (dataform-data obj))
			       (safe-funcall (dataedit-on-add-item-completed grid) grid (dataform-data obj))
			       (when (or (dataedit-mixin-flash-message-on-first-add-p grid)
					 (> (dataseq-data-count grid) 1))
				 (flash-message (dataseq-flash grid)
						(format nil "Added ~A."
							(humanize-name (class-name (class-of (dataform-data obj)))))))
			       (dataedit-reset-state grid)
			       (throw 'annihilate-dataform nil))))

(defmethod dataedit-create-drilldown-widget ((grid standard-gridedit) item)
  (make-instance (item-dataform-class-of (class-of grid))
		 :data item
		 :class-store (dataseq-class-store grid)
		 :ui-state (if (eql (gridedit-drilldown-type grid) :edit)
			       :form
			       :data)
		 :on-success (lambda (obj)
			       (declare (ignore obj))
			       (flash-message (dataseq-flash grid)
					      (format nil "Modified ~A."
						      (humanize-name  (class-name (class-of item)))))
			       (if (eql (gridedit-drilldown-type grid) :edit)
                                   (progn
                                     (dataedit-reset-state grid)
                                     (throw 'annihilate-dataform nil))
				   (mark-dirty grid)))
		 :on-cancel (when (eql (gridedit-drilldown-type grid) :edit)
			      (lambda (obj)
				(declare (ignore obj))
				(dataedit-reset-state grid)
                                (throw 'annihilate-dataform nil)))
		 :on-close (lambda (obj)
			     (declare (ignore obj))
			     (dataedit-reset-state grid))))

(defmethod with-table-view-body-row ((view table-view) obj (widget datagrid) &rest args
				     &key alternp &allow-other-keys)
  (if (and (dataseq-allow-drilldown-p widget)
	   (dataseq-on-drilldown widget))
      (let* ((store (dataseq-class-store widget))
	     (row-action (make-action
			 (lambda (&rest args)
			   (declare (ignore args))
			   (when (dataseq-autoset-drilled-down-item-p widget)
			     (setf (dataseq-drilled-down-item widget) obj))
			   (funcall (cdr (dataseq-on-drilldown widget)) widget obj))))
	    (drilled-down-p (and (dataseq-drilled-down-item widget)
				 (eql (private-id (dataseq-drilled-down-item widget) store)
				      (private-id obj store)))))
	(safe-apply (sequence-view-row-prefix-fn view) view obj args)
	(with-html
	  (:tr :class (when (or alternp drilled-down-p)
			(concatenate 'string
				     (when alternp "altern")
				     (when (and alternp drilled-down-p) " ")
				     (when drilled-down-p "drilled-down")))
	       :onclick (format nil "initiateActionOnEmptySelection(\"~A\", \"~A\");"
				row-action (session-name-string-pair))
	       :onmouseover "this.style.cursor = \"pointer\";"
	       :style "cursor: expression(\"hand\");"
	       (apply #'render-table-view-body-row view obj widget :row-action row-action args)))
	(safe-apply (sequence-view-row-suffix-fn view) view obj args))
      (call-next-method)))

(in-package :weblocks)

(defmethod render-view-field ((field datagrid-select-field) (view table-view)
			      (widget meta-dataform:standard-gridedit) presentation value obj &rest args)
  (declare (ignore args))
  (let ((checkbox-name (concatenate 'string
				    "item-" (attributize-name (meta-dataform:private-id obj (slot-value widget 'class-store)))))
	(drilldownp (and (dataseq-allow-drilldown-p widget)
			 (dataseq-on-drilldown widget))))
    (with-html
      (:td :class "select"
	   :onclick (when drilldownp "stopPropagation(event);")
	   :style (when drilldownp "cursor: default;")
	   (:div
	    (render-checkbox checkbox-name
			     (dataseq-item-selected-p widget (meta-dataform:private-id obj (slot-value widget 'class-store)))
			     :class nil))))))

