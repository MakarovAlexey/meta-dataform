(in-package meta-dataform)

(defclass private-store ()
  ((next-id :initform -1)
   (objects :initform nil :initarg :data-list :accessor objects-of)))

(defun next-id (store)
  (incf (slot-value store 'next-id)))

(defmethod open-store ((store-type (eql :private)) &rest args)
  (declare (ignore store-type args))
  (error "private-store является внутренним и непредназначен для исопльзования как *default-store*"))

(defmethod close-store ((store private-store))
  (declare (ignore store))
  (error "private-store является внутренним и непредназначен для исопльзования как *default-store*"))

(defmethod delete-persistent-object ((store private-store) object)
  (let ((objects (objects-of store)))
    (delete object objects :key #'cdr)))

(defmethod delete-persistent-object-by-id ((store private-store) class-name object-id)
  ;(declare (ignore class-name))
  (let ((object (find-persistent-object-by-id store class-name object-id)))
    (delete-persistent-object store object)))

(defmethod find-persistent-object-by-id ((store private-store) class-name object-id)
  (declare (ignore class-name))
  (cdr (assoc object-id (objects-of store))))

(defgeneric filter-objects (objects filter)
  (:method (objects (filter (eql nil))) objects)
  (:method (objects (filter function))
    (remove-if-not filter objects)))

(defmethod find-persistent-objects ((store private-store) class-name &key filter order-by range)
  (declare (ignore class-name))
  (range-objects-in-memory
   (order-objects-in-memory 
    (filter-objects (mapcar #'cdr (objects-of store)) filter)
    order-by)
   range))

(defmethod count-persistent-objects ((store private-store) class-name &key &allow-other-keys)
  (declare (ignore class-name))
  (length (objects-of store)))

(defun make-private-store (&optional (data-list nil))
  (make-instance 'private-store
		 :data-list (mapcar (lambda (object)
				      (cons (position object data-list) object))
				    data-list)))

(defmethod persist-object ((store private-store) object &key)
  (push (cons (next-id store) object) (objects-of store)))

(defun private-id (object store)
  (car (rassoc object (objects-of store))))

(export '(private-id))