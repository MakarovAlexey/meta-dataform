(in-package :meta-dataform)

(defclass test ()
  ((s1 :initarg :s1 :accessor s1-of)
   (s2 :initarg :s2 :accessor s2-of)))

(defun make-test (&key s1 s2)
  (make-instance 'test :s1 s1 :s2 s2))

(deform test-dataform (test)
  ((s1 :reader #'s1-of
       :writer #'(lambda (value obj)
		   (setf (s1-of obj) value))))
  #'(lambda (&key s1)
      (make-instance 'test :s1 s1)))

(deform test-dataform2 (test-dataform)
  ((s2 :reader #'s2-of
       :writer #'(lambda (value obj)
		   (setf (slot-value obj 's2) value))))
  #'make-test)

(defgrid my-gridedit ()
  ((:label "Колонка1" :slot-name s1))
  :dataform-class test-dataform)

(defgrid my-gridedit2 (my-gridedit)
  ((:label "Колонка2" :slot-name s2))
  :data-list (list (make-instance 'test :s1 1 :s2 2))
  :dataform-class test-dataform2)

(deform list-dataform ()
  ((first :reader #'first
	  :writer #'(lambda (list value)
		       (setf (first list) value)))
   (second :reader #'second
	   :writer #'(lambda (list value)
		       (setf (second list) value))))
  #'(lambda (&key first second)
      (list first second)))

(defgrid my-gridedit3 ()
  ((:label "Колонка1"
	   :reader #'(lambda (list)
		       (first  list))
	   :slot-name d2)
   (:label "Колонка2" :reader #'(lambda (list)
				  (second list))
	   :slot-name d4))
  :data-list (list (list 222 333) (list 111 444))
  :dataform-class list-dataform)

;; 1. Дополнить логику добавления объекта к хранилищу которое показывает таблица
;; 2. ПОдумать насчет отвязывания класс аобъекта от gridedit, в сущности он там не нужен, он нужен только чтобы создавать объект, для всего остального имя класса обекта не имеет значения

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (composite-widgets root)
	(list (make-instance 'my-gridedit3))))