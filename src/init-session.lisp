(in-package :meta-dataform)

(defclass test ()
  ((s1 :initarg :s1 :accessor s1-of)
   (s2 :initarg :s2 :accessor s2-of)))

(deform test-dataform (test)
  (s1)
  (lambda (&key s1)
    (make-instance 'test :s1 s1)))

(deform test-dataform2 (test-dataform)
  (s2)
  (lambda (&key s1 s2)
    (make-instance 'test :s1 s1 :s2 s2)))

(defgrid my-gridedit ()
  ((:label "Колонка1" :slot-name s1))
  :dataform-class test-dataform)

(defgrid my-gridedit2 (my-gridedit)
  ((:label "Колонка2" :slot-name s2))
  :data-list (list (make-instance 'test :s1 1 :s2 2))
  :dataform-class test-dataform2)

;; 1. Дополнить логику добавления объекта к хранилищу которое показывает таблица
;; 2. ПОдумать насчет отвязывания класс аобъекта от gridedit, в сущности он там не нужен, он нужен только чтобы создавать объект, для всего остального имя класса обекта не имеет значения

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (composite-widgets root)
	(list (make-instance 'my-gridedit2))))