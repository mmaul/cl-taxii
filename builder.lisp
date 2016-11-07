(defpackage #:cl-taxii.builder
  (:use #:cl)
  (:use #:drakma)
  (:use #:cxml)
  
  )

(in-package #:cl-taxii.builder)
;;; XML Class Definitions


(defun doittoit ()
  (cxml:parse #p"/home/mmaul/quicklisp/local-projects/cl-taxii/stix/stix_common.xsd" (cxml-xmls:make-xmls-builder))
  )

(defun doit () (cxml:parse #p"/home/mmaul/quicklisp/local-projects/cl-taxii/stix/stix_common.xsd" (cxml-dom:make-dom-builder)))
(defclass node ()
  ((name :accessor node-name :initarg :name :initform nil)
   (ns :accessor node-ns :initarg :ns :initform nil)
   (attributes :accessor node-attributes :initarg :attributes :initform nil)
   (next :accessor node-next :initarg :next :initform nil)
   (offset :accessor node-offset :initarg :offset :initform 0)
   )
  )

(defun <node> (name-ns attributes next offset)
  (let ((name (car name-ns))
        (ns (cdr name-ns)))
  (make-instance 'node :name name :ns ns :attributes attributes :next next :offset offset)
  ))

(defmethod print-object ((c node) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "(~a... )" (node-name c))
    )
  )

(defmethod get-next-node ((node n ) doc)
  (subseq o 0 3))

(defun get-first-node ( doc)
  (apply '<node> (append (subseq doc 0 3) '(0)))
  )


(defmethod get-next-node ((n node) doc)
  (let ((no (+ (node-offset n) 3)))
    (apply '<node> (append (subseq doc no (+ no 3)) (list no)))
    ))

(defun build (p)
  (let ((cts (get-elements-by-tag-name p "xs:complexType"))

        )
    (loop for e being the elements of cts
          do
             (progn
               (format t "(defclass ~a ()~%  " (get-attribute e "name"))
               (let ((a (get-elements-by-tag-name e "attribute")))
                 (format t "(~a :accessor ~a :initform \"\" :initarg :~a :attribute (~a))" a a a a))
               (format t "~%"))
          )
    ))
