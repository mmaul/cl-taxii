(setq xmlisp:*xmlisp-packages* (list (find-package :cl-user) (find-package :<taxii_11)))

(defpackage #:<taxii_11
  (:use #:cl)
  (:use #:xmlisp)
  (:export :discovery_response :protocol_binding :LOAD-OBJECT-FROM-STREAM :LOAD-OBJECT-FROM-STREAM)
  (:nicknames :xmlns :taxii_11)
  )

(in-package #:<taxii_11)


(defun LOAD-OBJECT-FROM-STREAM (stream &key Verbose (Package *Package*)) "
  in: stream
    &key Verbose boolean;
         package package default *Package*.
 out: Object t.
 Load XML object contained in <Filename> into package and return it."
  (when Verbose (format t ";; loading object in file: ~A~%" Filename))
  (let ((*Package* Package))
    (let ((*Xml-Stream* stream))
      (declare (special *Xml-Stream*))
      (let ((next nil)
            (output nil))
        (loop
          (setf output (if (typep next (find-class 'sgml-tag))
                           output
                           next))
          (setf next (read stream nil :eof))
          (when (eq next :eof) (return output)))))
    ))
(defun LOAD-OBJECT-FROM-STRING (str &key Verbose (Package *Package*)) "
  in: str
    &key Verbose boolean;
         package package default *Package*.
 out: Object t.
 Load XML object contained in <Filename> into package and return it."
  (when Verbose (format t ";; loading object in file: ~A~%" Filename))
  (let ((*Package* Package))
    (let ((next nil)
            (output nil))
        (loop
          (setf output (if (typep next (find-class 'sgml-tag))
                           output
                           next))
          (setf next (read-from-string str))
           (when (eq next :eof) (return output))))
    
    ))

(defclass Response (xml-serializer)
  ((xmlns :accessor name :initform "" :initarg :name)
   (comments :accessor comments :initform nil)
   (taxii_11 :accessor taxii_11 :initform nil)
   (taxii :accessor taxii :initform nil)
   (tdq :accessor tdq :initform nil)
   )
  )

(defclass Discovery_Response (Response)
  ((message_id :accessor message_id :initform nil)
   (in_response_to :accessor in_response_to :initform nil)
   ;; subelements
   (service_instances :accessor service_instances :initform nil)
   ))

(defclass Service_Instance (Response)
  ((service_type :accessor service_type :initform nil)
   (service_version :accessor service_version :initform nil)
   (available :accessor available :initform nil)
   ;;subelements
   (protocol_binding :accessor protocol_binding :initform nil)
   (address :accessor address :initform nil)
   (message_binding :accessor message_binding :initform nil)
   (message :accessor message :initform nil)
   (content_binding :accessor content_binding :initform nil)
   ))
(defclass Service_Binding_Elements (xml-serializer) ())
(defclass Protocol_Binding (Service_Binding_Elements)
  ())
(defclass Address (Service_Binding_Elements)
  ())
(defclass Message_Binding (Service_Binding_Elements)
  ())
(defclass Message (Service_Binding_Elements)
  ())
(defclass Content_Binding (Service_Binding_Elements)
  ((binding_id :accessor binding_id :initform nil)))
