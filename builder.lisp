(defpackage #:cl-taxii.builder
  (:use #:cl)
  (:use #:drakma)
  (:use #:cxml #:dom) 
  ;(:use #:xml-mop)
  (:use #:manardb)
  )

(in-package #:cl-taxii.builder)
;;; XML Class Definitions
(defparameter *processed* '())

;https://www.us-cert.gov/sites/default/files/STIX_Namespace/ISAMarkingsType.v2.xsd
(defun doittoit ()
  (cxml:parse #p"/home/mmaul/quicklisp/local-projects/cl-taxii/stix/stix_common.xsd" (cxml-xmls:make-xmls-builder))
  )

(defun doit () (cxml:parse #p"/home/mmaul/quicklisp/local-projects/cl-taxii/stix/stix_core.xsd" (cxml-dom:make-dom-builder)))

(defun process-xsd (path)
  (when (not (find path *processed* :test #'equalp))
    (setf *processed* (cons path *processed*))
    (print(buildm (cxml:parse path (cxml-dom:make-dom-builder)) :base-path (cl-fad:merge-pathnames-as-directory path))))
)
(defun process-dir (dir)
  (cl-fad:walk-directory dir #'process-xsd :test (lambda (x) (cl-ppcre:scan ".*\.xsd" (namestring x))))
  )


(defun build (p)
  (let ((cts (get-elements-by-tag-name p "xs:complexType")))
    (loop for e being the elements of cts
          do
             (progn
               (format t "(defclass ~a ()  ~%  (" (get-attribute e "name"))
               (loop for a being the elements of (get-elements-by-tag-name e "xs:attribute")
                  when a
                    do
                    (let ((v (get-attribute a "name")))
                        (format t "  (~a :accessor ~a :initform \"\" :initarg :~a :attribute (\"~a\"))~%" v v v v)))
               (format t "  )(:metaclass element-class))~%"))
         )
    ))

(defun make-attribute-slots (e)
  (loop for a being the elements of (get-elements-by-tag-name e "xs:attribute")
      when a
      collect
        (let ((v (get-attribute a "name")))
          `(,(intern  v) :accessor ,(intern  v) :initform nil :initarg ,(intern v "KEYWORD") :attribute (,v))
          )))

(defun make-element-slots (e)
  (loop for a being the elements of (get-elements-by-tag-name e "xs:element")
      when a
      collect
       (let ((v (get-attribute a "name")))
         `(,(intern  v) :accessor ,(intern  v) :initform nil 
                        :subelement (:element-type ,v :multiple t)
                                        ;(simple-text-element :alias ,v)
                     )
         )))

(defun get-extensions (e)
  (loop for a being the elements of (get-elements-by-tag-name e "xs:extension")
      when a
      collect (intern (get-attribute a "base"))
       )
  )

(defmacro buildm (p &key (base-path #p"."))
  `(let ((cts (get-elements-by-tag-name ,p "xs:complexType")))
     (concatenate 'list
                  (let ((bpth ,base-path))
                    (loop for i being the elements of  (get-elements-by-tag-name ,p "xs:import")
                          collect (import-xsd (get-attribute i "schemaLocation") :base-path bpth)
                       ))
                  (loop for e being the elements of cts
                     collect `(defclass  ,(intern  (get-attribute e "name")) ,(if (string= "" (get-attribute e "type")) (get-extensions e) (cons (get-attribute e "type") (get-extensions e)) )
                                ,(append (make-attribute-slots e) (make-element-slots e))
                                (:metaclass element-class))))
     )
  )


(defun import-xsd (schema-location &key (base-path #p"."))
  (handler-case
      (let (( pth (cl-fad:merge-pathnames-as-file base-path schema-location)))
        (when (not (find pth *processed* :test #'equalp))
          (setf *processed* (cons pth *processed*))
          (buildm (cxml:parse pth (cxml-dom:make-dom-builder)) :base-path (cl-fad:merge-pathnames-as-directory pth)
                )))
    (file-error (ex)
      (format t "error: ~a~%" ex))
    )
  )


