;;;; cl-taxii.lisp
(defpackage #:cl-taxii
  (:use #:cl)
  (:use #:drakma)
                                        ;(:use #:<taxii_11)
  (:use #:xml-mop)
  )

(in-package #:cl-taxii)
;;; XML is stuff



(defclass discovery-response ()
  ((xmlns-taxii_11 :accessor xmlns-taxii_11 :initform "" :initarg :xmlns-taxii_11 :attribute ("xmlns:taxii_11") )
   (xmlns-taxii :accessor xmlns-taxii :initform "" :initarg :xmlns-taxii :attribute ("xmlns:taxii") )
   (message_id :accessor message_id :initform "" :initarg :message_id :attribute ("message_id"))
   (in_response_to :accessor in_response_to :initform "" :initarg :in_response_to :attribute ("in_response_to"))
   (tdq :accessor tdq :initform "" :initarg :tdq :attribute ("xmlns:tdq"))
   (service-instances :accessor service-instances :initform nil :initarg :service-instances
                      :subelement (:element-type service-instance  :multiple t)))
  (:metaclass element-class)
  (:tags ("Discovery_Response" :primary t))
  (:allowed-elements service-instance)
  (:documentation "Discover_Response element in TAXII 1.1"))

(defclass service-instance ()
  ((service_type :accessor service_type :initform nil :initarg :service_type :attribute ("service_type"))
   (service_version :accessor service_version :initform nil :initarg :service_version :attribute ("service_version"))
   (available :accessor available :initform nil :initarg :available :attribute ("available")))
  (:metaclass element-class)
  (:tags ("Service_Instance"))
  (:documentation "Service_Instance element in TAXII 1.1"))

  (defun test-xml-discovery ()
    (with-open-file (stream "/home/mmaul/quicklisp/local-projects/cl-taxii/discovery-response-med.xml")
      (parse-xml-stream stream (list (find-class 'discovery-response)))))

;;; "cl-taxii" goes here. Hacks and glory await!

(defclass taxii-feed ()
  ((url  :accessor taxii-feed-uri  :initarg :uri :initform "")
   (name :accessor taxii-feed-name :initarg :name :initform "")
   (certificate :accessor taxii-feed-certificate :initarg :certificate  :initform nil)
   (key :accessor taxii-feed-key :initarg :key :initform nil)
   (message_id :accessor taxii-feed-message_id :initarg :message_id :initform 0)
   ))

(defmethod new-message_id (feed) (setf (taxii-feed-message_id feed) (+ (taxii-feed-message_id feed) 1)) )

(defun <taxii-feed> (name uri &key certificate key)
  (make-instance 'taxii-feed :name name :uri  (net.uri:parse-uri uri)
                 :certificate certificate :key key)
  )

(defmethod make-taxii-request ((feed taxii-feed) content &key stream)
  ""
  (http-request (taxii-feed-uri feed) :method :post :accept "application/xml"
                :content-type "application/xml" :content content
                :additional-headers `(("X-TAXII-Accept" . "urn:taxii.mitre.org:message:xml:1.1")
                                      ("X-TAXII-Content-Type" . "urn:taxii.mitre.org:message:xml:1.1")
                                      ("X-TAXII-Protocol" . ,(concatenate 'string  "urn:taxii.mitre.org:protocol:" (string-downcase (symbol-name (net.uri:uri-scheme (taxii-feed-uri feed)))) ":1.0"))
                                      ("X-TAXII-Services" . "urn:taxii.mitre.org:services:1.1"))
                :certificate (taxii-feed-certificate feed) 
                                      :key (taxii-feed-key feed) 
                                      :want-stream stream
                )
  
  )

(defun taxii-discovery-request (feed) (format nil  "<Discovery_Request xmlns=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\" message_id=\"~a\"/>" (new-message_id feed)))

(defun taxii-collection-information-request (feed) (format nil "<taxii_11:Collection_Information_Request xmlns:taxii_11=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\"message_id=\"~a\"/>" (new-message_id feed))) 


;; ----------------------------------- TEST STUFF

;; TAXII Servers
(defun haila-taxii ()
  (<taxii-feed> "Hailataxii" "http://hailataxii.com/taxii-discovery-service"))

(defmethod make-taxii-discovery-request ((feed taxii-feed))
  (flexi-streams:octets-to-string
   (make-taxii-request feed (taxii-discovery-request feed))
   :external-format :utf-8
   ))

(defun make-taxii-collection-information-request (site)
  (flexi-streams:octets-to-string
   (make-taxii-request site (taxii-collection-information-request site))
   :external-format :utf-8
   ))


(defun make-taxii-discovery-request-obj (feed)
  (load-object-from-stream
   (make-taxii-request feed (taxii-discovery-request feed) :stream t)
   
   ))

(defun dhs-taxii ()
  (<taxii-feed> "DHS" "https://taxii.dhs.gov:8443/flare/taxii11/discovery"
                :certificate "/opt/mm-taxii/server.crt" :key "/opt/mm-taxii/server.key")
  )
(defun make-dhs-taxii-discovery-request ()
  (flexi-streams:octets-to-string
   (make-taxii-request (dhs-taxii) (taxii-discovery-request) )
   :external-format :utf-8)
  )

(defun dhs-taxii-discovery-request ()
  (load-object-from-stream
   (make-taxii-request (dhs-taxii) (taxii-discovery-request) )
   
   ))
(defun get-dhs-taxii (fn)
  (with-open-file (s fn :direction :output :if-exists :supersede)
    (format s "~a" (make-dhs-taxii-request))
    ))
                                        ;(flexi-streams:octets-to-string (make-dhs-taxii-request) :external-format :utf-8)
#|
(defun load-dhs-taxii ()
  (<taxii_11::load-object "/tmp/dhs.xml")
  (get-dhs-taxii "/tmp/dhs.xml"))
;;;
(defun test-make-taxii-request ()
  (cxml:parse-octets (make-taxii-request (test-taxii) (taxii-discovery-request))  (cxml-dom:make-dom-builder)))
(defun test-make-taxii-xmls-request ()
  (cxml:parse-octets (make-taxii-request (test-taxii) (taxii-discovery-request)) (cxml-xmls:make-xmls-builder)  ))

(defun test-make-taxii-stp-request ()
  (cxml:parse-octets (make-taxii-request (test-taxii) (taxii-discovery-request)) (stp:make-builder)))

|#
