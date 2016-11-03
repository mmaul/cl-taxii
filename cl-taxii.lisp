;;;; cl-taxii.lisp
(defpackage #:cl-taxii
  (:use #:cl)
  (:use #:drakma)
  (:use #:<taxii_11)
  )

(in-package #:cl-taxii)


;;; "cl-taxii" goes here. Hacks and glory await!

(defclass taxii-feed ()
  ((url  :accessor taxii-feed-uri  :initarg :uri :initform "")
   (name :accessor taxii-feed-name :initarg :name :initform "")
   ))

(defun <taxii-feed> (name uri)
  (make-instance 'taxii-feed :name name :uri  (net.uri:parse-uri uri))
  )

(defmethod make-taxii-request ((feed taxii-feed) content &key certificate key stream)
  ""
  (http-request (taxii-feed-uri feed) :method :post :accept "application/xml"
                :content-type "application/xml" :content content
                :additional-headers `(("X-TAXII-Accept" . "urn:taxii.mitre.org:message:xml:1.1")
                                      ("X-TAXII-Content-Type" . "urn:taxii.mitre.org:message:xml:1.1")
                                      ("X-TAXII-Protocol" . ,(concatenate 'string  "urn:taxii.mitre.org:protocol:" (string-downcase (symbol-name (net.uri:uri-scheme (taxii-feed-uri feed)))) ":1.0"))
                                      ("X-TAXII-Services" . "urn:taxii.mitre.org:services:1.1"))
                :certificate certificate
                                      :key key
                                      :want-stream stream
                )
  
  )

(defun taxii-discovery-request () "<Discovery_Request xmlns=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\" message_id=\"1\"/>")




;; ----------------------------------- TEST STUFF

;; TAXII Servers
(defun haila-taxii ()
  (<taxii-feed> "Hailataxii" "http://hailataxii.com/taxii-discovery-service"))

(defun make-haila-taxii-request ()
  (flexi-streams:octets-to-string
   (make-taxii-request (haila-taxii) (taxii-discovery-request))
   :external-format :utf-8
   ))
(defun haila-taxii-request ()
  (load-object-from-stream
   (make-taxii-request (haila-taxii) (taxii-discovery-request) :stream t)
   
   ))

(defun dhs-taxii ()
  (<taxii-feed> "DHS" "https://taxii.dhs.gov:8443/flare/taxii11/discovery")
  )
(defun make-dhs-taxii-request ()
  (flexi-streams:octets-to-string
   (make-taxii-request (dhs-taxii) (taxii-discovery-request) :certificate "/opt/mm-taxii/server.crt" :key "/opt/mm-taxii/server.key")
   :external-format :utf-8)
  )


;(flexi-streams:octets-to-string (make-dhs-taxii-request) :external-format :utf-8)

;;;
(defun test-make-taxii-request ()
  (cxml:parse-octets (make-taxii-request (test-taxii) (taxii-discovery-request))  (cxml-dom:make-dom-builder)))
(defun test-make-taxii-xmls-request ()
  (cxml:parse-octets (make-taxii-request (test-taxii) (taxii-discovery-request)) (cxml-xmls:make-xmls-builder)  ))

(defun test-make-taxii-stp-request ()
  (cxml:parse-octets (make-taxii-request (test-taxii) (taxii-discovery-request)) (stp:make-builder)))

