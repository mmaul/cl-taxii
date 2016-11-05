;;;; cl-taxii.lisp - TAXII Implementaion

(defpackage #:cl-taxii
  (:use #:cl)
  (:use #:drakma)
  (:use #:xml-mop)
  (:use #:manardb)
  )

(in-package #:cl-taxii)
;;; XML Class Definitions

(defclass simple-text-element () () (:metaclass element-class))

(defclass abstract-response ()
  ((xmlns-taxii_11 :accessor xmlns-taxii_11 :initform "" :initarg :xmlns-taxii_11 :attribute ("xmlns:taxii_11") )
   (xmlns-taxii :accessor xmlns-taxii :initform "" :initarg :xmlns-taxii :attribute ("xmlns:taxii") )
   (message_id :accessor message_id :initform "" :initarg :message_id :attribute ("message_id"))
   (in_response_to :accessor in_response_to :initform "" :initarg :in_response_to :attribute ("in_response_to"))
   (tdq :accessor tdq :initform "" :initarg :tdq :attribute ("xmlns:tdq")))
  
  (:metaclass element-class)
  (:documentation "Abstract Response")
  )

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
  ((service_type :accessor service-type :initform nil :initarg :service_type :attribute ("service_type"))
   (service_version :accessor service-version :initform nil :initarg :service_version :attribute ("service_version"))
   (available :accessor available :initform nil :initarg :available :attribute ("available"))
   (protocol-binding :accessor protocol-binding :initform "" 
                     :subelement (simple-text-element :alias "Protocol_Binding")
                     )
   (address :subelement (simple-text-element :alias "Address") :accessor address :initform "")
   (message-binding :subelement (simple-text-element :alias "Message_Binding") :accessor message-binding :initform "")
   (content-bindings :subelement (:element-type content-binding :multiple t) :accessor content-bindings :initform nil :initarg :content-bindings)
   (message :subelement (simple-text-element :alias "Message") :accessor message :initform "")
   )

  (:metaclass element-class)
  (:tags ("Service_Instance"))
  (:documentation "Service_Instance element in TAXII 1.1"))

(defclass abstract-service ()
  (
   (protocol-binding :accessor protocol-binding :initform "" 
                     :subelement (simple-text-element :alias "Protocol_Binding")
                     )
   (address :subelement (simple-text-element :alias "Address") :accessor address :initform "")
   (message-binding :subelement (simple-text-element :alias "Message_Binding") :accessor message-binding :initform "")
   (content-bindings :subelement (:element-type content-binding :multiple t) :accessor content-bindings :initform nil :initarg :content-bindings)
   (message :subelement (simple-text-element :alias "Message") :accessor message :initform "")
   )

  (:metaclass element-class)
  (:documentation "Abstract Service"))



#|
(defclass protocol-binding ()
  ()
  (:tags ("Protocol_Binding" :primary t))
  (:metaclass element-class))
|#
(defclass content-binding ()
  ((binding_id :accessor binding_id :initform nil :initarg :binding_id :attribute ("binding_id")))
  (:tags ("Content_Binding" ))
  (:metaclass element-class))


(defclass collection-information-response (abstract-response)
  (
   (collections :accessor collections :initform nil :initarg :service-instances
                      :subelement (:element-type collection  :multiple t)))

  (:metaclass element-class)
  (:tags ("Collection_Information_Response" :primary t))
  (:allowed-elements collection)
  (:documentation "Collection_Information_Response element in TAXII 1.1"))

(defmethod print-object ((c collection-information-response) stream)
  (let (( ci  (collections c)))
    (print-unreadable-object (c stream :type t)
      (format stream "(~{~S ~}... )" (subseq (mapcar 'collection_name ci) 0 3)))))

(defclass collection ()
  ((collection_name :accessor collection_name :initform nil :initarg :collection_name :attribute ("collection_name"))
   (collection_type :accessor collection_type :initform nil :initarg :collection_type :attribute ("collection_type"))
   (available :accessor available :initform nil :initarg :available :attribute ("available"))
   (description :accessor description :initform "" :subelement (simple-text-element :alias "Description"))
   (content-bindings :subelement (:element-type content-binding :multiple t) :accessor content-bindings :initform nil :initarg :content-bindings)
   (push-methods :subelement (:element-type push-method :multiple t) :accessor push-methods :initform nil :initarg :push-methodss)
   (polling-services :subelement (:element-type polling-service :multiple t) :accessor polling-servicess :initform nil :initarg :polling-servicess)
   (subscription-services :subelement (:element-type subscription-service :multiple t) :accessor subscription-services :initform nil :initarg :subscription-servicess)
   (receiving-inbox-services :subelement (:element-type receiving-inbox-service :multiple t) :accessor receiving-inbox-services :initform nil :initarg :receiving-inbox-services)
   )

  (:metaclass element-class)
  (:tags ("Collection"))
  (:documentation "Collection element in TAXII 1.1"))

(defclass push-method (abstract-service)
  ()
  (:metaclass element-class)
  (:tags ("Push_Method" :primary t))
  (:documentation "Celement in TAXII 1.1"))

(defclass polling-service (abstract-service)
  ()
  (:metaclass element-class)
  (:tags ("Polling_Service" :primary t))
  (:documentation "Celement in TAXII 1.1"))

(defclass subscription-service (abstract-service)
  ()
  (:metaclass element-class)
  (:tags ("Subscription_Service" :primary t))
  (:documentation "Celement in TAXII 1.1"))
(defclass receiving-inbox-service (abstract-service)
  ()
  (:metaclass element-class)
  (:tags ("Receiving_Inbox_Service" :primary t))
  (:documentation "Celement in TAXII 1.1"))

(defun find-service-instance (si name)
  (find-if (lambda (x) (string= name  (service-type x))) (service-instances si)))

;;; TAXII Implementation

(defmmclass taxii-feed ()
  ((url  :accessor taxii-feed-uri  :initarg :uri :initform "")
   (name :accessor taxii-feed-name :initarg :name :initform "")
   (certificate :accessor taxii-feed-certificate :initarg :certificate  :initform nil)
   (key :accessor taxii-feed-key :initarg :key :initform nil)
   (user :accessor taxii-user :initarg :user :initform nil)
   (password :accessor taxii-password :initarg :password :initform nil)
   (message_id :accessor taxii-feed-message_id :initarg :message_id :initform 0)
   (subscription_id :accessor taxii-feed-subscription_id :initarg :subscription_id :initform "")
   (discovery :accessor taxii-discovery :initarg :discovery :initform nil)
   (collections :accessor taxii-collections :initarg :collections :initform nil)
   )
  (:documentation "")
  )

(defmethod print-object ((c taxii-feed) stream)
  (print-unreadable-object (c stream :type t) (format stream "(~S)" (taxii-feed-name c ))
    ))

(defmethod new-message_id! ((feed taxii-feed)) (setf (taxii-feed-message_id feed) (+ (taxii-feed-message_id feed) 1)) )

(defmethod collection-uri ((feed taxii-feed)) (ELEMENT-TEXT (address (find-service-instance (taxii-discovery feed) "COLLECTION_MANAGEMENT" ))))

(defmethod poll-uri ((feed taxii-feed)) (ELEMENT-TEXT (address (find-service-instance (taxii-discovery feed) "POLL" ))))

(defun <taxii-feed> (name uri &key certificate key subscription_id)
  (make-instance 'taxii-feed :name name :uri  (net.uri:parse-uri uri)
                 :certificate certificate :key key :subscription_id subscription_id)
  )
(defmethod discovery! ((feed taxii-feed) )
  (setf (taxii-discovery feed) (car
                                (parse-xml-stream (make-taxii-request feed (taxii-feed-uri feed) (taxii-discovery-request feed) :stream t)
                                                  (list (find-class 'discovery-response)))))
  )

(defmethod <collection-information-response> ((feed taxii-feed))
  (car (parse-xml-stream (make-taxii-request feed (collection-uri feed)
                                             (taxii-collection-information-request feed) :stream t)
                         (list (find-class 'collection-information-response))
                         ))
  )

(defmethod collections! ((feed taxii-feed) )
  "Set collections slot with result of <collection-information-response>"
  (setf (taxii-discovery feed) (<collection-information-response> feed)))

(defmethod make-taxii-request ((feed taxii-feed) url content &key stream)
  ""
  (http-request url :method :post :accept "application/xml"
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

;;; TAXII Request Builders

(defun taxii-discovery-request (feed) (format nil  "<Discovery_Request xmlns=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\" message_id=\"~a\"/>" (new-message_id! feed)))

(defun taxii-poll-request (feed feed-name) (format nil
"<taxii_11:Poll_Request xmlns:taxii_11=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\" message_id=\"~a\" collection_name=\"~a\">

 
</taxii_11:Poll_Request>" (new-message_id! feed) feed-name (taxii-feed-subscription_id feed) 
))
;(taxii-feed-subscription_id feed)urn:uuid:

(defun taxii-poll-request (feed feed-name) (format nil  "<taxii_11:Poll_Request
xmlns:taxii_11=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\" message_id=\"~a\" collection_name=\"~a\">
<taxii_11:Exclusive_Begin_Timestamp>2016-10-19T00:00:00Z</taxii_11:Exclusive_Begin_Timestamp>
    <taxii_11:Inclusive_End_Timestamp>2016-11-1T12:00:00Z</taxii_11:Inclusive_End_Timestamp>
    <taxii_11:Poll_Parameters allow_asynch=\"false\">
    <taxii_11:Response_Type>FULL</taxii_11:Response_Type>
    </taxii_11:Poll_Parameters>
</taxii_11:Poll_Request>" (new-message_id! feed) feed-name ))
#| <taxii_11:Poll_Request 
    xmlns:taxii_11="http://taxii.mitre.org/messages/taxii_xml_binding-1.1"
    message_id="42158"
    collection_name="default">
    <taxii_11:Exclusive_Begin_Timestamp>2014-12-19T00:00:00Z</taxii_11:Exclusive_Begin_Timestamp>
    <taxii_11:Inclusive_End_Timestamp>2014-12-19T12:00:00Z</taxii_11:Inclusive_End_Timestamp>
    <taxii_11:Poll_Parameters allow_asynch="false">
        <taxii_11:Response_Type>FULL</taxii_11:Response_Type>
    </taxii_11:Poll_Parameters>
</taxii_11:Poll_Request> |#

(defun taxii-collection-information-request (feed) (format nil "<taxii_11:Collection_Information_Request xmlns:taxii_11=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\" message_id=\"~a\"/>" (new-message_id! feed)))


(defun taxii-collection-information-request1 (feed) (format nil "<taxii_11:Collection_Information_Request xmlns:taxii_11=\"http://taxii.mitre.org/messages/taxii_xml_binding-1.1\" message_id=\"~a\" subscription_id=\"~a\"/>" (new-message_id! feed) (taxii-feed-subscription_id feed))) 


;;; TEST STUFF

;;; TAXII Servers

(defun haila-taxii ()
  (<taxii-feed> "Hailataxii" "http://hailataxii.com/taxii-discovery-service"))

( manardb:use-mmap-dir  "/tmp/" )
(defparameter haila (<taxii-feed> "Hailataxii" "http://hailataxii.com/taxii-discovery-service"))
