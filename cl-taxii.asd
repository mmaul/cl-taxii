


(asdf:defsystem #:cl-taxii
  :description "Describe cl-taxii here"
  :author "Mike Maul <maul.mike@gmail>"
  :license "BSD"
  :serial t
  :depends-on ( :cxml :cxml-stp :drakma :flexi-streams :xmlisp)
  :components ( ;(:file "package")
               (:file "cl-taxii-xml")
               (:file "cl-taxii")))
