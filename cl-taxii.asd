


(asdf:defsystem #:cl-taxii
  :description "Describe cl-taxii here"
  :author "Mike Maul <maul.mike@gmail>"
  :license "BSD"
  :serial t
  ;:depends-on ( :cxml :cxml-stp :drakma :flexi-streams :manardb)
  :depends-on ( :drakma :flexi-streams :xml-mop :manardb :cxml)
  :components ( ;(:file "package")
               (:file "builder")
               (:file "cl-taxii")))

