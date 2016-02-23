#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "fortunes"
  :version "0"
  :description "Fortune file scripts"
  :license "GPL"
  :author "CoselTech"
  :class :package-inferred-system
  :depends-on (#:unix-opts
               "fortunes/manage"))
