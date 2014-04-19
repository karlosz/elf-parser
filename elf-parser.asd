;;;; elf-parser.asd

(asdf:defsystem #:elf-parser
  :serial t
  :description "Parses ELF files"
  :license "GPL v3"
  :components ((:file "package")
               (:file "elf-parser")))

