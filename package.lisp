;;;; package.lisp

(defpackage #:elf-parser
  (:use #:cl)
  (:export :elf-p
	   :elf-header
	   :read-elf-header
	   :display-elf-header))

