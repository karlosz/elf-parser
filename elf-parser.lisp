;;;; elf-parser.lisp

(in-package #:elf-parser)

(defun elf-p (filespec)
  "Determines if file has an ELF header."
  (with-open-file (input filespec
			 :element-type '(unsigned-byte 8))
    (= (read-ubytes 4 input) #x7f454c46)))

(defclass elf-header ()
  ((EI_MAG0 :initarg :EI_MAG0 :accessor magic-number
	    :documentation "Magic number")
   (EI_CLASS :initarg :EI_CLASS :accessor elf-class
	     :documentation "1/2 signals 32/64 bit format.")
   (EI_DATA :initarg :EI_DATA :accessor endian
	    :documentation "1/2 signals little/big endianness,")
   (EI_VERSION :initarg :EI_VERSION :accessor elf-version
	       :documentation "Version of ELF.")
   (EI_OSABI :initarg :EI_OSABI :accessor os-abi
	     :documentation "Operating System ABI
                             0x00 System V
                             0x01 HP-UX
                             0x02 NetBSD
                             0x03 Linux
                             0x06 Solaris
                             0x07 AIX
                             0x08 IRIX
                             0x09 FreeBSD
                             0x0c OpenBSD")
   (EI_ABIVERSION :initarg :EI_ABIVERSION :accessor abi-version
		  :documentation "ABI Version")
   (EI_PAD :initarg :EI_PAD :accessor padding
	   :documentation "Unused")
   (e_type :initarg :e_type :accessor exec-type
	   :documentation "1 - relocatable
                           2 - executable
                           3 - shared
                           4 - core")
   (e_machine :initarg :e_machine :accessor machine
	      :documentation "Machine architecture
                              0x02 SPARC
                              0x03 x86
                              0x08 MIPS
                              0x14 PowerPC
                              0x28 ARM
                              0x32 IA-64
                              0x3E x86-64
                              0xB7 AArch64")
   (e_version :initarg :e_version :accessor another-elf-version
	      :documentation "Version of ELF; again")
   (e_entry :initarg :e_entry :accessor entry
	    :documentation "Entry point address of process.")
   (e_phoff :initarg :e_phoff :accessor header-table
	    :documentation "Header table location of file.")
   (e_shoff :initarg :e_shoff :accessor section-table
	    :documentation "Section table location of file.")
   (e_flags :initarg :e_flags :accessor flags
	    :documentation "Depends on target architecture.")
   (e_ehsize :initarg :e_ehsize :accessor elf-header-size
	     :documentation "Size of elf header
                             52 32 bits
                             64 64 bits")
   (e_phentsize :initarg :e_phentsize :accessor program-header-size
		:documentation "Size of program header.")
   (e_phnum :initarg :e_phnum :accessor program-header-count
	    :documentation "Number of entries in program header table")
   (e_shentsize :initarg :e_shentsize :accessor section-header-size
		:documentation "Size of section header.")
   (e_shnum :initarg :e_shnum :accessor section-header-count
	    :documentation "Number of entries in section header table.")
   (e_shstrndx :initarg :e_shstrndx :accessor section_name_index
	       :documentation "Contains index of section names in
                               section header tables.")))

(defun read-elf-header (in)
  (let ((header (make-instance 'elf-header)) endianness bits)
    (with-slots (EI_MAG0 EI_CLASS EI_DATA EI_VERSION EI_OSABI
			 EI_ABIVERSION EI_PAD e_type e_machine
			 e_version e_entry e_phoff e_shoff
			 e_flags e_ehsize e_phentsize e_phnum
			 e_shentsize e_shnum e_shstrndx) header
      (setf EI_MAG0 (read-ubytes 4 in))
      (setf EI_CLASS (read-ubytes 1 in))
      (setf bits (= EI_CLASS 1))
      (setf EI_DATA (read-ubytes 1 in))
      (setf endianness (= EI_DATA 1))
      (setf EI_VERSION (read-ubytes 1 in))
      (setf EI_OSABI (read-ubytes 1 in))
      (setf EI_ABIVERSION (read-ubytes 1 in))
      (setf EI_PAD (read-ubytes 7 in))
      (setf e_type (read-ubytes 2 in endianness))
      (setf e_machine (read-ubytes 2 in endianness))
      (setf e_version (read-ubytes 4 in endianness))
      (setf e_entry (if bits
			(read-ubytes 4 in endianness)
			(read-ubytes 8 in endianness)))
      (setf e_phoff (if bits
			(read-ubytes 4 in endianness)
			(read-ubytes 8 in endianness)))
      (setf e_shoff (if bits
			(read-ubytes 4 in endianness)
			(read-ubytes 8 in endianness)))
      (setf e_flags (read-ubytes 4 in endianness))
      (setf e_ehsize (read-ubytes 2 in endianness))
      (setf e_phentsize (read-ubytes 2 in endianness))
      (setf e_phnum (read-ubytes 2 in endianness))
      (setf e_shentsize (read-ubytes 2 in endianness))
      (setf e_shnum (read-ubytes 2 in endianness))
      (setf e_shstrndx (read-ubytes 2 in endianness)))
    header))

(defun read-ubytes (bytes in &optional little-endian)
  (if little-endian
      (loop with value = 0
	 for low-bit from 0 to (* 8 (1- bytes)) by 8 do
	   (setf (ldb (byte 8 low-bit) value) (read-byte in))
	   finally (return value))
      (loop with value = 0
	 for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
	   (setf (ldb (byte 8 low-bit) value) (read-byte in))
	 finally (return value))))

(defun display-elf-header (filespec)
  (with-open-file (in filespec :element-type '(unsigned-byte 8))
    (let ((header (read-elf-header in)))
      (format t "Magic: 0x~X~%" (magic-number header))
      (format t "Class: 0x~X~%" (elf-class header))
      (format t "Endianness: ~A~%" (endian header))
      (format t "Version: ~A~%" (elf-version header))
      (format t "OS ABI: ~A~%" (os-abi header))
      (format t "ABI Version: 0x~X~%" (abi-version header))
      (format t "Padding: 0x~X~%" (padding header))
      (format t "Type: 0x~X~%" (exec-type header))
      (format t "Machine: 0x~X~%" (machine header))
      (format t "Version: 0x~X~%" (another-elf-version header))
      (format t "Entry Point Address: 0x~X~%" (entry header))
      (format t "Start of Program Headers: ~A (bytes into file)~%"
	      (header-table header))
      (format t "Start of Section Headers: ~A (bytes into file)~%"
	      (section-table header))
      (format t "Flags: 0x~X~%" (flags header))
      (format t "Size of ELF Header: ~A (bytes)~%"
	      (elf-header-size header))
      (format t "Size of Program Headers: ~A (bytes)~%"
	      (program-header-size header))
      (format t "Number of Program Headers: ~A~%"
	      (program-header-count header))
      (format t "Size of section headers: ~A (bytes)~%"
	      (section-header-size header))
      (format t "Number of section headers: ~A~%"
	      (section-header-count header))
      (format t "Section header string table index: ~A~%"
	      (section_name_index header)))))
