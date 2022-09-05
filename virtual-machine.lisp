;;;; virtual-machine.lisp

(in-package #:virtual-machine)

(defun remove-comment (str)
  "Remove comment and trailing spaces."
  (let ((idx (search "//" str)))
    (if idx
	(string-trim " " (subseq str 0 idx))
	(string-trim " " str))))

(defun get-instructions (file)
  (let ((lines (uiop:read-file-lines file)))
    (mapcar #'remove-comment
	    (remove-if
	     #'(lambda (str) (or (string= "" str) (string= (elt str 0) "/")))
	     lines))))

(defun command-type (command)
  (let ((first-word (first (split-sequence:split-sequence #\space command :test 'string=))))
    (case (intern first-word :keyword)
      (:|push| 'c-push)
      (:|pop| 'c-pop)
      (:|label| 'c-label)
      (:|goto| 'c-goto)
      (:|if-goto| 'c-if)
      (:|function| 'c-function)
      (:|return| 'c-return)
      (:|call| 'c-call)
      (otherwise 'c-arithmetic))))

(defun get-args (command)
  (let ((args (rest (split-sequence:split-sequence #\space command :test 'string=))))
    (if (= 1 (length args))
	(values (first args) nil)
	(values (first args) (second args)))))

(defun translate (command filename)
  (let ((command-type (command-type command)))
    (cond ((eql command-type 'c-push)
	   (write-push command filename))
	  ((eql command-type 'c-pop)
	   (write-pop command filename))
	  ((eql command-type 'c-arithmetic)
	   (arithmetic-logical-op command))
	  ((eql command-type 'c-label)
	   (write-label command))
	  ((eql command-type 'c-goto)
	   (write-goto command))
	  ((eq command-type 'c-if)
	   (write-if command))
	  ((eq command-type 'c-function)
	   (write-function command))
	  ((eq command-type 'c-return)
	   (write-return command))
	  ((eq command-type 'c-call)
	   (write-call command))
	  (t (error "Cannot process it for now.")))))

(defun arithmetic-logical-op (command)
  (cond ((string= "add" command)
	 (list "@SP" "M=M-1" "@SP" "A=M" "D=M" "@SP" "AM=M-1" "M=M+D" "@SP" "M=M+1"))
	((string= "sub" command)
	 (list "@SP" "M=M-1" "@SP" "A=M" "D=M" "@SP" "AM=M-1" "M=M-D" "@SP" "M=M+1"))
	((string= "neg" command)
	 (list "@SP" "M=M-1" "@SP" "A=M" "M=-M" "@SP" "M=M+1"))
	((string= "eq" command)
	 (let* ((eq (string (gensym "EQ")))
		(end (string (gensym "END")))
		(@eq (concatenate 'string "@" eq))
		(label-eq (concatenate 'string "(" eq ")"))
		(@end (concatenate 'string "@" end))
		(label-end (concatenate 'string "(" end ")")))
	   (list "@SP" "M=M-1" "@SP" "A=M" "D=M" "@SP" "AM=M-1" "D=M-D" @eq
		 "D;JEQ" "@0" "D=A" "@SP" "A=M" "M=D" @end "0;JMP" label-eq
		 "@1" "D=-A" "@SP" "A=M" "M=D" label-end "@SP" "M=M+1")))
	((string= "gt" command)
	 (let* ((gt (string (gensym "GT")))
		(end (string (gensym "END")))
		(@gt (concatenate 'string "@" gt))
		(label-gt (concatenate 'string "(" gt ")"))
		(@end (concatenate 'string "@" end))
		(label-end (concatenate 'string "(" end ")")))
	   (list "@SP" "M=M-1" "@SP" "A=M" "D=M" "@SP" "AM=M-1" "D=M-D" @gt
		 "D;JGT" "@0" "D=A" "@SP" "A=M" "M=D" @end "0;JMP" label-gt
		 "@1" "D=-A" "@SP" "A=M" "M=D" label-end "@SP" "M=M+1")))
	((string= "lt" command)
	 (let* ((lt (string (gensym "LT")))
		(end (string (gensym "END")))
		(@lt (concatenate 'string "@" lt))
		(label-lt (concatenate 'string "(" lt ")"))
		(@end (concatenate 'string "@" end))
		(label-end (concatenate 'string "(" end ")")))
	   (list "@SP" "M=M-1" "@SP" "A=M" "D=M" "@SP" "AM=M-1" "D=M-D" @lt
		 "D;JLT" "@0" "D=A" "@SP" "A=M" "M=D" @end "0;JMP" label-lt
		 "@1" "D=-A" "@SP" "A=M" "M=D" label-end "@SP" "M=M+1")))
	((string= "and" command)
	 (list "@SP" "M=M-1" "@SP" "A=M" "D=M" "@SP" "AM=M-1" "M=D&M" "@SP" "M=M+1"))
	((string= "or" command)
	 (list "@SP" "M=M-1" "@SP" "A=M" "D=M" "@SP" "AM=M-1" "M=D|M" "@SP" "M=M+1"))
	((string= "not" command)
	 (list "@SP" "M=M-1" "@SP" "A=M" "M=!M" "@SP" "M=M+1"))
	(t (error "Invalid arithmetic or logical operation."))))

(defun write-file (input-file output-file)
  (let* ((commands (get-instructions input-file))
	 (filename (pathname-name input-file))
	 (assembly-lists (mapcar #'(lambda (c) (translate c filename)) commands)))
    (with-open-file (stream output-file :direction :output :if-exists :supersede)
      (dolist (assembly-list assembly-lists)
	(dolist (str assembly-list)
	  (format stream (concatenate 'string str "~%")))))))

(defun write-push (command &optional filename)
  (multiple-value-bind (segment index) (get-args command)
    (let* ((@index (concatenate 'string "@" index))
	   (rest (list "D=M" @index "A=D+A" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
	   (@static (concatenate 'string "@" filename "." index)))
      (cond ((string= "constant" segment)
	     (list @index "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
	    ((string= "local" segment)
	     (append '("@LCL") rest))
	    ((string= "argument" segment)
	     (append '("@ARG") rest))
	    ((string= "this" segment)
	     (append '("@THIS") rest))
	    ((string= "that" segment)
	     (append '("@THAT") rest))
	    ((string= "temp" segment)
	     (list "@R5" "D=A" @index "A=D+A" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
	    ((string= "pointer" segment)
	     (list "@R3" "D=A" @index "A=D+A" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
	    ((string= "static" segment)
	     (append (list @static) rest))
	    (t (error "Segment not recognized."))))))

(defun write-pop (command &optional filename)
  (multiple-value-bind (segment index) (get-args command)
    (let* ((@index (concatenate 'string "@" index))
	   (rest (list "D=M" @index "D=D+A" "@R13" "M=D" "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"))
	   (@static (concatenate 'string "@" filename "." index)))
      (cond ((string= "local" segment)
	     (append '("@LCL") rest))
	    ((string= "argument" segment)
	     (append '("@ARG") rest))
	    ((string= "this" segment)
	     (append '("@THIS") rest))
	    ((string= "that" segment)
	     (append '("@THAT") rest))
	    ((string= "temp" segment)
	     (list "@R5" "D=A" @index "D=D+A" "@R13" "M=D" "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"))
	    ((string= "pointer" segment)
	     (list "@R3" "D=A" @index "D=D+A" "@R13" "M=D" "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"))
	    ((string= "static" segment)
	     (append (list @static) rest))
	    (t (error "Segment not recognized."))))))

(defun write-label (command)
  (let ((label-name (second (split-sequence:split-sequence #\space command :test 'string=))))
    (list (concatenate 'string "(" label-name ")"))))

(defun write-goto (command)
  (let ((goto-label (second (split-sequence:split-sequence #\space command :test 'string=))))
    (list (concatenate 'string "@" goto-label) "0;JMP")))

(defun write-if (command)
  (let ((goto-label (second (split-sequence:split-sequence #\space command :test 'string=))))
    (list "@SP" "AM=M-1" "D=M" (concatenate 'string "@" goto-label) "D;JNE")))

(defun write-function (command)
  (let* ((name-argument (rest (split-sequence:split-sequence #\space command :test 'string=)))
	 (func-name (first name-argument))
	 (n-args (concatenate 'string "@" (second name-argument)))
	 (local-label (concatenate 'string func-name "$" (string (gensym "local")))))
    `(,(concatenate 'string "(" func-name ")") "@SP" "D=M" "@LCL" "M=D" ,n-args "D=A" "@R13" "M=D"
      ,(concatenate 'string "(" local-label ")") ,@(write-push "push constant 0") "@R13" "MD=M-1"
      ,(concatenate 'string "@" local-label) "D;JGT")))

(defun write-return (command)
  (assert (string= command "return"))
  (list "@LCL" "D=M" "@R13" "M=D" "@5" "A=D-A" "D=M" "@R14" "M=D" "@SP" "AM=M-1" "D=M" "@ARG" "A=M" "M=D" "@ARG" "D=M" "@SP" "M=D+1" "@R13" "A=M-1" "D=M" "@THAT" "M=D" "@2" "D=A" "@R13" "A=M-D" "D=M" "@THIS" "M=D" "@3" "D=A" "@R13" "A=M-D" "D=M" "@ARG" "M=D" "@4" "D=A" "@R13" "A=M-D" "D=M" "@LCL" "M=D" "@R14" "A=M" "0;JMP"))

(defun write-call (command)
  (multiple-value-bind (f n) (get-args command)
   (let ((return-address (string (gensym "return-address"))))
     (list (concatenate 'string "@" return-address) "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1" "@LCL" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1" "@ARG" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1" "@THIS" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1" "@THAT" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1" (concatenate 'string "@" n) "D=A" "@5" "D=D+A" "@SP" "D=M-D" "@ARG" "M=D" "@SP" "D=M" "@LCL" "M=D" (concatenate 'string "@" f) "0;JMP" (concatenate 'string "(" return-address ")")))))

(write-file "~/nand2tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm"
	    "~/nand2tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd.asm")

(write-file "~/nand2tetris/projects/07/StackArithmetic/StackTest/StackTest.vm"
	    "~/nand2tetris/projects/07/StackArithmetic/StackTest/StackTest.asm")

(write-file "~/nand2tetris/projects/07/MemoryAccess/BasicTest/BasicTest.vm"
	    "~/nand2tetris/projects/07/MemoryAccess/BasicTest/BasicTest.asm")

(write-file "~/nand2tetris/projects/07/MemoryAccess/PointerTest/PointerTest.vm"
	    "~/nand2tetris/projects/07/MemoryAccess/PointerTest/PointerTest.asm")

(write-file "~/nand2tetris/projects/07/MemoryAccess/StaticTest/StaticTest.vm"
	    "~/nand2tetris/projects/07/MemoryAccess/StaticTest/StaticTest.asm")

(write-file "~/nand2tetris/projects/08/ProgramFlow/BasicLoop/BasicLoop.vm"
	    "~/nand2tetris/projects/08/ProgramFlow/BasicLoop/BasicLoop.asm")

(write-file "~/nand2tetris/projects/08/ProgramFlow/FibonacciSeries/FibonacciSeries.vm"
	    "~/nand2tetris/projects/08/ProgramFlow/FibonacciSeries/FibonacciSeries.asm")

(write-file "~/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm"
	    "~/nand2tetris/projects/08/FunctionCalls/SimpleFunction/SimpleFunction.asm")
