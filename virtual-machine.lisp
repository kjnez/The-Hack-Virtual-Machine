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
    (cond ((string= first-word "push") 'c-push)
	  ((string= first-word "pop") 'c-pop)
	  ((string= first-word "label") 'c-label)
	  ((string= first-word "goto") 'c-goto)
	  ((string= first-word "if-goto") 'c-if)
	  ((string= first-word "function") 'c-function)
	  ((string= first-word "return") 'c-return)
	  ((string= first-word "call") 'c-call)
	  (t 'c-arithmetic))))

(defun get-args (command)
  (let ((args (rest (split-sequence:split-sequence #\space command :test 'string=))))
    (if (= 1 (length args))
	(values (first args) nil)
	(values (first args) (second args)))))

(defun translate (command filename)
  (let ((command-type (command-type command)))
    (cond ((eql command-type 'c-push)
	   (prepend-comment command (write-push command filename)))
	  ((eql command-type 'c-pop)
	   (prepend-comment command (write-pop command filename)))
	  ((eql command-type 'c-arithmetic)
	   (prepend-comment command (arithmetic-logical-op command)))
	  ((eql command-type 'c-label)
	   (prepend-comment command (write-label command)))
	  ((eql command-type 'c-goto)
	   (prepend-comment command (write-goto command)))
	  ((eql command-type 'c-if)
	   (prepend-comment command (write-if command)))
	  ((eql command-type 'c-function)
	   (prepend-comment command (write-function command)))
	  ((eql command-type 'c-return)
	   (prepend-comment command (write-return command)))
	  ((eql command-type 'c-call)
	   (prepend-comment command (write-call command)))
	  (t (error "Cannot process it for now.")))))

(defmacro prepend-comment (command &body body)
  `(append (list (concatenate 'string "//--: " ,command))
	   . ,body))

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

(defun write-stream (input-file stream)
  "Read one VM source file and write to output-file."
  (let* ((filename (pathname-name input-file))
	 (commands (get-instructions input-file))
	 (assembly-lists (mapcar #'(lambda (c) (translate c filename)) commands)))
    (dolist (assembly-list assembly-lists)
      (dolist (str assembly-list)
	(format stream (concatenate 'string str "~%"))))))

(defun compile-dir (dir)
  (let* ((vm-files (directory (concatenate 'string dir "/*.vm")))
	 (output-file-name (pathname-name dir))
	 (output-file (concatenate 'string dir "/" output-file-name ".asm")))
    (with-open-file (stream output-file :direction :output :if-exists :supersede)
      ;; Bootstrapping
      (when (> (length vm-files) 1)
	(let ((bootstrap-code
		`("//--: SP=256" "@256" "D=A" "@SP" "M=D"
				 "//--: call Sys.init 0",@(write-call "call Sys.init 0"))))
	    (dolist (line bootstrap-code)
	      (format stream (concatenate 'string line "~%")))))
      (dolist (vm-file vm-files)
	(write-stream vm-file stream)))))

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
	     (append (list @static) (list "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1")))
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
	     (append (list @static)
		     (list "D=A" "@R13" "M=D" "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D")))
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
    (if (> (parse-integer (second name-argument)) 0)
	`(,(concatenate 'string "(" func-name ")") "@SP" "D=M" "@LCL" "M=D" ,n-args "D=A" "@R13" "M=D"
	  ,(concatenate 'string "(" local-label ")") ,@(write-push "push constant 0") "@R13" "MD=M-1"
	  ,(concatenate 'string "@" local-label) "D;JGT")
	(list (concatenate 'string "(" func-name ")")))))

(defun write-return (command)
  (assert (string= command "return"))
  (list "@LCL" "D=M" "@R13" "M=D" "@5" "A=D-A" "D=M" "@R14" "M=D" "@SP" "AM=M-1" "D=M" "@ARG" "A=M"
	"M=D" "@ARG" "D=M" "@SP" "M=D+1" "@R13" "A=M-1" "D=M" "@THAT" "M=D" "@2" "D=A" "@R13" "A=M-D"
	"D=M" "@THIS" "M=D" "@3" "D=A" "@R13" "A=M-D" "D=M" "@ARG" "M=D" "@4" "D=A" "@R13" "A=M-D"
	"D=M" "@LCL" "M=D" "@R14" "A=M" "0;JMP"))

(defun write-call (command)
  (multiple-value-bind (f n) (get-args command)
   (let ((return-address (string (gensym "return-address"))))
     (list (concatenate 'string "@" return-address) "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1" "@LCL" "D=M"
	   "@SP" "A=M" "M=D" "@SP" "M=M+1" "@ARG" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1" "@THIS" "D=M"
	   "@SP" "A=M" "M=D" "@SP" "M=M+1" "@THAT" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"
	   (concatenate 'string "@" n) "D=A" "@5" "D=D+A" "@SP" "D=M-D" "@ARG" "M=D" "@SP" "D=M" "@LCL"
	   "M=D" (concatenate 'string "@" f) "0;JMP" (concatenate 'string "(" return-address ")")))))
