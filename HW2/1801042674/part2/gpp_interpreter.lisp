(load "gpp_lexer.lisp")

 (defun parser-gppinterpreter ()
	(Read-Line-Console-Parser)
	)

(defun Read-Line-Console-Parser()
	(loop
        (format t "~%Enter a string: ")
        (parse-START (read-line))
    )		
)

(defparameter saveEXPI (list "END"))
(defparameter saveEXPI1 (list -1))

(defparameter my-list (list "END"))
(defparameter my-list1 (list -1))
(defun parse-START (seq)
	(let ((token-name (str-to-list seq)) (token-id (token-list seq)))
		(write (parse-INPUT token-name token-id))
		)
	)
(defun parse-INPUT(token_name token_id)

	(cond ((string= (car token_id) "OP_OP"))
		(t (format t "Syntax Error!")))
	(terpri)
	(setq y (cdr token_name))
	(setq x (cdr token_id))	
	(setq result 0)

	(format t "RESULT IS : ")
	(when (string= (car x) "OP_PLUS")
		(return-from parse-INPUT (PLUS_Function (cdr x)(cdr y) result)))
	(when (string= (car x) "OP_MINUS")
		(return-from parse-INPUT (MINUS_Function (cdr x)(cdr y) result)))
	(when (string= (car x) "OP_DIV")
		(return-from parse-INPUT (DIV_Function (cdr x)(cdr y) result)))
	(when (string= (car x) "OP_MULT")
		(return-from parse-INPUT (MULT_Function (cdr x)(cdr y) result)))
	(when (string= (car x) "OP_DBMULT")
		(return-from parse-INPUT (DBLMULT_Function (cdr x)(cdr y) result)))
	(when (string= (car x) "KW_EXIT")
		(return-from parse-INPUT (EXIT_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_IF")
		(return-from parse-INPUT (IF_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_FOR")
		(return-from parse-INPUT (FOR_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_SET")
		(SET_Function (cdr x)(cdr y))
		(write my-list)
		(write my-list1)
		(terpri)
		(return-from parse-INPUT "SYNTAX OK")
	)
	(when (string= (car x) "KW_APPEND")
		(return-from parse-INPUT (APPEND_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_LOAD")
		(return-from parse-INPUT (LOAD_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_DISP")
		(return-from parse-INPUT (DISP_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_CONCAT")
		(return-from parse-INPUT (concat_helper (cdr x)(cdr y))))
	(when (string= (car x) "KW_LIST")
		(return-from parse-INPUT (LIST_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_LESS")
		(return-from parse-INPUT (LESS_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_EQUAL")
		(return-from parse-INPUT (EQUAL_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_NOT")
		(return-from parse-INPUT (NOT_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_OR")
		(return-from parse-INPUT (OR_Function (cdr x)(cdr y))))
	(when (string= (car x) "KW_AND")
		(return-from parse-INPUT (AND_Function (cdr x)(cdr y))))
	(return-from parse-INPUT "SYNTAX_ERROR Expression not recognized")
)
(defun reach_identifier (my-list my-list1 identifier)
	(when (string= (car my-list) identifier)
		
		(return-from reach_identifier (car my-list1))

	)
	
	(if (string= (car my-list) "END")
		(return-from reach_identifier "END"))

	(reach_identifier (cdr my-list)(cdr my-list1) identifier)
)
(defun PLUS_Function (token_id token_name result)
	(setq check "false")
	(setq check1 "false")
	(when (> 2 (list-length token_name))
		(print "+ operand requires minimum 2 numbers.")
		(return-from PLUS_Function)
	)

	(setq check2 "true")

	(if (string= (car token_id) "IDENTIFIER")
		(setq check2 "false") )
	(if (string= (car token_id) "VALUE")
		(setq check2 "false") )

	(if (string= check2 "false")
		t (return-from PLUS_Function "Syntax Error! "))


	(if (string= (car token_id) "OP_CP")
		(return-from PLUS_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check "true")
	))
	(if (string= (car token_id) "OP_CP")
		(return-from PLUS_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq a (car token_name))
		(setf a (reach_identifier my-list my-list1 a))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp a))
		(return-from PLUS_Function "Syntax Error!") )
	))
	
	(if (string= check1 "false")
	(when (string= (car token_id) "VALUE")
		(setq b (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check1 "true")
	))
	(if (string= check1 "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq b (car token_name))
		(setf b (reach_identifier my-list my-list1 b))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp b))
		(return-from PLUS_Function "Syntax Error!") )
	))

	(setf result ( perform-op a b "+"))

	(setf token_name (reverse token_name))
	(setf token_id (reverse token_id))

	(setf result1 (write-to-string result))
	(push result1 (cdr (last token_name))) token_name
	(setf token_name (reverse token_name))
	(push "VALUE" (cdr (last token_id))) token_id
	(setf token_id (reverse token_id))


	(PLUS_Function token_id token_name result)
)
(defun MINUS_Function (token_id token_name result)
	

	(setq check "false")
	(setq check1 "false")
	(when (> 2 (list-length token_name))
		(print "+ operand requires minimum 2 numbers.")
		(return-from MINUS_Function)
	)

	(setq check2 "true")

	(if (string= (car token_id) "IDENTIFIER")
		(setq check2 "false") )
	(if (string= (car token_id) "VALUE")
		(setq check2 "false") )

	(if (string= check2 "false")
		t (return-from MINUS_Function "Syntax Error! "))


	(if (string= (car token_id) "OP_CP")
		(return-from MINUS_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check "true")
	))
	(if (string= (car token_id) "OP_CP")
		(return-from MINUS_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq a (car token_name))
		(setf a (reach_identifier my-list my-list1 a))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp a))
		(return-from MINUS_Function "Syntax Error!") )
	))
	
	(if (string= check1 "false")
	(when (string= (car token_id) "VALUE")
		(setq b (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check1 "true")
	))
	(if (string= check1 "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq b (car token_name))
		(setf b (reach_identifier my-list my-list1 b))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp b))
		(return-from MINUS_Function "Syntax Error!") )
	))

	(setf result ( perform-op a b "-"))

	(setf token_name (reverse token_name))
	(setf token_id (reverse token_id))

	(setf result1 (write-to-string result))
	(push result1 (cdr (last token_name))) token_name
	(setf token_name (reverse token_name))
	(push "VALUE" (cdr (last token_id))) token_id
	(setf token_id (reverse token_id))


	(MINUS_Function token_id token_name result)
)
(defun DIV_Function (token_id token_name result)
	
	(when (/= 3 (list-length token_name))
    	(return-from DIV_Function "/ operand requires exactly 2 numbers.")      
    )

	(setq check "false")
	(setq check1 "false")

	(setq check2 "true")

	(if (string= (car token_id) "IDENTIFIER")
		(setq check2 "false") )
	(if (string= (car token_id) "VALUE")
		(setq check2 "false") )

	(if (string= check2 "false")
		t (return-from DIV_Function "Syntax Error! "))


	(if (string= (car token_id) "OP_CP")
		(return-from DIV_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check "true")
	))
	(if (string= (car token_id) "OP_CP")
		(return-from DIV_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq a (car token_name))
		(setf a (reach_identifier my-list my-list1 a))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp a))
		(return-from DIV_Function "Syntax Error!") )
	))
	
	(if (string= check1 "false")
	(when (string= (car token_id) "VALUE")
		(setq b (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check1 "true")
	))
	(if (string= check1 "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq b (car token_name))
		(setf b (reach_identifier my-list my-list1 b))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp b))
		(return-from DIV_Function "Syntax Error!") )
	))

	(setf result ( perform-op a b "/"))

	(return-from DIV_Function result)
)
(defun MULT_Function (token_id token_name result)
	
	(setq check "false")
	(setq check1 "false")
	(when (> 2 (list-length token_name))
		(print "+ operand requires minimum 2 numbers.")
		(return-from MULT_Function)
	)

	(setq check2 "true")

	(if (string= (car token_id) "IDENTIFIER")
		(setq check2 "false") )
	(if (string= (car token_id) "VALUE")
		(setq check2 "false") )

	(if (string= check2 "false")
		t (return-from MULT_Function "Syntax Error! "))


	(if (string= (car token_id) "OP_CP")
		(return-from MULT_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check "true")
	))
	(if (string= (car token_id) "OP_CP")
		(return-from MULT_Function result))

	(if (string= check "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq a (car token_name))
		(setf a (reach_identifier my-list my-list1 a))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp a))
		(return-from MULT_Function "Syntax Error!") )
	))
	
	(if (string= check1 "false")
	(when (string= (car token_id) "VALUE")
		(setq b (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setf check1 "true")
	))
	(if (string= check1 "false")
	(when (string= (car token_id) "IDENTIFIER")
		(setq b (car token_name))
		(setf b (reach_identifier my-list my-list1 b))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(if (equal T (stringp b))
		(return-from MULT_Function "Syntax Error!") )
	))

	(setf result ( perform-op a b "*"))

	(setf token_name (reverse token_name))
	(setf token_id (reverse token_id))

	(setf result1 (write-to-string result))
	(push result1 (cdr (last token_name))) token_name
	(setf token_name (reverse token_name))
	(push "VALUE" (cdr (last token_id))) token_id
	(setf token_id (reverse token_id))


	(MULT_Function token_id token_name result)
)
(defun DBLMULT_Function (token_id token_name result )
	
	(when (/= 3 (list-length token_name))
    	(return-from DBLMULT_Function "** operand requires exactly 2 numbers.")      
    )
	(if (string= (car token_id) "VALUE")
		t  (return-from DBLMULT_Function) )
	
	(setq a (parse-integer (car token_name)))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))


	(if (string= (car token_id) "VALUE")
		t (return-from DBLMULT_Function "Syntax Error"))
	
	(setq b (parse-integer (car token_name)))
	(setq token_id (cdr token_id))
	(setq token_name (cdr token_name))
	
	(if (string= (car token_id) "OP_CP")
		t (return-from DBLMULT_Function "Syntax Error"))

	(setq result a)
	
	(dotimes (n (- b 1))
   		(setf result ( perform-op result a "*"))
	)
	

	(return-from DBLMULT_Function result)
)
(defun EXIT_Function (token_id token_name)
	(if (string= (car token_id) "OP_CP")
		t (return-from EXIT_Function "Syntax Error"))

	(write "Program End ! ")
	
	(if (string= (car token_id) "OP_CP")
		(exit))
)

(defun IF_Function (token_id token_name)

	(when (/= 4 (list-length token_name))
    	(return-from IF_Function "if operand requires exactly 2 numbers one true false statement.")      
    )

	(if (string= (car token_id) "OP_CP")
		(return-from IF_Function))

	(setq check "true")

	(if (string= (car token_id) "KW_TRUE")
		(setq check "false") )
	(if (string= (car token_id) "KW_FALSE")
		(setq check "false") )

	(if (string= check "false")
		t (return-from IF_Function "Syntax Error! "))

	(setq check1 (car token_name))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(if (string= (car token_id) "OP_CP")
		(return-from IF_Function result))

	(if (string= (car token_id) "VALUE")
		t (return-from IF_Function "Syntax Error"))

	(setq a (parse-integer (car token_name)))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(if (string= (car token_id) "OP_CP")
		(return-from IF_Function result))

	(if (string= (car token_id) "VALUE")
		t (return-from IF_Function "Syntax Error"))
	
	(setq b (parse-integer (car token_name)))
	(setq token_id (cdr token_id))
	(setq token_name (cdr token_name))

	(write "Syntax OK.")
	(if (string= check1 "true")
		(return-from IF_Function a))
	(return-from IF_Function b)	
)

(defun SET_Function (token_id token_name)
	
	(if (string= (car token_id) "IDENTIFIER")
		t  (return-from SET_Function) )

	
	(setq a (car token_name))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))


	(if (string= (car token_id) "VALUE")
		t (return-from SET_Function "Syntax Error"))
	
	(setq b (parse-integer (car token_name)))
	(setq token_id (cdr token_id))
	(setq token_name (cdr token_name))
	
	(if (string= (car token_id) "OP_CP")
		t (return-from SET_Function "Syntax Error"))
	
	(setf my-list(reverse my-list))
	(setf my-list1(reverse my-list1))
	
	(add-to-last my-list a)
	(add-to-last my-list1 b)

	(setf my-list(reverse my-list))
	(setf my-list1(reverse my-list1))
	
	(return-from SET_Function)		
)
(defun concat_helper(token_id token_name)
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(setf token_name (remove-last token_name))
	(setf token_id (remove-last token_id))
	(setf token_name (remove-last token_name))
	(setf token_id (remove-last token_id))
	(CONCAT_Function token_id token_name)
	
)
(defun concat_helper1 (token_id token_name)
	
	(if (string= (car token_id) "VALUE1")
		(return-from concat_helper1 token_name) )
	(if (string= (car token_id) "IDENTIFIER1")
		(return-from concat_helper1 token_name ))
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(add-to-last token_name a)
		(add-to-last token_id "VALUE1")
	)
	(when (string= (car token_id) "IDENTIFIER")
		(setq a (car token_name))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(add-to-last token_name a)
		(add-to-last token_id "IDENTIFIER1")
	)
	(concat_helper1 token_id token_name	)
)
(defun CONCAT_Function (token_id token_name)
	

	(when (string= (car token_id) "OP_CP")
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))	
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(return-from CONCAT_Function (concat_helper1 token_id token_name)))

	(setq check "true")

	(if (string= (car token_id) "VALUE")
		(setq check "false") )
	(if (string= (car token_id) "IDENTIFIER")
		(setq check "false") )

	(if (string= check "false")
		t (return-from CONCAT_Function "Syntax Error! "))
	
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(add-to-last token_name a)
		(add-to-last token_id "VALUE1")
	)
	(when (string= (car token_id) "IDENTIFIER")
		(setq a (car token_name))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(add-to-last token_name a)
		(add-to-last token_id "IDENTIFIER1")
	)
	(CONCAT_Function token_id token_name)
)
(defun DISP_Function (token_id token_name)
	
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(return-from DISP_Function a)
	)

	(when (string= (car token_id) "KW_LIST")
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		
		(setf token_name (remove-last token_name))
		(setf token_id (remove-last token_id))

		(return-from DISP_Function (LIST_Function token_id token_name))
		
	)

	(return-from DISP_Function (car token_name))	
	
)
(defun LIST_Function (token_id token_name)
	(when (string= (car token_id) "OP_CP")
		(setf token_name (reverse token_name))
		(setf token_name (remove-last token_name))
		(setf token_name (reverse token_name))
		(return-from LIST_Function token_name))
	
	(when (string= (car token_id) "VALUE")
		(setq a (parse-integer (car token_name)))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(add-to-last token_name a)
	)
	(when (string= (car token_id) "IDENTIFIER")
		(setq a (car token_name))
		(setq token_name (cdr token_name))
		(setq token_id (cdr token_id))
		(add-to-last token_name a)
	)
	
	(LIST_Function token_id token_name)
	
	
)
(defun APPEND_Function (token_id token_name)

	(setq check "true")

	(if (string= (car token_id) "IDENTIFIER")
		(setq check "false") )
	(if (string= (car token_id) "VALUE")
		(setq check "false") )

	(if (string= check "false")
		t (return-from APPEND_Function "Syntax Error! "))
	
	(setq value (car token_name))
	(setq value1 (car token_id))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(if (string= (car token_id) "OP_OP")
		t (return-from APPEND_Function "Syntax Error! "))

	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(if (string= (car token_id) "KW_LIST")
		t (return-from APPEND_Function "Syntax Error! "))

	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(setf token_name(remove-last token_name))

	(setf token_name(remove-last token_name))

	(setf token_id(remove-last token_id))
	(setf token_id(remove-last token_id))

	(setf token_name (add-to-last token_name value))
	(setf token_id (add-to-last token_id value1))

	(setf token_name (add-to-last token_name ")"))
	(setf token_id (add-to-last token_id "OP_CP"))

	
	(LIST_Function token_id token_name)
	
)

(defun add-to-last (lst a)
	(push a (cdr (last lst))) lst)

(defun LESS_Function (token_id token_name)
	(when (/= 3 (list-length token_id))
        (print "LESS requires exactly 2 numbers.")
    )

	(if (string= (car token_id) "VALUE")
		t  (return-from LESS_Function))

	(setq a (parse-integer (car token_name)))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(if (string= (car token_id) "VALUE")
		t (return-from LESS_Function "Syntax Error"))

	(setq b (parse-integer (car token_name)))
	(setq token_id (cdr token_id))
	(setq token_name (cdr token_name))

	(if (string= (car token_id) "OP_CP")
		t (return-from LESS_Function "Syntax Error"))

	(setf result (< a b))
	
	(if (string= result "T")
		(setf result1 "True"))

	(if (string= result "NIL")
		(setf result1 "False"))

	(return-from LESS_Function result1)	
)
(defun remove-last(l)
    (reverse (cdr (reverse l))))
(defun EQUAL_Function (token_id token_name)	
	
	(if (string= (car token_id) "VALUE")
		t  (return-from EQUAL_Function) )
	
	(setq a (parse-integer (car token_name)))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))


	(if (string= (car token_id) "VALUE")
		t (return-from EQUAL_Function "Syntax Error"))
	
	(setq b (parse-integer (car token_name)))
	(setq token_id (cdr token_id))
	(setq token_name (cdr token_name))

	(if (string= (car token_id) "OP_CP")
		t (return-from EQUAL_Function "Syntax Error"))

	(setf result ( perform-logic-op "equal" a b))

	(return-from EQUAL_Function result)	
)	
(defun NOT_Function (token_id token_name)

	
	(setq check "true")

	(if (string= (car token_id) "KW_TRUE")
		(setq check "false") )
	(if (string= (car token_id) "KW_FALSE")
		(setq check "false") )

	(if (string= check "false")
		t (return-from NOT_Function "Syntax Error! "))

	(setq a (car token_name))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))


	(if (string= (car token_id) "OP_CP")
		t (return-from NOT_Function "Syntax Error"))
	
	(setf result ( perform-logic-op "not" a))
	
	(return-from NOT_Function result)	
)
(defun OR_Function (token_id token_name)
	
	(setq check "true")

	(if (string= (car token_id) "KW_TRUE")
		(setq check "false") )
	(if (string= (car token_id) "KW_FALSE")
		(setq check "false") )

	(if (string= check "false")
		t (return-from OR_Function "Syntax Error! "))

	(setq a (car token_name))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(setq check "true")

	(if (string= (car token_id) "KW_TRUE")
		(setq check "false") )
	(if (string= (car token_id) "KW_FALSE")
		(setq check "false") )

	(if (string= check "false")
		t (return-from OR_Function "Syntax Error!"))

	(setq b (car token_name))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))


	(if (string= (car token_id) "OP_CP")
		t (return-from OR_Function "Syntax Error!"))
	
	(setf result ( perform-logic-op "or" a b))
	
	(return-from OR_Function result)
)
(defun AND_Function (token_id token_name)
	
	(setq check "true")

	(if (string= (car token_id) "KW_TRUE")
		(setq check "false") )
	(if (string= (car token_id) "KW_FALSE")
		(setq check "false") )

	(if (string= check "false")
		t (return-from AND_Function "Syntax Error! "))

	(setq a (car token_name))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))

	(setq check "true")

	(if (string= (car token_id) "KW_TRUE")
		(setq check "false") )
	(if (string= (car token_id) "KW_FALSE")
		(setq check "false") )

	(if (string= check "false")
		t (return-from AND_Function "Syntax Error!"))

	(setq b (car token_name))
	(setq token_name (cdr token_name))
	(setq token_id (cdr token_id))


	(if (string= (car token_id) "OP_CP")
		t (return-from AND_Function "Syntax Error!"))
	
	(setf result ( perform-logic-op "and" a b))
	
	(return-from AND_Function result)
)

(defun perform-op (op1 op2 op)
	(let ((num1 op1) (num2 op2)) 
	(cond ((string= "+" op) (+ num1 num2))
		  ((string= "-" op) (- num1 num2))
		  ((string= "*" op) (* num1 num2))
		  ((string= "/" op) (/ num1 num2)))))

(defun get-logic-val (op)
	(if (and (not (integerp op)) op (or (string= "true" op) (string= "false" op)))
	(string= "true" op)))

(defun perform-logic-op (op op1 &optional op2)

	(if (and op2 (string= "equal" op) (and (integerp op1) 
		(integerp op2)))
		(if (= op1 op2) 
			(return-from perform-logic-op "true")
			(return-from perform-logic-op "false")))

	(if (let ((var1 (get-logic-val op1)) (var2 (get-logic-val op2)))
		(if op2 (cond ((string= "and" op) (and var1 var2))
					((string= "or" op) (or var1 var2))
					((string= "equal" op) (equal var1 var2)))
				(if (string= "not" op) (not var1)))) "true" "false"))

(defun parser-interpret-file (filename)
	(let ((in (open filename :if-does-not-exist nil)))		
	  (when in (loop for line = (read-line in nil)
	         	while line do (parse-START line)) (close in))
	  (unless in (format t "ERROR: No such file: '~a'" filename))))

(defun token-list (seq)
	(let ((lst (str-to-list seq)))(let ((token-lst (if (string= (car lst) ";;") "COMMENT"  
		(map 'list #'(lambda (token) (lexize token (lexlst))) lst)))) token-lst)))

(defun parser-gppinterpreter (&optional filename)
	(if filename (parser-interpret-file filename) (Read-Line-Console-Parser))
	)

(if *args* (parser-gppinterpreter (car *args*)) (parser-gppinterpreter))