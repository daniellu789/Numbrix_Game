;(setq BRD '(((1) (2) (3) (4)) ((5) (6) (7) (8)) ((9) (10) (11) (12)) ((13) (14) (15) (16))))
;((1 0 3)(0 0 0)(7 0 9))
(defun numbrix() 
	(princ "please select test table from 1~17 and 18,19...24: ")
	(setq selectTable (read))
	(selecTablefun selectTable)
	(P-BOARD BRD)
	(setq count 1)
	(setq moves '())
	(findone BRD)
	(TERPRI)
	(princ "load data succeed!")
	(TERPRI)
	(princ "Option: entry 1 play automatically, entry 2 solve manually: ")
	(setq playoption (read))
	(setq timebegin (get-internal-run-time))
	(cond ((equal playoption 1) (solve)) ((equal playoption 2)  (manul_welcome)(start)) (t (princ "Please entry correct number") (numbrix) ) )
	
	

	(P-BOARD BRD)
	(TERPRI)
	(princ "Time cost to solve this board:")
	(princ (/ (- (get-internal-run-time)  timebegin) (float INTERNAL-TIME-UNITS-PER-SECOND) ))
	(princ " seconds")
	(TERPRI)
	(ifend)
	)

(defun selecTablefun (selectTable)
	(cond ((equal selectTable 1)  (defparameter input (open "1.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "1.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 2)  (defparameter input (open "2.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "2.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 3)  (defparameter input (open "3.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "3.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 4)  (defparameter input (open "4.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "4.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 5)  (defparameter input (open "5.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "5.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 6)  (defparameter input (open "6.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "6.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 7)  (defparameter input (open "7.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "7.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 8)  (defparameter input (open "8.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "8.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 9)  (defparameter input (open "9.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "9.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 10)  (defparameter input (open "10.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "10.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 11)  (defparameter input (open "11.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "11.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 12)  (defparameter input (open "12.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "12.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 13)  (defparameter input (open "13.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "13.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 14)  (defparameter input (open "14.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "14.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 15)  (defparameter input (open "15.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "15.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 16)  (defparameter input (open "16.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "16.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 17)  (defparameter input (open "17.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "17.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 18)  (defparameter input (open "18.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "18.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 19)  (defparameter input (open "19.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "19.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 20)  (defparameter input (open "20.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "20.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 21)  (defparameter input (open "21.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "21.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 22)  (defparameter input (open "22.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "22.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 23)  (defparameter input (open "23.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "23.txt")) 	(setq cbrd (read input)) (close input) ) 
		  ((equal selectTable 24)  (defparameter input (open "24.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "24.txt")) 	(setq cbrd (read input)) (close input) ) 
		  (t (numbrix)))
		)
	


 

;(defun inputbig()
;	(defparameter input (open "databig.txt")) 	(setq BRD (read input)) 	(close input) 	(defparameter input (open "databig.txt")) 	(setq cbrd (read input)) (close input)
;	)

;((1 0 3)(0 0 0)(7 0 9))
;(setq BRD '((1 0 0 4) (8 0 0 5) (9 0 0 12) (16 0 0 13)))
(defun manul_welcome()
(princ "------------------------------------------------------------------------------")(TERPRI)
(princ "Dear Player:") (TERPRI) (princ "Welcome to Numbrix Game. Please type \"(numbrix)\"to begain the game.(It will automatically begain first time)") (TERPRI) (princ "The lower left corner is position (1, 1) and higher right corner is position (n, n). ") (TERPRI) (princ "Each time you type 3 parameters for Row, Col, Value to fill into the matrix.") (TERPRI) (princ "When there is a 1 to n \'line\', you win the game. Enjoy yourself!")
(TERPRI)(princ "------------------------------------------------------------------------------")(TERPRI))



(DEFUN SQR (ROW COL BRD)
	(if (equal ROW 0) (setq ROW 1))
	(if (equal COL 0) (setq COL 1))
	(setq bbsize (length brd))
	(if (equal ROW (+ bbsize 1)) (setq ROW bbsize))
	(if (equal COL (+ bbsize 1)) (setq COL bbsize))	
       (NTH(1- COL)
          (NTH (- (length BRD)  ROW) BRD)))


(DEFUN SET-SQR(ROW COL BRD VAL)
	(setq movesR Row)
	(setq movesC COL)
	(setq movesV VAL)
	   ;(setq moves (append moves () ))
       (SETF (NTH (1- COL)
                   (NTH (- (length BRD)  ROW) BRD))
              VAL))

(defun putintomovies()
		(setq temp '())
    (setq temp (append temp (list movesR)))
    (setq temp (append temp (list movesC)))
    (setq temp (append temp (list movesV)))
    (setq moves (append moves (list temp))))

(defun printmoves(moves)
	(setq countnum 1)
	(dolist (itm moves t)
        (TERPRI)(princ "Step ")(princ countnum)(princ " => ")(princ itm) (setq countnum (+ countnum 1)))  )
;-----------------------------------------------
(DEFUN printhelper(BRD)
	   (setq bsize (length BRD))
	   (cond ((< bsize 4) 
	   (loop
	   	(PRINC "+-")
	   	(setq bsize (1- bsize))
	   	(when (equal bsize 0) (return))) )
	   	 ((< bsize 10) 	   
	   	(loop
	   	(PRINC "+--")
	   	(setq bsize (1- bsize))
	   	(when (equal bsize 0) (return))) ) 
	    (t (loop
	   	(PRINC "+---")
	   	(setq bsize (1- bsize))
	   	(when (equal bsize 0) (return))) ))

	   (PRINC "+")
)

(DEFUN findone(BRD)
	(setq onex 0) (setq oney 0)
	(setq bsize (length BRD))
	(setq x 1)
	(loop
		(setq y 1)
		(loop
		(if (equal (SQR x y BRD) 1) (setq onex x))
		(if (equal (SQR x y BRD) 1) (setq oney y) )
		(setq y (+ y 1))
		(when (equal y (+ bsize 1) )(return)
		))
	(setq x (+ x 1))
	(when (equal x (+ bsize 1)) (return)))
	)

(defun P-BOARD (BRD)
	(printhelper BRD)
       (dolist (itm BRD t) 
         (TERPRI)(P-ROW itm)(TERPRI)(printhelper BRD))
       )

(defun P-ROW (lst)
	(setq bsize (length BRD))
       (dolist (itm lst t)
         (princ "|") (cond ((> bsize 9) (cond ((< itm 10) (princ "  ")) ((< itm 100) (princ " ")) )   ) ((> bsize 3) (if (< itm 10) (princ " "))) )  (princ itm)) (PRINC "|"))


;(if (< itm 10) (princ " "))


(defun insert (x y VAL)
	(SET-SQR x y BRD VAL)
	(cond ((equal (+ (SQR x y BRD) 1) (SQR (+ 1 x) y BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  ((equal (+ (SQR x y BRD) 1) (SQR (- x 1) y BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  ((equal (+ (SQR x y BRD) 1) (SQR x (+ y 1) BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  ((equal (+ (SQR x y BRD) 1) (SQR x (- y 1) BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  ((equal (- (SQR x y BRD) 1) (SQR (+ 1 x) y BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  ((equal (- (SQR x y BRD) 1) (SQR (- x 1) y BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  ((equal (- (SQR x y BRD) 1) (SQR x (+ y 1) BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  ((equal (- (SQR x y BRD) 1) (SQR x (- y 1) BRD) )  (if (equal VAL 1) (setq onex x)) (if (equal VAL 1) (setq oney y)) (check))
		  (T (PRINC "Oops, you asnwer might be not correct, please enter again") (TERPRI)
		  	 (SET-SQR x y BRD 0)
		  ) ))

(defun check()
	(putintomovies)
	(setq count 1)
	(cond ((equal onex 0) )
		   (T (checkhelper onex oney 1))))

(defun checkhelper (x y value)
	(if (equal count (totaln)) (PRINC "Congratulations, you win the game!")  )
	(if (equal count (totaln)) (TERPRI))
	(if (equal count (totaln)) (P-BOARD BRD))
	(if (equal count (totaln)) (printmoves moves))

	(cond ((equal (+ value 1) (SQR (+ 1 x) y BRD)) (setq count (+ count 1)) (checkhelper (+ 1 x) y (+ value 1)) )
		  ((equal (+ value 1) (SQR (- x 1) y BRD)) (setq count (+ count 1)) (checkhelper (- x 1) y (+ value 1)) )
		  ((equal (+ value 1) (SQR x (+ y 1) BRD)) (setq count (+ count 1)) (checkhelper x (+ 1 y) (+ value 1)) )
		  ((equal (+ value 1) (SQR x (- y 1) BRD)) (setq count (+ count 1)) (checkhelper x (- y 1) (+ value 1)) )
		  (T (TERPRI) )
		   ))

(defun totaln()
	(* (length BRD) (length BRD) ))

(defun ifend()
	(TERPRI)
	(PRINC "Please option: type \"1\" to end the game, type \"2\" to replay the game: ") 
	(setf d (read))
	(TERPRI)
	(if (equal d 1) (goodbye))
	(if (equal d 2) (numbrix))
	)

(defun goodbye()
	(princ "Goodbye!"))

(defun start()
	(cond ( (< count (totaln)) 
	 (P-BOARD BRD)
	 (TERPRI)
	 (princ "Please enter 3 parameters, Row, Col, Value respectly: ")
	 (setf a (read))
	 (setf b (read))
	 (setf c (read))

	 (cond ( (or (not (numberp a)) (not (numberp b)) (not (numberp c)) ) (notnumber)) (t ))
     (cond ( (not (or (not (numberp a)) (not (numberp b)) (not (numberp c)) ))    (checkScopeAndOrginal))  (t ))
	 (start)
	 )
	))

(defun checkScopeAndOrginal()
	
	(if (or  (< a 1) (> a (length BRD)) (< b 1) (> b (length BRD)) )  (exceedscope))
	;(if (not (equal (SQR a b cbrd) 0)) (cannotoriginal))
    (if (not  (or  (< a 1) (> a (length BRD)) (< b 1) (> b (length BRD)) (not (numberp a)) (not (numberp b)) (not (numberp c))) )   (cond ((equal (SQR a b cbrd) 0)  (insert a b c)) (t (cannotoriginal) ) ))
	)

(defun cannotoriginal()
	(princ "Oops, you cannot change orginal data, please enter again")(TERPRI) )

(defun exceedscope()
	(princ "Oops, your input exceed the scope of matrix, please enter again")(TERPRI) )

(defun notnumber()
	(princ "Oops, your input  is not number, please enter again")(TERPRI) )

(defun test()
	(setq testa (read))
	(if (or (> testa 15) (< testa 10) (equal testa 12)) (princ "a is a number"))
	(if (not (numberp testa)) (princ "a  is not a number")))


;-----------------------------------AI

(defun createsortedLists()
	(setq sortedLists '())

	(dolist (templist BRD t) 
         (dolist (cur templist t)
         	(cond ((not (equal cur 0))
         	(setq sortedLists (append sortedLists (list cur)))))
         )
    )	
    (setq sortedLists (sort sortedLists #'<))
)

(defun solve()
	(princ "Solving...")(TERPRI)
	(createsortedLists)
	(setq permutations 0)
	(setq sortedList_index 0)
	(setq brdlength (length BRD))
	(setq r 1)

	(findone BRD)

	(cond ((equal onex 0)  	(loop 
		(setq c 1)
		(loop 
			(solvehelper r c 1)
			(setq c (+ c 1))
			(when (equal c (+ brdlength 1) ) (return) ) )	
		(setq r (+ r 1))
		(when (equal r (+ brdlength 1) ) (return) ) ) ) (t (solvehelper onex oney 1))

	))

(defun solvehelper(row col val)
	
	(cond ((checkInputScope row col) 
		(setq onsortedList (onsortedListHelper val))


		(cond ((> (SQR row col BRD) 0)   ;this (r,c) is not 0
		          (cond (onsortedList 
		                            (cond (  (equal (SQR row col BRD) val) (solvehelper2 row col val onsortedList) )
		                                  (t  (not t) ) )  )
		                (t (not t))  ) )
		      (t (cond (onsortedList (not t) ) ;this (r,c) is 0
		               (t (SET-SQR row col BRD val)   (solvehelper2 row col val onsortedList) ) ))

		) )))

(defun solvehelper2(row col val onPr)
	
	(setq permutations (+ permutations 1))
	(cond ((equal val (totaln) ) t)
		  (t (cond (onPr (setq sortedList_index (+ sortedList_index 1))   ))
		  	 (cond ((solvehelper (+ row 1) col (+ val 1) ) t )
		  		  ((solvehelper (- row 1) col (+ val 1) ) t )
		  		  ((solvehelper row (+ col 1) (+ val 1) ) t )
		  		  ((solvehelper row (- col 1) (+ val 1) ) t )
		  		  (t   (cond ((not onPr) (SET-SQR row col BRD 0)  (not t) )
		  		        (t  (setq sortedList_index (- sortedList_index 1))  (not t) )) 

		  	            )
		  	           )
		  		  )
		     )
		   )
	

(defun onsortedListHelper(val)
	(cond ((and (< sortedList_index (length sortedLists)) (equal val (NTH sortedList_index sortedLists)) ) t )
		   (t (not t))))


(defun checkInputScope(r c)
	(cond ( (or (< r 1) (< c 1) (> r (length brd)) (> c (length brd)) )  (not t) )  
		   (t )
		))


(numbrix)


