(setq ext:*warn-on-redefinition*)
; List Functions

(defun reverse(l)
	(reverse-aux l nil)
)

(defun reverse-aux(l a)
	(if (eq l nil)
		a
	(reverse-aux (cdr l) (cons (car l) a) ) ;build reversed list from an empty list

	)


)

(defun append(l1 l2)
	(append-aux (reverse l1) l2) ;reversing the list so conforms with -aux
)
(defun append-aux(l a)
	(if (eq l nil)
		a
		(append-aux (cdr l) (cons (car l) a)  ) ;building a reversed appended list

	)

)

(defun addtoend(n l)
	(setq l1 (reverse(cons n (reverse l) ) )) ;building a new list that first adds the element to the front of the reversed list, then reverses it
	l1

)



(defun indexof(x l)
	(indexof-aux x l 0)
)

(defun indexof-aux(x l i)
	(if(eq l nil)
		-1
		(if (eq (car l) x)
			i
			(indexof-aux x (cdr l) (+ i 1)) ;counter to check
		)
	)


)

; Set Functions

(defun cardinality(l)
	(cardinality-aux l 0)
)

(defun cardinality-aux(l n)
	(if(eq l nil)
		n
		(cardinality-aux (cdr l) (+ n 1)) ;removing elements from list and adding to counter
	)
)


(defun member(l x)
	(if (eq l nil)
		nil
		(if(eq x (car l))
			t
			(member (cdr l) x) ;checking if each element of the set is in set,  until no more elements
		)
	)

)


(defun insert(l x)
	(if(eq nil (member l x)) ; if not a member, then add it.
		(cons x l)
		l
	)

)


(defun union (list1 list2)
  (if (eq nil list1)
      list2
      (union (cdr list1)
                       (insert list2 (car list1))) ;add the element to list2 if not in list 2 and remov it from l1 
    )
  )


;Math Functions

(defun factorial(x)	
	(if(= x 0)
		 1
	 (* x (factorial (- x 1))) ) ) ;multiply and subtract counter



(defun abs(x)
	(if(> x -1 )
		x
	(* -1 x)
	)
)

 
(defun right-tri(s1 s2 h)
	(eq (* h h ) (+ (* s1 s1) (* s2 s2))) ;pythagorean theorem
)


(defun nth-fibo(n)
	(if (>= 1 n)
		n
	(+ (nth-fibo(- n 1)) (nth-fibo(- n 2))) ;fibo property
	)
)


; Required functions

;implementing remainder for the factor func.
(defun rem (n m)
	(if (>= n m)
		(rem (- n m) m) 
		n
	)
)

(defun sum(l)
	(sum-aux l 0)
)

(defun sum-aux(l n)
	(if(eq l nil)
		n
		(sum-aux (cdr l) (+ (car l) n)) ;add to the list and remove it from the list
	)
)

(defun factors(n) ;gets factors up to but not including n
	(factors-aux n (- n 1))
)

(defun factors-aux(n i)
	(if (eq i 1)
		(list 1)
		(if (eq 0 (rem n i))
			(append (factors-aux n (- i 1)) (list i)) ;if there is no remainder, we add it to the list and subtract from i
			(factors-aux n (- i 1))
		)
	)
)

(defun perfect(n)
	(= n (sum(factors n)))
)


(defun abundant(n)
	(< n (sum(factors n)))
)


(defun deficient(n)
	(> n (sum(factors n)))
)

;REPL SHIT


(defun print-fun (f a b c) ;evaluate and print function "E, P"
	(if (eq c nil)
		(if (eq b nil)
			(format t "(~a ~a) => ~a ~%" f a (funcall f a))
			(format t "(~a ~a ~a) => ~a ~%" f a b (funcall f a b))
		)
		(format t "(~a ~a ~a ~a) => ~a ~%" f a b c (funcall f a b c))
	)
)

(defun repl1(f) ;repl for 1 arg
	(format t "Enter arguments for ~a (q to stop): " f)
	(finish-output nil)
	(setq arg1 (read))
	(if (equalp (format nil "~a" arg1) "q") ;check for q
		nil
		(if (print-fun f arg1 nil nil)
			nil
			(repl1 f)
		)		
	)

)
(defun repl2(f) ;repl 1for 2 arg
	(format t "Enter arguments for ~a (q to stop): " f)
	(finish-output nil)
	(setq arg1 (read))
	(if (equalp (format nil "~a" arg1) "q")
		nil
		(if (print-fun f arg1 (read) nil)
			nil
			(repl2 f)
		)		
	)		
)
(defun repl3(f) ;repl for 3 args
	(format t "Enter arguments for ~a (q to stop): " f)
	(finish-output nil)
	(setq arg1 (read))
	(if (equalp (format nil "~a" arg1) "q")
		nil
		(if (print-fun f arg1 (read) (read))
			nil
			(repl3 f)
		)		
	)

)


(format t "Project 3: Functional Programming In Lisp ~% ***List Functions*** ~%")
(format t " (APPEND (1 3 X A) (4 2 B)) => (~{~a~^, ~}) ~%" (append (list 1 3 'X 'A) (list 4 2 'B)) )
(repl2 'append)
(format t "(ADDTOEND D (A B C) => (~{~a~^, ~}) ~%" (addtoend 'D (list 'A 'B 'C)) )
(repl2 'addtoend)
(format t "(REVERSE (A B C D) => (~{~a~^, ~}) ~%" (reverse (list 'A 'B 'C 'D)) )
(repl1 'reverse)
(format t "(INDEXOF A (B C A D) => ~D ~%" (indexof 'A (list 'B 'C 'A 'D)) )
(format t "(INDEXOF F (B C A D) => ~D ~%" (indexof 'F (list 'B 'C 'A 'D)) )
(repl2 'indexof)
(format t "*** Set Functions**** ~%")
(format t "(CARDINALITY (A B C) => ~D ~%" (cardinality '(list A B C)))
(repl1 'cardinality)
(format t "(UNION (A B C) (A C D) => (~{~a~^, ~}) ~%" (union (list 'A 'B 'C) (list 'A 'C 'D)))
(repl2 'union)
(format t "(MEMBER (B C A D) A) => ~D ~%"(member (list 'B 'C 'A 'D) 'A ))
(format t "(MEMBER (B C A D) F) => ~D ~%"(member (list 'B 'C 'A 'D) 'F ))
(repl2 'member)
(format t "(INSERT (B C D) A)  => (~{~a~^, ~}) ~%" (insert (list 'B 'C 'D) 'A ))
(repl2 'insert)
(format t "*** Math Functions*** ~%")
(format t "(ABS 7) => ~D ~%" (abs 7))
(format t "(ABS -7) => ~D ~%" (abs -7))
(repl1 'abs)
(format t "(RIGHT-TRI 3 4 5) => ~D ~%" (right-tri 3 4 5))
(format t "(RIGHT-TRI 1 2 3) => ~D ~%" (right-tri 1 2 3))
(repl3 'right-tri)
(format t "(NTH-FIBO 6) = > ~D ~%" (nth-fibo 6))
(format t "(NTH-FIBO 10) = > ~D ~%" (nth-fibo 10))
(repl1 'nth-fibo)
(format t "*** Required Functions*** ~%")
(format t "(PERFECTP 5) => ~D ~%" (perfect 5))
(format t "(PERFECTP 6) => ~D ~%" (perfect 6))
(format t "(PERFECTP 36) => ~D ~%" (perfect 36))
(format t "(PERFECTP 496) => ~D ~%" (perfect 496))
(repl1 'perfect)
(format t "(ABUNDANTP 5) => ~D ~%" (abundant 5))
(format t "(ABUNDANTP 12) => ~D ~%" (abundant 12))
(repl1 'abundant)
(format t "(DEFICIENTP 5) => ~D ~%" (deficient 5))
(format t "(DEFICIENTP 12) => ~D ~%" (deficient 12))
(repl1 'deficient)
(format t "*** Finished Testing! ****")


