
(defun MAKETREE (e a b)
 	
 (cons e 
	   (cons ;;construct a new list
			(cons a (cons b nil) )  ;; append a to b
		 nil)
	   )

 )

(defun ROOT (TREE) 
	
	(car TREE) ;; the head of list
)

(defun LEFT (TREE)
	(car (car (cdr TREE)) )
)

(defun RIGHT (TREE)
	(car (cdr (car (cdr TREE)) ))
)

(defun ISEMPTY (TREE)
	(if TREE   
		nil	 ;;if tree is a nil then returns nil, nil represents FALSE 
		t  	 ;;else returns 1 (TRUE)
		) 
)
(defun ISLEAF (TREE)
	(and 
	 (ISEMPTY (LEFT TREE)) 
	 (ISEMPTY (RIGHT TREE))	 
		 )
)

(defun insert (TREE x)
	( if (ISEMPTY TREE)
	 	(MAKETREE x nil nil)
	 		( if (>= (ROOT TREE) x)
		 		( MAKETREE (ROOT TREE) (INSERT (LEFT TREE) x) (RIGHT TREE) )
		 		( MAKETREE (ROOT TREE) (LEFT TREE ) (INSERT (RIGHT TREE) x) )
		 	)
	 )
)

(defun list-to-tree (mylist &optional(tree nil) ) 
	(if mylist  
		(list-to-tree  (cdr mylist) (insert tree (car mylist)))  ;;if mylist is not nil, recursively insert the first element to tree
		tree ;;if mylist is nill, return tree
	)	
)

(defun MERGELIST (listA listB &optional(result nil)) ;;this func merges listA and ListB, starting from listA
	(if listA 
		(MERGELIST (cdr listA) listB (cons (car listA) result))
		(if listB 
			(MERGELIST listA (cdr listB) (cons (car listB) result))
			(reverse result) 
		)
	)
 )

(defun inorder (tree)
	(if tree
		 ( if (ISLEAF tree)
			(cons (ROOT tree) nil)
				( if (ISEMPTY (LEFT tree) )
					(cons (ROOT tree) (inorder (RIGHT tree) ) )
						(if (ISLEAF (LEFT tree))
							(cons (ROOT (LEFT tree)) (cons (ROOT tree) (inorder (RIGHT tree))))
								(MERGELIST (inorder (LEFT tree)) (cons (ROOT tree) (inorder (RIGHT tree))))
						)
				  )
			)
		 
		nil
	)
)
;;(print (if (ISEMPTY nil) 1 2))
(print (insert (MAKETREE 2 NIL NIL) 1))
;;(print  (insert (MAKETREE 1 (MAKETREE 2 NIL NIL) (MAKETREE 3 NIL NIL )) 4) )
;;(print  (insert (insert (insert nil 2)  1 ) 3 ) )

;;(print (MERGELIST (list 1 2 3 ) (list 4 5 6)))
;;(print (list-to-tree (list 4 6 2 0 2 8 2) ) )

;;(print (inorder (MAKETREE 2 (MAKETREE 1 NIL NIL) (MAKETREE 3 NIL NIL ))))
;;(print (inorder	(list-to-tree (list 1 2 3 5 4)))   )
(print
 	(RIGHT (MAKETREE 2 (MAKETREE 1 NIL NIL) (MAKETREE 3 NIL NIL ))))

