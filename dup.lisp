
(defun MAKETREE (e a b)
 	
 (cons e 
	   (cons 
			(cons a (cons b nil) )  
		 nil)
	   )

 )

(defun ROOT (TREE) 
	
	(car TREE) 
)

(defun LEFT (TREE)
	(car (car (cdr TREE)) )
)

(defun RIGHT (TREE)
	(car (cdr (car (cdr TREE)) ))
)

(defun ISEMPTY (TREE)
	(if TREE   
		nil	 
		t  	 
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
		(list-to-tree  (cdr mylist) (insert tree (car mylist)))  
		tree 
	)	
)

(defun MERGELIST (listA listB &optional(result nil)) 
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

