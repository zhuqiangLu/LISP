(defunMAKETREE  ( e   a  b )(conse(cons(consa   (consb   n i l )   )n i l )))(defunROOT  (TREE)( c a r  TREE))(defunLEFT  (TREE)( c a r   ( c a r   ( c d r  TREE) )   ))(defunRIGHT  (TREE)( c a r   ( c d r   ( c a r   ( c d r  TREE) )   ) ))(defunISEMPTY  (TREE)( i f   TREEn i lt))(defunISLEAF   (TREE)(and(ISEMPTY  (LEFT  TREE) )(ISEMPTY  (RIGHT  TREE) )))(defuni n s e r t   (TREE  x )(   i f   (ISEMPTY  TREE)(MAKETREE  x   n i l   n i l )(   i f   (>=  (ROOT TREE)   x )(  MAKETREE  (ROOT TREE)   (INSERT  (LEFT  TREE)   x )   (RIGHT  TREE)   )