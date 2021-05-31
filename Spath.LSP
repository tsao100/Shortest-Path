; Dijkstra Algorithm implementation. Finish on 2020/11/29.
;Test ok in AutoCAD Release 9.03i which was learned in NTU 1991.

(setq pi 3.14159265355879353846)

(defun degrees (angle)
  (* 180 (/ angle pi)))

(setq nodeQ nil
      inf 999999
      map nil
      dis nil
      vis nil
      path nil
      pts '()
      s nil  
      e nil)

(defun c:MakeNode()
    (setq i '0)
    (setq pts '())
    (while (setq pt (getpoint (strcat "Node " (itoa (1+ i)) ", Finish<Enter>: \n")))
	(progn
	    (command "circle" pt '10)
	    (command "text" pt "10" "0" (itoa (setq i (1+ i))))
	    (setq num (entget (entlast)))
	    (setq num (subst '(72 . 4) '(72 . 0) num))
	    (setq dxf11 (assoc '11 num))
	    (setq ins (subst '11 '10 (assoc '10 num)))
	    (if (= "13" (getvar "acadver"))
		(setq num (append num (list ins)))
	    	(setq num (subst ins dxf11 num)))
	    (entmod num)
	    (setq pts (append pts (list (cons i pt))))	
	)
    )
    (setq nodeQ i)
    (initMap i)
    (princ)
)

(defun mid(pt1 pt2)
(list (/ (+ (car pt1) (car pt2)) 2) (/ (+ (cadr pt1) (cadr pt2)) 2))
)


(defun c:Makelink()
    (setq osmode (getvar "osmode"))
    (setvar "osmode" 0)
    (while (setq text1 (entsel "Select First Node number: \n"))
	(progn
	    (setq pt1 (cdr (assoc '11 (entget (car text1)))))
	    (setq text2 (entsel "Select Second Node number: \n"))
	    (setq pt2 (cdr (assoc '11 (entget (car text2)))))
	    (command "line" (polar pt1 (angle pt1 pt2) 10) (polar pt2 (angle pt2 pt1) 10) "")
	    (command "text" (mid pt1 pt2) "8" (degrees (angle pt1 pt2)) (rtos (distance pt1 pt2) '2 '1))
	    (setq num (entget (entlast)))
	    (setq num (subst '(72 . 1) '(72 . 0) num))
	    (setq num (subst '(73 . 1) '(73 . 0) num))
	    (setq dxf11 (assoc '11 num))
	    (setq ins (subst '11 '10 (assoc '10 num)))
	    (if (= "13" (getvar "acadver"))
		(setq num (append num (list ins)))
	    	(setq num (subst ins dxf11 num)))
	    (entmod num)
	    (setq m (1- (atoi (cdr (assoc '1 (entget (car text1))))))	
	          n (1- (atoi (cdr (assoc '1 (entget (car text2)))))))	
	    (setq map (SetAt2d (distance pt1 pt2) m n map))
	    (setq map (SetAt2d (distance pt1 pt2) n m map));;; Graph without direction.
	    (command "text" '(10000 10000) "3.1415" "0" (strcat "(" (itoa m) " " (itoa n) ")")) ; save link
	)
    )
    (setvar "osmode" osmode)
    (princ)
)

;; Replaces the item at specified index by the new one

(defun SubstAt (new ind lst)
    (if (or (zerop ind) (null lst))
	(cons new (cdr lst))
	(cons (car lst) (SubstAt new (1- ind) (cdr lst)))
    )
)

(defun initMap(Nodes)
    (setq a nil
          map nil)
    (repeat Nodes (setq a (cons inf a)))
    (repeat Nodes (setq map (cons a map)))
    (setq i 0)
    (repeat Nodes (progn (setq map (SetAt2d 0 i i map)) (setq i (1+ i))))
)


(defun SetAt2d (new m n array) ; array: m x n Array.
    (substat (substat new n (nth m array)) m array)
)

(defun GetAt2d (m n array) ; array: m x n Array.
    (nth n (nth m array))
)

(defun dijkstra(nodeQ)
    (setq i 0 j 0 pos 0)
    (setq dis (nth s map)) 
    (setq vis (substat '1 s vis))  
    (while (< i (1- nodeQ))
	(setq pos s
	      minimum inf
	      j 0)
	(while (< j nodeQ)
	    (if (and (not (= 1 (nth j vis))) (< (nth j dis) minimum))	
        	(progn  (setq minimum (nth j dis)) (setq pos j))
	    (setq j (1+ j))))
	(setq vis (substat '1 pos vis))

	(setq j 0)
        (while (< j nodeQ)
            (if (< (GetAt2d j pos map) inf)
		    (if (> (nth j dis) (+ (nth pos dis) (GetAt2d j pos map)))
		      (progn
                (setq dis (substat (+ (nth pos dis) (GetAt2d j pos map)) j dis))
		(setq path (substat (append (nth pos path) (list (1+ pos))) j path)))))
            (setq j (1+ j)))
	(setq i (1+ i)))
  	(setq i 0)
  	(repeat nodeQ (progn (if (/= i s)
		(setq path (substat (append (nth i path) (list (1+ i))) i path)))	
		(setq i (1+ i))))
    (if (/= (nth e dis) inf)
	(progn
            (print (nth e dis))
	    (command "color" "yellow")
	    (command "pline" (foreach ptn (nth e path) (command (cdr (assoc ptn pts)))))
	    (command "color" "bylayer")
	    (command "pedit" (entlast) "w" 4 "x")
	)
        (print "-1\n")))

(defun c:spath()
	(if (and (= map nil) (entnext) ) (fdata))
	(if (= pts nil) (progn (print "There is no map, make it step by step...") 
		(command "pline" "0,0" "600,0" "600,450" "0,450" "c" "zoom" "all")
		(c:makenode) (c:makelink)))
	(setq vis nil)
	(repeat nodeQ (setq vis (cons '0 vis)))
        (setq s (1- (atoi (cdr (assoc '1 (entget (car (entsel "start node: \n"))))))))
        (setq e (1- (atoi (cdr (assoc '1 (entget (car (entsel "end node: \n"))))))))
  	(setq path nil)
	(repeat nodeQ (setq path (cons (list (1+ s)) path)))
	(dijkstra nodeQ)
	(setq ss (ssadd))
  	(ssadd (entlast) ss)
  	(sssetFirst nil ss))

(defun fdata() 
	;Restore pts
    (setq pts '()) 
    (setq a (entnext))
    (while (setq a (entnext a))
	(if (= (cdr (assoc '0 (entget a))) "TEXT")
	    (if (= (cdr (assoc '40 (entget a))) 10)
		(setq pts (append pts (list (cons  (atoi (cdr (assoc '1 (entget a))))
			 (cdr (assoc '11 (entget a))))))))))
    (setq nodeQ (length pts))
    (initMap nodeQ)
	;Restore link from text @(10000, 10000) texthight=3.1415
    (setq a (entnext)) 
    (while (setq a (entnext a))
	(if (= (cdr (assoc '0 (entget a))) "TEXT")
	    (if (= (rtos (cdr (assoc '40 (entget a))) 2 4) "3.1415")
		(progn 
		    (setq link (read (cdr (assoc '1 (entget a)))))
		    (setq m (car link))
		    (setq n (cadr link))
		    (setq pt1 (cdr (assoc (1+ m) pts)))
		    (setq pt2 (cdr (assoc (1+ n) pts)))
		    (setq map (SetAt2d (distance pt1 pt2) m n map))
		    (setq map (SetAt2d (distance pt1 pt2) n m map)))))))
