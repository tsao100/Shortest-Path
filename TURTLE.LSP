;
; Turtle Graphics
; from Lisp in Small Parts, http://lisp.plasticki.com/
; Licensed under CC0 1.0: http://creativecommons.org/publicdomain/zero/1.0/
; 30/6/2012
;
;(while (setq a (entnext)) (entdel a))
;(command "pline" "0,0" "640,0" "640,480" "0,480" "c" "zoom" "all")

(setq pi 3.14159265355879353846)
(defun radians (angle)
  (* pi (/ angle 180.0)))

(defun degrees (angle)
  (* 180 (/ angle pi)))

(setq turtle-pane nil)
(setq turtle-x 320)
(setq turtle-y 240)
(setq turtle-theta (radians 90))
(setq turtle-draw t)

(defun land()
(setq turtle-y 0)
)

(defun reset ()
  (while (setq a (entnext)) (entdel a))
  (command "pline" "0,0" "640,0" "640,480" "0,480" "c" "zoom" "all")
  (setq turtle-x 320)
  (setq turtle-y 240)
  (setq turtle-theta (radians 90))
  (setq turtle-draw t))

(defun forward (length)
  (setq new-x (+ turtle-x (* length (cos turtle-theta)))
        new-y (+ turtle-y (* length (sin turtle-theta))))
    (if turtle-draw
	(progn
        (command "line" (list turtle-x turtle-y) (list new-x new-y) "")
    (setq turtle-x new-x)
    (setq turtle-y new-y))))

(defun backto (length)
  (setq new-x (- turtle-x (* length (cos turtle-theta)))
        new-y (- turtle-y (* length (sin turtle-theta))))
    (setq turtle-x new-x)
    (setq turtle-y new-y))


(defun right (angle)
  (setq turtle-theta (- turtle-theta (radians angle))))

(defun back (length)
  (forward (- length)))

(defun left (angle)
  (right (- angle)))

(defun penup ()
  (setq turtle-draw nil))

(defun pendown ()
  (setq turtle-draw t))

;(defun plot ()
;  (capi:contain 
;   (make-instance 'capi:output-pane :display-callback 'draw) 
;   :best-width 480 :best-height 360))

(defun inspi (side angle inc count)
  (forward side)
  (right angle)
  (if (> count 0)
      (inspi side (+ angle inc) inc (- count 1))))

(defun ldragon (size level)
  (if (= level 0)
      (forward size)
    (progn
      (ldragon size (- level 1))
      (left 90)
      (rdragon size (- level 1)))))

(defun rdragon (size level)
  (if (= level 0)
      (forward size)
    (progn
      (ldragon size (- level 1))
      (right 90)
      (rdragon size (- level 1)))))

(defun c:turtle ()
  (reset)
  (ldragon 4 11))

(defun c:TreeGo ()
  ;(reset)
  (tree 185 11)
  (command "redraw")
)

(defun color (c)
    (command "color" c)
)


(defun tree(size level)
	(if (> level 0) (progn
	  (color (/ 15 level))
	  (forward size)
	  (left 30)
	  (tree (* size 0.618) (1- level))
	  ;(penup)
	  ;(back size)
	  (right 60)
	  ;(pendown)
	  (tree (* size 0.618) (1- level))
	  (left 30)
	  ;(penup)
	  (backto size)
	)
))

(defun tree1(size)
	(if (> size  5)
	(progn
	(forward (/ size 3.0))
	(left 30) (tree1 (* (/ size 3.0) 2.0)) (right 30)
	(forward (/ size 6.0))
	(right 25) (tree1 (/ size 2.0)) (left 25)           
	(forward (/ size 3.0))
	(right 25) (tree1 (/ size 2.0)) (left 25)
	(forward (/ size 6.0))
	(back size)))
)

(defun c:FlyTo()
	(setq turtle-theta (radians 90))
	(setq pt (getpoint "\nPoint: \n"))
	(setq turtle-x (car pt))
	(setq turtle-y (cadr pt)))

(load "c:/acad9/funshape")
;(load "c:/acad9/spath")
