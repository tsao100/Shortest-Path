(defun c:WhiteSun()
	(right 15)
	(repeat 12 
	(progn
	(forward 100)
	(right 150)
	(forward 100)
	(right -120))))

(defun c:Star()
	(right 90)
	(repeat 5 
	(progn
	(forward 400)
	(right 144)
	)))

(defun c:InfTriangle()
	(setq a 2)
	(repeat 273 
	(progn
	(forward (setq a (+ a 1.5)))
	(right 121)
	)))

(defun fern (size)
(if  (> size 4) (progn
(forward (/ size 1.35))
(left 90) (fern (* size 0.3))
(right 90)
(right 90) (fern (* size 0.3))
(left 90) (fern (* size 0.85))
(backto (/ size 1.35))
)))

(defun c:DrawFern()(fern 100))

(defun thing()
    (forward 100)
    (right 90)
    (forward 100)
    (right 90)
    (forward 50)
    (right 90)
    (forward 50)
    (right 90)
    (forward 100)
    (right 90)
    (forward 25)
    (right 90)
    (forward 25)
    (right 90)
    (forward 50)
)

(defun c:THING1()
    (c:flyto)
    (repeat 4 (thing))
)

(defun c:THING2()
    (c:flyto)
    (repeat 9 (progn
	(thing)
	(right 10)
	(forward 50)))
)

(defun c:THING3()
    (c:flyto)
    (repeat 16 (progn
	(thing)
	(left 45)
	(forward 100)))
)
































