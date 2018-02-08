;; P01
(defun my-last (xlist)
  (car (reverse xlist)))

;; P02
(defun my-but-last (x)
  (cadr (reverse x)))

;; P03
(defun element-at (x n)
  (if (= n 1)
	(car x)
	(element-at (cdr x) (1- n))))

;; P04
(defun my-length (x)
  (if x
    (1+ (my-length (cdr x)))
    0))

;; P05
(defun my-reverse (x)
  (let ((xrev ()))
	(dolist (y x xrev) (push y xrev))))

;; P 06
(defun palindrome-p (x)
  (every
    (lambda (pair) (eql (car pair) (cdr pair)))
	  (mapcar #'cons x (reverse x))))

;; P07
(defun my-flatten (x)
  (cond ((not x) nil)
	    ((listp (car x)) (append (my-flatten (car x)) (my-flatten (cdr x))))
		(t (append (list (car x)) (my-flatten (cdr x))))))

;; P08
(defun my-compress (x)
  (cond ((eql x nil) nil)
        ((eql (car x) (cadr x)) (my-compress (cdr x)))
		(t (append (list (car x)) (my-compress (cdr x))))))

;; P09
(defun pack (x)
  (let ((acc () ))
  (loop for y in x
    collect ( cond ((eql y nil) nil)
 	               ((eql y (caar acc)) (push y (car acc)))
	               (t (push (list y) acc)) ) )
  (reverse acc)))

;; P10
(defun encode (x)
  (mapcar (lambda (y) (list (length y) (car y))) (pack x)))

;; P11
(defun encode-modified (x)
  (mapcar (lambda (y)
			(if (= (car y) 1)
			  (cadr y)
			  y)) (encode x)))

;; P12
(defun decode (x)
  (my-flatten (mapcar (lambda (y)
                    (if (atom y)
	                  y
	                  (loop for i from 1 to (car y)
	   	                collect (cadr y)))) x)))

;; P13
(defun encode-direct (x)
  (labels
	((count (x xs n)
      (cond ((null x) nil)
	        ((eq x (car xs)) (count (car xs) (cdr xs) (+ n 1)))
	        (t (append (list (cons x n)) (count (car xs) (cdr xs) 1))))))
	 (count (car x) (cdr x) 1)))

;; P14
(defun dupli (x)
  (mapcan (lambda (y) (list y y)) x))

;; P15
(defun repli (x n)
  (labels ((repsymb (y m)
    (if (> m 0)
      (append (list y) (repsymb y (- m 1)))
      nil)))
  (mapcan (lambda (z) (repsymb z n)) x)))

;; P16
(defun drop (x n)
  (labels ((droponzero (x m)
    (cond ((null x) nil)
          ((zerop m) (droponzero (cdr x) (1- n)))
          (t (append (list (car x)) (droponzero (cdr x) (1- m)))))))
  (droponzero x (1- n))))

;; P17
(defun split (x n)
  (labels ((trailsplit (x n)
    (if (zerop n)
      (cons nil x)
      (let ((parts (trailsplit (cdr x) (1- n))))
        (cons (append (list (car x)) (car parts)) (cdr parts))))))
  (let ((result (trailsplit x n)))
    (list (car result) (cdr result)))))
    
;; P18
(defun slice (x a b)
  (loop for y in x
        for i from 1
        when (and (>= i a) (<= i b))
        collect y))

;; P19
(defun rotate (x n)
  (let ((spl (split x (mod n (length x)))))
    (append (cadr spl) (car spl))))

;; P20
(defun remove-at (x n)
  (loop for y in x
       for i from 1
       when (not (eq i n))
       collect y))

;; P21
(defun insert-at (e x n)
  (loop for y in x
        for i from 1
        append (if (eq i n)
               (list e y)
               (list y))))

;; P22
(defun range (a b)
  (loop for x from a to b
    collect x))

;; P23
(defun rnd-select (x n)
  (let ((num (if x (random (length x)) 0)))
  (if (zerop n)
      ()
      (append (list (nth num x)) (rnd-select (remove-at x (1+ num)) (1- n))))))

;; P24
(defun lotto-select (n m)
  (rnd-select (range 1 m) n))

;; P25
(defun rnd-permu (x)
  (rnd-select x (length x)))

;; P26
(defun combination (n x)
  (if (zerop (1- n))
    (mapcar #'list x)
    (loop for xx on x
          when (>= (length xx) n)
          append (mapcar (lambda (y)
                            (append (list (car xx)) y)) (combination (1- n) (cdr xx))))))
;; P27
(defun group (x n)
  (if (null n)
     x
     (flet ((rest (y) (set-difference x y)))
       (mapcar (lambda (y) (concatenate 'list (list y) (group (rest y) (cdr n)))) (combination (car n) x)))))

;; P28 a
(defun lsort (x)
  (sort x (lambda (a b) (< (length a) (length b)))))

;; P28 b
(defun partition (x fn)
  (let ((tl ()) (fl ()))
  (loop for e in x
   do (push e (if (funcall fn e) tl fl)))
   (list tl fl)))

(defun lsort (x)
  (labels ((freq (z)
    (if z
      (let ((p (partition z (lambda (y) (equal (car z) y)))))
         (cons (cons (caar p) (length (car p))) (freq (cadr p))))
      ())))
  (let ((freqlist (sort (freq (mapcar #'length x)) (lambda (a b) (< (cdr a) (cdr b))))))
    (mapcan (lambda (y) (remove-if-not (lambda (z) (equal (car y) (length z))) x)) freqlist))))

;; P31
(defun is-prime (n)
  (let ((nums (make-array (1+ n))))
  (flet ((mark (x)
    (loop for i from (+ x x) to n by x
      do (setf (aref nums i) 't))))
  (loop for i from 2 to n
    when (null (aref nums i))
    do (mark i))
  (null (aref nums n)))))

;; P32
(defun my-gcd (m n)
  (if (zerop (mod m n))
    n
    (my-gcd n (mod m n))))

;; P33
(defun coprime (m n)
  (= 1 (my-gcd m n)))

;; P34
(defun totient-phi (n)
  (length (remove-if-not (lambda (m) (coprime m n)) (loop for i from 1 to n collect i))))

;; P35
(defun prime-factors (n)
  (loop for i from floor(sqrt(n)) downto 2
    when (zerop (mod n i))
    return (i 
