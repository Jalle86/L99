(defun my-last (xlist)
  (car (reverse xlist)))

(defun my-but-last (x)
  (cadr (reverse x)))

(defun element-at (x n)
  (if (= n 1)
	(car x)
	(element-at (cdr x) (1- n))))

(defun my-length (x)
  (if x
    (1+ (my-length (cdr x)))
    0))

(defun my-reverse (x)
  (let ((xrev ()))
	(dolist (y x xrev) (push y xrev))))

(defun palindrome-p (x)
  (every
    (lambda (pair) (eql (car pair) (cdr pair)))
	  (mapcar #'cons x (reverse x))))

(defun my-flatten (x)
  (cond ((not x) nil)
	    ((listp (car x)) (append (my-flatten (car x)) (my-flatten (cdr x))))
		(t (append (list (car x)) (my-flatten (cdr x))))))

(defun my-compress (x)
  (cond ((eql x nil) nil)
        ((eql (car x) (cadr x)) (my-compress (cdr x)))
		(t (append (list (car x)) (my-compress (cdr x))))))

(defun pack (x)
  (let ((acc () ))
  (loop for y in x
    collect ( cond ((eql y nil) nil)
 	               ((eql y (caar acc)) (push y (car acc)))
	               (t (push (list y) acc)) ) )
  (reverse acc)))

(defun encode (x)
  (mapcar (lambda (y) (list (length y) (car y))) (pack x)))

(defun encode-modified (x)
  (mapcar (lambda (y)
			(if (= (car y) 1)
			  (cadr y)
			  y)) (encode x)))

(defun decode (x)
  (my-flatten (mapcar (lambda (y)
                    (if (atom y)
	                  y
	                  (loop for i from 1 to (car y)
	   	                collect (cadr y)))) x)))

(defun encode-direct (x)
  (let ((count nil))
	(labels ((theloop (acc y) 
					  (cond ((eq y nil) acc)
							((eq y (car y)) () acc))
							(t (push count acc))))
	  (reduce #'theloop x :initial-value nil))))
