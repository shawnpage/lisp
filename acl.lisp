
(defun our-third (x)
  (car (cdr (cdr x))))

(defun our-fourth (x)
  (car (cdr (cdr (cdr x)))))

(defun greater-than (x y)
  (if (> x y)
      x
      y))

(defun sum-greater (x y z)
  (> (+ x y) z))

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun has-list (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
	  T
	  (has-list (cdr lst)))))

(defun has-list-short (lst)
  (if (listp (car lst))
      T
      (has-list (cdr lst))))

(defun ask-em (string)
  (format t "~A " string)
  (read))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun recursive-show-squares (i end)
  (if (> i end)
      'done
      (progn
	(format t "~A ~A~%" i (* i i))
	(show-squares (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun recursive-our-length (lst)
  (format t "~A~%" lst)
       (if (null lst)
	   0
	   (+ (recursive-our-length(cdr lst)) 1)))

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))
