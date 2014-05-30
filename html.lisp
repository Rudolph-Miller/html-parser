(defpackage html
  (:use :common-lisp)
  (:export :get-tag
		   :pic-cont
		   :spec-in-tag-list
		   :spec))

(load "util")

(in-package html)

;;;str -> (list "<a>" tag-position )
(defun get-tag (str)
  (declare (string str))
  (let ((pos-< (position #\< str))
		(pos-> (position #\> str)))
	(if (and (not (null pos-<)) (not (null pos->)))
	  (let ((tag (subseq str pos-< (1+ pos->))))
		(the list (list tag pos-< pos->))))))

(defun tag-p (tag)
  (if (and (equal (subseq tag 0 1) "<") (equal (subseq tag (1- (length tag)) (length tag)) ">"))
	t nil))

(defun end-p (tag)
  (if (equal (subseq tag 1 2) "/")
	t nil))

(defun content-p (txt)
  (if (not (equal (subseq txt 0 1) "<"))
	t nil))

;;;str -> (tag list ..)
(defun parse-tag (str)
  (let ((result nil))
  (loop
	for tag = (get-tag str)
	while tag
	do (if (not (eql (cadr tag) 0))
		 (progn (ncons (subseq str 0 (cadr tag)) result)
				(ncons (car tag) result))
		 (ncons (car tag) result))
	do (setf str (subseq str (1+ (caddr tag)))))
  (nreverse result)))

;;;input -> list
(defun input->html->list (input)
  (with-open-file (f input :direction :input)
	(let ((result nil))
  (loop
		for line = (read-line f nil)
		while line
		do (setf result (concatenate 'list result (parse-tag line))))
	  (remove-if #'null result))))


(defun categorize (txt)
  (cond
	((content-p txt) 'cont)
	((tag-p txt)
	 (cond 
	   ((end-p txt) 'end)
	   (t 'beg)))
	(t 'unknown)))

(defun remove-space (lst)
  (remove-if #'(lambda (item) (< (char-code (char item 0)) 33)) lst))

;;;tag-countがnのときにconsみたいにすれば階層づけできる。
;;;or end のときにtag-count -1 にconsするとか
(defun parse (html-list)
  (let ((result nil)
		(tag-count 0)
		(acc (make-hash-table :test #'eq)))
  (loop
	for line = (car html-list)
	while line
	for tag = (categorize line)
	do (cond
		 ((eql tag 'beg)
		  (setf tag-count (1+ tag-count)) 
		  (push line (gethash tag-count acc)))
		 ((eql tag 'cont)
		  (push line (gethash tag-count acc)))
		 ((eql tag 'end)
		  (push line (gethash tag-count acc))
		  (ncons (nreverse (gethash tag-count acc)) result)
		  (setf (gethash tag-count acc) nil)
		  (setf tag-count (1- tag-count)))
		 (t nil))
	do (setf html-list (cdr html-list)))
  (mapcar #'formatting (nreverse result))))

(defun get-tag-contente (tag)
  (split #\Space (subseq tag 1 (1- (length tag)))))

(defun formatting (tag-list)
  (let ((tag-content (get-tag-contente (car tag-list)))
		(content (if (not (eq (length tag-list) 2))
				   (cadr tag-list)
				   nil)))
	(list (read-from-string (concatenate 'string ":" (car tag-content)))
		  (cons :tag-cont (cdr tag-content))
		  (cons :cont content))))

(defun get-a-tag (lst)
  (remove-if #'null (mapcar #'(lambda (item) (if (eq (car item) ':a)
							 item))
		  lst)))

(defun get-href (lst)
  (mapcar #'caar (mapcar #'cdadr (get-a-tag lst))))

(defun get-link (lst)
  (mapcar #'(lambda (item) (subseq item 6 (1- (length item))))
		  (get-href lst)))

;;;tag = :tag
;;;input = file name
(defun spec (tag input)
  (let ((lst (parse (input->html->list input))))
	(remove-if-not #'(lambda (lst) (eql (car lst) tag))
				   lst)))

(defun spec-in-tag-list (tag-list input)
  (let ((lst (parse (input->html->list input))))
		(remove-if-not #'(lambda (lst) (find (car lst) tag-list))
					   lst)))

(defun pic-cont (tag-list input)
  (let ((lst (parse (input->html->list input))))
	(apply #'concatenate 'string (mapcar #'(lambda (lst) (cdaddr lst))
			(remove-if-not #'(lambda (lst) (find (car lst) tag-list))
						   lst)))))

