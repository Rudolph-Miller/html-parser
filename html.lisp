
(defmacro ncons (lst1 lst2)
  `(setf ,lst2 (cons ,lst1 ,lst2)))

(defun split (key line)
  (let ((result nil))
	(declare (list result))
	(loop
	  for pos = (position key line)
	  do (ncons (the string (subseq line 0 pos)) result)
	  while pos
	  do (setf line (subseq line (1+ pos))))
	(nreverse result)))
 

(defmacro setappend (lst result)
  `(setf ,result (cons ,lst ,result)))

(defun get-tag (str)
  (let ((pos-< (position #\< str))
		(pos-> (position #\> str)))
	(if (and (not (null pos-<)) (not (null pos->)))
	  (let ((tag (subseq str pos-< (1+ pos->))))
		(list tag pos-< pos->)))))

(defun tag-p (tag)
  (if (and (equal (subseq tag 0 1) "<") (equal (subseq tag (1- (length tag)) (length tag)) ">"))
	t nil))

(defun end-p (tag)
  (if (equal (subseq tag 1 2) "/")
	t nil))

(defun br-p (tag)
  (if (equal (subseq tag 1 3) "br")
	t nil))

(defun doctype-p (tag)
  (if (find "DOCTYPE" tag :test #'equal)
	t nil))

(defun parse-tag (str)
  (let ((result nil))
  (loop
	for tag = (get-tag str)
	while tag
	do (if (not (eql (cadr tag) 0))
		 (progn (setappend (subseq str 0 (cadr tag)) result)
				(setappend (car tag) result))
		 (setappend (car tag) result))
	do (setf str (subseq str (1+ (caddr tag)))))
  (nreverse result)))

(defun content-p (txt)
  (if (not (equal (subseq txt 0 1) "<"))
	t nil))


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
	((content-p txt) 'content)
	((tag-p txt)
	 (cond 
	   ((end-p txt) 'end)
	   (t 'beg)))
	(t 'unknown)))

(defun remove-space (lst)
  (remove-if #'(lambda (item) (< (char-code (char item 0)) 33)) lst))

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
		 ((eql tag 'content)
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
		  (list :tag-content (cdr tag-content))
		  (list :content content))))

(defun get-a-tag (lst)
  (remove-if #'null (mapcar #'(lambda (item) (if (eq (car item) ':a)
							 item))
		  lst)))

(defun get-href (lst)
  (mapcar #'caar (mapcar #'cdadr (get-a-tag lst))))

(defun get-link (lst)
  (mapcar #'(lambda (item) (subseq item 6 (1- (length item)))
		  (get-href lst)))

;(print (parse (remove-space (parse-tag "<a href=\"http\">Tap</a> <script ></script>"))))
(print (get-link (parse (input->html->list "../test/test1.html"))))





