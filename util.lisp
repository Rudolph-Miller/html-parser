(in-package html)

;;;(setf let2 (cons lst1 lst2))
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
 

