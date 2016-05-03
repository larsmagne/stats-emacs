
(defun stats-emacs-get ()
  (apply 'debbugs-get-status (debbugs-get-bugs :archive "both")))

(defun stats-emacs-filter (data)
  (seq-filter (lambda (elem)
		(let ((merged (cdr (assq 'mergedwith elem))))
		  (and (member "emacs" (cdr (assq 'package elem)))
		       ;;(not (equal (cdr (assq 'severity elem)) "wishlist"))
		       (or (null merged)
			   (< (cdr (assq 'id elem))
			      (if (numberp merged)
				  merged
				(seq-min merged)))))))
	      data))

(defun stats-emacs-sort (data &optional modified)
  (seq-sort (lambda (a1 a2)
	      (let ((key (if modified
			     'last_modified
			   'date)))
		(< (cdr (assq key a1))
		   (cdr (assq key a2)))))
	    (copy-list data)))

(defvar stats-emacs-get nil)

(defun stats-emacs ()
  (interactive)
  (stats-emacs-generate (stats-emacs-filter
			 (stats-emacs-sort
			  (setq stats-emacs-cache (stats-emacs-get))))))

(defun stats-emacs-generate (data)
  (with-temp-buffer
    (insert "emacsData = [[\"Date\", \"Open\"],\n")
    (let ((date (stats-emacs-date (cdr (assq 'date (car data)))))
	  (all-data data)
	  (opened 0))
      (while data
	(while (and data
		    (<= (stats-emacs-date (cdr (assq 'date (car data))))
			date))
	  (incf opened)
	  (pop data))
	(stats-emacs-line date all-data opened)
	(message "%s" date)
	(setq date nil)
	(when data
	  (setq date (stats-emacs-date (cdr (assq 'date (car data)))))))
      (when date
	(stats-emacs-line date all-data opened)))
    (search-backward ",")
    (delete-char 1)
    (insert "];")
    (write-region (point-min) (point-max) "~/src/stats-emacs/stats-emacs.js")))

(defun stats-emacs-line-all (date all-data opened)
  (let ((closed (stats-emacs-closed all-data date)))
    (insert (format "[new Date(%d, %d, %d), %d, %d, %d],\n"
		    (/ date 10000)
		    (1- (mod (/ date 100) 100))
		    (mod date 100)
		    (- opened closed)
		    opened
		    closed))))

(defun stats-emacs-line (date all-data opened)
  (let ((closed (stats-emacs-closed all-data date)))
    (insert (format "[new Date(%d, %d, %d), %d],\n"
		    (/ date 10000)
		    (1- (mod (/ date 100) 100))
		    (mod date 100)
		    (- opened closed)))))

(defun stats-emacs-closed (data date)
  (let ((close 0))
    (dolist (elem data)
      (when (and (<= (stats-emacs-date (cdr (assq 'last_modified elem)) t)
		     date)
		 (equal (cdr (assq 'pending elem)) "done"))
	(incf close)))
    close))

(defun stats-emacs-date (time &optional close)
  (let ((month (* 30 60 60 24)))
    ;; If it's more than a month in the past, deduct the "archive"
    ;; action.
    (when (and close
	       (or (> (- (float-time) time) month)
		   (zerop (random 2))))
      (setq time (- time month))))
  (string-to-number (format-time-string "%Y%m%d" time)))

