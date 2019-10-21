;;; stats-emacs.el --- computing Emacs bug statistics
;; Copyright (C) 2016 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; stats-emacs.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stats-emacs.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:


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
	      (< (stats-emacs-date a1 modified)
		 (stats-emacs-date a2 modified)))
	    (copy-list data)))

(defvar stats-emacs-get nil)

(defun stats-emacs ()
  (interactive)
  (stats-emacs-generate (stats-emacs-filter
			 (stats-emacs-sort
			  (setq stats-emacs-cache (stats-emacs-get))))))

(defun stats-emacs-regenerate ()
  (interactive)
  (stats-emacs-generate (stats-emacs-filter
			 (stats-emacs-sort stats-emacs-cache))))

(defun stats-emacs-computed-closed (data)
  (setq data (stats-emacs-sort data t))
  (let ((date (stats-emacs-date (car data) t))
	(severities (make-hash-table :test #'equal))
	(tags (make-hash-table :test #'equal))
	(total (make-hash-table))
	(closed 0))
    (while data
      (while (and data
		  (<= (stats-emacs-date (car data) t) date))
	(when (equal (cdr (assq 'pending (car data))) "done")
	  (incf closed)
	  (incf (gethash (cdr (assq 'severity (car data))) severities 0))
	  (dolist (tag (cdr (assq 'tags (car data))))
	    (incf (gethash tag tags 0))))
	(pop data))
      (setf (gethash date total)
	    (list :closed closed
		  :severities (stats-emacs-hash-to-alist severities)
		  :tags (stats-emacs-hash-to-alist tags)))
      (setq date nil)
      (when data
	(setq date (stats-emacs-date (car data) t))))
    (when date
      (setf (gethash date total)
	    (list :closed closed
		  :severities (stats-emacs-hash-to-alist severities)
		  :tags (stats-emacs-hash-to-alist tags))))
    total))

(defun stats-emacs-hash-to-alist (hash)
  (let ((alist nil))
    (maphash (lambda (key val)
	       (push (cons key val) alist))
	     hash)
    alist))

(defun stats-emacs-generate (data)
  "Generate the .js stats file based on DATA in the slowest way imaginable."
  (with-temp-buffer
    (insert "emacsData = [[\"Date\", \"Open\", \"Opened\", \"Closed\", \"Critical\", \"Important\", \"Normal\", \"Minor\", \"Wishlist\", \"Patch\", \"Moreinfo\", \"Wontfix\"],\n")
    (let ((date (stats-emacs-date (car data)))
	  (closed-data (sort
			(stats-emacs-hash-to-alist
			 (stats-emacs-computed-closed data))
			(lambda (e1 e2)
			  (< (car e1) (car e2)))))
	  (all-data data)
	  (severities (make-hash-table :test #'equal))
	  (tags (make-hash-table :test #'equal))
	  (opened 0))
      (while data
	(while (and data
		    (<= (stats-emacs-date (car data)) date))
	  (incf opened)
	  (incf (gethash (cdr (assq 'severity (car data))) severities 0))
	  (dolist (tag (cdr (assq 'tags (car data))))
	    (incf (gethash tag tags 0)))
	  (pop data))
	(stats-emacs-line-all date opened severities tags closed-data)
	(message "%s" date)
	(setq date nil)
	(when data
	  (setq date (stats-emacs-date (car data)))))
      (when date
	(stats-emacs-line-all date opened severities tags closed-data)))
    (search-backward ",")
    (delete-char 1)
    (insert "];")
    (write-region (point-min) (point-max)
		  "~/src/stats-emacs/stats-emacs-all.js")))

(defun stats-emacs-line-all (date opened severities tags closed-data)
  (let ((cd (stats-emacs-closed date closed-data)))
    (insert (format "[new Date(%d, %d, %d), %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d],\n"
		    (/ date 10000)
		    (1- (mod (/ date 100) 100))
		    (mod date 100)
		    (- opened (getf cd :closed))
		    opened
		    (getf cd :closed)
		    (- (gethash "critical" severities 0)
		       (or (cdr (assoc "critical" (getf cd :severities)))
			   0))
		    (- (gethash "important" severities 0)
		       (or (cdr (assoc "important" (getf cd :severities)))
			   0))
		    (- (gethash "normal" severities 0)
		       (or (cdr (assoc "normal" (getf cd :severities)))
			   0))
		    (- (gethash "minor" severities 0)
		       (or (cdr (assoc "minor" (getf cd :severities)))
			   0))
		    (- (gethash "wishlist" severities 0)
		       (or (cdr (assoc "wishlist" (getf cd :severities)))
			   0))
		    (- (gethash "patch" tags 0)
		       (or (cdr (assoc "patch" (getf cd :tags)))
			   0))
		    (- (gethash "moreinfo" tags 0)
		       (or (cdr (assoc "moreinfo" (getf cd :tags)))
			   0))
		    (- (gethash "wontfix" tags 0)
		       (or (cdr (assoc "wontfix" (getf cd :tags)))
			   0))
		    ))))

(defun stats-emacs-closed (date closed-data)
  (cl-loop with prev = (list :closed 0)
	   for elem in closed-data
	   when (>= (car elem) date)
	   return prev
	   do (setq prev (cdr elem))))

(defun stats-emacs-date (elem &optional close)
  (let ((open-time (cdr (assq 'date elem)))
	(close-time (cdr (assq 'last_modified elem))))
    (string-to-number
     (format-time-string
      "%Y%m%d" 
      (cond
       ((not close) open-time)
       ((equal (cdr (assq 'location elem)) "archive")
	(let ((month (* 30 60 60 24)))
	  ;; If it's more than a month in the past, deduct the "archive"
	  ;; action.
	  (- close-time month)))
       (t close-time))))))

(defun stats-emacs-tally (data date)
  (- (length data)
     (loop for elem in data
	   when (and (<= (stats-emacs-date (cdr (assq 'last_modified elem)) t)
			 data)
		     (equal (cdr (assq 'pending elem)) "done"))
	   sum 1)))

(provide 'stats-emacs)

;;; stats-emacs.el ends here
