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

(push "~/src/emacs/elpa/packages/debbugs/" load-path)

(require 'debbugs-gnu)
(require 'cl)
(require 'iso8601)
(require 'parse-time)

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
	    (cl-copy-list data)))

(defvar stats-emacs-get nil)
(defvar stats-emacs-cache nil)

(defun stats-emacs ()
  (interactive)
  (setq stats-emacs-cache (stats-emacs-get))
  (when (< (length stats-emacs-cache) 35900)
    (error "Didn't get all bug reports"))
  (stats-emacs-generate (stats-emacs-filter
			 (stats-emacs-sort stats-emacs-cache)))
  (stats-emacs-percentage-time stats-emacs-cache nil)
  (stats-emacs-percentage-time stats-emacs-cache t))

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
	  (cl-incf closed)
	  (cl-incf (gethash (cdr (assq 'severity (car data))) severities 0))
	  (dolist (tag (cdr (assq 'tags (car data))))
	    (cl-incf (gethash tag tags 0))))
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
    ;; Add the current day, too.
    (setf (gethash (string-to-number (format-time-string "%Y%m%d")) total)
	  (list :closed closed
		:severities (stats-emacs-hash-to-alist severities)
		:tags (stats-emacs-hash-to-alist tags)))
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
    (insert "emacsData = [[\"Date\", \"Open\", \"Opened\", \"Closed\", \"Normal+\", \"Minor\", \"Wishlist\", \"Patch\", \"Moreinfo\", \"Wontfix\"],\n")
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
	  (cl-incf opened)
	  (cl-incf (gethash (cdr (assq 'severity (car data))) severities 0))
	  (dolist (tag (cdr (assq 'tags (car data))))
	    (cl-incf (gethash tag tags 0)))
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
    (insert (format "[new Date(%d, %d, %d), %d, %d, %d, %d, %d, %d, %d, %d, %d],\n"
		    (/ date 10000)
		    (1- (mod (/ date 100) 100))
		    (mod date 100)
		    (- opened (cl-getf cd :closed))
		    opened
		    (cl-getf cd :closed)
		    (+ (- (gethash "critical" severities 0)
			  (or (cdr (assoc "critical" (cl-getf cd :severities)))
			      0))
		       (- (gethash "important" severities 0)
			  (or (cdr (assoc "important" (cl-getf cd :severities)))
			      0))
		       (- (gethash "normal" severities 0)
			  (or (cdr (assoc "normal" (cl-getf cd :severities)))
			      0)))
		    (- (gethash "minor" severities 0)
		       (or (cdr (assoc "minor" (cl-getf cd :severities)))
			   0))
		    (- (gethash "wishlist" severities 0)
		       (or (cdr (assoc "wishlist" (cl-getf cd :severities)))
			   0))
		    (- (gethash "patch" tags 0)
		       (or (cdr (assoc "patch" (cl-getf cd :tags)))
			   0))
		    (- (gethash "moreinfo" tags 0)
		       (or (cdr (assoc "moreinfo" (cl-getf cd :tags)))
			   0))
		    (- (gethash "wontfix" tags 0)
		       (or (cdr (assoc "wontfix" (cl-getf cd :tags)))
			   0))
		    ))))

(defun stats-emacs-closed (date closed-data)
  (cl-loop with prev = (list :closed 0)
	   for elem in closed-data
	   when (> (car elem) date)
	   return prev
	   do (setq prev (cdr elem))
	   finally return prev))

(defun stats-emacs-date (elem &optional close format)
  (let* ((open-time (cdr (assq 'date elem)))
	 (close-time (cdr (assq 'last_modified elem)))
	 (time
	  (cond
	   ((not close) open-time)
	   ((equal (cdr (assq 'location elem)) "archive")
	    (let ((month (* 30 60 60 24)))
	      ;; If it's more than a month in the past, deduct the "archive"
	      ;; action.
	      (- close-time month)))
	   (t close-time))))
    (cond
     ((eq format 'int)
      time)
     ((eq format 'date)
      (decode-time time))
     (t
      (string-to-number (format-time-string "%Y%m%d" time))))))

(defun stats-emacs-tally (data date)
  (- (length data)
     (cl-loop for elem in data
	      when (and (<= (stats-emacs-date
			     (cdr (assq 'last_modified elem)) t)
			    data)
			(equal (cdr (assq 'pending elem)) "done"))
	      sum 1)))

(defun stats-emacs-same-month-p (date1 date2)
  (or (> (decoded-time-year date1) (decoded-time-year date2))
      (and (= (decoded-time-year date1) (decoded-time-year date2))
	   (>= (decoded-time-month date1) (decoded-time-month date2)))))

(defun stats-emacs-percentage-time (data &optional no-wishlist)
  (setq data (stats-emacs-filter (stats-emacs-sort data)))
  (with-temp-buffer
    (insert (format
	     "percentData%s = [[\"Date\", \"Year\", \"Month\", \"Week\"],\n"
	     (if no-wishlist
		 "NW"
	       "")))
    (let ((date (make-decoded-time :day 1 :month 1 :year 2008))
	  (last-year (- (time-convert (current-time) 'integer)
			(* 365 24 60 60)))
	  (last-month (- (time-convert (current-time) 'integer)
			 (* 31 24 60 60)))
	  (last-week (- (time-convert (current-time) 'integer)
			(* 7 24 60 60)))
	  opened closed-year closed-month closed-week)
      (while data
	(setq opened nil
	      closed-year 0
	      closed-month 0
	      closed-week 0)
	(while (and data
		    (stats-emacs-same-month-p
		     date (stats-emacs-date (car data) nil 'date)))
	  (when (or (not no-wishlist)
		    (not (equal (cdr (assq 'severity (car data))) "wishlist")))
	    (push (car data) opened))
	  (pop data))
	(dolist (elem opened)
	  (when (equal (cdr (assq 'pending elem)) "done")
	    (let ((diff (- (stats-emacs-date elem t 'int)
			   (stats-emacs-date elem nil 'int))))
	      (when (< diff (* 7 24 60 60))
		(cl-incf closed-week))
	      (when (< diff (* 30 24 60 60))
		(cl-incf closed-month))
	      (when (< diff (* 365 24 60 60))
		(cl-incf closed-year)))))
	(insert (format "[new Date(%d, %d, %d), %s, %s, %s],\n"
			(decoded-time-year date)
			(decoded-time-month date)
			(decoded-time-day date)
			(cond
			 ((zerop (length opened)) 0)
			 ((> (time-convert (encode-time date) 'integer)
			     last-year)
			  "null")
			 (t
			  (truncate
			   (* 100 (/ (float closed-year) (length opened))))))
			(cond
			 ((zerop (length opened)) 0)
			 ((> (time-convert (encode-time date) 'integer)
			     last-month)
			  "null")
			 (t
			  (* 100 (/ (float closed-month) (length opened)))))
			(cond
			 ((zerop (length opened)) 0)
			 ((> (time-convert (encode-time date) 'integer)
			     last-week)
			  "null")
			 (t
			  (* 100 (/ (float closed-week) (length opened)))))))
	(setq date (decoded-time-add date
				     (make-decoded-time :month 3))))
      (search-backward ",")
      (delete-char 1)
      (insert "];")
      (write-region (point-min) (point-max)
		    (if no-wishlist
			"~/src/stats-emacs/stats-percent.js"
		      "~/src/stats-emacs/stats-percent-no-wishlist.js")))))

(defun stats-emacs-find-at-date (date)
  (cl-loop with time = (float-time
			(encode-time (decoded-time-set-defaults
				      (iso8601-parse-date date))))
	   for elem in (stats-emacs-filter stats-emacs-cache)
	   for open-time = (cdr (assq 'date elem))
	   for close-time = (cdr (assq 'last_modified elem))
	   for status = (cdr (assq 'pending elem))
	   when (and (member "moreinfo" (cdr (assq 'tags elem)))
		     (or (and (not (equal status "done"))
			      (< open-time time))
			 (and (equal status "done")
			      (< open-time time close-time))))
	   collect elem))

(provide 'stats-emacs)

;;; stats-emacs.el ends here
