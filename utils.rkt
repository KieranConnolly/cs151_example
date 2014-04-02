#lang racket
(require gigls/unsafe)

;;; File:
;;;   utils.rkt
;;; Author:
;;;   Kieran Connolly, taken from Sam Rebelsky/Grinnell CS
;;; Summary:
;;;   A few sample utilities, created as part of a lab on GitHub.

;;; Procedure:
;;;   nums
;;; Parameters:
;;;   n, a positive integer
;;; Purpose:
;;;   Create a list of the integers [1 ... n].
;;; Produces:
;;;   list-of-nums, a list of integers
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length list-of-nums) == n
;;;   (list-ref list-of-nums i) = i+1
;;;     for all i, 0 <= i < n.
(define first-n-nums
  (lambda (n)
    (cdr (iota (+ n 1)))))

<<<<<<< HEAD
;; This is me just adding some comments so I can do some commits.


;; Just me adding some more comments so I can commit again and practice
;; my level of commitment.
=======
;;; If this shows up then the pull merged the documents
>>>>>>> b4e934b549502981471929997cea9a4e56e54037
