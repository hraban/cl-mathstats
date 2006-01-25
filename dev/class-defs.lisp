(in-package cl-mathstats)

(defcondition data-error (error) 
  ())

;;; ---------------------------------------------------------------------------

(defcondition insufficient-data (data-error)
  ())

;;; ---------------------------------------------------------------------------

(defcondition no-data (insufficient-data)
  ())

;;; ---------------------------------------------------------------------------

(defcondition zero-standard-deviation (data-error)
  ())

;;; ---------------------------------------------------------------------------

(defcondition zero-variance (data-error)
  ())

;;; ---------------------------------------------------------------------------

(defcondition unmatched-sequences (data-error)
  ())

;;; ---------------------------------------------------------------------------

(defcondition not-binary-variables (data-error)
  ())

;;; ---------------------------------------------------------------------------

(defcondition enormous-contingency-table ()
  ())
