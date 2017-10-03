(in-package #:cl-jpeg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error conditions

(define-condition jpeg-error (error)
  ())

(define-condition jpeg-encoder-error (jpeg-error)
  ())

(define-condition internal-jpeg-encoder-error (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Internal error"))))

(define-condition illegal-number-of-components (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Illegal number of components specified"))))

(define-condition invalid-sampling-list (jpeg-encoder-error)
  ((components :reader components :initarg :ncomp))
  (:report (lambda (condition stream)
	     (format stream "Wrong sampling list for ~D component(s)" (components condition)))))

(define-condition invalid-quantization-tables (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Too many quantization tables specified"))))

(define-condition invalid-q-factor (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Invalid Q factor!"))))

(define-condition invalid-sampling (jpeg-encoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Invalid sampling specification!"))))

(define-condition jpeg-decoder-error (jpeg-error)
  ())

(define-condition unsupported-jpeg-frame-marker (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unsupported marker in the frame header"))))

(define-condition unsupported-jpeg-format (jpeg-decoder-error) 
  ((code :reader marker-code :initarg :code))
  (:report (lambda (condition stream)
	     (format stream "Unsupported JPEG format: ~X" (marker-code condition)))))

(define-condition unrecognized-file-format (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unrecognized JPEG format"))))

(define-condition invalid-buffer-supplied (jpeg-decoder-error)
  ((buffer :reader buffer :initarg :buffer))
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Invalid buffer supplied: ~A" (buffer condition)))))

(define-condition unsupported-arithmetic-encoding (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Arithmetic encoding not supported"))))

(define-condition unsupported-dnl-marker (jpeg-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "DNL marker is not supported"))))

