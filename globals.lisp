(in-package #:jpeg)

(deftype uint8 () '(unsigned-byte 8))
(deftype uint8-array () '(simple-array uint8 (*)))
(deftype uint8-2d-array () '(simple-array uint8-array (*)))

(deftype suint8 () '(signed-byte 8))
(deftype sint8-array () '(simple-array sint8 (*)))
(deftype sint8-2d-array () '(simple-array sint8-array (*)))

(deftype sint16 () '(signed-byte 16))
(deftype sint16-array () '(simple-array sint16 (*)))
(deftype sint16-2d-array () '(simple-array sint16-array (*)))

(deftype uint16 () '(unsigned-byte 16))
(deftype uint16-array () '(simple-array uint16 (*)))
(deftype uint16-2d-array () '(simple-array uint16-array (*)))

(deftype fixnum-array () '(simple-array fixnum (*)))
(deftype fixnum-2d-array () '(simple-array fixnum-array (*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize* '(optimize (safety 0) (space 0) (debug 0) (speed 3))))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; For ease of reference
(defmacro u8ref (data x y)
  `(the uint8 (aref (the uint8-array (aref (the uint8-2d-array ,data) ,y)) ,x)))

(defmacro s16ref (data x y)
  `(the sint16 (aref (the sint16-array (aref (the sint16-2d-array ,data) ,y)) ,x)))

(defmacro u16ref (data x y)
  `(the uint16 (aref (the uint16-array (aref (the uint16-2d-array ,data) ,y)) ,x)))

(defmacro fixref (data x y)
  `(the fixnum (aref (the fixnum-array (aref (the fixnum-2d-array ,data) ,y)) ,x)))

;;; Integer arithmetic wrappers
(defmacro plus (a b)
  `(the fixnum (+ (the fixnum ,a) (the fixnum ,b))))

(defmacro minus (a b)
  #+(or clisp abcl)
  `(- ,a ,b)
  #-(or clisp abcl)
  `(the fixnum (- (the fixnum ,a) (the fixnum ,b))))

(defmacro mul (a b)
  `(the fixnum (* (the fixnum ,a) (the fixnum ,b))))

(defmacro plus3 (x y z)
  `(plus (plus ,x ,y) ,z))

(defmacro mul3 (x y z)
  `(mul (mul ,x ,y) ,z)))

;;; Somewhat silly, but who knows...
(when (/= (integer-length most-positive-fixnum)
          (integer-length most-negative-fixnum))
  (error "Can't compile with asymmetric fixnums!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here we define some constants (markers, quantization and huffman tables etc.)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(eval-when (:compile-toplevel :load-toplevel)

(defun uint8-array (&rest contents)
  (the uint8-array (make-array (length contents) :element-type 'uint8
			       :initial-contents contents)))
  
(defun 2d-uint8-array (&rest contents)
  (let ((nrow (length contents)))
    (the uint8-2d-array
	 (make-array nrow
		:element-type 'uint8-array
		:initial-contents
                (loop for row in contents
                      collecting (make-array (length row) :element-type 'uint8
                                             :initial-contents row))))))

(defun 2d-sint16-array (&rest contents)
  (let ((nrow (length contents)))
    (the sint16-2d-array
	 (make-array nrow
		:element-type 'sint16-array
		:initial-contents
                (loop for row in contents
                      collecting (make-array (length row) :element-type 'sint16
                                             :initial-contents row))))))

;;; Source huffman tables for the encoder
(define-constant +luminance-dc-bits+
    (uint8-array #x00 #x01 #x05 #x01 #x01 #x01 #x01 #x01
		 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

(define-constant +luminance-dc-values+
    (uint8-array #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b))

(define-constant +chrominance-dc-bits+
    (uint8-array #x00 #x03 #x01 #x01 #x01 #x01 #x01 #x01
		 #x01 #x01 #x01 #x00 #x00 #x00 #x00 #x00))

(define-constant +chrominance-dc-values+
    (uint8-array #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b))

(define-constant +luminance-ac-bits+
    (uint8-array #x00 #x02 #x01 #x03 #x03 #x02 #x04 #x03
		 #x05 #x05 #x04 #x04 #x00 #x00 #x01 #x7d))

(define-constant +luminance-ac-values+
    (uint8-array
     #x01 #x02 #x03 #x00 #x04 #x11 #x05 #x12
     #x21 #x31 #x41 #x06 #x13 #x51 #x61 #x07
     #x22 #x71 #x14 #x32 #x81 #x91 #xa1 #x08
     #x23 #x42 #xb1 #xc1 #x15 #x52 #xd1 #xf0
     #x24 #x33 #x62 #x72 #x82 #x09 #x0a #x16
     #x17 #x18 #x19 #x1a #x25 #x26 #x27 #x28
     #x29 #x2a #x34 #x35 #x36 #x37 #x38 #x39
     #x3a #x43 #x44 #x45 #x46 #x47 #x48 #x49
     #x4a #x53 #x54 #x55 #x56 #x57 #x58 #x59
     #x5a #x63 #x64 #x65 #x66 #x67 #x68 #x69
     #x6a #x73 #x74 #x75 #x76 #x77 #x78 #x79
     #x7a #x83 #x84 #x85 #x86 #x87 #x88 #x89
     #x8a #x92 #x93 #x94 #x95 #x96 #x97 #x98
     #x99 #x9a #xa2 #xa3 #xa4 #xa5 #xa6 #xa7
     #xa8 #xa9 #xaa #xb2 #xb3 #xb4 #xb5 #xb6
     #xb7 #xb8 #xb9 #xba #xc2 #xc3 #xc4 #xc5
     #xc6 #xc7 #xc8 #xc9 #xca #xd2 #xd3 #xd4
     #xd5 #xd6 #xd7 #xd8 #xd9 #xda #xe1 #xe2
     #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #xea
     #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8
     #xf9 #xfa))

(define-constant +chrominance-ac-bits+
    (uint8-array #x00 #x02 #x01 #x02 #x04 #x04 #x03 #x04
		#x07 #x05 #x04 #x04 #x00 #x01 #x02 #x77))

(define-constant +chrominance-ac-values+
    (uint8-array
     #x00 #x01 #x02 #x03 #x11 #x04 #x05 #x21
     #x31 #x06 #x12 #x41 #x51 #x07 #x61 #x71
     #x13 #x22 #x32 #x81 #x08 #x14 #x42 #x91
     #xa1 #xb1 #xc1 #x09 #x23 #x33 #x52 #xf0
     #x15 #x62 #x72 #xd1 #x0a #x16 #x24 #x34
     #xe1 #x25 #xf1 #x17 #x18 #x19 #x1a #x26
     #x27 #x28 #x29 #x2a #x35 #x36 #x37 #x38
     #x39 #x3a #x43 #x44 #x45 #x46 #x47 #x48
     #x49 #x4a #x53 #x54 #x55 #x56 #x57 #x58
     #x59 #x5a #x63 #x64 #x65 #x66 #x67 #x68
     #x69 #x6a #x73 #x74 #x75 #x76 #x77 #x78
     #x79 #x7a #x82 #x83 #x84 #x85 #x86 #x87
     #x88 #x89 #x8a #x92 #x93 #x94 #x95 #x96
     #x97 #x98 #x99 #x9a #xa2 #xa3 #xa4 #xa5
     #xa6 #xa7 #xa8 #xa9 #xaa #xb2 #xb3 #xb4
     #xb5 #xb6 #xb7 #xb8 #xb9 #xba #xc2 #xc3
     #xc4 #xc5 #xc6 #xc7 #xc8 #xc9 #xca #xd2
     #xd3 #xd4 #xd5 #xd6 #xd7 #xd8 #xd9 #xda
     #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9
     #xea #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8
     #xf9 #xfa))

;;;Zigzag encoding matrix
(define-constant +zigzag-index+
  (2d-uint8-array '(0  1  5  6 14 15 27 28)
                  '(2  4  7 13 16 26 29 42)
                  '(3  8 12 17 25 30 41 43)
                  '(9 11 18 24 31 40 44 53)
                  '(10 19 23 32 39 45 52 54)
                  '(20 22 33 38 46 51 55 60)
                  '(21 34 37 47 50 56 59 61)
                  '(35 36 48 49 57 58 62 63)))

;;;JPEG file markers
(defconstant +M_COM+ #xfe)
(defconstant +M_SOF0+ #xc0)
(defconstant +M_SOF2+ #xc2)
(defconstant +M_DHT+ #xc4)
(defconstant +M_RST0+ #xd0)
(defconstant +M_RST7+ #xd7)
(defconstant +M_SOI+ #xd8)
(defconstant +M_EOI+ #xd9)
(defconstant +M_SOS+ #xda)
(defconstant +M_DQT+ #xdb)
(defconstant +M_DNL+ #xdc)
(defconstant +M_DRI+ #xdd)
(defconstant +M_DAC+ #xcc)
(defconstant +M_APP0+ #xe0)
(defconstant +M_APP14+ #xee)

;;; Default quantization tables
(define-constant +q-luminance+
  (2d-uint8-array '(16 11 10 16 24 40 51 61)
                  '(12 12 14 19 26 58 60 55)
                  '(14 13 16 24 40 57 69 56)
                  '(14 17 22 29 51 87 80 62)
                  '(18 22 37 56 68 109 103 77)
                  '(24 35 55 64 81 104 113 92)
                  '(49 64 78 87 103 121 120 101)
                  '(72 92 95 98 112 100 103 99)))

(define-constant +q-chrominance+
  (2d-uint8-array '(17 18 24 47 99 99 99 99)
                  '(18 21 26 66 99 99 99 99)
                  '(24 26 56 99 99 99 99 99)
                  '(47 66 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)
                  '(99 99 99 99 99 99 99 99)))

(define-constant +q-luminance-hi+
  (2d-uint8-array '(10 7 6 10 15 25 32 38)
                  '(8 8 9 12 16 36 38 34)
                  '(9 8 10 15 25 36 43 35)
                  '(9 11 14 18 32 54 50 39)
                  '(11 14 23 35 42 68 64 48)
                  '(15 22 34 40 51 65 71 58)
                  '(31 40 49 54 64 76 75 63)
                  '(45 58 59 61 70 62 64 62)))

(define-constant +q-chrominance-hi+
  (2d-uint8-array '(11 11 15 29 62 62 62 62)
                  '(11 13 16 41 62 62 62 62)
                  '(15 16 35 62 62 62 62 62)
                  '(29 41 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)
                  '(62 62 62 62 62 62 62 62)))

(defconstant +max-sample+ 255)

)

(define-constant +q-tables+ (vector +q-luminance+ +q-chrominance+))

;;; This table is used to map coefficients into SSSS value
(define-constant +csize+ (make-array 2047
                                 :initial-contents
                                 (loop for i fixnum from 0 to 2046
                                       collecting (integer-length (abs (minus i 1023))))))

;;; Some constants for colorspace mapper
(defconstant shift (1- (integer-length (ash most-positive-fixnum -7))))
(defconstant +.299+ (round (+ (* 0.299 (ash 1 shift)) 0.5)))
(defconstant +.587+ (round (+ (* 0.587 (ash 1 shift)) 0.5)))
(defconstant +.114+ (round (+ (* 0.114 (ash 1 shift)) 0.5)))
(defconstant +-.1687+ (round (+ (* -0.1687 (ash 1 shift)) 0.5)))
(defconstant +-.3313+ (round (+ (* -0.3313 (ash 1 shift)) 0.5)))
(defconstant +-.4187+ (round (+ (* -0.4187 (ash 1 shift)) 0.5)))
(defconstant +-.0813+ (round (+ (* -0.0813 (ash 1 shift)) 0.5)))
(defconstant +.5+ (round (+ (* 0.5 (ash 1 shift)) 0.5)))
(defconstant +uvoffset+ (ash 128 shift))
(defconstant +one-half+ (1- (ash 1 (1- shift))))
(defconstant +r-y-off+ 0)
(defconstant +g-y-off+ 256)
(defconstant +b-y-off+ (* 2 256))
(defconstant +r-u-off+ (* 3 256))
(defconstant +g-u-off+ (* 4 256))
(defconstant +b-u-off+ (* 5 256))
(defconstant +r-v-off+ +b-u-off+)
(defconstant +g-v-off+ (* 6 256))
(defconstant +b-v-off+ (* 7 256))

(declaim (type fixnum-array +ctab+ +cr-r-tab+ +cb-g-tab+ +cr-g-tab+ +cb-b-tab+))

;;;Direct color conversion table
(define-constant +ctab+
    (let ((table (make-array 2048 :element-type 'fixnum :initial-element 0)))
      (loop for i fixnum from 0 to 255 do
           (setf (aref table (plus i +r-y-off+))
                 (mul +.299+ i))
           (setf (aref table (plus i +g-y-off+))
                 (mul +.587+ i))
           (setf (aref table (plus i +b-y-off+))
                 (mul +.114+ i))
           (setf (aref table (plus i +r-u-off+))
                 (mul +-.1687+ i))
           (setf (aref table (plus i +g-u-off+))
                 (mul +-.3313+ i))
           (setf (aref table (plus i +b-u-off+))
                 (+ (mul +.5+ i) +uvoffset+ +one-half+))
           (setf (aref table (plus i +r-v-off+))
                 (+ (mul +.5+ i) +uvoffset+ +one-half+))
           (setf (aref table (plus i +g-v-off+))
                 (mul +-.4187+ i))
           (setf (aref table (plus i +b-v-off+))
                 (mul +-.0813+ i)))
      table))

;;; Constantsants for the inverse colorspace conversion
(defconstant +1.40200+ (round (+ (* 1.40200 (ash 1 shift)) 0.5)))
(defconstant +1.77200+ (round (+ (* 1.77200 (ash 1 shift)) 0.5)))
(defconstant +-0.71414+ (round (+ (* -0.71414 (ash 1 shift)) 0.5)))
(defconstant +-0.34414+ (round (+ (* -0.34414 (ash 1 shift)) 0.5)))

;;; Inverse color conversion tables
(define-constant +cr-r-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (ash (plus (mul +1.40200+ x) +one-half+) (- shift)))))
(define-constant +cb-g-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (plus (mul +-0.34414+ x) +one-half+))))
(define-constant +cr-g-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (mul +-0.71414+ x))))
(define-constant +cb-b-tab+ (make-array 256
                                        :element-type 'fixnum
                                        :initial-contents
                                        (loop for i from 0 to 255
                                              for x from -127
                                              collect (ash (plus (mul +1.77200+ x) +one-half+) (- shift)))))

;;; Constants for LLM DCT
(defconstant dct-shift  ; defining DCT scaling
  (if (<= (integer-length most-positive-fixnum) 31)
      (minus 13 (round (minus 31 (integer-length most-positive-fixnum)) 2))
    13))

(defconstant +shift-1+ (1- dct-shift))
(defconstant +shift+1+ (1+ dct-shift))
(defconstant +shift+4+ (+ dct-shift 4))
(defconstant +FIX-0-298631336+ (round (+ (* 0.298631336 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-390180644+ (round (+ (* 0.390180644 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-541196100+ (round (+ (* 0.541196100 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-765366865+ (round (+ (* 0.765366865 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-0-899976223+ (round (+ (* 0.899976223 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-175875602+ (round (+ (* 1.175875602 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-501321110+ (round (+ (* 1.501321110 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-847759065+ (round (+ (* 1.847759065 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-1-961570560+ (round (+ (* 1.961570560 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-2-053119869+ (round (+ (* 2.053119869 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-2-562915447+ (round (+ (* 2.562915447 (ash 1 dct-shift)) 0.5)))
(defconstant +FIX-3-072711026+ (round (+ (* 3.072711026 (ash 1 dct-shift)) 0.5)))

(declaim (type uint8-array *idct-limit-array*))
;;; Post-IDCT limiting array
(defvar *idct-limit-array* (make-array 512 :initial-element 0 :element-type 'uint8))
(loop for n from 0
      for i from 128 to 383 do
      (setf (aref *idct-limit-array* i) n))
(loop for i from 384 to 511 do
      (setf (aref *idct-limit-array* i) 255))

