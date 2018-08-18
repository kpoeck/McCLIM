;;;;  Copyright (c) 1998-2000       Michael McDonald <mikemac@mikemac.com>
;;;;  Copyright (c) 2000-2014       Robert Strandh <robert.strandh@gmail.com>
;;;;  Copyright (c) 1998-2002       Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;;  Copyright (c) 2016-2018       Daniel Kochmański <daniel@turtleware.eu>
;;;;
;;;;    License:  LGPL-2.1-or-later

;;; Patterns are a bounded rectangular arrangements of desings, like a
;;; checkboard. Pattern may be transformed and composed with other designs.
;;;
;;; Extensions:
;;;
;;;   IMAGE-PATTERN                                                      [class]
;;;
;;;      Represents a raster image.
;;;
;;;   TRANSFORMED-PATTERN                                                [class]
;;;
;;;      Represents a pattern which was transformed. May be recursive - pattern
;;;      which is transformed may be another transformed-pattern.
;;;
;;;   EFFECTIVE-TRANSFORMED-PATTERN                                   [function]
;;;
;;;      Returns a transformed pattern with all transformations collapset into a
;;;      single transformation and a design being the source pattern. If
;;;      resulting transformation is an identity returns source pattern
;;;      itself. If function is called with a pattern which is not transformed
;;;      that pattern is returned.
;;;
;;;   RECTANGULAR-TILE-DESIGN tile                                      [method]
;;;
;;;      Returns a design used in the rectangular tile.
;;;
;;; Internals (i.e for use by a backend):
;;;
;;;   %ARRAY-PATTERN                                                     [class]
;;;
;;;      Base class for other all patterns which are based on an array having
;;;      pattern size. In case of transformations we start from that array.
;;;      Array is immutable, transformation should allocate its own array when
;;;      needed.
;;;
;;;   %RGBA-PATTERN                                                      [class]
;;;
;;;      Internal class. Its purpose is to hold cached precomputed RGBA array
;;;      for other patterns (so we collapse designs used as inks and opacities
;;;      into their final values). Computing such array is not necessarily
;;;      trivial, for instance an INDEXED-PATTERN may have a RECTANGULAR-TILE as
;;;      one of its designs, in which case we "blit" retangular tile instead of
;;;      simple color to all original array elements pointing at the tile. In
;;;      case of transformations this pattern should contain final (possibly
;;;      interpolated) values. This instance may be computed lazily and cached.
;;;
;;;   %RGBA-VALUE ink                                                 [function]
;;;
;;;      Collapses ink into a single RGBA value. Use only on uniform designs.
;;;
;;;   %PATTERN-RGBA-VALUE pattern x y                                   [method]
;;;
;;;      Returns pattern color in RGBA for a point [X,Y]. Unoptimized
;;;      implementation of %COLLAPSE-PATTERN may use that function.
;;;
;;;   %COLLAPSE-PATTERN pattern                                         [method]
;;;
;;;      Takes an arbitrary pattern and returns a %RGBA-PATTERN.
;;;
;;; Note: rectangular-tile is (an "infinite") pattern which has a special
;;; treatment for drawing. That is a consequence of wording in 14.2: "To create
;;; an infinite pattern, apply make-rectangular-tile to a pattern".
;;;
;;; Q: Should pattern be a region?
;;;
;;;    That could make the pattern composition easier. Patterns should certainly
;;;    implement bounding-rectangle protocol. In case of rectangular-tile it
;;;    should work on its base design size (and a transformation).
;;;
;;; A: This is implied by the fact that it is adviced to use transform-region on
;;;    a pattern in order to transform it. See 14.5.
;;;
;;; Q: Should a pattern which is not transformed have a starting position?
;;;
;;;    Most transformations on patterns will revolve around moving them. If we
;;;    decide that pattern is a region it could be useful.

(in-package #:clim-internals)

(define-protocol-class pattern (design) ()
  (:documentation "Abstract class for all pattern-like designs."))

(defclass %array-pattern (pattern)
  ((array :initarg :array :reader pattern-array))
  (:documentation "Abstract class for all patterns based on an array (indexed
pattern, stencil, image etc)."))

(defmethod pattern-width ((pattern %array-pattern))
  (array-dimension (pattern-array pattern) 1))

(defmethod pattern-height ((pattern %array-pattern))
  (array-dimension (pattern-array pattern) 0))

(defclass %rgba-pattern (%array-pattern)
  ((array :type '(simple-array (unsigned-byte 32) 2)))
  (:documentation "Helper class of RGBA result of another pattern."))

(defun %rgba-value (element)
  "Helper function collapsing uniform design into 4-byte RGBA value."
  (flet ((transform (parameter)
           (logand (logand (truncate (* parameter 255)) 255))))
    (etypecase element
      (color (with-slots (red green blue) element
               (logior (ash (transform red)   24)
                       (ash (transform green) 16)
                       (ash (transform blue)   8)
                       255)))
      ;; If no color is supplied, we assume black
      (opacity (transform (opacity-value element)))
      ;; Uniform-compositium is a masked-compositum rgb + opacity
      (uniform-compositum
       (let ((ink (compositum-ink element))
             (opacity (compositum-mask element)))
         (with-slots (red green blue) ink
           (let ((opacity-value (opacity-value opacity)))
             (logior (ash (transform red)   24)
                     (ash (transform green) 16)
                     (ash (transform blue)   8)
                     (transform opacity-value))))))
      (indirect-ink (error "Can't convert an INDIRECT-INK to RGBA.")))))

(defgeneric %pattern-rgba-value (pattern x y)
  (:documentation "Returns a collapsed RGBA value for position [X, Y].")
  (:method ((pattern %rgba-pattern) (x fixnum) (y fixnum))
    (aref (pattern-array pattern) y x)))

(defgeneric %collapse-pattern (pattern)
  (:documentation "Returns a %RGBA-PATTERN with colors.")
  (:method ((pattern pattern)) ;; default method
    (let ((array (make-array (list (pattern-height pattern)
                                   (pattern-width pattern))
                             :element-type '(unsigned-byte 32))))
      (dotimes (i (pattern-height pattern))
        (dotimes (j (pattern-width pattern))
          (setf (aref array i j) (%pattern-rgba-value pattern j i))))
      (make-instance '%rgba-pattern :array array)))
  (:method ((pattern %rgba-pattern)) pattern))


;;; Rectangular patterns

(defclass indexed-pattern (%array-pattern)
  ((designs :initarg :designs :reader pattern-designs))
  (:documentation "Indexed pattern maps numbers in array to designs."))

(defun make-pattern (array designs)
  (make-instance 'indexed-pattern :array array :designs designs))

(defmethod %pattern-rgba-value ((pattern indexed-pattern) x y)
  (let* ((array (pattern-array pattern))
         ;; indexed-pattern may be used as a design in the rectangular-tile. If
         ;; it is bigger than our pattern we return +transparent-ink+.
         (element (if (array-in-bounds-p array y x)
                      (elt (pattern-designs pattern)
                           (aref (pattern-array pattern) y x))
                      +transparent-ink+)))
    (if (patternp element)
        ;; If design is a pattern we delegate the question
        (%pattern-rgba-value element x y)
        (%rgba-value element))))

(defclass stencil (%array-pattern)
  ((array :type '(simple-array (single-float 0.0 1.0) 2)))
  (:documentation "Stencil pattern provides opacity mask."))

(defun make-stencil (array)
  (make-instance 'stencil :array array))

;;; If we had wanted to convert stencil to indexed array these functions would
;;; come handy what would not serve much purpose though.
#+(or)
(defun indexed-pattern-array ((pattern stencil))
  (let ((array (make-array (list (pattern-height pattern)
                                 (pattern-width pattern)))))
    (dotimes (i (pattern-height pattern))
      (dotimes (j (pattern-width pattern))
        (setf (aref array i j) (+ (* i (array-dimension array 1)) j))))
    array))

#+(or)
(defun indexed-pattern-designs ((pattern stencil))
  (with-slots (array) pattern
    (let ((designs (make-array (* (pattern-height pattern)
                                  (pattern-width pattern)))))
      (dotimes (i (length designs))
        (setf (aref designs i) (make-opacity (row-major-aref array i))))
      array)))

(defclass rectangular-tile (pattern)
  ((width  :initarg :width   :reader pattern-width)
   (height :initarg :height  :reader pattern-height)
   (design :initarg :design  :reader rectangular-tile-design))
  (:documentation "Rectangular tile repeats a rectangular portion of a design
throughout the drawing plane. This is most commonly used with patterns."))

(defun make-rectangular-tile (design width height)
  (make-instance 'rectangular-tile
                 :width  width
                 :height height
                 :design design))

(defmethod %pattern-rgba-value ((pattern rectangular-tile) x y
                                &aux
                                  (x (mod x (pattern-width pattern)))
                                  (y (mod y (pattern-height pattern))))
  (let ((element (rectangular-tile-design pattern)))
    (if (patternp element)
        ;; If design is a pattern we delegate the question
        (%pattern-rgba-value element x y)
        (%rgba-value element))))

(defclass image-pattern (%rgba-pattern) ()
  (:documentation "RGBA pattern. Class defined for specialization. Instances of
this class may be returned by MAKE-PATTERN-FROM-BITMAP-FILE."))


;;; Bitmap images (from files)
;;;
;;; Based on CLIM 2.2, with an extension permitting the definition of
;;; new image formats by the user.

(defvar *bitmap-file-readers* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming bitmap image formats to a
function that can read an image of that format. The functions will be called
with one argument, the pathname of the file to be read. The functions should
return two values as per READ-BITMAP-FILE.")

(defmacro define-bitmap-file-reader (bitmap-format (&rest args) &body body)
  "Define a method for reading bitmap images of format BITMAP-FORMAT that will
be used by READ-BITMAP-FILE and MAKE-PATTERN-FROM-BITMAP-FILE. BODY should
return two values as per `read-bitmap-file'."
  `(setf (gethash ,bitmap-format *bitmap-file-readers*)
         #'(lambda (,@args) ,@body)))

(defun bitmap-format-supported-p (format)
  "Return true if FORMAT is supported by READ-BITMAP-FILE."
  (not (null (gethash format *bitmap-file-readers*))))

(define-condition unsupported-bitmap-format (simple-error) ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Unsupported bitmap format")))
  (:documentation "This condition is signaled when trying to read a
  bitmap file whose format is not supported." ))

(defun read-bitmap-file (pathname &key (format :bitmap))
  "Read a bitmap file named by PATHNAME. FORMAT is a keyword symbol naming any
defined bitmap file format defined by CLIM-EXTENSIONS:DEFINE-BITMAP-FILE-READER.

Two values are returned: a two-dimensional array of pixel values and an array of
either colors or color names. If the second value is non-NIL, the pixel values
are assumed to be indexes into this array. Otherwise, the pixel values are taken
to be RGBA values encoded in 32 bit unsigned integers, with the three most
significant octets being the values R, G, B and A, in order."
  (funcall (or (gethash format *bitmap-file-readers*)
               (gethash :fallback *bitmap-file-readers*)
               (error 'unsupported-bitmap-format))
           pathname))

(defun make-pattern-from-bitmap-file (pathname &key designs (format :bitmap))
  "Read a bitmap file named by PATHNAME. FORMAT is a keyword symbol naming any
defined bitmap file format defined by CLIM-EXTENSIONS:DEFINE-BITMAP-FILE-READER.
Returns a pattern representing this file."
  (multiple-value-bind (array read-designs)
      (read-bitmap-file pathname :format format)
    (if read-designs
        (make-pattern array (or designs read-designs))
        (make-instance 'image-pattern :array array))))


;;; Transformed patterns

(defclass transformed-pattern (transformed-design pattern)
  ())

(defun effective-transformed-pattern (pattern &aux source-pattern)
  "Merges all transformations along the way and returns a shallow, transformed
pattern. If pattern is not transformed (or effective transformation is an
identity-transformation) then source pattern is returned."
  (labels ((effective-transformation (p)
             (let* ((pattern* (transformed-design-design p))
                    (transformation (transformed-design-transformation p)))
               (etypecase pattern*
                 (transformed-pattern
                  (compose-transformations (effective-transformation pattern*)
                                           transformation))
                 (pattern
                  (setf source-pattern pattern*)
                  transformation)))))
    (etypecase pattern
      (transformed-pattern
       (if (identity-transformation-p (transformed-design-transformation pattern))
           source-pattern
           (make-instance 'transformed-pattern
                          :design source-pattern
                          :transformation (effective-transformation pattern))))
      (pattern pattern))))

;;; this is not right (bounding rectangle of a transformed region)
(defmethod pattern-width ((pattern transformed-pattern))
  (pattern-width (transformed-design-design pattern)))

;;; this is not right (bounding rectangle of a transformed region)
(defmethod pattern-height ((pattern transformed-pattern))
  (pattern-height (transformed-design-design pattern)))

(defmethod transform-region (transformation (design pattern))
  (if (identity-transformation-p transformation)
      design
      (make-instance 'transformed-pattern
                     :transformation transformation
                     :design design)))
