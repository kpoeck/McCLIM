(in-package :mcclim-render-internals)

;;;
;;; Image
;;;
(defclass image (climi::%rgba-pattern)
  ((climi::array :initarg :pixels
                 :accessor image-pixels)))

;;;
;;; Image mixins
;;;
(defclass image-mixin ()
  ())

(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (draw-pattern* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

(defun %vals->rgba (r g b &optional (a #xff))
  (declare (type octet r g b a)
           (optimize (speed 3) (safety 0)))
  (dpb r (byte 8 24) (dpb g (byte 8 16) (dpb b (byte 8 8) a))))

(defun %rgba->vals (rgba)
  (declare (type (unsigned-byte 32) rgba)
           (optimize (speed 3) (safety 0)))
  (values (ldb (byte 8 24) rgba)
          (ldb (byte 8 16) rgba)
          (ldb (byte 8 08) rgba)
          (ldb (byte 8 00) rgba)))

(declaim (inline %rgba->vals %vals->rgba))


;;;
;;; Image Design
;;;
(defclass image-design (design)
  ((image :reader image
          :initarg :image)))

(defun make-image-design (image)
  (make-instance 'image-design :image image))

(defmethod clim:draw-design
    (medium (design image-design) &rest options
     &key (x 0) (y 0) &allow-other-keys)
  (climi::with-medium-options (medium options)
    (draw-pattern* medium (slot-value design 'image) x y)))

;;;
;;; Image operations
;;;

(defun make-image (width height)
  "Create an empty transparent image of size WIDTH x HEIGHT."
  ;; XXX: something in text rendering depends image being transparent by
  ;; default. This should be fixed.
  (make-instance 'rgba-image :array (make-array (list height width)
                                                :element-type '(unsigned-byte 32)
                                                :initial-element #xFFFFFF00)))

;;; Unsafe versions of COPY-IMAGE. Caller must ensure that all arguments are
;;; valid and arrays are of proper type.
(defun %copy-image (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2))
    (setf (aref dst-array dest-j dest-i)
          (aref src-array src-j src-i))))

(defun %copy-image* (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2) :backward t)
    (setf (aref dst-array dest-j dest-i)
          (aref src-array src-j src-i))))

(declaim (inline %copy-image %copy-image*))

;;; XXX: We should unify it with COPY-AREA and MEDIUM-COPY-AREA. That means that
;;; raster images should be mediums on their own rights (aren't they?).
(defun copy-image (src-image sx sy width height dst-image dx dy)
  "Copies SRC-IMAGE to DST-IMAGE region-wise. Both may be the same image."
  (let ((sx (round sx))
        (sy (round sy))
        (dx (round dx))
        (dy (round dy))
        (width (round width))
        (height (round height))
        (src-array (climi::pattern-array src-image))
        (dst-array (climi::pattern-array dst-image)))
    (unless (%check-coords src-array dst-array sx sy dx dy width height)
      (return-from copy-image nil))
    (let ((max-x (+ dx width -1))
          (max-y (+ dy height -1)))
      (declare (fixnum max-x max-y))
      (if (eq src-array dst-array)
          (cond ((> sy dy) #1=(%copy-image src-array dst-array sx sy dx dy max-x max-y))
                ((< sy dy) #2=(%copy-image* src-array dst-array sx sy dx dy max-x max-y))
                ((> sx dx) #1#)
                ((< sx dx) #2#)
                (T NIL))
          #1#))
    (make-rectangle* dx dy (+ dx width) (+ dy height))))

(defun %blend-image (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2))
    (let-rgba ((r.fg g.fg b.fg a.fg) (aref src-array src-j src-i))
      (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dest-j dest-i))
        (setf (aref dst-array dest-j dest-i)
              (octet-blend-function* r.fg g.fg b.fg a.fg
                                     r.bg g.bg b.bg a.bg))))))

(defun %blend-image* (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2) :backward t)
    (let-rgba ((r.fg g.fg b.fg a.fg) (aref src-array src-j src-i))
      (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dest-j dest-i))
        (setf (aref dst-array dest-j dest-i)
              (octet-blend-function* r.fg g.fg b.fg a.fg
                                     r.bg g.bg b.bg a.bg))))))

(declaim (inline %blend-image %blend-image*))

(defun blend-image (src-image sx sy width height dst-image dx dy)
  "Copies SRC-IMAGE to DST-IMAGE region-wise. Both may be the same image."
  (let ((sx (round sx))
        (sy (round sy))
        (dx (round dx))
        (dy (round dy))
        (width (round width))
        (height (round height))
        (src-array (climi::pattern-array src-image))
        (dst-array (climi::pattern-array dst-image)))
    (unless (%check-coords src-array dst-array sx sy dx dy width height)
      (return-from blend-image nil))
    (let ((max-x (+ dx width -1))
          (max-y (+ dy height -1)))
      (if (eq src-array dst-array)
          (cond ((> sy dy) #1=(%blend-image src-array dst-array sx sy dx dy max-x max-y))
                ((< sy dy) #2=(%blend-image* src-array dst-array sx sy dx dy max-x max-y))
                ((> sx dx) #1#)
                ((< sx dx) #2#)
                (T NIL))
          #1#))
    (make-rectangle* dx dy (+ dx width) (+ dy height))))

(defun %blend-xor-image (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0) (debug 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2))
    (let ((src-pixel (aref src-array src-j src-i))
          (dst-pixel (aref dst-array dest-j dest-i)))
      (let-rgba ((r.fg g.fg b.fg a.fg) src-pixel)
        (let-rgba ((r.bg g.bg b.bg a.bg) dst-pixel)
          (setf (aref dst-array dest-j dest-i)
                (octet-blend-function* (logxor r.bg r.fg)
                                       (logxor g.bg g.fg)
                                       (logxor b.bg b.fg)
                                       (logxor a.bg a.fg)
                                       r.bg g.bg b.bg a.bg)))))))

(defun clone-image (image)
  (let ((src-array (climi::pattern-array image)))
    (declare (type (simple-array (unsigned-byte 32) 2) src-array))
    (make-instance 'climi::%rgba-pattern :array (alexandria:copy-array src-array))))

(defun crop-image (image sx sy width height)
  (let ((dest (make-image width height)))
    (copy-image image sx sy width height dest 0 0)
    dest))

(defun clone-alpha-channel (image)
  (let ((dest (make-image (pattern-width image) (pattern-height image)))
        (width (pattern-width image))
        (height (pattern-height image)))
    (copy-alpha-channel image 0 0 width height dest 0 0)
    dest))


(defun %copy-alpha-channel (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2))
    (setf (aref dst-array dest-j dest-i)
          (dpb (ldb (byte 8 0) (aref src-array src-j src-i))
               (byte 8 0)
               (aref dst-array dest-j dest-i)))))

(defun %copy-alpha-channel* (src-array dst-array x1s y1s x1d y1d x2 y2)
  (declare (type fixnum x1s y1s x1d y1d x2 y2)
           (type (simple-array (unsigned-byte 32) 2) src-array dst-array)
           (optimize (speed 3) (safety 0)))
  (do-regions ((src-j dest-j y1s y1d y2)
               (src-i dest-i x1s x1d x2) :backward t)
    (setf (aref dst-array dest-j dest-i)
          (dpb (ldb (byte 8 0) (aref src-array src-j src-i))
               (byte 8 0)
               (aref dst-array dest-j dest-i)))))

(defun copy-alpha-channel (src-image sx sy width height dst-image dx dy)
  "Copies SRC-IMAGE to DST-IMAGE region-wise. Both may be the same image."
  (let ((sx (round sx))
        (sy (round sy))
        (dx (round dx))
        (dy (round dy))
        (width (round width))
        (height (round height))
        (src-array (climi::pattern-array src-image))
        (dst-array (climi::pattern-array dst-image)))
    (unless (%check-coords src-array dst-array sx sy dx dy width height)
      (return-from copy-alpha-channel nil))
    (let ((max-x (+ dx width -1))
          (max-y (+ dy height -1)))
      (if (eq src-array dst-array)
          (cond ((> sy dy) #1=(%copy-alpha-channel src-array dst-array sx sy dx dy max-x max-y))
                ((< sy dy) #2=(%copy-alpha-channel* src-array dst-array sx sy dx dy max-x max-y))
                ((> sx dx) #1#)
                ((< sx dx) #2#)
                (T NIL))
          #1#))
    (make-rectangle* dx dy (+ dx width) (+ dy height))))

(defun fill-image (image design stencil &key (x 0) (y 0)
                                          (width (pattern-width image))
                                          (height (pattern-height image))
                                          (stencil-dx 0) (stencil-dy 0))
  "Blends DESIGN onto IMAGE with STENCIL."
  (let* ((dx (round x))
         (dy (round y))
         (sx (round stencil-dx))
         (sy (round stencil-dy))
         (src-fn (pixeled-rgba-fn design))
         (dst-array (climi::pattern-array image))
         (x2 (+ dx width -1))
         (y2 (+ dy height -1)))
    (loop for j from dy to y2
       do (loop for i from dx to x2
             do (let ((alpha (if (null stencil)
                                 #xff
                                 (climi::%pattern-rgba-value stencil (+ sx i) (+ sy j)))))
                  (multiple-value-bind (r.fg g.fg b.fg a.fg) (funcall src-fn i j)
                    (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array j i))
                      (setf (aref dst-array j i)
                            (octet-blend-function* r.fg g.fg b.fg (%octet-mult a.fg alpha)
                                                   r.bg g.bg b.bg a.bg)))))))
    (make-rectangle* x y (+ x width) (+ y height))))
