
(in-package :mcclim-render-internals)

(defclass image-mirror-mixin ()
  ((image :initform nil :reader image-mirror-image)
   (image-lock :initform (climi::make-lock "image"))
   (resize-image-p :initform t :reader image-mirror-resize-image-p)
   (dirty-region :initform nil)
   (state :initform (aa:make-state))))

(defmethod (setf image-mirror-image) (img (mirror image-mirror-mixin))
  (when img
    (with-slots (image resize-image-p) mirror
      (setf resize-image-p nil)
      (setf image img))))

;;;
;;; protocols
;;;


(defgeneric %draw-image (mirror image x y width height x-dest y-dest clip-region))
(defgeneric %fill-paths (mirror paths transformation clip-region ink background foreground))
(defgeneric %stroke-paths (mirror paths line-style transformation clip-region ink background foreground))
(defgeneric %fill-image-mask (mirror image-mask x y width height x-dest y-dest clip-region ink background foreground))
(defgeneric %fill-image (mirror x y width height ink background foreground clip-region))

(defgeneric %mirror-force-output (mirror))

;;;
;;; implementation
;;;

(defun %make-image (mirror sheet)
  (check-type mirror image-mirror-mixin)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	(sheet-region sheet)
      (let ((width (ceiling (- max-x min-x)))
	    (height (ceiling (- max-y min-y))))
	(%create-mirror-image mirror (1+ width) (1+ height))))))

(defun %set-image-region (mirror region)
  (check-type mirror image-mirror-mixin)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
        region
      (let ((width (1+ (ceiling (- max-x min-x))))
	    (height (1+ (ceiling (- max-y min-y)))))
	(if (and resize-image-p
                 (or (null image)
                     (/= width (pattern-width image))
                     (/= height (pattern-height image))))
	    (%create-mirror-image mirror width height)
	    nil)))))

(defmethod %create-mirror-image (mirror width height)
  (check-type mirror image-mirror-mixin)
  (with-slots (image) mirror
    (setf image (make-image width height)))
  (with-slots (dirty-region) mirror
    (setf dirty-region nil)))

(defun %notify-image-updated (mirror region)
  (check-type mirror image-mirror-mixin)
  (when region
    (with-slots (dirty-region) mirror
      (if dirty-region
          (setf dirty-region (region-union dirty-region region))
          (setf dirty-region region)))))

(defmethod %draw-image :around ((mirror image-mirror-mixin) 
				image x y width height x-dest y-dest clip-region)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %fill-image-mask :around ((mirror image-mirror-mixin) 
				     image-mask x y width height x-dest y-dest clip-region ink background foreground)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %fill-image :around ((mirror image-mirror-mixin) 
				x y width height ink background foreground clip-region)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %fill-paths :around ((mirror image-mirror-mixin) paths transformation region ink background foreground)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %stroke-paths :around ((mirror image-mirror-mixin) paths line-style transformation region ink background foreground)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %draw-image ((mirror image-mirror-mixin)
			image x y width height
			to-x to-y
			clip-region)
  (when (or (not (rectanglep clip-region))
            (not (region-contains-region-p clip-region (make-rectangle* to-x to-y (+ to-x width) (+ to-y height)))))
    (warn "copy image not correct"))
  (let* ((mimage (image-mirror-image mirror))
         (region #- (or) (copy-image image x y width height mimage to-x to-y)
                 #+ (or) (blend-image image x y width height mimage to-x to-y)))
    (%notify-image-updated mirror region)))

(defmethod %fill-image-mask ((mirror image-mirror-mixin)
			     image-mask x y width height x-dest y-dest clip-region ink background foreground)
  #+(or) (when (or (not (rectanglep clip-region))
            (not (region-contains-region-p clip-region (make-rectangle* x y (+ x width) (+ y height)))))
    (warn "fill image mask not correct [~A -> ~A]" clip-region (make-rectangle* x y (+ x width) (+ y height))))
  (let ((region (fill-image (image-mirror-image mirror)
                            (make-pixeled-design ink
                                                 :foreground foreground
                                                 :background background)
                            image-mask
                            :x x :y y :width width :height height
                            :stencil-dx x-dest :stencil-dy y-dest)))
    (%notify-image-updated mirror region)))

(defmethod %fill-image ((mirror image-mirror-mixin)
			x y width height ink background foreground clip-region)
  #+(or) (when (or (not (rectanglep clip-region))
            (not (region-contains-region-p clip-region (make-rectangle* x y (+ x width) (+ y height)))))
    (warn "fill image not correct [~A -> ~A]"
          clip-region
          (make-rectangle* x y (+ x width) (+ y height))))
  (let ((region (fill-image
                 (image-mirror-image mirror)
                 (make-pixeled-design ink :background background :foreground foreground)
                 nil
                 :x x :y y :width width :height height)))
    (%notify-image-updated mirror region)))

(defmethod %fill-paths ((mirror image-mirror-mixin) paths transformation region ink background foreground)
  (with-slots (state) mirror
    (let ((reg
           (aa-fill-paths (image-mirror-image mirror)
                          (and ink (make-pixeled-design ink :foreground foreground :background background))
                          paths
                          state
                          transformation
                          region)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          reg
        (%notify-image-updated mirror (make-rectangle* (floor min-x) (floor min-y)
                                                       (ceiling max-x) (ceiling max-y)))))))

(defmethod %stroke-paths ((mirror image-mirror-mixin) paths line-style transformation region ink background foreground)
  (with-slots (state) mirror
    (let ((reg
           (aa-stroke-paths (image-mirror-image mirror)
                            (and ink (make-pixeled-design ink :foreground foreground :background background))
                            paths
                            line-style
                            state
                            transformation
                            region)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          reg
        (%notify-image-updated mirror (make-rectangle* (floor min-x) (floor min-y)
                                                       (ceiling max-x) (ceiling max-y)))))))

(defmethod %mirror-force-output ((mirror image-mirror-mixin))
  nil)
