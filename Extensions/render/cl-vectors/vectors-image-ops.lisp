(in-package :mcclim-render-internals)

;;;
;;; aa render functions
;;;

(defun aa-render-draw-fn (image clip-region design)
  (LET ((BLEND-FN (IMAGE-RGBA-BLEND-FN IMAGE)))
    (declare (type image-rgba-blend-fn blend-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (when
          (or (null clip-region) (region-contains-position-p clip-region x y))
        (setf alpha (min (abs alpha) 255))
        (when (plusp alpha)
          (multiple-value-bind (r.fg g.fg b.fg a.fg)
              (%rgba->vals (climi::%pattern-rgba-value design x y))
            (if (> (octet-mult a.fg alpha) 250)
                (setf (aref (climi::pattern-array image) x y)
                      (%vals->rgba r.fg g.fg b.fg #xff))
                (funcall blend-fn x y r.fg g.fg b.fg
                         (octet-mult a.fg ALPHA)))))))))

(defun AA-RENDER-DRAW-SPAN-FN
    (IMAGE CLIP-REGION PIXELED-DESIGN)
  (LET ((SET-FN (IMAGE-RGBA-SET-FN IMAGE))
        (BLEND-FN (IMAGE-RGBA-BLEND-FN IMAGE))
        (DESIGN-FN (PIXELED-RGBA-FN PIXELED-DESIGN)))
    (DECLARE (TYPE IMAGE-RGBA-SET-FN SET-FN)
             (TYPE IMAGE-RGBA-BLEND-FN BLEND-FN)
             (TYPE PIXELED-DESIGN-FN DESIGN-FN))
    (LAMBDA (X1 X2 Y ALPHA)
      (DECLARE (TYPE FIXNUM X1 X2 Y)
               (TYPE FIXNUM ALPHA))
      (LOOP FOR X FROM X1 BELOW X2
         DO (WHEN
                (OR (NULL CLIP-REGION)
                    (REGION-CONTAINS-POSITION-P CLIP-REGION X Y))
              (SETF ALPHA (MIN (ABS ALPHA) 255))
              (WHEN (PLUSP ALPHA)
                (MULTIPLE-VALUE-BIND (R.FG G.FG B.FG A.FG)
                    (FUNCALL DESIGN-FN X Y)
                  (IF (> (OCTET-MULT A.FG ALPHA) 250)
                      (FUNCALL SET-FN X Y R.FG G.FG B.FG #xff)
                      (FUNCALL BLEND-FN X Y R.FG G.FG B.FG
                               (OCTET-MULT A.FG ALPHA))))))))))

(defun AA-RENDER-XOR-DRAW-FN
    (IMAGE CLIP-REGION PIXELED-DESIGN)
  (LET ((SET-FN (IMAGE-RGBA-SET-FN IMAGE))
        (BLEND-FN (IMAGE-RGBA-XOR-BLEND-FN IMAGE))
        (DESIGN-FN (PIXELED-RGBA-FN PIXELED-DESIGN)))
    (DECLARE (TYPE IMAGE-RGBA-SET-FN SET-FN)
             (TYPE IMAGE-RGBA-XOR-BLEND-FN BLEND-FN)
             (TYPE PIXELED-DESIGN-FN DESIGN-FN)
             (IGNORABLE SET-FN))
    (LAMBDA (X Y ALPHA)
      (DECLARE (TYPE FIXNUM X Y)
               (TYPE FIXNUM ALPHA))
      (WHEN
          (OR (NULL CLIP-REGION) (REGION-CONTAINS-POSITION-P CLIP-REGION X Y))
        (SETF ALPHA (MIN (ABS ALPHA) 255))
        (WHEN (PLUSP ALPHA)
          (MULTIPLE-VALUE-BIND (R.FG G.FG B.FG A.FG)
              (FUNCALL DESIGN-FN X Y)
            (FUNCALL BLEND-FN X Y R.FG G.FG B.FG
                     (OCTET-MULT A.FG ALPHA))))))))

(defun AA-RENDER-XOR-DRAW-SPAN-FN
    (IMAGE CLIP-REGION PIXELED-DESIGN)
  (LET ((SET-FN (IMAGE-RGBA-SET-FN IMAGE))
        (BLEND-FN (IMAGE-RGBA-XOR-BLEND-FN IMAGE))
        (DESIGN-FN (PIXELED-RGBA-FN PIXELED-DESIGN)))
    (DECLARE (TYPE IMAGE-RGBA-SET-FN SET-FN)
             (TYPE IMAGE-RGBA-XOR-BLEND-FN BLEND-FN)
             (TYPE PIXELED-DESIGN-FN DESIGN-FN)
             (IGNORABLE SET-FN))
    (LAMBDA (X1 X2 Y ALPHA)
      (DECLARE (TYPE FIXNUM X1 X2 Y)
               (TYPE FIXNUM ALPHA))
      (LOOP FOR X FROM X1 BELOW X2
         DO (WHEN
                (OR (NULL CLIP-REGION)
                    (REGION-CONTAINS-POSITION-P CLIP-REGION X Y))
              (SETF ALPHA (MIN (ABS ALPHA) 255))
              (WHEN (PLUSP ALPHA)
                (MULTIPLE-VALUE-BIND (R.FG G.FG B.FG A.FG)
                    (FUNCALL DESIGN-FN X Y)
                  (FUNCALL BLEND-FN X Y R.FG G.FG B.FG
                           (OCTET-MULT A.FG ALPHA)))))))))

(defun AA-RENDER-ALPHA-DRAW-FN (IMAGE CLIP-REGION)
  (LET ((SET-FN (IMAGE-GRAY-SET-FN IMAGE)))
    (DECLARE (TYPE IMAGE-GRAY-SET-FN SET-FN))
    (LAMBDA (X Y ALPHA)
      (DECLARE (TYPE FIXNUM X Y)
               (TYPE FIXNUM ALPHA))
      (WHEN
          (OR (NULL CLIP-REGION) (REGION-CONTAINS-POSITION-P CLIP-REGION X Y))
        (SETF ALPHA (MIN (ABS ALPHA) 255))
        (WHEN (PLUSP ALPHA) (FUNCALL SET-FN X Y ALPHA))))))
(defun AA-RENDER-ALPHA-DRAW-SPAN-FN (IMAGE CLIP-REGION)
  (LET ((SET-FN (IMAGE-GRAY-SET-FN IMAGE)))
    (DECLARE (TYPE IMAGE-GRAY-SET-FN SET-FN))
    (LAMBDA (X1 X2 Y ALPHA)
      (DECLARE (TYPE FIXNUM X1 X2 Y)
               (TYPE FIXNUM ALPHA))
      (LOOP FOR X FROM X1 BELOW X2
         DO (WHEN
                (OR (NULL CLIP-REGION)
                    (REGION-CONTAINS-POSITION-P CLIP-REGION X Y))
              (SETF ALPHA (MIN (ABS ALPHA) 255))
              (WHEN (PLUSP ALPHA) (FUNCALL SET-FN X Y ALPHA)))))))
