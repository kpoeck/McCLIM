(in-package :mcclim-render-internals)

;;;
;;; image manipulation functions
;;;
;;rgba
(defgeneric image-rgba-get-code (image-class pixels-var x-var y-var))
(defgeneric image-gray-alpha-get-code (image-class pixels-var x-var y-var))
(defgeneric image-rgba-set-code (image-class pixels-var x-var y-var
                                 red-var green-var blue-var alpha-var))
(defgeneric image-rgba-blend-code (image-class pixels-var x-var y-var
                                   red-var green-var blue-var alpha-var))
;; gray
(defgeneric image-gray-get-code (image-class pixels-var x-var y-var))

(defgeneric image-gray-set-code (image-class pixels-var x-var y-var
                                 gray-var))
(defgeneric image-gray-blend-code (image-class pixels-var x-var y-var
                                   gray-var alpha-var))
;; alpha
(defgeneric image-alpha-get-code (image-class pixels-var x-var y-var))
(defgeneric image-alpha-set-code (image-class pixels-var x-var y-var
                                 gray-var))
(defgeneric image-alpha-blend-code (image-class pixels-var x-var y-var
                                    alpha-var))
;; rgba
(deftype image-rgba-get-fn () '(function (fixnum fixnum) (values octet octet octet octet)))
(deftype image-gray-alpha-get-fn () '(function (fixnum fixnum) (values octet octet)))
(deftype image-rgba-set-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgba-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgba-xor-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; gray
(deftype image-gray-get-fn () '(function (fixnum fixnum) octet))
(deftype image-gray-set-fn () '(function (fixnum fixnum octet)))
(deftype image-gray-blend-fn () '(function (fixnum fixnum octet octet)))
;; alpha
(deftype image-alpha-get-fn () '(function (fixnum fixnum) octet))
(deftype image-alpha-set-fn () '(function (fixnum fixnum octet )))
(deftype image-alpha-blend-fn () '(function (fixnum fixnum octet octet)))

;; rgba
(defgeneric image-gray-alpha-get-fn (image &key dx dy region))
(defgeneric image-rgba-set-fn (image &key dx dy))
(defgeneric image-rgba-blend-fn (image &key dx dy))
(defgeneric image-rgba-xor-blend-fn (image &key dx dy))
;; gray
(defgeneric image-gray-get-fn (image &key dx dy region))
(defgeneric image-gray-set-fn (image &key dx dy))
(defgeneric image-gray-blend-fn (image &key dx dy))
;; alpha
(defgeneric image-alpha-get-fn (image &key dx dy region))
(defgeneric image-alpha-set-fn (image &key dx dy))
(defgeneric image-alpha-blend-fn (image &key dx dy))
