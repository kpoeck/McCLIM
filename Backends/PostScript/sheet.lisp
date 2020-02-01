;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)
;;;           Gilbert Baumann (unk6@rz.uni-karlsruhe.de)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; TODO:
;;;
;;; - do smth with POSTSCRIPT-GRAFT.

;;; Also missing IMO:
;;;
;;; - WITH-OUTPUT-TO-POSTSCRIPT-STREAM should offer a :PAPER-SIZE option.
;;; - NEW-PAGE should also offer to specify the page name.
;;; - device fonts are missing
;;; - font metrics are missing
;;;
;;;--GB

(in-package :clim-postscript)

(defun write-font-to-postscript-stream (stream text-style)
  (with-open-file (font-stream
		   (clim-postscript-font:postscript-device-font-name-font-file
                    (clim-internals::device-font-name text-style))
		   :direction :input
		   :external-format :latin-1)
    (let ((font (make-string (file-length font-stream))))
      (read-sequence font font-stream)
      (write-string font (postscript-medium-file-stream stream)))))

(defmacro with-output-to-postscript-stream ((stream-var file-stream
                                             &rest options)
                                            &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream-var)
              ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-postscript-stream #',cont
                                                ,file-stream ,@options))))

(defun invoke-with-output-to-postscript-stream (continuation
                                                file-stream &key device-type
                                                              multi-page scale-to-fit
                                                              (orientation :portrait)
                                                              header-comments)
  (flet ((make-it (file-stream)
           (climb:with-port (port :ps :stream file-stream)
             (let ((stream (make-postscript-stream file-stream port device-type
                                                   multi-page scale-to-fit
                                                   orientation header-comments))
                   translate-x translate-y)
               (unwind-protect
                    (with-slots (file-stream title for orientation paper) stream
                      (with-output-recording-options (stream :record t :draw nil)
                        (with-graphics-state (stream)
                          ;; we need at least one level of saving -- APD, 2002-02-11
                          (funcall continuation stream)
                          (unless (eql paper :eps)
                            (new-page stream)))) ; Close final page.
                      (format file-stream "%!PS-Adobe-3.0~@[ EPSF-3.0~*~]~@
                                           %%Creator: McCLIM~@
                                           %%Title: ~A~@
                                           %%For: ~A~@
                                           %%LanguageLevel: 2~%"
                              (eq device-type :eps) title for)
                      (case paper
                        ((:eps)
                         (let ((record (stream-output-history stream)))
                           (with-bounding-rectangle* (lx ly ux uy) record
                                                     (setf translate-x (- (floor lx))
                                                           translate-y (ceiling uy))
                                                     (format file-stream "%%BoundingBox: ~A ~A ~A ~A~%"
                                                             0 0
                                                             (+ translate-x (ceiling ux))
                                                             (- translate-y (floor ly))))))
                        (t
                         (multiple-value-bind (width height) (paper-size paper)
                           (format file-stream "%%BoundingBox: 0 0 ~A ~A~@
                                                %%DocumentMedia: ~A ~A ~A 0 () ()~@
                                                %%Orientation: ~A~@
                                                %%Pages: (atend)~%"
                                   width height paper width height
                                   (ecase orientation
                                     (:portrait "Portrait")
                                     (:landscape "Landscape"))))))
                      (format file-stream "%%DocumentNeededResources: (atend)~@
                                           %%EndComments~%~%")
                      (write-postscript-dictionary file-stream)
                      (dolist (text-style (clim-postscript-font:device-fonts (sheet-medium stream)))
                        (write-font-to-postscript-stream (sheet-medium stream) text-style))
                      (start-page stream)
                      (format file-stream "~@[~A ~]~@[~A translate~%~]" translate-x translate-y)

                      (with-output-recording-options (stream :draw t :record nil)
                        (with-graphics-state (stream)
                          (case paper
                            ((:eps) (replay (stream-output-history stream) stream))
                            (t (let ((last-page (first (postscript-pages stream))))
                                 (dolist (page (reverse (postscript-pages stream)))
                                   (replay page stream)
                                   (unless (eql page last-page)
                                     (emit-new-page stream)))))))))

                 (with-slots (file-stream current-page document-fonts) stream
                   (format file-stream "end~%showpage~%~@
                                        %%Trailer~@
                                        %%Pages: ~D~@
                                        %%DocumentNeededResources: ~{font ~A~%~^%%+ ~}~@
                                        %%EOF~%"
                           current-page (reverse document-fonts))
                   (finish-output file-stream)))))))
    (typecase file-stream
      ((or pathname string)
       (with-open-file (stream file-stream :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
         (make-it stream)))
      (t (make-it file-stream)))))

(defun start-page (stream)
  (with-slots (file-stream current-page transformation) stream
    (format file-stream "%%Page: ~D ~:*~D~%" (incf current-page))
    (format file-stream "~A begin~%" *dictionary-name*)))

(defmethod new-page ((stream postscript-stream))
  (push (stream-output-history stream) (postscript-pages stream))
  (let ((history (make-instance 'standard-tree-output-history :stream stream)))
    (setf (slot-value stream 'climi::output-history) history
	  (stream-current-output-record stream) history))
  (setf (stream-cursor-position stream)
        (stream-cursor-initial-position stream)))

(defun emit-new-page (stream)
  ;; FIXME: it is necessary to do smth with GS -- APD, 2002-02-11
  ;; FIXME^2:  what do you mean by that? -- TPD, 2005-12-23
  (postscript-restore-graphics-state stream)
  (format (postscript-stream-file-stream stream) "end~%showpage~%")
  (start-page stream)
  (postscript-save-graphics-state stream))


;;; Output Protocol

(defmethod medium-drawable ((medium postscript-medium))
  (postscript-medium-file-stream medium))

(defmethod make-medium ((port postscript-port) (sheet postscript-stream))
  (make-instance 'postscript-medium :sheet sheet))

(defmethod medium-miter-limit ((medium postscript-medium))
  #.(* pi (/ 11 180))) ; ?

(defmethod sheet-direct-mirror ((sheet postscript-stream))
  (postscript-stream-file-stream sheet))

(defmethod sheet-mirrored-ancestor ((sheet postscript-stream))
  sheet)

(defmethod sheet-mirror ((sheet postscript-stream))
  (sheet-direct-mirror sheet))

(defmethod realize-mirror ((port postscript-port) (sheet postscript-stream))
  (sheet-direct-mirror sheet))

(defmethod destroy-mirror ((port postscript-port) (sheet postscript-stream))
  (error "Can't destroy mirror for the postscript stream ~S." sheet))

;;; Some strange functions

(defmethod pane-viewport ((pane postscript-stream))
  nil)

(defmethod scroll-extent ((pane postscript-stream) x y)
  (declare (ignore x y))
  (values))

;;; POSTSCRIPT-GRAFT

(defclass postscript-graft (sheet-leaf-mixin basic-sheet)
  ((width  :initform 210 :reader postscript-graft-width)
   (height :initform 297 :reader postscript-graft-height)))

(defmethod graft-orientation ((graft postscript-graft))
  :graphics)

(defmethod graft-units ((graft postscript-graft))
  :device)

(defun graft-length (length units)
  (* length (ecase units
              (:device       (/ 720 254))
              (:inches       (/ 10 254))
              (:millimeters  1)
              (:screen-sized (/ length)))))

(defmethod graft-width ((graft postscript-graft) &key (units :device))
  (graft-length (postscript-graft-width graft) units))

(defmethod graft-height ((graft postscript-graft) &key (units :device))
  (graft-length (postscript-graft-height graft) units))

(defun make-postscript-graft ()
  (make-instance 'postscript-graft))

(defmethod sheet-region ((sheet postscript-graft))
  (let ((units (graft-units sheet)))
    (make-rectangle* 0 0
                     (graft-width sheet :units units)
                     (graft-height sheet :units units))))

(defmethod graft ((sheet postscript-graft))
  sheet)

;;; Port

(setf (get :ps :port-type) 'postscript-port)
(setf (get :ps :server-path-parser) 'parse-postscript-server-path)

(defun parse-postscript-server-path (path)
  path)
