;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2003 by Tim Moore (moore@bricoworks.com)
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

#| Random notes:

An accepting-values stream diverts the calls to accept into calling
accept-present-default, as described in the spec.  The output record
produced by accept-present-default, as well as the current value of
that query, arguments that were passed to accept, etc. are stored in a
query object. The stream stores all the query objects for this
invocation of accepting-values. The record created and returned by
accept-present-default must be a subclass of updating-output-record.

After the initial output records are drawn, invoke-accepting-values
blocks accepting commands. The state of the dialog state machine is changed
via these commands. The commands currently are:

COM-SELECT-QUERY query-id -- calls the method select-query with the
corresponding query object and output record object. When select-query returns
the "next" field, if any, is selected so the user can move from field to field
easily.

COM-CHANGE-QUERY query-id value -- This command is used to directly change the
value of a query field that does not need to be selected first for input. For
example, a user would click directly on a radio button without selecting the
gadget first.

COM-DESELECT-QUERY -- deselects the currently selected query.

COM-QUERY-EXIT -- Exits accepting-values

COM-QUERY-ABORT -- Aborts accepting-values

These commands are generated in two ways. For query fields that are entirely
based on CLIM drawing commands and presentations, these are emitted by
presentation translators. There is a presentation type selectable-query that
throws com-select-query for the :select gesture. Fields that are based on
gadgets have to throw presentations from their callbacks. This can be done
using  the method on p. 305 of the Franz CLIM user guide, or by using  the
McCLIM function throw-object-ptype.

After a command is executed the body of accepting-values is rerun, calling
accept-present-default again to update the fields' graphic appearance. [This
may be calling these methods too often an may change in the future]. The
values returned by the user's calls to accept are come from the query objects.


If a query field is selectable than it should implement the method
select-query:

SELECT-QUERY stream query record -- Make a query field active and do any
input. This should change the query object and setf (changedp query). This
method might be interrupted at any time if the user selects another field.

|#

(in-package :clim-internals)

(defclass query ()
  ((query-identifier :accessor query-identifier :initarg :query-identifier)
   (ptype :accessor ptype :initarg :ptype)
   (view :accessor view :initarg :view)
   (default :accessor default :initarg :default :initform nil)
   (default-supplied-p :accessor default-supplied-p
     :initarg :default-supplied-p :initform nil)
   (value :accessor value :initarg :value :initform nil)
   (changedp :accessor changedp :initform nil)
   (record :accessor record :initarg :record)
   (activation-gestures :accessor activation-gestures
			:initform *activation-gestures*
			:documentation "Binding of *activation-gestures* on
entry to this accept") 
   (delimeter-gestures :accessor delimiter-gestures
		       :initform *delimiter-gestures*
		       :documentation "Binding of *delimeter-gestures* on entry
to this accept")
   (accept-arguments :accessor accept-arguments :initarg :accept-arguments)
   (accept-condition :accessor accept-condition :initarg :accept-condition
		     :initform nil
		     :documentation "Condition signalled, if any, during
accept of this query")))

(defclass accepting-values-record (standard-updating-output-record)
  ())

(defclass accepting-values-stream (standard-encapsulating-stream)
  ((queries :accessor queries :initform nil)
   (selected-query :accessor selected-query :initform nil)
   (align-prompts :accessor align-prompts :initarg :align-prompts
		  :initform nil)
   (last-pass :accessor last-pass :initform nil
	      :documentation "Flag that indicates the last pass through the
  body of ACCEPTING-VALUES, after the user has chosen to exit. This controls
  when conditions will be signalled from calls to ACCEPT.")))

(defmethod stream-default-view ((stream accepting-values-stream))
  +textual-dialog-view+)

(define-condition av-exit (condition)
  ())

;;; The accepting-values state machine is controlled by commands. Each
;;; action (e.g., "select a text field") terminates 

(define-command-table accept-values)    ; :inherit-from nil???

(defvar *default-command* '(accepting-values-default-command))

;;; The fields of the query have presentation type query.  Fields that
;;; are "selectable", like the default text editor field, have type
;;; selectable-query.  The presentation object is the query
;;; identifier.

(define-presentation-type query () :inherit-from t)

(define-presentation-type selectable-query () :inherit-from 'query)

(define-presentation-type exit-button () :inherit-from t)

(define-presentation-type abort-button () :inherit-from t)

(defvar *accepting-values-stream* nil)

(defmacro with-stream-in-own-window ((&optional (stream '*query-io*)
                                                &rest further-streams)
                                     (&optional label)
                                     &rest body)
  `(let* ((,stream (open-window-stream :label ,label
                                       :input-buffer (climi::frame-event-queue *application-frame*)))
          ,@(mapcar (lambda (a-stream)
                      (list a-stream stream))
                    further-streams))
     (unwind-protect
         (progn
           ,@body)
       (close ,stream))))

(defmacro accepting-values
    ((&optional (stream t)
      &rest args
      &key own-window exit-boxes initially-select-query-identifier
           modify-initial-query resynchronize-every-pass resize-frame
           align-prompts label scroll-bars
           x-position y-position width height command-table frame-class)
     &body body)
  (declare (ignorable exit-boxes initially-select-query-identifier
            modify-initial-query resynchronize-every-pass resize-frame
            align-prompts scroll-bars
            x-position y-position width height command-table frame-class))
  (setq stream (stream-designator-symbol stream '*standard-input*))
  (with-gensyms (accepting-values-continuation)
    (let* ((return-form
            `(flet ((,accepting-values-continuation (,stream)
                      ,@body))
               (invoke-accepting-values ,stream
                                        #',accepting-values-continuation
                                        ,@args)))
           (true-form `(with-stream-in-own-window (,stream *standard-input* *standard-output*)
                         (,label)
                         ,return-form)))
      ;; To avoid unreachable-code warnings, if `own-window' is a
      ;; boolean constant, don't generate the `if' form.
      (cond ((eq own-window t) true-form)
            ((eq own-window nil) return-form)
            (t `(if ,own-window
                    ,true-form
                    ,return-form))))))

(defun invoke-accepting-values
    (stream body
     &key own-window exit-boxes
     (initially-select-query-identifier nil initially-select-p)
     modify-initial-query resynchronize-every-pass resize-frame
     align-prompts label scroll-bars
     x-position y-position width height
     (command-table 'accept-values)
     (frame-class 'accept-values))
  (declare (ignore own-window exit-boxes modify-initial-query
    resize-frame label scroll-bars x-position y-position
    width height frame-class))
  (when (and align-prompts ;; t means the same as :right
             (not (eq align-prompts :left)))
    (setf align-prompts :right))
  (multiple-value-bind (cx cy) (stream-cursor-position stream)
    (let* ((*accepting-values-stream*
            (make-instance 'accepting-values-stream
                           :stream stream
                           :align-prompts align-prompts))
           (arecord (updating-output (stream
                                      :record-type 'accepting-values-record)
                      (if align-prompts
                          (formatting-table (stream)
                            (funcall body *accepting-values-stream*))
                          (funcall body *accepting-values-stream*))
                      (display-exit-boxes *application-frame*
                                          stream
                                          (stream-default-view
                                           *accepting-values-stream*))))
           (first-time t)
           (current-command (if initially-select-p
                                `(com-select-query
                                  ,initially-select-query-identifier)
                                *default-command*)))
      (letf (((frame-command-table *application-frame*)
              (find-command-table command-table)))
        (unwind-protect
             (handler-case
                 (loop
                    (if first-time
                        (setq first-time nil)
                        (when resynchronize-every-pass
                          (redisplay arecord stream)))
                    (with-input-context
                        ('(command :command-table accept-values))
                      (object)
                      (progn
                        (apply (command-name current-command)
                               (command-arguments current-command))
                        ;; If current command returns without throwing a
                        ;; command, go back to the default command
                        (setq current-command *default-command*))
                      (t (setq current-command object)))
                    (redisplay arecord stream))
               (av-exit ()
                 (finalize-query-records *accepting-values-stream*)
		 (setf (last-pass *accepting-values-stream*) t)
                 (redisplay arecord stream)))
          (erase-output-record arecord stream)
          (setf (stream-cursor-position stream)
                (values cx cy)))))))

(defgeneric display-exit-boxes (frame stream view))

(defmethod display-exit-boxes (frame stream (view textual-dialog-view))
  (declare (ignore frame))
  (updating-output (stream :unique-id 'buttons :cache-value t)
    (fresh-line stream)
    (with-output-as-presentation
	(stream nil 'exit-button)
      (format stream "OK"))
    (write-char #\space stream)
    (with-output-as-presentation
	(stream nil 'abort-button)
      (format stream "Cancel"))
    (terpri stream)))

(defmethod stream-accept ((stream accepting-values-stream) type
			  &rest rest-args
			  &key
			  (view (stream-default-view stream))
			  (default nil default-supplied-p)
			  default-type
			  provide-default
			  insert-default
			  replace-input
			  history
			  active-p
			  prompt
			  prompt-mode
			  display-default
			  (query-identifier prompt)
			  activation-gestures
			  additional-activation-gestures
			  delimiter-gestures
			  additional-delimiter-gestures)
  (declare (ignore activation-gestures additional-activation-gestures
		   delimiter-gestures additional-delimiter-gestures))
  (let ((query (find query-identifier (queries stream)
		     :key #'query-identifier :test #'equal))
	(align (align-prompts stream)))
    (unless query
      ;; If there's no default but empty input could return a sensible value,
      ;; use that as a default.
      (unless default-supplied-p
	(setq default
	      (ignore-errors (accept-from-string type
						 ""
						 :view +textual-view+ ))))
      (setq query (make-instance 'query
				 :query-identifier query-identifier
				 :ptype type
				 :view view
				 :default default
				 :default-supplied-p default-supplied-p
				 :value default))
      (setf (queries stream) (nconc (queries stream) (list query)))
      (when default
        (setf (changedp query) t)))
    (setf (accept-arguments query) rest-args)
    ;; If the program changes the default, that becomes the value.
    (unless (equal default (default query)) 
      (setf (default query) default)
      (setf (value query) default))
    (flet ((do-prompt ()
	     (apply #'prompt-for-accept stream type view rest-args))
	   (do-accept-present-default ()
	     (funcall-presentation-generic-function
	      accept-present-default
	      type (encapsulating-stream-stream stream) view
	      (value query)
	      default-supplied-p nil query-identifier)))
      (let ((query-record nil))
	(if align
	    (formatting-row (stream)
	      (formatting-cell (stream :align-x align)
		(do-prompt))
	      (formatting-cell (stream)
		(setq query-record (do-accept-present-default))))
	    (progn
	      (do-prompt)
	      (setq query-record (do-accept-present-default))))
	(setf (record query) query-record)
	(when (and (last-pass stream) (accept-condition query))
	  (signal (accept-condition query)))
	(multiple-value-prog1
	    (values (value query) (ptype query) (changedp query))
	  (setf (default query) default)
	  (setf (ptype query) type)
	  (setf (changedp query) nil))))))


(defmethod prompt-for-accept ((stream accepting-values-stream)
			      type view
			      &rest args)
  (declare (ignore view))
  (apply #'prompt-for-accept-1 stream type :display-default nil args))

(define-command (com-query-exit :command-table accept-values
				:name nil
				:provide-output-destination-keyword nil)
    ()
  (signal 'av-exit))

(define-command (com-query-abort :command-table accept-values
				 :name nil
				 :provide-output-destination-keyword nil)
    ()
  (and (find-restart 'abort)
       (invoke-restart 'abort)))

(define-command (com-change-query :command-table accept-values
				  :name nil
				  :provide-output-destination-keyword nil)
    ((query-identifier t)
     (value t))
  (when *accepting-values-stream*
    (let ((query (find query-identifier (queries *accepting-values-stream*)
		       :key #'query-identifier :test #'equal)))
      (when query
	(setf (value query) value)
	(setf (changedp query) t)))))

(defgeneric select-query (stream query record)
  (:documentation "Does whatever is needed for input (e.g., calls accept) when
a query is selected for input. It is responsible for updating the
  query object when a new value is entered in the query field." ))

(defgeneric deselect-query (stream query record)
  (:documentation "Deselect a query field: turn the cursor off, turn off
highlighting, etc." ))

(define-command (com-select-query :command-table accept-values
				  :name nil
				  :provide-output-destination-keyword nil)
    ((query-identifier t))
  (when *accepting-values-stream*
    (with-accessors ((selected-query selected-query))
	*accepting-values-stream*
      (let* ((query-list (member query-identifier
				 (queries *accepting-values-stream*)
				 :key #'query-identifier :test #'equal))
	     (query (car query-list)))
	(when selected-query
	  (unless (equal query-identifier (query-identifier selected-query)) 
	    (deselect-query *accepting-values-stream*
			    selected-query
			    (record selected-query))))
	(when query
	  (setf selected-query query)
	  (select-query *accepting-values-stream* query (record query))
	  (let ((command-ptype '(command :command-table accept-values)))
	    (if (cdr query-list)
	      (throw-object-ptype `(com-select-query ,(query-identifier
						       (cadr query-list)))
				  command-ptype)
	      (throw-object-ptype '(com-deselect-query) command-ptype))))))))

(define-command (com-deselect-query :command-table accept-values
				    :name nil
				    :provide-output-destination-keyword nil)
    ()
  (when *accepting-values-stream*
    (with-accessors ((selected-query selected-query))
	*accepting-values-stream*
      (when selected-query
	(deselect-query *accepting-values-stream*
			selected-query
			(record selected-query))
	(setf selected-query nil)))))

(defclass av-text-record (standard-updating-output-record)
  ((editing-stream :accessor editing-stream)
   (snapshot :accessor snapshot :initarg :snapshot :initform nil
	     :documentation "A copy of the stream buffer before accept
is called. Used to determine if any editing has been done by user")))

(defparameter *no-default-cache-value* (cons nil nil))

;;; Hack until more views / dialog gadgets are defined.

(define-default-presentation-method accept-present-default
    (type stream (view text-field-view) default default-supplied-p
     present-p query-identifier)
  (if (width view)
      (multiple-value-bind (cx cy)
	  (stream-cursor-position stream)
	(declare (ignore cy))
	(letf (((stream-text-margin stream) (+ cx (width view))))
	  (funcall-presentation-generic-function accept-present-default
						 type
						 stream
						 +textual-dialog-view+
						 default default-supplied-p
						 present-p
						 query-identifier)))))

(define-default-presentation-method accept-present-default
    (type stream (view textual-dialog-view) default default-supplied-p
     present-p query-identifier)
  (declare (ignore present-p))
  (let* ((editing-stream nil)
	 (record (updating-output (stream :unique-id query-identifier
                                          :cache-value (if default-supplied-p
                                                           default
                                                           *no-default-cache-value*)
                                          :record-type 'av-text-record)
                   (with-output-as-presentation
                       (stream query-identifier 'selectable-query
                               :single-box t)
                     (surrounding-output-with-border
                         (stream :shape :inset :move-cursor t)
                       (setq editing-stream
                             (make-instance (if *use-goatee*
                                                'goatee-input-editing-stream
                                                'standard-input-editing-stream)
                                            :stream stream
                                            :cursor-visibility nil
                                            :background-ink +grey90+
                                            :single-line t
                                            :min-width t))))
                   (when default-supplied-p
                     (input-editing-rescan-loop ;XXX probably not needed
                      editing-stream
                      (lambda (s)
                        (presentation-replace-input s default type view
                                                    :rescan t)))))))
    (when editing-stream
      (setf (editing-stream record) editing-stream))
    record))

(defun av-do-accept (query record interactive)
  (let* ((estream (editing-stream record))
	 (ptype (ptype query))
	 (view (view query))
	 (default (default query))
	 (default-supplied-p (default-supplied-p query))
	 (accept-args (accept-arguments query))
	 (*activation-gestures* (apply #'make-activation-gestures
				       :existing-activation-gestures
				       (activation-gestures query)
				       accept-args))
	 (*delimiter-gestures* (apply #'make-delimiter-gestures
				      :existing-delimiter-args
				      (delimiter-gestures query)
				      accept-args)))
    ;; If there was an error on a previous pass, set the insertion pointer to
    ;; 0 so the user has a chance to edit the field without causing another
    ;; error. Otherwise the insertion pointer should already be at the end of
    ;; the input (because it was activated); perhaps we should set it anyway.
    (when (accept-condition query)
      (setf (stream-insertion-pointer estream) 0))
    (reset-scan-pointer estream)
    (setf (accept-condition query) nil)
    ;; If a condition is thrown, then accept should return the old value and
    ;; ptype.
    (block accept-condition-handler
      (setf (changedp query) nil)
      (setf (values (value query) (ptype query))
	    (input-editing-rescan-loop
	     estream
	     #'(lambda (s)
		 (handler-bind
		     ((error
		       #'(lambda (c)
			   (format *trace-output*
                                   "accepting-values accept condition: ~A~%"
                                   c)
			   (if interactive
                               (progn
                                 (beep)
                                 (setf (stream-insertion-pointer estream)
                                       (max 0 (1- (stream-scan-pointer estream))))
                                 (immediate-rescan estream)
                                 (format *trace-output* "Ack!~%"))
                               (progn
                                 (setf (accept-condition query) c)
                                 (return-from accept-condition-handler
                                   c))))))
		   (if default-supplied-p
		       (accept ptype :stream s
			       :view view :prompt nil :default default)
		       (accept ptype :stream s :view view :prompt nil))))))
      (setf (changedp query) t))))




;;; The desired 
(defmethod select-query (stream query (record av-text-record))
  (declare (ignore stream))
  (let ((estream (editing-stream record))
	(ptype (ptype query))
	(view (view query)))
    (declare (ignore ptype view))	;for now
    (with-accessors ((stream-input-buffer stream-input-buffer))
	estream
      (setf (cursor-visibility estream) t)
      (setf (snapshot record) (copy-seq stream-input-buffer))
      (av-do-accept query record t))))


;;; If the query has not been changed (i.e., ACCEPT didn't return) and there is
;;; no error, act as if the user activated the query.
(defmethod deselect-query (stream query (record av-text-record))
  (let ((estream (editing-stream record)))
    (setf (cursor-visibility estream) nil)
    (when (not (or (changedp query) (accept-condition query)))
      (finalize-query-record query record))))


(defgeneric finalize-query-record (query record)
  (:documentation "Do any cleanup on a query before the accepting-values body
is run for the last time"))

(defmethod finalize-query-record (query record)
  nil)

;;; If the user edits a text field, selects another text field and
;;; then exits from accepting-values without activating the first
;;; field, the values returned would be some previous value or default
;;; for the field, not what's on the screen.  That would be completely
;;; bogus.  So, if a field has been edited but not activated, activate
;;; it now.  Unfortunately that's a bit hairy.

(defmethod finalize-query-record (query (record av-text-record))
  (let ((estream (editing-stream record)))
    (when (and (snapshot record)
	       (not (equal (snapshot record)
			   (stream-input-buffer estream))))
      (let* ((activation-gestures (apply #'make-activation-gestures
					 :existing-activation-gestures
					 (activation-gestures query)
					 (accept-arguments query)))
	     (gesture (car activation-gestures)))
	(when gesture
	  (let ((c (character-gesture-name gesture)))
	    (activate-stream estream c)
	    (reset-scan-pointer estream)
	    (av-do-accept query record nil)))))))

(defun finalize-query-records (av-stream)
  (loop for query in (queries av-stream)
	do (finalize-query-record query (record query))))


(define-presentation-to-command-translator com-select-field
    (selectable-query com-select-query accept-values
     :gesture :select
     :documentation "Select field for input"
     :pointer-documentation "Select field for input"
     :echo nil
     :tester ((object)
	      (let ((selected (selected-query *accepting-values-stream*)))
		(or (null selected)
		    (not (eq (query-identifier selected) object))))))
  (object)
  `(,object))

(define-presentation-to-command-translator com-exit-button
    (exit-button com-query-exit accept-values
     :gesture :select
     :documentation "Exit dialog"
     :pointer-documentation "Exit dialog"
     :echo nil)
  ()
  ())

(define-presentation-to-command-translator com-abort-button
    (abort-button com-query-abort accept-values
     :gesture :select
     :documentation "Abort dialog"
     :pointer-documentation "Abort dialog"
     :echo nil)
  ()
  ())

(defun accepting-values-default-command ()
  (loop
   (read-gesture :stream *accepting-values-stream*)))
