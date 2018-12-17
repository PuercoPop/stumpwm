;;; Commentary

;; A Completing-read replacement for input.lisp
;; There are 3 different parts to this
;; 1) A 'readline' on top of CLX.
;; 2) An Auto-completion 'framework'
;; 3) A plug-able ranking system.

;; The readline implementation on top of CLX just presents a prompt for the
;; user to write into and returns the string entered by the user. It has hooks
;; to navigate a 'history'/'completion' list.

;; The plug-able ranking systems that sorts a set of candidates according to
;; their similarity to a pattern represented as a number from 0 to 1.

;; TODO
;; Make file compile

;;; Code

(defpackage #:ido
  (:use #:cl
        #:stumpwm)
  (:shadow #:input-point)
  ;; The internal symbols
  (:import-from #:stumpwm
                #:screen-message-gc
                #:screen-input-window
                #:screen-font)
  (:export #:completing-read
           #:input-line
           #:input-contents
           #:input-point

           #:accept
           #:abort
           #:complete
           #:process-input

           #:insert-char))

(in-package #:ido)


(defgeneric accept (input)
  (:documentation "Accept the input"))
(defgeneric abort (input)
  (:documentation "Abort."))
(defgeneric complete (input)
  (:documentation "Present candidates for completion.")

(defvar *completing-read-map*)

(defgeneric insert-char (char line))
(defgeneric insert-string (char line))
(defgeneric forward-char (line &optinal (count 1)))
(defgeneric backward-word (line))

(defclass input-line ()
  ((contents :initarg :contents
             :initform ""
             :accessor input-contents)
   (point :initarg :point
          :initform 0
          :accessor input-point)
   ;; history
   ;; history-bk
   ))

(defclass password-input (input-line)
  ())

(defmethod print-object ((obj input-line) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Contents: \"~A\"." (input-contents obj))))

(defmethod print-object ((obj password-input) stream)
  (print-unreadable-object (obj stream :type t)
    (dotimes (c (length (input-contents obj)))
      (princ "*" stream))))


(defun before-point (line)
  (subseq (input-contents line)
          0
          (input-point line)))

(defun after-point (line)
  (subseq (input-contents line)
          (input-point line)))

(defmethod insert-char ((char character) (line input-line))
  (setf (input-contents line)
        (concatenate 'string
                     (before-point line)
                     (string char)
                     (after-point line)))
  (incf (input-point line)))

;; TODO: Delete
;; Should I replace it with a symbol-macro inserted with-current-screen?
(defun current-screen ()
  (car *screen-list*))

(defun completing-read  (prompt completions)
  "Setup the prompt"
  (let* ((current-screen (car *screen-list*))
         (ctx (stumpwm::screen-message-gc current-screen))
         (win (stumpwm::screen-input-window current-screen))
         (font (stumpwm::screen-font current-screen)))
    (xlib:draw-rectangle win ctx
                         0 0
                         200 200
                         t)))

(defun delete-backward-char ()
  )
(defun delete-backward-word ()
  )

;; (defvar *input*
;;   (let ((map (make-sparse-keymap)))
;;     ))
