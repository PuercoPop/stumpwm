;; Copyright (C) 2016 Caio Oliveira
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;;; Macro for defining interactive command. Just pushes and pops new keymaps.
;;
;; Code:

(in-package #:stumpwm)

(export 'deficommand)

(defun icommand-enter-interactive-mode (kmap name)
  "Enters interactive mode."
  (message "~A started" name)
  (push-top-map kmap))

(defun icommand-exit-interactive-mode (name)
  "Exits interactive  mode."
  (message "~A finished" name)
  (pop-top-map))

(defmacro deficommand (name (&key on-entry on-exit) &body key-bindings)
  "Defines an interactive command."
  (multiple-value-bind (key-bindings decls documentation)
      (parse-body key-bindings :documentation t)
    (declare (ignore decls)) ;; TODO: When defcommand handles declarations use decls argument

    (let* ((command (if (listp name) (car name) name))
           (exit-command (symbolicate "EXIT-" command))
           (keymap (gensym "KEYMAP-")))
      `(let ((,keymap (make-sparse-keymap)))
         ,@(loop for key-binding in key-bindings
                 collect `(define-key ,keymap ,@key-binding))
         (define-key ,keymap (kbd "RET") ,(string exit-command))
         (define-key ,keymap (kbd "C-g") ,(string exit-command))
         (define-key ,keymap (kbd "ESC") ,(string exit-command))

         (defcommand ,name () ()
           ,(or documentation
                (format nil "Starts interactive command \"~A\"" command))
           ,@(when on-entry
               `((funcall ,on-entry)))
           (icommand-enter-interactive-mode ,keymap ',command))

         (defcommand ,exit-command () ()
           ,@(when on-exit
               `((funcall ,on-exit)))
           (icommand-exit-interactive-mode ',command))))))
