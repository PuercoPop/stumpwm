;; Copyright (C) 2008 Julian Stecklina, Shawn Betts, Ivy Foster
;; Copyright (C) 2014 David Bjergaard
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
;; Use `set-module-dir' to set the location stumpwm searches for modules.

;; Code:

(in-package #:stumpwm)

(export '(load-module
          list-modules
          find-module
          add-to-load-path))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules) :require-match t)))

(defun list-modules ()
  "Return a list of the available modules."
  (remove-if #'null
             (let (results)
               (asdf:map-systems (lambda (system)
                                   (when (stumpwm-module-p system)
                                     (setf results (cons system results)))))
               results)))

(defvar *only-stumpwm-modules* nil
  "A sentinel to inform `asdf:find-system' to only retrieve StumpWM modules.")

(defmethod asdf:find-system :around (system &optional error-p)
  (if *only-stumpwm-modules*
      (let ((system (call-next-method)))
        (if (stumpwm-module-p system)
            system
            nil))
      (call-next-method)))

(defun find-module (name)
  (let ((*only-stumpwm-modules* t))
    (asdf:find-system name)))

(defcommand add-to-load-path (path) ((:string "Directory: "))
  "If `PATH' is not in `*LOAD-PATH*' add it, check if `PATH' contains
an asdf system, and if so add it to the central registry. When not called as a command it also understands ASDF's configuration DSL."
  (asdf:ensure-source-registry path))

(defcommand load-module (name) ((:module "Load Module: "))
  "Loads the contributed module with the given NAME."
  (let ((module (find-module name)))
    (when module
      (asdf:operate 'asdf:load-op module))))
