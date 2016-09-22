;; Copyright (C) 2003-2008 Shawn Betts
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
;; This file contains the code needed to handle the persistence and loading of
;; data used by StumpWM between sessions.
;;
;; Code:

(defpackage #:stumpwm-configuration
  (:use #:cl)
  (:export
   #:*data-dir*
   #:data-dir-file
   #:with-data-file))

(in-package #:stumpwm-configuration)

(defvar *data-dir* nil
  "The directory used by stumpwm to store data between sessions.")

(defun data-dir-file (name &optional type)
  "Return a pathname inside stumpwm's data dir with the specified name and type"
  (ensure-directories-exist *data-dir*)
  (make-pathname :name name :type type :defaults *data-dir*))

(defmacro with-data-file ((s file &rest keys &key (if-exists :supersede) &allow-other-keys) &body body)
  "Open a file in StumpWM's data directory. keyword arguments are sent
directly to OPEN. Note that IF-EXISTS defaults to :supersede, instead
of :error."
  (declare (ignorable if-exists))
  `(progn
     (ensure-directories-exist *data-dir*)
     (with-open-file (,s ,(merge-pathnames file *data-dir*)
                         ,@keys)
       ,@body)))
