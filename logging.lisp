;; Copyright (C) 2016 Javier Olaechea
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

(defpackage #:stumpwm-logging
  (:use #:cl)
  (:import-from #:stumpwm-configuration
                #:data-dir-file)
  (:export
   #:+stumpwm-log-file+
   #:+stumpwm-log-rotation-interval+
   #:start-log)
  (:documentation "This module provides options for configuring the logging
  done by StumpWM."))

(in-package #:stumpwm-logging)

(defparameter +stumpwm-log-file+ (data-dir-file "stumpwm" "log")
  "The file where logging is written to.")

(defparameter +stumpwm-log-rotation-interval+ "0 0 1 * *"
  "The frequency with which StumpWM rotates is log file. Specified using the
  crontab notation. For more information chech man 5 crontab.")


(defun start-log ()
  "Start a rotating log. The log is saved in the file, +STUMPWM-LOG-FILE+ and
rotated according to +STUMPWM-LOG-ROTATION-INTERVAL+."
  (v:add-pipe
   (make-instance 'piping:predicate-filter :predicate #'(lambda (message) (v:message-visible message :WARN)))
   (make-instance 'v:rotating-log-faucet
                  :file +stumpwm-log-file+
                  :interval (v:make-cron-interval "* * * * *"))))
