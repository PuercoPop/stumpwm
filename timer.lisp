;; Copyright (C) 2016 Fredrik Tolf <fredrik@dolda2000.com>
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
;; Code:

(defpackage #:stumpwm-timer
  (:use #:cl)
  (:export #:cancel-timer
           #:run-with-timer
           ;; #:*toplevel-io*
           #:timer-p)
  (:documentation))

(in-package #:stumpwm-timer)

;; Move to priority-queue
(defvar *timer-list* ()
  "List of active timers.")

(defstruct timer
  time repeat function args)

(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if repeat is non-nil.
SECS and REPEAT may be reals.
The action is to call FUNCTION with arguments ARGS."
  (check-type secs (real 0 *))
  (check-type repeat (or null (real 0 *)))
  (check-type function (or function symbol))
  (let ((timer (make-timer
                :repeat repeat
                :function function
                :args args)))
    (schedule-timer timer secs)
    (setf *timer-list* (merge 'list *timer-list* (list timer) #'< :key #'timer-time))
    timer))

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (check-type timer timer)
  (setf *timer-list* (remove timer *timer-list*)))

(defun schedule-timer (timer when)
  "Schedule the timer to execute in "
  (setf (timer-time timer)
        (+ (get-internal-real-time)
           (* when internal-time-units-per-second))))

(defun run-expired-timers ()
  (let ((now (get-internal-real-time))
	(timers *timer-list*)
	(pending ())
	(remaining ()))
    (dolist (timer timers)
	    (if (<= (timer-time timer) now)
		(progn (push timer pending)
		       (when (timer-repeat timer)
			 (schedule-timer timer (timer-repeat timer))
			 (push timer remaining)))
		(push timer remaining)))
    (setf *timer-list*
          (sort remaining #'< :key #'timer-time))
    (dolist (timer pending)
      (apply (timer-function timer) (timer-args timer)))))

;; Rework API to (get-next-timer (timers)) -> (or timer nil) (or next-timout nil)
(defun get-next-timeout (timers)
  "Return the number of seconds until the next timeout or NIL if there are no
timers."
  (when timers
    (max (/ (- (timer-time (car timers)) (get-internal-real-time))
            internal-time-units-per-second)
         0)))
