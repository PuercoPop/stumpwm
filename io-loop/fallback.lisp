;; Copyright (C) 2016  Fredrik Tolf <fredrik@dolda2000.com>

;; This file is part of StumpWM.

;; StumpWM is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; StumpWM is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Comentary:

;;; Fall-back ioloop implementation

;; This file is left mostly for pedagogical purposes as SBCL is the only
;; implementation supported.

(in-package #:stumpwm)

;;; Dummy XLIB I/O loop
(defclass xlib-io-loop ()
  ((display-ch :initform nil)
   (display :initform nil)
   (timers :initform '()))
  (:documentation
   "Implements a \"dummy\" I/O loop for Lisps lacking an actual
  implementation. The dummy loop should be sufficient for StumpWM
  usage, but lacks support for listening to multiple I/O channels. It
  supports monitoring exactly one XLIB:DISPLAY object, and any number
  of virtual channels."))

(defmethod io-loop-add ((info xlib-io-loop) channel)
  (let ((fd (io-channel-ioport info channel)))
    (cond
      ((and (listp fd) (eq (first fd) :display))
       (with-slots (display-ch display) info
         (when display-ch
           (error "Dummy I/O loop implementation only supports one XLIB display"))
         (setf display-ch channel
               display (second fd))))
      ((null fd)
       (with-slots (timers) info
         (when (find channel timers)
           (error "Timer channel is already registered"))
         (push channel timers)))
      (t (error "Non-display, non-pure-timeout channels not supported by dummy I/O loop")))))

(defmethod io-loop-remove ((info xlib-io-loop) channel)
  (with-slots (display display-ch timers) info
    (cond ((eq display-ch channel)
           (setf display-ch nil
                 display nil))
          ((find channel timers)
           (setf timers (delete channel timers)))
          (t (error "I/O channel is not currently registered")))))

(defmethod io-loop-update ((info xlib-io-loop) channel)
  (declare (ignore info channel)))

(defmethod io-loop ((info xlib-io-loop) &key description)
  (with-simple-restart (quit-ioloop "Quit I/O loop~A"
                                    (if description
                                        (format nil " (~A)" description)
                                        ""))
    (labels ((channel-timeout (ch)
               (let ((evs (io-channel-events ch)))
                 (second (find :timeout evs :key (lambda (ev) (and (listp ev) (car ev)))))))
             (next-timeout ()
               (let ((timers (remove nil (mapcar #'channel-timeout (slot-value info 'timers)))))
                 (and timers
                      (max (/ (- (apply 'min timers)
                                 (get-internal-real-time))
                              internal-time-units-per-second)
                           0)))))
      (block io-loop
        (loop
           (with-simple-restart (restart-ioloop "Restart at I/O loop~A"
                                                (if description
                                                    (format nil " (~A)" description)
                                                    ""))
             (with-slots (display-ch display timers) info
               (let ((rem-ch '()))
                 (dolist (channel timers)
                   (let ((evs (io-channel-events channel)))
                     (cond ((null evs)
                            (push channel rem-ch))
                           ((find :loop evs)
                            (io-channel-handle channel :loop)))))
                 (dolist (channel rem-ch)
                   (io-loop-remove info channel)))
               (let ((timeout (next-timeout)))
                 (cond (display-ch
                        (io-channel-handle display-ch :loop)
                        (let ((nevents (xlib:event-listen display (and timeout (ceiling timeout)))))
                          (when nevents
                            (io-channel-handle display-ch :read))))
                       (timeout
                        (sleep timeout))
                       (t (return-from io-loop))))
               (when timers
                 (let ((now (get-internal-real-time)))
                   (dolist (channel timers)
                     (when (< (channel-timeout channel) now)
                       (io-channel-handle channel :timeout))))))))))))

(defmethod io-channel-ioport ((io-loop xlib-io-loop) (channel xlib:display))
  (list :display channel))
