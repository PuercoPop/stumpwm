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

;;; Commentary:

;;; SBCL IO-loop implementation

;; It would be generally nice if SBCL supported epoll/kqueue, but it
;; doesn't. The general I/O loop interface is consistent with such
;; implementations, however, so if support is added at any time, it
;; could be supported fairly easily.

;; If need should arise, it should also be quite simple to add
;; thread-safe operation.

(in-package #:stumpwm)

(defclass sbcl-io-loop ()
  ((channels :initarg :channels
             :initform ()
             :accessor channels
             :documentation "The list channels the I/O Loop is listening on."))
  (:documentation
   "Implements a select(2)-based I/O loop for SBCL. The
  implementation is not particularly optimal, mostly because any
  efficiency ambitions are mostly pointless as long as SBCL lacks
  support for epoll/kqueue, but should work well enough for I/O loops
  with relatively few channels.

  The implementation currently supports monitoring SB-SYS:FD-STREAM
  and XLIB:DISPLAY objects."))

;; TODO: rename info parameter. To loop?
(defmethod io-loop-add ((info sbcl-io-loop) channel)
  ;; TODO: Should I log  on this? (error "I/O channel is already registered")
  (unless (find channel (channels info))
    (push channel (channels info))))

(defmethod io-loop-remove ((info sbcl-io-loop) channel)
  ;; TODO: (error "I/O channel is not currently registered")
  (setf (channels info)
        (delete channel (channels info))))

(defmethod io-loop-update ((info sbcl-io-loop) channel)
  (declare (ignore info channel)))

(defmethod io-loop ((info sbcl-io-loop) &key description)
  (with-simple-restart (:quit-ioloop "Quit I/O loop~A"
                                     (if description
                                         (format nil " (~A)" description)
                                         ""))
    (block io-loop
      (loop
        (with-simple-restart (:restart-ioloop "Restart at I/O loop~A"
                                              (if description
                                                  (format nil " (~A)" description)
                                                  ""))
          (macrolet ((with-channel-restarts ((channel &optional remove-code) &body body)
                       (let ((ch (gensym "CHANNEL")))
                         `(let* ((,ch ,channel)
                                 (*current-io-channel* ,ch))
                            (restart-case
                                (progn ,@body)
                              (:skip-channel ()
                               :report (lambda (s)
                                         (format s "Continue as if without channel ~S" ,ch))
                                nil)
                              (:remove-channel ()
                               :report (lambda (s)
                                         (format s "Unregister channel ~S and continue" ,ch))
                                ,(or remove-code `(io-loop-remove info ,ch))
                                nil))))))
            (let ((ch-map (make-hash-table :test 'eql)) (rfds 0) (wfds 0) (maxfd 0)
                  (timeouts ())
                  (loop-ch ()))
              ;; Since it is select(2)-based, this implementation
              ;; updates the entire set of interesting events once
              ;; every iteration.
              (let ((remove ()))
                (dolist (channel (slot-value info 'channels))
                  (with-channel-restarts (channel (push channel remove))
                    (let ((fd (io-channel-ioport info channel)))
                      (let ((events (io-channel-events channel)))
                        (if events
                            (dolist (event events)
                              (multiple-value-bind (event data)
                                  (if (consp event) (values (car event) (cdr event)) (values event nil))
                                (case event
                                  (:read
                                   (setf maxfd (max maxfd fd)
                                         rfds (logior rfds (ash 1 fd)))
                                   (push (cons :read channel) (gethash fd ch-map ())))
                                  (:write
                                   (setf maxfd (max maxfd fd)
                                         wfds (logior wfds (ash 1 fd)))
                                   (push (cons :write channel) (gethash fd ch-map ())))
                                  (:timeout
                                   (let ((timeout (car data)))
                                     (check-type timeout real)
                                     (push (cons timeout channel) timeouts)))
                                  (:loop
                                    (push channel loop-ch)))))
                            (push channel remove))))))
                (dolist (channel remove)
                  (io-loop-remove info channel))
                (unless (slot-value info 'channels)
                  (return-from io-loop)))
              ;; Call any :LOOP channels
              (dolist (channel loop-ch)
                (with-channel-restarts (channel)
                  (io-channel-handle channel :loop)))
              (setf timeouts (sort timeouts '< :key 'car))
              (multiple-value-bind (to-sec to-usec)
                  (if timeouts
                      (let ((left (max (round (* (/ (- (car (first timeouts)) (get-internal-real-time))
                                                    internal-time-units-per-second)
                                                 1000000))
                                       0)))
                        (floor left 1000000))
                      (values nil nil))
                ;; Actually block for events
                (multiple-value-bind (result rfds wfds efds)
                    (sb-unix:unix-select (1+ maxfd)
                                         rfds wfds rfds to-sec to-usec)
                  (cond ((null result)
                         (let ((errno rfds))
                           (cond ((eql errno sb-unix:eintr)
                                  nil)
                                 (t (error "Unexpected ~S error: ~A" 'sb-unix:unix-select (sb-int:strerror errno))))))
                        ((> result 0)
                         ;; Notify channels for transpired events
                         (let ((afds (logior rfds wfds efds)))
                           (maphash (lambda (fd evs)
                                      (when (not (= (logand afds (ash 1 fd)) 0))
                                        (let ((r (not (= (logand rfds (ash 1 fd)) 0)))
                                              (w (not (= (logand wfds (ash 1 fd)) 0)))
                                              (e (not (= (logand efds (ash 1 fd)) 0))))
                                          (dolist (ev evs)
                                            (with-channel-restarts ((cdr ev))
                                              (cond ((and (eq (car ev) :read)
                                                          (or r e))
                                                     (io-channel-handle (cdr ev) :read))
                                                    ((and (eq (car ev) :write)
                                                          w)
                                                     (io-channel-handle (cdr ev) :write))))))))
                                    ch-map))))))
              ;; Check for timeouts
              (when timeouts
                (block timeouts
                  (let ((now (get-internal-real-time)))
                    (dolist (to timeouts)
                      (if (<= (car to) now)
                          (with-channel-restarts ((cdr to))
                            (io-channel-handle (cdr to) :timeout))
                          (return-from timeouts)))))))))))))

;;; IO-CHANNEL-IOPORT methods for support facilities
(defmethod io-channel-ioport (io-loop (channel sb-sys:fd-stream))
  (declare (ignore io-loop))
  (sb-sys:fd-stream-fd channel))

(defmethod io-channel-ioport ((io-loop sbcl-io-loop) (channel xlib:display))
  (io-channel-ioport io-loop (xlib::display-input-stream channel)))
