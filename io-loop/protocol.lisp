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

;; This module implements a generic multiplexing I/O loop for listening
;; to I/O events from multiple sources.

;; The model is as follows:

;; An I/O multiplexer is represented as an object, with which I/O
;; channels can be registered to be monitored for events when the I/O
;; loop runs. An I/O channel is any object for which the generic
;; functions IO-CHANNEL-IOPORT, IO-CHANNEL-EVENTS and
;; IO-CHANNEL-HANDLE are implemented.

;; IO-CHANNEL-IOPORT, given an I/O multiplexer and an I/O channel,
;; should return the underlying system I/O facility that the channel
;; operates on. The actual objects used to represent an I/O facility
;; depends on the Lisp implementation, operating system and the
;; specific I/O loop implementation, but, for example, on Unix
;; implementations they will likely be numeric file descriptors. The
;; I/O loop implementation implements IO-CHANNEL-IOPORT methods for
;; the facilities it understands (such as FD-STREAMs on SBCL), so
;; user-implemented channels should simply call IO-CHANNEL-IOPORT
;; recursively on whatever it operates on.

;; IO-CHANNEL-EVENTS, given an I/O channel, should return a list of
;; the events that the channel is interested in. See the
;; documentation for IO-CHANNEL-EVENTS for further details.

;; The I/O loop guarantees that it will check what events a channel
;; is interested in when it is first registered, and also at any time
;; the channel has been notified of an event. If the channel changes
;; its mind at any other point in time, it should use the
;; IO-LOOP-UPDATE function to notify the I/O loop of such
;; changes. The I/O loop may very well also update spuriously at
;; other times, but such updates are not guaranteed.

;; IO-CHANNEL-HANDLE is called by the I/O loop to notify a channel of
;; an event.

;; An I/O multiplexer is created with a MAKE-INSTANCE call on the class of the
;; desired multiplexer implementation. If the code using the multiplexer has no
;; certain preferences on an implementation (which should be the usual case),
;; the variable *DEFAULT-IO-LOOP* holds the class name that is optimal given
;; the current Lisp implementation and operating system.

;; Given a multiplexer, channels can be registered with it using
;; IO-LOOP-ADD, unregistered with IO-LOOP-REMOVE, and updated with
;; IO-LOOP-UPDATE (as described above). Call IO-LOOP on the
;; multiplexer to actually run it.

(in-package #:stumpwm)

(export '(io-channel-ioport
          io-channel-events
          io-channel-handle
          *default-io-loop*
          *current-io-loop*
          *current-io-channel*
          io-loop
          io-loop-add
          io-loop-remove
          io-loop-update
          callback-channel
          callback-channel-stream
          callback-channel-events))

;;; General interface

(defgeneric io-channel-ioport (io-loop channel)
  (:documentation
   "Returns the I/O facility operated on by CHANNEL, in a
  representation understood by IO-LOOP. CHANNEL may be either an I/O
  channel or an object representing an underlying I/O facility, such
  as a stream object. An I/O loop implementation should implement
  methods for any primitive I/O facilities that it can monitor for
  events, and abstract channels should return whatever
  IO-CHANNEL-IOPORT returns for the primitive facility that it
  operates on.

  An I/O channel may also return NIL to indicate that it is only
  interested in purely virtual events, such as :TIMEOUT or :LOOP."))

;; XXX: Timeout type is broken.
(deftype io-channel-events ()
  '(member :read :write :timeout :loop))

(defgeneric io-channel-events (channel)
  (:documentation
   "Returns a list of events that CHANNEL is interested in. An event
  specification may be a simple symbol, or a list of a symbol and
  additional data for the event. Specific I/O loop implementations may
  implement additional events, but the following event specifications
  should be supported by all I/O loops:

      :READ -- The channel will be notified when its I/O port can be
      read from without blocking.

      :WRITE -- The channel will be notified when its I/O port can
      be written to without blocking.

      (:TIMEOUT TIME-SPEC) -- TIME-SPEC is a point in time in the
      same units as from (GET-INTERNAL-REAL-TIME), at which point
      the channel will be notified. It is permissible for TIME-SPEC
      to be a real number of any representation, but the system does
      not guarantee any particular level of accuracy.

      :LOOP -- The channel will be notifed for each iteration of the
      I/O loop, just before blocking for incoming events. This should
      be considered a hack to be avoided, but may be useful for
      certain libraries (such as XLIB).

  If, at any time, an empty list is returned, the channel is
  unregistered with the I/O loop.

  The I/O loop will check what events a channel is interested in when
  it is first registered with the loop, and whenever the channel has
  been notified of an event. If the channel changes its mind at any
  other point in time, it should use the IO-LOOP-UPDATE function to
  notify the I/O loop of such changes. The I/O loop may also update
  spuriously at any time, but such updates are not guaranteed."))

(defgeneric io-channel-handle (channel event &key &allow-other-keys)
  (:documentation
   "Called by the I/O loop to notify a channel that an event has
  occurred. EVENT is the symbol corresponding to the event
  specification from IO-CHANNEL-EVENTS (that is, :READ, :WRITE,
  :TIMEOUT or :LOOP). A number of keyword arguments with additional
  data specific to a certain event may also be passed, but no such
  arguments are currently defined."))

(defgeneric io-loop-add (io-loop channel)
  (:documentation "Add a channel to the given I/O multiplexer to be monitored."))

(defgeneric io-loop-remove (io-loop channel)
  (:documentation "Unregister a channel from the I/O multiplexer."))

(defgeneric io-loop-update (io-loop channel)
  (:documentation "Make the I/O loop update its knowledge of what
  events CHANNEL is interested in. See the documentation for
  IO-CHANNEL-EVENTS for more information."))

;;; TODO: Rename to start-io-loop
(defgeneric io-loop (io-loop &key &allow-other-keys)
  (:documentation "Run the given I/O multiplexer, watching for events
  on any channels registered with it. IO-LOOP will return when it has
  no channels left registered with it."))

(defvar *default-io-loop*)
(setf (documentation '*default-io-loop* 'variable)
      "The default I/O loop implementation. Should be chosen at start up
      according to the Lisp implementation and operating system.")

(setf *default-io-loop*
      (cond ((member :sbcl *features*) 'sbcl-io-loop)
            (t 'xlib-io-loop)))

(defvar *current-io-loop*)
(setf (documentation '*current-io-loop* 'variable)
      "Bound to the I/O loop currently running, providing an easy way for event
   callbacks to register new channels(?).")

(defvar *current-io-channel*)
(setf (documentation '*current-io-channel* 'variable)
      "Bound to the CHANNEL that is currently being
  processed. Provided primarily for error-handling code(?).")

;; Default methods for the above

(defmethod io-channel-handle (channel event &key &allow-other-keys)
  (declare (ignore channel event)))

;;; Default methods for widely(?) supported objects

(defmethod io-channel-ioport (io-loop (channel synonym-stream))
  (io-channel-ioport io-loop (symbol-value (synonym-stream-symbol channel))))

;;; Callback channel implementation

(defclass callback-channel ()
  ((current :initform nil)
   (stream :initarg :stream
           :reader callback-channel-stream)
   (read-function :initarg :read
                  :initform nil
                  :reader read-function)
   (write-function :initarg :write :initform nil)
   (events :initarg :events :initform :auto :accessor callback-channel-events))
  (:documentation
   "Implements a convenience I/O channel which takes an underlying I/O
   facility and calls the given callback functions when an event
   occurs on the channel. The :STREAM init-argument specifies the I/O
   facility to monitor, :READ specifies a function to be called back
   when a read event occurs, and :WRITE a corresponding function for
   write events. Timeouts are not supported.

   By default, the channel will listen for read events iff a read
   callback function is given and correspondingly for write events,
   but CALLBACK-CHANNEL-EVENTS can be SETF'd to specify events
   explicitly in case certain events are only interesting
   sporadically. To restore default behavior, set it to :AUTO."))

(defmethod io-loop-add :before (info (channel callback-channel))
  (when (slot-value channel 'current)
    (error "Callback channel is already registered with an I/O loop")))

(defmethod io-loop-add :after (info (channel callback-channel))
  (setf (slot-value channel 'current) info))

(defmethod io-loop-remove :after (info (channel callback-channel))
  (setf (slot-value channel 'current) nil))

(defmethod io-channel-ioport (io-loop (channel callback-channel))
  (io-channel-ioport io-loop (slot-value channel 'stream)))

(defmethod io-channel-events ((channel callback-channel))
  (with-slots (events) channel
    (if (eq events :auto)
        (let ((ret ()))
          (when (slot-value channel 'read-function)
            (push :read ret))
          (when (slot-value channel 'write-function)
            (push :write ret))
          ret)
        events)))

(defmethod io-channel-handle ((channel callback-channel) (event (eql :read)) &key)
  (funcall (read-function channel) channel))

(defmethod (setf callback-channel-events) (events channel)
  (setf (slot-value channel 'events) events)
  (with-slots (current) channel
    (when current (io-loop-update current channel))))
