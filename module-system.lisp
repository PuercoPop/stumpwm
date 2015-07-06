(defpackage #:stumpwm/module-system
  (:use #:cl #:asdf)
  (:export #:stumpwm-module
           #:stumpwm-module-p)
  (:documentation "Extends the ASDF model so that StumpWM moduels can register
  as such and be recognized by StumpWM."))

(in-package #:stumpwm/module-system)

(defclass stumpwm-module (system)
  ())

;; XXX: Figure how to make #'asdf/system:coerce-class work lookup the class in
;; the proper module to avoid importing the symbol to asdf.
(import 'stumpwm-module (find-package 'asdf/interface))

(defgeneric stumpwm-module-p (system)
  (:documentation "Is the system an StumpWM module?"))

(defmethod stumpwm-module-p (system)
  nil)

(defmethod stumpwm-module-p ((system stumpwm-module))
  system)
