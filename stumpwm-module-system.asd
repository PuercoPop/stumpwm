(defpackage #:stumwpm-module-system-system
  (:use #:cl #:asdf))
(in-package #:stumwpm-module-system-system)

(defsystem :stumpwm-module-system
  :name "StumpWM Module System"
  :author "Javier Olaechea <pirata@gmail.com"
  :version "0.9.9"
  :maintainer "David Bjergaard <dbjergaard@gmail.com>"
  :license "GNU General Public License"
  :serial t
  :depends-on (#:asdf)
  :components ((:file "module-system")))
