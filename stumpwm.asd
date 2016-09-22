(in-package #:asdf-user)

(defsystem :stumpwm
  :name "StumpWM"
  :author "Shawn Betts <sabetts@vcn.bc.ca>"
  :version "0.9.9"
  :maintainer "David Bjergaard <dbjergaard@gmail.com>"
  :license "GNU General Public License"
  :description "A tiling, keyboard driven window manager" 
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:clx
               #:piping
               (:feature :sbcl #:sb-posix)
               #:split-sequence
               #:verbose)
  :components ((:file "timer")
               (:file "configuration")
               (:file "logging" :depends-on ("configuration"))
               (:file "package" :depends-on ("timer" "logging" "configuration"))
               (:file "primitives")
               (:file "workarounds")
               (:file "wrappers")
               (:file "pathnames")
               (:file "font-rendering")
               (:file "keysyms")
               (:file "keytrans")
               (:file "kmap")
               (:file "input")
               (:file "core")
               (:file "command")
               (:file "menu")
               (:file "screen")
               (:file "head")
               (:file "group")
               (:file "bindings")
               (:file "events")
               (:file "window")
               (:file "floating-group")
               (:file "tile-group")
               (:file "tile-window")
               (:file "window-placement")
               (:file "message-window")
               (:file "selection")
               (:file "module")
               (:module "io-loop"
                :components ((:file "protocol")
                             (:file "sbcl" :if-feature :sbcl)
                             (:file "fallback" :if-feature (:not :sbcl))))
               (:file "stumpwm" :depends-on ("timer"))
               (:file "user")
               (:file "iresize")
               (:file "help")
               (:file "fdump")
               (:file "time")
               (:file "mode-line")
               (:file "color")
               (:file "wse")
               ;; keep this last so it always gets recompiled if
               ;; anything changes
               (:file "version")))
