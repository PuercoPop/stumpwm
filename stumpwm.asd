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
               (:feature :sbcl #:sb-posix)
               #:split-sequence)
  :components ((:file "package")
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
               (:file "stumpwm")
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
