;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(ql:update-dist "quicklisp")
(ql:quickload '(cl-json engine hunchensocket pf-arena))

(defpackage #:ws-arena-asd
  (:use :cl :asdf))

(in-package :ws-arena-asd)

(defsystem ws-arena
  :name "ws-arena"
  :version "0.0.1"
  :maintainer "Oladon"
  :author "Oladon"
  :description "WebSocket server for the PF Arena game."
  :long-description "A WebSocket interface for the text-based game where you fight your way up the food chain."
  :components ((:file "ws-arena")))
