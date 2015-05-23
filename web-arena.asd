;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(ql:update-dist "quicklisp")
(ql:quickload '(cl-json engine hunchensocket pf-arena))

(defpackage #:web-arena-asd
  (:use :cl :asdf))

(in-package :web-arena-asd)

(defsystem web-arena
  :name "web-arena"
  :version "0.0.1"
  :maintainer "Oladon"
  :author "Oladon"
  :description "Web interface for the PF Arena game."
  :long-description "An interface for the text-based game where you fight your way up the food chain."
  :components ((:file "web-arena")))
