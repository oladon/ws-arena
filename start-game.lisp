;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start-hunchentoot.lisp
;;;;
;;;; Author:  William Bruschi
;;;; Date:    02-14-2009
;;;;
;;;; Starts Hunchentoot and Swank, then listens for a shutdown
;;;; command on the specified port.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'asdf)
(require 'cl-json)
(require 'cl-who)
(require 'hunchentoot)
(require 'hunchensocket)
(require 'sb-bsd-sockets)
(ql:quickload '(engine pf-arena web-arena))

(defpackage :gameserver
(:use :common-lisp :hunchentoot :cl-who))

(in-package :gameserver)

(defparameter *game-port* 8887)
(defparameter *server* ())
(defparameter *shutdown-port* 6440)
(defparameter *swank-loader*
  "~/quicklisp/dists/quicklisp/software/slime-2.13/swank-loader.lisp")
(defparameter *swank-port* 4006)

(defun game-setup ()
;;;; Start the game server
  (setf *server*
    (web-arena:start-server *game-port*))
  (princ "Game server started on port ")
  (princ *game-port*) (terpri)
  
;;; Start swank
  (load *swank-loader*)
  (swank-loader:init)
  (swank:create-server :port *swank-port* :dont-close t)
  (princ "Loaded Swank on port ")
  (princ *swank-port*)(terpri)
  
;;; Wait and listen for shutdown command
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream :protocol :tcp)))
    
    ;; Listen on a local port for a TCP connection
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) *shutdown-port*)
    (sb-bsd-sockets:socket-listen socket 1)
    
    ;; When it comes, close the sockets and continue
    (multiple-value-bind (client-socket addr port)
	(sb-bsd-sockets:socket-accept socket)
      (sb-bsd-sockets:socket-close client-socket)
      (sb-bsd-sockets:socket-close socket)))
  
  ;; Shut down the game
  (princ "Stopping Game...")(terpri)
  (stop *server*)
  
  ;; Shut down Swank and anyone else by terminating all threads
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread)))
  (sleep 1)
  (sb-ext:exit))

(defun make-exec ()
  (sb-ext:save-lisp-and-die "run-arena"
                            :executable t
                            :purify t
;                            :compression 9
                            :toplevel #'game-setup))
