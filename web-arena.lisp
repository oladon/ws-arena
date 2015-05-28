(defpackage :web-arena
  (:use :common-lisp :hunchensocket :cl-json :engine :pf-arena
	#+sbcl sb-mop #+clisp clos #+clisp mop)
  (:export start-server))
(in-package :web-arena)

(defparameter *user* nil)

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name
	 :initform (error "Name this room!")
	 :reader name))
  (:default-initargs :client-class 'web-player))

;(defclass user (hunchensocket:websocket-client)
;  ((name :initarg :name
;	 :initform nil
;	 :accessor name)))

(engine:defgameobject web-player (pf-arena:pfcc-player hunchensocket:websocket-client)
  ())

(defclass pfcc-game (hunchensocket:websocket-resource) 
  ((name :initarg :name
	 :initform (error "Name this room!")
	 :reader name)
   (monsters :initarg :monsters
	     :accessor monsters))
  (:default-initargs :client-class 'web-player))

(defvar *resources* (list (make-instance 'chat-room :name "/bongo")
			  (make-instance 'chat-room :name "/fury")
			  (make-instance 'pfcc-game :name "/game" 
					            :monsters (sort (pf-arena:init-monsters 
								     (copy-list engine:*monsters*)) 
								    #'< :key #'car))))

(defgeneric event-message (event))

(defmethod event-message (event)
  (format nil "~A" event))

(defmethod event-message ((event engine:miss))
;  (warn (format nil "Attacker is ~A, weapon is ~A" 
;		(engine:attacker event)
;		(engine:weapon event)))
  (format nil "~A misses with ~A." 
	  (engine:name (engine:attacker event))
	  (engine:name (engine:weapon event))))

(defmethod event-message ((event engine:hit))
  (format nil "~A hits with ~A" 
	  (engine:name (engine:attacker event))
	  (engine:name (engine:weapon event))))

(defmethod event-message ((event engine:crit))
  (format nil "~A CRITS with ~A"
	  (engine:name (engine:attacker event))
	  (engine:name (engine:weapon event))))

(defmethod event-message ((event engine:trip-success))
  (format nil "~A trips ~A."
	  (engine:name (engine:attacker event))
	  (engine:name (engine:defender event))))

(defmethod event-message ((event engine:trip-failure))
  (format nil "~A tries to trip ~A, but fails!"
	  (engine:name (engine:attacker event))
	  (engine:name (engine:defender event))))

(defmethod event-message ((event engine:damage-taken))
;  (warn (format nil "Damage is ~A" (engine:damage event)))
  (let* ((source (engine:source event))
	 (creature (engine:creature event))
	 (name (engine:name creature))
	 (damage (engine:damage event))
	 (total-damage (if (listp damage)
			   (apply #'+ (mapcar #'engine:amount damage))
			   (engine:amount damage))))
    (cond ((equal source creature)
	   (format nil "~A bleeds from the strain!" name))
	  ((typep source 'engine:bleed-inflicted)
	   (format nil "~A bleeds for ~d damage!" name total-damage))
	  (t (format nil " for ~d damage!"
		     total-damage)))))

(defmethod event-message ((event engine:save-failed))
  (format nil "~A fails a save against ~A."
	  (engine:name (engine:creature event))
	  (engine:name (engine:source event))))

(defmethod event-message ((event engine:save-succeeded))
  (format nil "~A saves versus ~A!"
	  (engine:name (engine:creature event))
	  (engine:name (engine:source event))))

(defmethod event-message ((event engine:poisoned!))
  (format nil "~A is poisoned!"
	  (engine:name (engine:creature event))))

(defmethod event-message ((event engine:poison-removed))
  (format nil "The poison wears off."))

(defmethod event-message ((event engine:ability-damage-taken))
  (format nil "~A takes ~d ~A damage!"
	  (engine:name (engine:creature event))
	  (engine:amount event)
	  (engine:ability-score event)))

(defmethod event-message ((event engine:bleed-inflicted))
  "")

(defmethod event-message ((event engine:condition-added))
  (let ((name (engine:name (engine:creature event))))
    (case (engine:game-condition event)
      (engine:condition-dead (format nil "~A is dead!" name))
      (engine:condition-disabled (format nil "~A is disabled!" name))
      (engine:condition-unconscious (format nil "~A is knocked out!" name))
      (engine:condition-dying (format nil "~A is dying!" name))
      (engine:condition-staggered (format nil "~A is staggered!" name))
      (engine:condition-prone (format nil "~A falls down!" name))
      (engine:condition-bleed (format nil "~A bleeds from the wound!" name))
      (t (format nil "~A" (engine:game-condition event))))))

(defmethod event-message ((event engine:condition-removed))
  (let ((name (engine:name (engine:creature event))))
    (case (engine:game-condition event)
      (engine:condition-prone (format nil "~A stands up" name)))))

(defmethod event-message ((event engine:action-taken))
  nil)

(defun send-status (user history)
;  (warn (format nil "~A's form name is ~A~%" (engine:name user) (engine:form-name user)))
  (let* ((hash (make-hash-table))
	 (json-hash (cl-json:encode-json-to-string 
		     (progn (setf (gethash 'status hash) `(("current_form" . ,(engine:form-name user))
							   ("current_hp" . ,(engine:current-hit-points user history))
							   ("max_hp" . ,(engine:hit-points user history))
							   ("nonlethal" . ,(engine:total-nonlethal-damage user history))
							   ("ac" . ,(engine:armor-class nil user (car (engine:melee (car engine:*monsters*))) history))))
			    hash))))
;    (warn "Outputting: ~A" json-hash)
    (engine:output json-hash)))

(defun send-stats (user)
  (let* ((hash (make-hash-table))
	 (json-hash (cl-json:encode-json-to-string
		     (progn (setf (gethash 'stats hash) `(("kills" . ,(pf-arena:kills user))
							  ("xp_earned" . ,(pf-arena:xp-earned user))))
			    hash))))
;    (warn "Outputting stats: ~A" json-hash)
    (engine:output json-hash)))

(defmethod event-message ((eventlist list))
;	    (warn (format nil "Oh noes! Event list is ~A!~%" eventlist))
  (when (and eventlist *user*)
    (cond ((equal eventlist (list nil)) nil)
	  ((every #'(lambda (x) (and (not (atom x))
				     (cdr x) 
				     (atom (cdr x)))) eventlist)
	   (let ((init-winner (caar eventlist))
		 (opponent (car (remove *user* (mapcar #'car eventlist)))))
;		     (warn (format nil "Opponent: ~A~%" opponent))
	     (format nil "Combat with ~A the ~A begins. ~A initiative."
		     (engine:name opponent)
		     (string-capitalize (engine:form-name opponent))
		     (if (equal init-winner *user*)
			 "You win"
			 (format nil "~A wins" (engine:name opponent))))))
	  (t (let* ((events-this-turn (reverse (engine:this-turn-events eventlist)))
		    (messagelist (remove nil 
					 (maplist #'(lambda (list) 
						      (let ((event (car list))
							    (rest (cdr list)))
							(cond ((or (typep event 'engine:hit)
								   (typep event 'engine:crit))
							       (let ((damage (find-if #'(lambda (x) (and (typep x 'engine:damage-taken)
													 (equal (engine:source x) event)))
										      rest)))
								 (format nil "~A~A" 
									 (event-message event) 
									 (event-message damage))))
							      ((and (typep event 'engine:damage-taken)
								    (typep (engine:source event) 'engine:hit))
							       nil)
							      (t (event-message event)))))
						  events-this-turn)))
		    (message (format nil "~{~A~^ ~}" messagelist)))
;		       (warn (format nil "~A" messagelist))
;							     (mapcar #'event-message 
;								     (engine:this-turn-events eventlist)))))))
	       (send-status *user* eventlist)
	       (send-stats *user*)
;		       (warn message)
	       (format nil 
		       "~A~A" 
		       message 
		       (let ((lastchar (elt message (1- (length message)))))
			 (if (not (some #'(lambda (x) (equal lastchar x)) '(#\. #\! #\?)))
			     "."
			     ""))))))))

(defmethod engine:output (message)
  (when (and message *user*)
    (hunchensocket:send-text-message *user* (event-message message))))

(defmacro with-user-output (user &body body)
; TODO: currently assumes a single opponent
  `(let* ((*user* ,user))
     ,@body))

(defmethod pf-arena:update-form ((it web-player) newform)
;  (warn "Form updated.")
  (pf-arena:change-form it newform 'web-player)
  (send-status it nil)
  (send-stats it))

(defmethod engine:prompt-input ((user web-player) &key (validation #'identity) prompt resource &allow-other-keys)
  (loop with response
        with valid?
        do (when prompt (engine:output prompt))
           (setf response (hunchensocket:new-read-handle-loop (or resource (car *resources*)) user))
;           (warn "Test: ~A said ~A (~A)" (engine:name user) response (type-of response))
           (setf valid? (funcall validation response))
           (unless valid? 
	     (engine:output "Invalid entry.")
	     (engine:output prompt))
        until valid?
        finally (return response)))

(defmethod engine:select ((it web-player) actions &key display prompt)
  (let* ((hash (make-hash-table))
	 (json-list (cl-json:encode-json-to-string 
		     (progn (setf (gethash 'choices hash) (mapcar #'(lambda (x) 
								      (cond (display (funcall display x))
									    (t (engine:pretty-print x))))
								  actions))
			    hash))))
    (if (and (= (length actions) 1) 
	     (subtypep (class-of (car actions)) (find-class 'engine:actor)))
	(car actions)
	(progn (engine:output json-list)
	       (let ((choice (engine:prompt-input it 
						  :validation #'(lambda (x) (let ((new-x (parse-integer x)))
									      (when new-x
										(< -1 new-x (length actions)))))
						  :resource (car *resources*)
						  :prompt prompt)))
;		 (warn (format nil "Returning ~A...~%" (nth (parse-integer choice) actions)))
		 (nth (parse-integer choice) actions))))))

;pretty-print is from the textengine project
(defmethod engine:pretty-print ((it string))
  (format nil "~A" it))

(defmethod engine:pretty-print ((it symbol))
  (format nil "~A" (string-capitalize it)))

(defun find-room (request)
  (find (hunchentoot:script-name request) *resources*
	:test #'string= :key #'name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

(defun setup-web-game (user)
  (when (or (not (engine:name user))
	    (string= "" (engine:name user)))
    (let ((username (engine:prompt-input user 
					 :validation #'pf-arena:valid-name 
					 :resource (car *resources*) 
					 :prompt "{\"textprompt\": \"Enter your name: \"}")))
      (setf (engine:name user) username)))
;      (engine:output (format nil "Set your name to: ~A" username))))
;  (when (not (pf-arena:difficulty user))
    (let ((choice (engine:select user (mapcar #'car pf-arena:*difficultymap*) :prompt "Select a difficulty.")))
      (setf (pf-arena:difficulty user) choice))
;      (warn "Test: ~A" (pf-arena:difficulty user))
;      (engine:output (format nil "You chose ~A." (engine:pretty-print choice)))))
    ; TODO: get rid of this on-event once event parsing is done
;  (engine:on-event engine:hp-changed (when (equal (engine:creature engine:event)
;						  user)
;				       (send-status user engine:history)))
  (setf pf-arena:*pfccmonsters* 
	(sort (pf-arena:init-monsters 
	       (copy-list engine:*monsters*)) #'< :key #'car))
  (pf-arena:update-form user (pf-arena:pick-starting-form pf-arena:*pfccmonsters*))
  (setf (pf-arena:kills user) nil)
  (setf (pf-arena:xp-earned user) 0))

(defun run-game (user)
  (setup-web-game user)
  (pf-arena:play user))

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (with-user-output user
    (engine:output "Initializing, please wait...")
    (loop do (run-game user) ; TODO: Needs an unwind-protect with a save-game
	 (broadcast room 
		    "{\"news\":\"~A has died after reaching the level of ~A.\"}" 
		    (engine:name user) 
		    (engine:form-name user)))))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (when (engine:name user) 
    (broadcast room "~A has left ~A" (engine:name user) (name room))))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
;  (when (engine:name user)
;      (broadcast room "~A said: ~A" (engine:name user) message))
  message)

(defun start-server (&optional port)
  (let ((server (make-instance 'hunchensocket:websocket-acceptor 
			       :port (or port 8887) 
			       :read-timeout 60 
			       :write-timeout 60)))
    (hunchentoot:start server)
    server))
