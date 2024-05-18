; !! TO-DO !!  CHANGE THE PATH TO THE FOLDER:
(defparameter path "/Users/kiliankramer/Desktop/final")

#|
(asdf:load-system "shop3")
(in-package :SHOP-USER)
(load "/Users/kiliankramer/Desktop/final/domain.lisp")
|#

;---------------- Functions ----------------
(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun get_by_key (key list)
  (loop for x in list do
    (if (eq key (car x))
	(return-from get_by_key (nth 1 x)))))


(defun delete_old_blockations (time l)
   (let (new)
     (dolist (x l)
       (when (or (and (= (nth 0 x) -1) (= (nth 1 x) 0)) (< time (+ (nth 0 x) (nth 1 x))))
         (setq new (append new (list x)))))
     (sort new #'< :key #'car)))

(Defun get_ore (p l)
   (let ((b (car l)))
     (if (<= (nth 1 b) p) 
         (if (> (length (cdr l)) 0)
             (append (list b) (list (cdr l)))
             (list b))
         (if (> (length (cdr l)) 0)
             (let* ((new_block_value (- (nth 1 b) p))
                    (new_block (append (list (nth 0 b)) (list new_block_value)))
                    (new_l (append (list new_block) (cdr l)))
                    (new_payload (list (car b) p)))
               (append (list new_payload) (list new_l)))
             (let* ((new_block_value (- (nth 1 b) p))
                    (new_block (append (list (nth 0 b)) (list new_block_value)))
                    (new_payload (list (car b) p)))
               (append (list new_payload) (list (list new_block))))))))

 (defun get_start_time (s d l)
   (dolist (x l)
     (let ((e (+ s d))
           (start (car x))
           (end (+ (car x) (nth 1 x))))
       (if (> end s)
           (if (and (= start s) (= end e))
               (setq s (+ (car x) (car (cdr x))))
               (if (and (and (> end s) (<= end e)) (and (>= start s) (< start e)))
                   (setq s (+ (car x) (car (cdr x))))
                   (if (and (<= start s) (<= e end))
                       (setq s (+ (car x) (car (cdr x))))
                       (if (and (> end s) (<= end e))
                           (setq s (+ (car x) (car (cdr x))))
                           (if (and (>= start s) (< start e))
                               (setq s (+ (car x) (car (cdr x))))
                               (return-from get_start_time s))))))))) s)

(defun get_ore_types (l)
  (setq new (apply #'append l))
  (setq new (remove-duplicates a :test #'equal))
  (return-from get_ore_types  new))

(defun update_assigned_tasks (assigned-tasks list-index truck truck-job-list is-truck-job-list-empty)
  (if (= is-truck-job-list-empty 0) (return-from  update_assigned_tasks (remove-nth-element list-index assigned-tasks))
                                    (and (setf (nth list-index assigned-tasks) (append (list truck) (list (cdr truck-job-list))))
                                         (return-from  update_assigned_tasks assigned-tasks)))
)

;---------------- FUNCTIONS END ---------------- 

(Defdomain Mine (
;---------------- OPERATORS ----------------	
	(:op (!drive-to ?truck ?old-loc ?new-loc ?start ?duration)
	 :delete ((truck ?class ?truck ?start ?old-loc ?old-tank ?ore-type ?payload) (distance ?old-distance))
	 :add ((truck ?class ?truck ?end ?new-loc ?new-tank ?ore-type ?payload) (distance ?new-distance))
	 :precond (and (truck ?class ?truck ?start ?old-loc ?old-tank ?ore-type ?payload)
	 	   (truck-class ?class ?_max-payload  ?_tank-size ?_refill-time ?_consumption-idle ?consumption-unloaded ?consumption-loaded ?_unachievable-goals)
	 	   (assign ?end (eval (+ ?start ?duration)))
		   (assign ?consumption (if (eq '?payload '0) (* (/ ?duration 60) ?consumption-unloaded)
		   	     	 	     	 	      (* (/ ?duration 60) ?consumption-loaded)))
		   (assign ?new-tank (- ?old-tank ?consumption))
		   (distance ?old-distance)
		   (routes ?routes)
		   (assign ?distance (car (get_by_key '?new-loc (get_by_key '?old-loc '?routes))))
		   (assign ?new-distance (+ ?old-distance ?distance)))
	 :cost 0
	)
	
	(:op (!queueing ?truck ?loc ?start ?duration)
	 :delete ((truck ?class ?truck ?start ?loc ?old-tank ?rock-type ?payload) (queue ?old-queue))
	 :add ((truck ?class ?truck ?end ?loc ?new-tank ?rock-type ?payload) (queue ?new-queue))
	 :precond (and (truck ?class ?truck ?start ?loc ?old-tank ?rock-type ?payload)
	 	   (truck-class ?class ?_max-payload  ?_tank-size ?_refill-time ?consumption-idle ?_consumption-unloaded ?_consumption-loaded ?_unachievable-goals)
		   (assign ?new-tank (- ?old-tank ?consumption-idle))
	 	   (assign ?end (eval (+ ?start ?duration)))
		   (queue ?old-queue)
		   (assign ?new-queue (+ ?old-queue ?duration)))
	 :cost 0
	)
	
	(:op (!load ?truck ?source ?start ?duration)
	 :delete ((truck ?class ?truck ?start ?source ?old-tank ?old-ore-type ?old-payload)
	 	  (source ?source ?old-ore-stack ?times-list ?old-idle-time-stemp ?old-total-utilization ?old-total-utilization-time ?old-blocked-list))
	 :add ((truck ?class ?truck ?end ?source ?new-tank ?new-ore-type ?new-payload)
	       (source ?source ?new-ore-stack ?times-list ?end ?new-total-utilization ?new-total-utilization-time ?new-blocked-list))
	 :precond (and (truck ?class ?truck ?start ?source ?old-tank ?old-ore-type ?old-payload)
	 	   (source ?source ?old-ore-stack ?times-list ?old-idle-time-stemp ?old-total-utilization ?old-total-utilization-time ?old-blocked-list)
		   (truck-class ?class ?max-payload  ?_tank-size ?_refill-time ?consumption-idle ?_consumption-unloaded ?_consumption-loaded ?_unachievable-goals)
		   (assign ?new-tank (- ?old-tank ?consumption-idle))
		   (assign ?end (+ ?start ?duration))
		   (assign ?ore-transfer-data (get_ore ?max-payload '?old-ore-stack))
		   (assign ?new-ore-stack (nth 1 '?ore-transfer-data))
		   (assign ?new-ore-type (nth 0 (car '?ore-transfer-data)))
		   (assign ?new-payload (nth 1 (car '?ore-transfer-data)))
		   (assign ?new-total-utilization (+ ?old-total-utilization 1))
		   (assign ?new-total-utilization-time (+ ?old-total-utilization-time ?duration))
		   (assign ?new-blocking (append (list '?start) (list '?duration)))
                   (assign ?new-blocked-list (append '?old-blocked-list (list '?new-blocking))))
	 :cost 0
	)
	
	(:op (!unload ?truck ?goal ?start ?duration)
	 :delete ((truck ?class ?truck ?start ?goal ?old-tank ?old-ore-type ?old-payload)
	 	  (goal ?goal ?old-ore-value ?times-list ?old-blending-order ?old-total-utilization ?old-total-utilization-time ?old-blocked-list))
	 :add ((truck ?class ?truck ?end ?goal ?new-tank 0 0)
	       (goal ?goal ?new-ore-value ?times-list ?new-blending-order ?new-total-utilization ?new-total-utilization-time ?new-blocked-list))
	 :precond (and (truck ?class ?truck ?start ?goal ?old-tank ?old-ore-type ?old-payload)
	 	   (goal ?goal ?old-ore-value ?times-list ?old-blending-order ?old-total-utilization ?old-total-utilization-time ?old-blocked-list)
		   (truck-class ?class ?_max-payload  ?_tank-size ?_refill-time ?consumption-idle ?_consumption-unloaded ?_consumption-loaded ?_unachievable-goals)
		   (assign ?new-tank (- ?old-tank ?consumption-idle))
		   (assign ?end (+ ?start ?duration))
		   (assign ?new-ore-value (+ ?old-ore-value ?old-payload))
		   (assign ?new-blending-order (append (cdr '?old-blending-order) (list (car '?old-blending-order))))
		   (assign ?new-total-utilization (+ ?old-total-utilization 1))
		   (assign ?new-total-utilization-time (+ ?old-total-utilization-time ?duration))
		   (assign ?new-blocking (append (list '?start) (list '?duration)))
                   (assign ?new-blocked-list (append '?old-blocked-list (list '?new-blocking))))
	 :cost 0
	)

	(:op (!refuel ?truck ?station ?start ?duration)
	 :delete ((truck ?class ?truck ?start ?station ?old-tank ?ore-type ?payload)
	          (consumption ?n ?old-consumption)
	 	  (filling ?station ?times-list ?old-blocked-list))
	 :add ((truck ?class ?truck ?end ?station ?new-tank ?ore-type ?payload) (consumption ?new-n ?new-consumption)
	       (filling ?station ?times-list ?new-blocked-list))
	 :precond (and (filling ?station ?times-list ?old-blocked-list)
	 	   (truck ?class ?truck ?start ?station ?old-tank ?ore-type ?payload)
	 	   (truck-class ?class ?_max-payload ?new-tank ?_refill-time ?_consumption-idle ?_consumption-unloaded ?_consumption-loaded ?_unachievable-goals)
	 	   (assign ?end (+ ?start ?duration))
		   (consumption ?n ?old-consumption)
		   (assign ?new-n (+ ?n 1))
		   (assign ?fill-amount (- ?new-tank ?old-tank))
		   (assign ?new-consumption (+ ?old-consumption ?fill-amount))
		   (assign ?new-blocking (append (list '?start) (list '?duration)))
                   (assign ?new-blocked-list (append '?old-blocked-list (list '?new-blocking))))
	 :cost 0
	)

	(:op (!!clear-blocking ?time)
	 :delete ((forall (?source)
			  ((source ?source ?ore-stack ?times-list ?idle ?total-utilization ?total-utilization-time ?blocked-list))
			  ((source ?source ?ore-stack ?times-list ?idle ?total-utilization ?total-utilization-time ?blocked-list)))
	          (forall (?goal)
			  ((goal ?goal ?ore-value ?times-list ?blending-order ?total-utilization ?total-utilization-time ?blocked-list))
			  ((goal ?goal ?ore-value ?times-list ?blending-order ?total-utilization ?total-utilization-time ?blocked-list))))
	 :add ((forall (?source)
		       ((source ?source ?ore-stack ?times-list ?idle ?total-utilization ?total-utilization-time ?blocked-list)
		        (assign ?new-blocked-list (delete_old_blockations '?time '?blocked-list)))
		       ((source ?source ?ore-stack ?times-list ?idle ?total-utilization ?total-utilization-time ?new-blocked-list)))
	       (forall (?goal)
		       ((goal ?goal ?ore-value ?times-list ?blending-order ?total-utilization ?total-utilization-time ?blocked-list)
		        (assign ?new-blocked-list (delete_old_blockations '?time '?blocked-list)))
		       ((goal ?goal ?ore-value ?times-list ?blending-order ?total-utilization ?total-utilization-time ?new-blocked-list))))
	 :cost 0
	)

	(:op (!!update-assigned-tasks ?truck ?truck-job-list)
	 :delete ((assigned-tasks ?assigned-tasks))
	 :add ((assigned-tasks ?new-assigned-tasks))
	 :precond ((assigned-tasks ?assigned-tasks)
		   (assign ?is-truck-job-list-empty (if (cdr '?truck-job-list) 1 0))
		   (assign ?list-index (position '(?truck ?truck-job-list) '?assigned-tasks :test 'equal))
		   (assign ?new-assigned-tasks (update_assigned_tasks '?assigned-tasks '?list-index '?truck '?truck-job-list '?is-truck-job-list-empty)))
	 :cost 0
	) 

;---------------- OPERATORS END ----------------
	
;------------- METHODS -------------

;--------------- SOME DEBUGGING HELPER METHODS ---------------
(:method (print-current-state) ((eval (print-current-state))) ())
(:method (print-current-tasks) ((eval (print-current-tasks))) ())
(:method (print-current-plan) ((eval (print-current-plan))) ())
;--------------- SOME DEBUGGING HELPER METHODS END ---------------

;--------------- LOOP METHOD ---------------
	(:method (haulage-loop)

	goal-not-reached-and-pick-next-truck
	((bagof ?time
	 	(truck ?class ?truck ?time ?loc ?tank ?ore-type ?payload)
	 	?list)
	 (assign ?time (car (sort '?list #'<)))
	 (truck ?class ?truck ?time ?loc ?tank ?ore-type ?payload)
	 (search-configuration ?time-limit ?heuristic)
	 (eval (<= ?time ?time-limit)))
	(:ordered (!!clear-blocking ?time)
	          ;(print-current-state)
		  (dispatch ?truck ?heuristic)
	 	  (haulage-loop))
	
	goal-reached-end-loop
	()
	()

	)
;---------------  LOOP METHOD END ---------------

;--------------- DISPATCH METHOD ---------------
	(:method (dispatch ?truck ?heuristic)

	haulage-at
	; add already existing tasks to plan
	((truck ?class ?truck ?_time ?_loc ?_tank ?_ore-type ?_payload)
	 (truck-class ?class ?_max-payload ?_tank-size ?_refill-time ?_consumption-idle ?_consumption-unloaded ?_consumption-loaded ?_unachievable-goals)
	 (assigned-tasks ?assigned-tasks)
	 (assign ?truck-job-list (get_by_key '?truck '?assigned-tasks))
	 (eval (if '?truck-job-list 1))
	 (assign ?next-assigned-job (car '?truck-job-list)))
	(:ordered (!!update-assigned-tasks ?truck ?truck-job-list)
	          (haulage-assigned-task ?truck ?next-assigned-job))

	refuel
	; truck tank is empty - go refueling
	(:sort-by ?waiting-time ((truck ?class ?truck ?time ?loc ?tank ?_ore-type ?_payload)
	 	  		 (truck-class ?class ?_max-payload ?tank-size ?refill-time ?_consumption-idle ?_consumption-unloaded ?_consumption-loaded ?unachievable-goals)
	 			 (eval (< ?tank (* ?refill-time ?tank-size))) 
				 (filling ?station ?times-list ?filling-blocked-list)
				 (eval (not (member '?station '?unachievable-goals)))
				 (assign ?refill-duration (get_by_key '?class '?times-list))
				 (routes ?routes)
		   		 (assign ?drive-time (get_by_key '?class (nth 1 (get_by_key '?station (get_by_key '?loc '?routes)))))
				 (assign ?arrival-time (+ ?time ?drive-time))
				 (assign ?start-time (get_start_time '?arrival-time '?refill-duration '?filling-blocked-list))
				 (assign ?waiting-time (- ?start-time ?arrival-time))))
	(:ordered (!drive-to ?truck ?loc ?station ?time ?drive-time)
		  (queueing ?truck ?station ?arrival-time ?waiting-time)
		  (!refuel ?truck ?station ?start-time ?refill-duration))
	  
        haulage-w
	; plan next tour - heuristic: wait
	((eval (eq '?heuristic 'wait)))
	(:ordered (haulage-waiting ?truck source)
		  (haulage-waiting ?truck goal))

	;haulage-i
	; plan next tour - heuristic: idle
	((eval (eq '?heuristic 'idle)))
	(:ordered (haulage-idle ?truck)
		  (haulage-waiting ?truck goal))

	; haulage-c
	; plan next tour - heuristic: cycle
	((eval (eq '?heuristic 'cycle)))
	(:ordered (haulage-cycle ?truck))

	;haulage-r
	; plan next tour - heuristic: random un/loader selection
	((eval (eq '?heuristic 'random)))
	(:ordered (haulage-random ?truck))
		  
	)
;--------------- DISPATCH METHOD END ---------------

;--------------- ALREADY ASSIGNED-TASKS ---------------
	(:method (haulage-assigned-task ?truck ?job)

	assigned-task-drive
	((assign ?job-type (nth 0 '?job))
	 (eval (eq '?job-type '!DRIVE-TO))
	 (assign ?old-loc (nth 2 '?job))
	 (assign ?loc (nth 3 '?job))
	 (assign ?time (nth 4 '?job))
	 (assign ?drive-time (nth 5 '?job)))
	((!drive-to ?truck ?old-loc ?loc ?time ?drive-time))

	assigned-task-un/load-wait-refuel
	((assign ?job-type (nth 0 '?job))
	 (assign ?loc (nth 2 '?job))
	 (assign ?start (nth 3 '?job))
	 (assign ?duration (nth 4 '?job)))
	((?job-type ?truck ?loc ?start ?duration))

	)
;--------------- ALREADY ASSIGNED-TASKS END ---------------

;--------------- HEURISTIC WAITING ---------------
	(:method (haulage-waiting ?truck ?destination)

	haulage
	(:sort-by ?waiting-time  ((truck ?class ?truck ?time ?old-loc ?_tank ?ore-type ?_payload)
	 	  		  (truck-class ?class ?_max-payload ?_tank-size ?_refill-time ?_consumption-idle ?_consumption-unloaded ?_consumption-loaded ?unachievable-goals)
		  		  (?destination ?loc ?_ore-stack ?times-list ?data ?_total-utilization ?_total-utilization-time ?blocked-list)
				  (eval (not (member '?loc '?unachievable-goals)))
				  (eval (if (eq '?destination 'goal) (if (member '?ore-type (car '?data)) 1) 1))
				  (routes ?routes)
				  (assign ?drive-time (get_by_key '?class (nth 1 (get_by_key '?loc (get_by_key '?old-loc '?routes)))))
				  (assign ?arrival-time (+ ?time ?drive-time))
				  (assign ?un-loading-time (get_by_key '?class '?times-list))
				  (assign ?start-time (get_start_time '?arrival-time '?un-loading-time '?blocked-list))
				  (assign ?waiting-time (- ?start-time ?arrival-time))
				  (assign ?task (if (eq '?destination 'source) '!load '!unload))))
	(:ordered (!drive-to ?truck ?old-loc ?loc ?time ?drive-time)
		  (queueing ?truck ?loc ?arrival-time ?waiting-time)
		  (?task ?truck ?loc ?start-time ?un-loading-time))

	)
;--------------- HEURISTIC WAITING END ---------------

;--------------- HEURISTIC IDLE ---------------
	(:method (haulage-idle ?truck)

	haulage
	(:sort-by ?idle ((truck ?class ?truck ?time ?old-loc ?_tank ?_ore-type ?_payload)
	 	  	 (truck-class ?class ?_max-payload ?_tank-size ?_refill-time ?_consumption-idle ?_consumption-unloaded ?_consumption-loaded ?unachievable-goals)
		  	 (source ?loc ?_ore-stack ?times-list ?idle ?_total-utilization ?_total-utilization-time ?blocked-list)
			 (eval (not (member '?loc '?unachievable-goals))) 
			 (routes ?routes)
			 (assign ?drive-time (get_by_key '?class (nth 1 (get_by_key '?loc (get_by_key '?old-loc '?routes)))))
			 (assign ?arrival-time (+ ?time ?drive-time))
			 (assign ?loading-time (get_by_key '?class '?times-list))
			 (assign ?start-time (get_start_time '?arrival-time '?loading-time '?blocked-list))
			 (assign ?waiting-time (- ?start-time ?arrival-time))))
	(:ordered (!drive-to ?truck ?old-loc ?loc ?time ?drive-time)
		  (queueing ?truck ?loc ?arrival-time ?waiting-time)
		  (!load ?truck ?loc ?start-time ?loading-time))

	)
;--------------- HEURISTIC IDLE END ---------------

;--------------- HEURISTIC CYCLE ---------------
	(:method (haulage-cycle ?truck)

	haulage
	(:sort-by ?cycle-time  ((truck ?class ?truck ?time ?old-loc ?_tank ?_ore-type ?_payload)
	 	  		(truck-class ?class ?_max-payload ?_tank-size ?_refill-time ?_consumption-idle ?_consumption-unloaded ?_consumption-loaded ?unachievable-goals)
		  		(source ?source ?ore-stack ?times-list ?_data ?_total-utilization ?_total-utilization-time ?blocked-list)
				(eval (not (member '?source '?unachievable-goals)))
				(routes ?routes)
				(assign ?drive-time (get_by_key '?class (nth 1 (get_by_key '?source (get_by_key '?old-loc '?routes)))))
				(assign ?arrival-time (+ ?time ?drive-time))
				(assign ?loading-time (get_by_key '?class '?times-list))
				(assign ?start-time (get_start_time '?arrival-time '?loading-time '?blocked-list))
				(assign ?waiting-time (- ?start-time ?arrival-time))
				(assign ?leaving-time (+ ?start-time ?loading-time))
		  		(goal ?goal ?_ore-value ?times-list2 ?current-ore-types ?_total-utilization2 ?_total-utilization-time2 ?blocked-list2)
				(eval (not (member '?goal '?unachievable-goals)))
				(assign ?new-ore-type (car (car '?ore-stack))) 
				(eval (member '?new-ore-type (car '?current-ore-types)))
				(assign ?drive-time2 (get_by_key '?class (nth 1 (get_by_key '?goal (get_by_key '?source '?routes)))))
				(assign ?arrival-time2 (+ ?leaving-time ?drive-time2))
				(assign ?unloading-time (get_by_key '?class '?times-list2))
				(assign ?start-time2 (get_start_time '?arrival-time2 '?unloading-time '?blocked-list2))
				(assign ?waiting-time2 (- ?start-time2 ?arrival-time2))
				(assign ?cycle-time (+ ?start-time2 ?unloading-time))))
	(:ordered (!drive-to ?truck ?old-loc ?source ?time ?drive-time)
		  (queueing ?truck ?source ?arrival-time ?waiting-time)
		  (!load ?truck ?source ?start-time ?loading-time)
		  (!drive-to ?truck ?source ?goal ?leaving-time ?drive-time2)
		  (queueing ?truck ?goal ?arrival-time2 ?waiting-time2)
		  (!unload ?truck ?goal ?start-time2 ?unloading-time))

	)
;--------------- HEURISTIC CYCLE END ---------------

;--------------- RANDOM ---------------
	(:method (haulage-random ?truck)

	haulage
	(:first ((truck ?class ?truck ?time ?old-loc ?_tank ?_ore-type ?_payload)
	 (truck-class ?class ?_max-payload ?_tank-size ?_refill-time ?_consumption-idle ?_consumption-unloaded ?_consumption-loaded ?_unachievable-goals)
	 (routes ?routes)
	 (bagof ?current-ore-types
	 	((goal ?goal ?ore-value ?times-list ?ore-types ?total-utilization ?total-utilization-time ?blocked-list)
		 (eval (not (member '?goal '?unachievable-goals)))
		 (assign ?current-ore-types (car '?ore-types)))
	 	?current-ore-types-stack-list)
	 (assign ?current-ore-types-list (get_ore_types '?current-ore-types-stack-list))
	 (bagof ?source
	 	((source ?source ?ore-stack ?times-list ?idle ?total-utilization ?total-utilization-time ?blocked-list)
		 (eval (not (member '?source '?unachievable-goals)))
		 (eval (member (car (car '?ore-stack)) '?current-ore-types-list)))
	 	?sources-list)
	 (assign ?source (nth (random (length '?sources-list)) '?sources-list))
	 (source ?source ?ore-stack ?times-list ?idle ?total-utilization ?total-utilization-time ?blocked-list)
	 (assign ?drive-time (get_by_key '?class (nth 1 (get_by_key '?source (get_by_key '?old-loc '?routes)))))
	 (assign ?arrival-time (+ ?time ?drive-time))
	 (assign ?loading-time (get_by_key '?class '?times-list))
	 (assign ?start-time (get_start_time '?arrival-time '?loading-time '?blocked-list))
	 (assign ?waiting-time (- ?start-time ?arrival-time))
	 (assign ?leaving-time (+ ?start-time ?loading-time))
	 (bagof ?goal
	 	((goal ?goal ?ore-value ?times-list2 ?current-ore-types ?total-utilization2 ?total-utilization-time2 ?blocked-list2)
		 (eval (not (member '?goal '?unachievable-goals)))
		 (eval (member (car (car '?ore-stack)) (car '?current-ore-types))))
	 	?goals-list)
	 (assign ?goal (nth (random (length '?goals-list)) '?goals-list))
	 (goal ?goal ?ore-value ?times-list2 ?current-ore-types ?total-utilization2 ?total-utilization-time2 ?blocked-list2)
	 (assign ?drive-time2 (get_by_key '?class (nth 1 (get_by_key '?goal (get_by_key '?source '?routes)))))
	 (assign ?arrival-time2 (+ ?leaving-time ?drive-time2))
	 (assign ?unloading-time (get_by_key '?class '?times-list2))
	 (assign ?start-time2 (get_start_time '?arrival-time2 '?unloading-time '?blocked-list2))
	 (assign ?waiting-time2 (- ?start-time2 ?arrival-time2))
	 (assign ?_cycle-time (+ ?start-time2 ?unloading-time))))
	(:ordered (!drive-to ?truck ?old-loc ?source ?time ?drive-time)
		  (queueing ?truck ?source ?arrival-time ?waiting-time)
		  (!load ?truck ?source ?start-time ?loading-time)
		  (!drive-to ?truck ?source ?goal ?leaving-time ?drive-time2)
		  (queueing ?truck ?goal ?arrival-time2 ?waiting-time2)
		  (!unload ?truck ?goal ?start-time2 ?unloading-time))

	)
;--------------- RANDOM END ---------------

;--------------- QUEUEING METHOD ---------------
	(:method (queueing ?truck ?loc ?arrival ?wait)

	queue
	((eval (> ?wait 0)))
	((!queueing ?truck ?loc ?arrival ?wait))

	not-queue
	((eval (<= ?wait 0)))
	()
	
	)
;--------------- QUEUEING METHOD END ---------------


))

;--------------- DEFINE PROBLEM ---------------
(defparameter *task-list* '(:ordered (print-current-state) (dispatch truck1 wait) (print-current-state) (dispatch truck1 wait)))
;  (!drive-to truck1 4367 4365 0.0 1.0)
(defparameter *problem-definition-vehicles*
    (with-open-file (str (concatenate 'string path "/vehicles10.lisp"))			
        (loop :for x = (read str nil nil) :while x :collect x)))
(defparameter *problem-definition-resources*
    (with-open-file (str (concatenate 'string path "/resources.lisp"))			
        (loop :for x = (read str nil nil) :while x :collect x)))
(make-problem 'problem (append *problem-definition-vehicles*  *problem-definition-resources*) *task-list*)
;--------------- DEFINE PROBLEM END  ---------------

;--------------- FIND PLAN ---------------
(find-plans 'problem :which :first :verbose :plans)
;(find-plans 'problem :which :all :verbose :plans :time-limit 60  :optimize-cost 100)
;(defparameter x (nth 0 (find-plans 'problem :which :first :verbose :plans)))
;--------------- FIND PLAN END ---------------

#|
(asdf:load-system "shop3")
(in-package :SHOP-USER)
(load "/Users/kiliankramer/Desktop/final/domain.lisp")
|#
