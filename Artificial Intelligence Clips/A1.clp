;;;======================================================
;;;   A1 Hdun445 Phone Start Up problem
;;;
;;;     Determines the reason and what you should do inorder
;;;	to get your phone to start up
;;;     
;;;	- Some of this code has be based off wine.clp used in tutorial 3 and from slides from tutorial 2...
;;;     To execute, load, reset and run.
;;;======================================================




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sets the template of the phone, everything is set to unknown and will be discovered as questions are asked
;;; This code was re-worked from tutorial 3 wine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate status
	(slot turnOn (default unknown))
	(slot charged (default unknown))
	(slot phoneDropped (default unknown))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slots that would for facts if the phone was not dropped on the ground or water;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(slot rebootWork (default unknown))
	(slot factoryReset (default unknown))
	(slot openPhone	(default unknown))
	(slot competence (default unknown))
	(slot batCondition (default unknown))
	(slot motherboardCondition (default unknown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slots that would for facts if the phone was dropped on the ground;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(slot screenBroken (default unknown))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creates the phone object upon reset;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code re-worked from examples in tutorial 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deffacts Phone-Status "Sets the inital status of the phones potential problems to unknown"
	(status(turnOn unknown)
	(charged unknown)
	(phoneDropped unknown)
	(rebootWork unknown)
	(factoryReset unknown)
	(openPhone unknown)
	(competence unknown)
	(batCondition unknown)
	(motherboardCondition unknown)
	(screenBroken unknown)
	)
)
;;;;================================================================================;;;;;
;;; ALL THE FOLLOWING DEFRULES WERE INFLUENCED BY THE IF-ELSE EXAMPLE IN TUTORIAL 2;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This is the first question to determine if the phone is working or not;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule preliminaryQuestion
	?stat <- (status(turnOn unknown))
	=>
	(printout t "Does the phone turn on, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string yes)0)
		then (printout t "your phone works fine, great!" crlf)
	else
		(if (= (str-compare ?string no)0)
			then (modify ?stat(turnOn no))
			else (modify ?stat(turnOn unknown))
		)
    	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Second question to determine if the phone is charged;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule isCharged
	?statcharged <-(status(turnOn no) (charged unknown))
	=>
	(printout t "is your phone charged, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string no)0)
		then (printout t "Charge your phone")
		
	else	
		(if (=(str-compare ?string yes)0)
		then (modify ?statcharged(charged yes))
		else (assert (turnOn ks))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Third question determines if and where the phone was dropped i.e in water or on the ground because they result in different problems;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule isGotDropped
	?statdropped <-(status(turnOn no) (charged yes) (phoneDropped unknown))
	=>
	(printout t "Did you drop your phone, if you did did it go on the ground or water? (accepted answers no:, ground or water)" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string no)0)
		then (modify ?statdropped(phoneDropped no))
		
	else	
		(if (=(str-compare ?string ground)0)
		then (modify ?statdropped(phoneDropped ground))
		else 
			(if (=(str-compare ?string water)0)
			then (modify ?statdropped(phoneDropped water))
			then (printout t "Put the phone in rice overnight" crlf)
			else(assert (done no))
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; If the phone wasn't dropped the following questions will follow this branch of the descision tree;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Fourth question asks if reboot works;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule re-BootWork
	?statReBoot <-(status(turnOn no) (charged yes)(phoneDropped no) (rebootWork unknown))
	=>
	(printout t "Does a manual re-boot of the phone fix it, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string yes)0)
		then (printout t "Great problem solved!!!" crlf)
		
	else	
		(if (=(str-compare ?string no)0)
		then (modify ?statReBoot(rebootWork no))
		else (assert (done no))
		)
	)
)

(defrule factoryResetWork
	?statReset <-(status(rebootWork no) (factoryReset unknown))
	=>
	(printout t "Does doing a factory reset work, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string yes)0)
		then (printout t "Great problem solved!!" crlf)
		
	else	
		(if (=(str-compare ?string no)0)
		then (modify ?statReset(factoryReset no))
		else (assert (done no))
		)
	)
)

(defrule OpenPhone
	?statOpenUp <-( status(factoryReset no) (openPhone unknown))
	=>
	(printout t "Can you physically open the phone up?, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string yes)0)
		then (modify ?statOpenUp(openPhone yes))
		
	else	
		(if (=(str-compare ?string no)0)
		then (printout t "There is an internal part issue that you cannot solve, take to a repair specialist" crlf)
		else (assert (done no))
		)
	)
)



(defrule Competent
	?statCompetence <-( status(openPhone yes)(competence unknown))
	=>
	(printout t "Are you competent at opening the phone up and checking/tinkering with parts? yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string yes)0)
		then (modify ?statCompetence(competence yes))
		
	else	
		(if (=(str-compare ?string no)0)
		then (printout t "There is an internal part issue that you cannot solve, take to a repair specialist" crlf)
		else (assert (done no))
		)
	)
)



(defrule batteryCondition
	?statBat <-( status(competence yes) (batCondition unknown))
	=>
	(printout t "Is the battery in good condition, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string yes)0)
		then (modify ?statBat(batCondition good))
		
	else	
		(if (=(str-compare ?string no)0)
		then (printout t "Replace battery")
		else (assert (done no))
		)
	)
)


(defrule motherCondition
	?statBat <-( status(batCondition good)(motherboardCondition unknown))
	=>
	(printout t "Is the motherboard in good condition, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string yes)0)
		then (printout t "There is an internal part issue that you cannot solve, take to a repair specialist" crlf)
		
	else	
		(if (=(str-compare ?string no)0)
		then (printout t "Replace motherboard")
		else (assert (done no))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; If the phone was dropped on the ground the following questions will follow this branch of the descision tree;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defrule screenCondition
	?statScreen <-( status(phoneDropped ground)(screenBroken unknown))
	=>
	(printout t "Is the screen intact, yes or no?" crlf)
	(bind ?string (readline))
	(if (= (str-compare ?string no)0)
		then (printout t "Replace the screen" crlf)
		
	else	
		(if (=(str-compare ?string yes)0)
		then (modify ?statScreen(phoneDropped no))
		else (assert (done no))
		)
	)
)