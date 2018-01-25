;;;; Emile Kaba & Emery Voirin




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          CREATION DE LA VM                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make_vm ( nom tailleMemoire )
    (progn
       ; mémoire -> tableau
      (setf (get nom 'memoire) (make-array tailleMemoire :initial-element ()))
      ; Registres
      (setf (get nom 'R0) 0)
      (setf (get nom 'R1) 0)
      (setf (get nom 'R2) 0)
      ; 
      (setf (get nom 'FP) 0)
      ; Stack pointer
      (setf (get nom 'SP) 0)
      ; Compteur ordinal 
      (setf (get nom 'CO) (- tailleMemoire 1))
      (setf (get nom 'place_libre_suiv) (- (taille_memoire nom) 1))
      ; Drapeaux pour la comparaison comparaison
      (setf (get nom 'FLT) 0)
      (setf (get nom 'FEQ) 0)
      (setf (get nom 'FGT) 0)
      ; etiquettes(LABELS) pour les sauts (JUMP)
      (setf (get nom 'ETIQUETTE) (make-hash-table :size 0))
      (setf (get nom 'REF_AVANT) (make-hash-table :size 0))
      ; gère l'arrêt de la machine
      (setf (get nom 'EXEC) nil)
      ;  Debugage quand la machine est en EXEC
      (setf (get nom 'DEBUG) nil)
      nom))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          Remise à 0 de la VM                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remise_zero (nom)
  (prog1
    (setf (get nom 'R0) 0)
    (setf (get nom 'R1) 0)
    (setf (get nom 'R2) 0)
    (setf (get nom 'FP) 0)
    (setf (get nom 'SP) 0)
    (setf (get nom 'FLT) 0)
    (setf (get nom 'FEQ) 0)
    (setf (get nom 'FGT) 0)
    (setf (get nom 'CO) (- (taille_memoire nom) 1))
    (setf (get nom 'place_libre_suiv) (- (taille_memoire nom) 1))
    (setf (get nom 'ETIQUETTE) (make-hash-table :size 0))
    (setf (get nom 'REF_AVANT) (make-hash-table :size 0))
    (setf (get nom 'ETAT) 'STOP )
    (setf (get nom 'EXEC) nil)
    (setf (get nom 'DEBUG) nil)
    ; empty memoire
    (loop for place from 0 to (- (taille_memoire nom) 1)
      do
      (set_memoire nom place () ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Réinitialisation de la VM                  :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reinitialisation (nom)
  (prog1
    (setf (get nom 'R0) 0)
    (setf (get nom 'R1) 0)
    (setf (get nom 'R2) 0)
    (setf (get nom 'FP) 0)
    (setf (get nom 'SP) 0)
    (setf (get nom 'FLT) 0)
    (setf (get nom 'FEQ) 0)
    (setf (get nom 'FGT) 0)
    (setf (get nom 'ETAT) 'STOP )
    (setf (get nom 'EXEC) nil)
    (setf (get nom 'DEBUG) nil)
    ; empty memoire
    (loop for place from 0 to (get_registre nom 'place_libre_suiv)
      do
      (set_memoire nom place () ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;            Initialisation VM                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun initialisation(nom)
  (remise_zero nom)
  (charger_fichier_vm nom "ASM_loader"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          Réalise le code à la VM                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply_vm (nom expression DEBUG)
  (let ((place (get_registre nom 'place_libre_suiv)))
    (charger_vm nom expression)
    (EXEC_vm nom place DEBUG)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Charge le code en memoire de la VM à partir d'une liste d'expression       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun charger_vm (nom expression)
  (loop for ligne in expression
	do
	(___loader___ nom ligne)))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Chargement du code en memoire de la machine virtuelle à partir d'un fihier ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun charger_fichier_vm (nom fichier)
  (labels ((readin (o)
		    (let ((lit (read o nil nil nil)))
		      (loop while lit
			    do
			    (print lit)
			    (___loader___ nom lit)
			    (setf lit (read o nil nil nil))))))
    (readin (open fichier))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 Accesseurs                                   ;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Memoire

(defun get_memoire (nom case_memoire )
  (aref (get nom 'memoire) case_memoire))

(defun set_memoire (nom  case_memoire valeur )
  (setf (aref (get nom 'memoire) case_memoire) valeur))

(defun taille_memoire (nom)
  (length (get nom 'memoire)))


; Registre

(defun get_registre ( nom registre )
  (get nom registre))

(defun set_registre ( nom registre valeur )
  (setf (get nom registre) valeur))


; Debug

(defun get_debug ( nom )
  (get nom 'DEBUG))

(defun set_debug ( nom valeur )
  (setf (get nom 'DEBUG) valeur))

; State 

(defun get_etat ( nom )
  (get nom 'EXEC))

(defun set_etat ( nom valeur )
  (setf (get nom 'EXEC) valeur))


; hastable

(defun get_hash ( nom idTable )
  (get nom idTable))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Instructions                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                   ;;; Accès Mémoire ;;;
; chargement de mémoire à registre  (LOAD <src> <dest>)

(defun LOAD_vm ( nom expListe )
  ( if (integerp (car expListe))
       (setf (get nom (cadr expListe)) (get_memoire nom (car expListe)))
       (if (registrep (car expListe))
          (setf (get nom (cadr expListe)) (get_memoire nom (get_registre (car expListe))))
          (setf (get nom (cadr expListe)) 
                (get_memoire nom (+ (get_registre nom (cadar expListe)) (caar expListe))))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))

; chargement de registre à mémoire   (STORE <src> <dest>

(defun STORE_vm ( nom expListe )
  (if (registrep (cadr expListe))
      (set_memoire nom (get_registre nom (cadr expListe)) (get_registre nom (car expListe)))
      (if (integerp (cadr expListe))
          (set_memoire nom (cadr expListe) (get_registre nom (car expListe))))
          (set_memoire nom (+ (get_registre nom (cadadr expListe)) (caadr expListe)) (get_registre nom (car expListe))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))

; mouvement de registre à registre    (MOVE <src> <dest>)

(defun MOVE_vm ( nom expListe )
  (if (registrep (car expListe))
      (setf (get nom (cadr expListe)) (get nom (car expListe)))
      (if (EQL (caar expListe) '$)
          (setf (get nom (cadr expListe)) (cadar expListe))
          (setf (get nom (cadr expListe)) (get_memoire nom (+ (get_registre nom (cadar expListe)) (caar expListe))))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))


                 ;;; Insctructions Arithmétiques ;;;


; Multiplication (MUL <src> <dest>)

(defun MUL_vm ( nom expListe )
  (set_registre nom (cadr expListe)
    (* (get_registre nom (cadr expListe)) (get_registre nom (car expListe)))) 
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))

; Addition (ADD <src> <dest>)

(defun ADD_vm ( nom expListe )
  (set_registre nom (cadr expListe)
    (+ (get_registre nom (cadr expListe)) (get_registre nom (car expListe))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))

; Division (DIV <src> <dest>)

(defun DIV_vm ( nom expListe )
  (set_registre nom (cadr expListe)
    (/ (get_registre nom (cadr expListe)) (get_registre nom (car expListe))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))

; Soustraction (SUB <src> <dest>)

(defun SUB_vm ( nom expListe )
  (set_registre nom (cadr expListe)
    (- (get_registre nom (cadr expListe)) (get_registre nom (car expListe))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))



                  ;;; Instructions de pile ;;;

          
; Incrément (INCR <dest>)

(defun INCR_vm (nom registre)
  (if (not (registrep registre))
      (error "[INCR_vm] INCR seulement pour les registres")
    (set_registre nom registre (+ (get_registre nom registre) 1)))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))



; Décrément (DECR <dest>)

(defun DECR_vm (nom registre)
  (if (not (registrep registre))
      (error "[DECR_vm] DECR seulement pour les registres")
    (set_registre nom registre (- (get_registre nom registre) 1)))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))


(defun PUSH_vm ( nom registre )
  (if (= (get_registre nom 'SP) (get_registre nom 'CO))
      (error "[PUSH_vm] Erreur le code se trouve sur la pile")
    (prog1
      (set_memoire nom (get_registre nom 'SP) (get_registre nom registre))
      (set_registre nom 'SP (+ (get_registre nom 'SP) 1))
      (set_registre nom 'CO (- (get_registre nom 'CO) 1))
      )))

(defun POP_vm ( nom registre )
  (if (= (get_registre nom 'SP) (get_registre nom 'FP))
      (error "[POP_vm] Implacesible de dépiler")
    (progn 
      (set_registre nom 'SP      (- (get_registre nom 'SP) 1))
      (set_registre nom registre ( get_memoire nom (get_registre nom 'SP)))
      (set_registre nom 'CO      (- (get_registre nom 'CO) 1 )))))



; Instructions de comparaison               

(defun CMP_vm ( nom expListe )

  (if (EQL (length expListe) 2) 
      (cond 
       ((EQL (get_registre nom (car expListe)) (get_registre nom (cadr expListe)))
	(progn 
	  (setf (get nom 'FEQ) 1)
	  (setf (get nom 'FGT) 0)
	  (setf (get nom 'FLT) 0))) 
	
       ((AND
	 (integerp (get_registre nom (car expListe)))
	 (integerp (get_registre nom (cadr expListe))))
	 
	(cond
	 (( < (get_registre nom (car expListe)) (get_registre nom (cadr expListe)))
	  (progn 
	    (setf (get nom 'FEQ) 0)
	    (setf (get nom 'FGT) 0)
	    (setf (get nom 'FLT) 1)))
	 (( > (get_registre nom (car expListe)) (get_registre nom (cadr expListe)))
	  (progn 
	    (setf (get nom 'FEQ) 0)
	    (setf (get nom 'FGT) 1)
	    (setf (get nom 'FLT) 0)))

	 (t
	  (progn 
	    (setf (get nom 'FEQ) 0)
	    (setf (get nom 'FGT) 0)
	    (setf (get nom 'FLT) 0)))))
       (t
	(progn 
	  (setf (get nom 'FEQ) 0)
	  (setf (get nom 'FGT) 0)
	  (setf (get nom 'FLT) 0))))
    
    ; Il s'agit d'une comparaison par fonction prédefinie (consp atom ...)
    (if (get_registre nom (car expListe))
	(progn 
	  (setf (get nom 'FEQ) 1)
	  (setf (get nom 'FGT) 0)
	  (setf (get nom 'FLT) 0))
      (progn 
	(setf (get nom 'FEQ) 0)
	(setf (get nom 'FGT) 0)
	(setf (get nom 'FLT) 0))))
	
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))
 

                          ;;; Instructions de saut


(defun JMP_vm ( nom etiquette )
  (set_registre nom 'CO etiquette))


(defun JSR_vm ( nom etiquette )
  (set_memoire nom (get_registre nom 'SP) (- (get_registre nom 'CO) 1))
  (set_registre nom 'SP (+ (get_registre nom 'SP) 1))
  (if (listp etiquette)
      (OTHER_vm nom (cadr etiquette))
    (JMP_vm nom etiquette)))


(defun JNE_vm ( nom etiquette )
  (if (EQL (get_registre nom 'FEQ) 0) 
      (set_registre nom 'CO etiquette)
    (set_registre nom 'CO (- (get_registre nom 'CO) 1))))


(defun JEQ_vm ( nom etiquette )
  (if (EQL (get_registre nom 'FEQ) 1) 
      (set_registre nom 'CO etiquette)
    (set_registre nom 'CO (- (get_registre nom 'CO) 1))))


(defun JL_vm ( nom etiquette )
  (if (EQL (get_registre nom 'FLT) 1) 
      (set_registre nom 'CO etiquette)
    (set_registre nom 'CO (- (get_registre nom 'CO) 1))))


(defun JLE_vm ( nom etiquette )
  (if (or
       (EQL (get_registre nom 'FEQ) 1)
       (EQL (get_registre nom 'FLT) 1))    
      (set_registre nom 'CO etiquette)
    (set_registre nom 'CO (- (get_registre nom 'CO) 1))))


(defun JG_vm ( nom etiquette )
  (if (EQL (get_registre nom 'FGT) 1) 
      (set_registre nom 'CO etiquette)
    (set_registre nom 'CO (- (get_registre nom 'CO) 1))))


(defun JGE_vm ( nom etiquette )
  (if (or
       (EQL (get_registre nom 'FEQ) 1)
       (EQL (get_registre nom 'FGT) 1)) 
      (set_registre nom 'CO etiquette)
    (set_registre nom 'CO (- (get_registre nom 'CO) 1))))


; retour
(defun RTN_vm (nom)
  (if (< (- (get_registre nom 'SP) 1) 0)
      (error "Error(RTN)")
    (progn
      (set_registre nom 'SP (- (get_registre nom 'SP) 1))
      (set_registre nom 'CO (get_memoire nom (get_registre nom 'SP))))))



           ;;; Instructions diverses ;;;


(defun NOP_vm (nom)
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))


(defun HALT_vm (nom)
  (set_etat nom nil))

;// Seulement sur les registres : (CAR R0)
;//----------------------------------------
(defun CAR_vm (nom listeRegistre)
  (set_registre nom (cadr listeRegistre) (car (get_registre nom (car listeRegistre))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))



; Seulement sur les registres : (CDR R0)
(defun CDR_vm (nom listeRegistre)
  (set_registre nom (cadr listeRegistre) (cdr (get_registre nom (car listeRegistre))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))


; Appel du setf contenant un get

(defun SETFGTET_vm (nom expListe)
  (set_registre nom (caddr expListe)
  		(setf (get (get_registre nom (car expListe)) (get_registre nom (cadr expListe))) (get_registre nom (caddr expListe))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))



; Appel du setf contenant un gethash

(defun SETFHASH_vm (nom expListe)  
  (set_registre nom (caddr expListe)
  		(setf (gethash (get_registre nom (car expListe)) (get_registre nom (cadr expListe))) (get_registre nom (caddr expListe))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))



; Appel du setf contenant un aref

(defun SETFAREF_vm (nom expListe)  
  (set_registre nom (caddr expListe)
  		(setf (aref (get_registre nom (car expListe)) (get_registre nom (cadr expListe))) (get_registre nom (caddr expListe))))
  (set_registre nom 'CO (- (get_registre nom 'CO) 1)))



; Les autres

(defun OTHER_vm ( nom expListe )
  (let ((liste (___OTHER___ nom (get_memoire nom (- (get_registre nom 'FP) 1)) ())))
    (setf (get nom 'R0) (apply expListe liste))
    (RTN_vm nom)))



                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;                   Methodes auxiliaires                       ;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ___loader___LABEL (nom ligne etiq)
  (if (atom (gethash etiq (get_hash nom 'ETIQUETTE)))
      (progn
	(setf (gethash etiq (get_hash nom 'ETIQUETTE)) (get_registre nom 'place_libre_suiv))
	(if (gethash etiq (get_hash nom 'REF_AVANT))
	    (ref_avant_vm nom ligne (gethash etiq (get_hash nom 'REF_AVANT)))))))

(defun ___loader___JUMP (nom ligne etiq saut)
  (progn
    (if (gethash etiq (get_hash nom 'ETIQUETTE))
	(set_memoire nom (get_registre nom 'place_libre_suiv) (list saut (gethash etiq (get_hash nom 'ETIQUETTE))))

      (if (EQL (gethash etiq (get_hash nom 'REF_AVANT)) NIL)
	  (progn 
	    (setf (gethash etiq (get_hash nom 'REF_AVANT)) (get_registre nom 'place_libre_suiv))
	    (set_memoire nom (get_registre nom 'place_libre_suiv) ligne))

	(progn
	  (setf (gethash etiq (get_hash nom 'REF_AVANT)) (list (get_registre nom 'place_libre_suiv)
								(gethash etiq (get_hash nom 'REF_AVANT))))
	  (set_memoire nom (get_registre nom 'place_libre_suiv) ligne))))

    (set_registre nom 'place_libre_suiv (- (get_registre nom 'place_libre_suiv) 1))))
  
(defun ___loader___LOAD (nom ligne etiq load reg)
  (progn
    (if (gethash etiq (get_hash nom 'ETIQUETTE))
	(set_memoire nom (get_registre nom 'place_libre_suiv) (list load (gethash etiq (get_hash nom 'ETIQUETTE)) reg))
	   
      (if (EQL (gethash etiq (get_hash nom 'REF_AVANT)) NIL)
	  (progn
	    (setf (gethash etiq (get_hash nom 'REF_AVANT)) (get_registre nom 'place_libre_suiv))
	    (set_memoire nom (get_registre nom 'place_libre_suiv) ligne))
			      
	(progn
	  (setf (gethash etiq (get_hash nom 'REF_AVANT))
		(list(get_registre nom 'place_libre_suiv) (gethash etiq (get_hash nom 'REF_AVANT)) reg))
	  (set_memoire nom (get_registre nom 'place_libre_suiv) ligne))))

    (set_registre nom 'place_libre_suiv (- (get_registre nom 'place_libre_suiv) 1))))
  
(defun ___loader___STORE (nom ligne etiq store reg)
  (progn
    (if (gethash etiq (get_hash nom 'ETIQUETTE))
      	(set_memoire nom (get_registre nom 'place_libre_suiv) (list store reg (gethash etiq (get_hash nom 'ETIQUETTE))))
      
      (if (EQL (gethash(car (cdaddr ligne)) (get_hash nom 'REF_AVANT)) NIL)
	  (progn
	    (setf (gethash etiq (get_hash nom 'REF_AVANT)) (get_registre nom 'place_libre_suiv))
	    (set_memoire nom (get_registre nom 'place_libre_suiv) ligne))
	
	(progn
	  (setf (gethash etiq (get_hash nom 'REF_AVANT))
		(list(get_registre nom 'place_libre_suiv) reg (gethash etiq (get_hash nom 'REF_AVANT))))
	  (set_memoire nom (get_registre nom 'place_libre_suiv) ligne))))
    
    (set_registre nom 'place_libre_suiv (- (get_registre nom 'place_libre_suiv) 1))))

(defun ___loader___ (nom ligne)
  (if (EQL (car ligne) 'LABEL)
      (___loader___LABEL nom ligne (cadr ligne))

    (if (AND (sautp (car ligne))
	     (EQL (caadr ligne) '@))
	(___loader___JUMP nom ligne (cadadr ligne) (car ligne))


      (if (AND (EQL 'LOAD (car ligne))
	       (listp (cadr ligne))
	       (EQL (caadr ligne) '@))
	  (___loader___LOAD nom ligne (cadadr ligne) (car ligne) (caddr ligne))


	(if (AND (EQL 'STORE (car ligne))
		 (listp (caddr ligne))
		 (EQL (caaddr ligne) '@))
	    (___loader___STORE nom ligne (car (cdaddr ligne)) (car ligne) (cadr ligne))

	  (progn
	    (if (integerp (car ligne))
		(set_memoire nom (get_registre nom 'place_libre_suiv) (car ligne))
	      (set_memoire nom (get_registre nom 'place_libre_suiv) ligne))
	    (set_registre nom 'place_libre_suiv (- (get_registre nom 'place_libre_suiv) 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           Evaluation d'une expression            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eval_vm ( nom expressionession )
  (case (car expressionession)
    (STORE     (STORE_vm     nom   ( cdr  expressionession )))
    (LOAD      (LOAD_vm      nom   ( cdr  expressionession )))
    (MOVE      (MOVE_vm      nom   ( cdr  expressionession ))) 
    (PUSH      (PUSH_vm      nom   ( cadr expressionession )))
    (POP       (POP_vm       nom   ( cadr expressionession )))
    (INCR      (INCR_vm      nom   ( cadr expressionession )))
    (DECR      (DECR_vm      nom   ( cadr expressionession )))
    (CAR       (CAR_vm       nom   ( cdr  expressionession )))
    (CDR       (CDR_vm       nom   ( cdr  expressionession )))
    (ADD       (ADD_vm       nom   ( cdr  expressionession )))
    (SUB       (SUB_vm       nom   ( cdr  expressionession )))
    (MUL       (MUL_vm       nom   ( cdr  expressionession )))
    (DIV       (DIV_vm       nom   ( cdr  expressionession )))
    (CMP       (CMP_vm       nom   ( cdr  expressionession )))
    (JSR       (JSR_vm       nom   ( cadr expressionession )))
    (JMP       (JMP_vm       nom   ( cadr expressionession )))
    (JEQ       (JEQ_vm       nom   ( cadr expressionession )))
    (JNEQ      (JNE_vm       nom   ( cadr expressionession )))
    (JLE       (JLE_vm       nom   ( cadr expressionession )))
    (JGE       (JGE_vm       nom   ( cadr expressionession )))
    (JL        (JL_vm        nom   ( cadr expressionession )))
    (JG        (JG_vm        nom   ( cadr expressionession )))
    (SETFGTET   (SETFGTET_vm   nom   ( cdr  expressionession )))
    (SETFHASH  (SETFHASH_vm  nom   ( cdr  expressionession )))
    (SETFAREF  (SETFAREF_vm  nom   ( cdr  expressionession )))
    (HALT      (HALT_vm      nom   ))
    (NOP       (NOP_vm       nom   ))
    (RTN       (RTN_vm       nom   ))))



;;;;;;;;;;;;;;;;
;   RUN VM     ;
;;;;;;;;;;;;;;;;
(defun EXEC_vm (nom place DEBUG)
  (if (> place 0)
      (set_registre nom 'CO place)
    (set_registre nom 'CO (- (taille_memoire nom) 1)))
  (set_debug nom DEBUG)
  (set_etat nom t )  
  (loop while (get_etat nom)
	do
	(if (get_debug nom)
	    (progn 
	      (print "----------------------------")
	      (print "expressionession :" )
	      (princ (get_memoire nom (get_registre nom 'CO)))))

	(eval_vm nom (get_memoire nom (get_registre nom 'CO)))
      
	(if (get_debug nom)
	    (progn 
	      (print "Valeur de R0 :" )
	      (princ (get_registre nom 'R0))
	      (print "Valeur de R1 :" )
	      (princ (get_registre nom 'R1))
	      (print "Valeur de R2 :" )
	      (princ (get_registre nom 'R2))
	      (print "Valeur de SP :" )
	      (princ (get_registre nom 'SP))
	      (print "Valeur de FP :" )
	      (princ (get_registre nom 'FP))
	      (print "Valeur de FEQ :" )
	      (princ (get_registre nom 'FEQ))
	      (print "Valeur de FLT :" )
	      (princ (get_registre nom 'FLT))
	      (print "Valeur de FGT :" )
	      (princ (get_registre nom 'FGT))
	      (print "Valeur de CO :" )
	      (princ (get_registre nom 'CO))
	      (print "Valeur de place_libre_suiv :" )
	      (princ (get_registre nom 'place_libre_suiv))
	      (affiche_vm nom 0 (get_registre nom 'SP))
	      (print "----------------------------")
	      (read-char)
	      )))

  (if (EQL (get_etat nom) nil)
      (progn
  	(print "Resultat :" )
  	(princ (get_registre nom 'R0)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; S'occupe des ref en avants lors du chargement du code dans la vm   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ref_avant_vm (nom ligne ref)
  (let ((refAvant ref))
    (loop while refAvant
	  do
	  (cond
	   ((listp refAvant)
	    (progn
	      (cond
	       ((EQ (car(get_memoire nom (car refAvant))) 'LOAD)
		(set_memoire nom (car refAvant) (list (car(get_memoire nom (car refAvant)))
						      (gethash (cadr ligne)(get_hash nom 'ETIQUETTE))
						      (nth 2 (get_memoire nom (car refAvant))))))
	       ((EQ (car(get_memoire nom (car refAvant))) 'STORE)
		(set_memoire nom (car refAvant) (list (car(get_memoire nom (car refAvant)))
						     (nth 1 (get_memoire nom (car refAvant)))
						     (gethash (cadr ligne)(get_hash nom 'ETIQUETTE)))))
	       (t
		(set_memoire nom (car refAvant) (list (car(get_memoire nom (car refAvant)))
						      (gethash (cadr ligne)(get_hash nom 'ETIQUETTE))))))
	      (if (EQL(length refAvant) 1)
		  (setf refAvant (car refAvant))		
		(setf refAvant (cadr refAvant)))))
	   (T
	    (progn
	      (cond
	       ((EQ (car(get_memoire nom refAvant)) 'LOAD)
		(set_memoire nom refAvant (list (car(get_memoire nom refAvant))
						(gethash (cadr ligne)(get_hash nom 'ETIQUETTE))
						(nth 2 (get_memoire nom refAvant)))))
	       ((EQ (car(get_memoire nom refAvant)) 'STORE)
		(set_memoire nom refAvant (list (car(get_memoire nom refAvant))
						(nth 1 (get_memoire nom refAvant))
						(gethash (cadr ligne)(get_hash nom 'ETIQUETTE)))))
	       (t
		(set_memoire nom refAvant (list (car(get_memoire nom refAvant))
						(gethash (cadr ligne)(get_hash nom 'ETIQUETTE))))))
	      (setf refAvant nil)))))))



; Liste à partir des paramètres chargés dans la memoire
(defun ___OTHER___ ( nom nbParam liste)
  (if (EQL (length liste) nbParam)
      liste
    (___OTHER___ nom nbParam (cons (get_memoire nom (- (get_registre nom 'FP) (+ (length liste) 2))) liste))))



; Print du code choisi 
(defun affiche_vm (nom debut fin)
  (loop for place from debut to (- fin 1)
	do
	(print place)
	(princ " : ")
	(prin1 (get_memoire nom place))))



; Type registre
(defun registrep (expressionession)
  (member expressionession '(R0 R1 R2 FP SP FEQ FGT FLT CO)))


; Tyoe saut
(defun sautp (expressionession)
  (member expressionession '(jmp jsr jeq jneq jg jl jge jle)))
