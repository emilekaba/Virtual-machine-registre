(defun is_op_arith ( op )
(member op '(+ - * /)))
(defun is_op_comp ( op )
(member op '(= < > != <= >= EQ EQL EQUAL)))   

;crée un compteur
(let ((etiq 0)) (defun compteur () etiq) (defun compteur++ () (setf etiq (+ etiq 1))) (defun reset () (setf etiq 0)))

;crée l'environnement pour un let
(defun create_let_env (lexpr env acc top_level envPre)
  (if (atom lexpr)
      env
    (progn
      (to_asm (cadar lexpr) env top_level envPre)
      (concat (list 'PUSH 'R0))
      (create_let_env (cdr lexpr) (add_to_env (caar lexpr) (list (+ (length env) acc) 'FP) env) acc top_level envPre ))))

;crée l'environnement pour une variable locale top level
(defun create_high_let_env (lexpr env)
  (if (atom lexpr)
      env
    (let ((etiq (compteur++)))
      ;(compteur++)
      (let ((var (compteur++)))
	;(compteur++)
	(concat (list 'JMP (list '@ etiq)))
	(concat (list 'LABEL var))
	(concat (list (cadar lexpr)))
	(concat (list 'LABEL etiq))
	(create_high_let_env (cdr lexpr) (add_to_env (caar lexpr) (list '@ var) env))))))

;Creer l'environnement pour un labels
(defun make_labels_env (lexpr etiq env)
  (if (atom lexpr)
      env
    (make_labels_env (cdr lexpr) (compteur++) (add_to_env (caar lexpr) etiq env))))

;ajoute un nveau couple symbole valeur à l'environnement
(defun add_to_env (symbole valeur env)
  (acons symbole (list valeur) env))


; test (a X Y)
(defun op_to_compil (expr etiq env top_level envPre)
  (cond
   ((EQL 't expr)
    (concat (list 'JMP (list '@ etiq))))

   ((atom expr)
    (to_asm expr env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JEQ (list '@ etiq))))
   
   ((is_op_comp (car expr))
    (op_to_compil_part expr env top_level envPre)
    (comp_to_compil expr etiq))

   ((EQL (car expr) 'AND)
    (and_to_compil (cdr expr) (compteur++) etiq (compteur++) env top_level envPre))

   ((EQL (car expr) 'OR)
    (or_to_compil (cdr expr) etiq env top_level envPre))

   ((EQL (car expr) 'NOT)
    (not_to_compil (cdr expr) etiq env top_level envPre))

   (t
    (to_asm expr env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JEQ (list '@ etiq))))))

(defun and_to_compil (expr etiqFaux etiqFin etiq env top_level envPre)
  (cond
   ((atom expr)
    (concat (list 'LABEL etiq))
    (concat (list 'JMP (list '@ etiqFin)))
    (concat (list 'LABEL etiqFaux)))
   ((is_op_comp (caar expr))
    (concat (list 'LABEL etiq))
    (op_to_compil_part (car expr) env top_level envPre)
    (comp_to_compil (car expr) (compteur++))      
    (concat (list 'JMP (list '@ etiqFaux)))
    (and_to_compil (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   ((EQ 'NOT (caar expr))
    (not_to_compil (cdar expr) (compteur++) env top_level envPre)
    (concat (list 'JMP (list '@ etiqFaux)))
    (and_to_compil (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   (t
    (concat (list 'LABEL etiq))
    (to_asm (car expr) env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JEQ (list '@ (compteur++))))
    (concat (list 'JMP (list '@ etiqFaux)))
    (and_to_compil (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))))

(defun not_and_to_compil (expr etiqFaux etiqFin etiq env top_level envPre)
  (cond
   ((atom expr)
    (concat (list 'LABEL etiq))
    (concat (list 'JMP (list '@ etiqFin)))
    (concat (list 'LABEL etiqFaux)))
   ((is_op_comp (caar expr))
    (concat (list 'LABEL etiq))
    (op_to_compil_part (car expr) env top_level envPre)
    (neg_comp_to_compil (car expr) (compteur++))      
    (concat (list 'JMP (list '@ etiqFaux)))
    (not_and_to_compil (cdr expr) etiqFaux etiqFin (compteur) env top_level envPre))
   (t
    (concat (list 'LABEL etiq))
    (to_asm (car expr) env top_level envPre)
    (concat (list 'CMP 'R0))
    (concat (list 'JNEQ (list '@ (compteur++))))
    (concat (list 'JMP (list '@ etiqFaux)))
    (not_and_to_compil (cdr expr) etiqFaux etiqFin (compteur) env top_level))))

(defun or_to_compil (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((is_op_comp (caar expr))
    (op_to_compil_part (car expr) env top_level envPre)
    (comp_to_compil (car expr) etiq) 
    (or_to_compil (cdr expr) etiq env top_level envPre))
   ((EQ 'NOT (caar expr))
    (not_to_compil (cdar expr) etiq env top_level envPre)
    (or_to_compil (cdr expr) etiq env top_level envPre))
   (t
    (to_asm (car expr) env top_level envPre)
    (concat (list 'CMP (caar expr) 'R0))
    (concat (list 'JEQ (list '@ etiq)))
    (or_to_compil (cdr expr) etiq  env top_level envPre))))

(defun not_or_to_compil (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((is_op_comp (caar expr))
    (op_to_compil_part (car expr) env top_level envPre)
    (neg_comp_to_compil (car expr) etiq) 
    (not_or_to_compil (cdr expr) etiq env top_level envPre))
   (t
    (to_asm (car expr) env top_level envPre)
    (concat (list 'CMP (caar expr) 'R0))
    (concat (list 'JNEQ (list '@ etiq)))
    (not_or_to_compil (cdr expr) etiq  env top_level envPre))))

(defun not_to_compil (expr etiq env top_level envPre)
  (cond
   ((atom expr)
    nil)
   ((is_op_comp (caar expr))
    (op_to_compil_part (car expr) env top_level envPre)
    (neg_comp_to_compil (car expr) etiq) 
    (not_to_compil (cdr expr) etiq env top_level envPre))
   ((EQ (caar expr) 'AND)
    (not_and_to_compil (cdar expr) (compteur++) etiq (compteur++) env top_level envPre)
    (not_to_compil (cdr expr) etiq env top_level envPre))
   ((EQ (caar expr) 'OR)
    (not_or_to_compil (cdar expr) etiq env top_level envPre)
    (not_to_compil (cdr expr) etiq env top_level envPre))

   (t
    (to_asm (car expr) env top_level envPre)
    (concat (list 'CMP (caar expr) 'R0))
    (concat (list 'JNEQ (list '@ etiq)))
    (not_to_compil (cdr expr) etiq  env top_level envPre))))

(defun op_to_compil_part (expr env top_level envPre)
  (progn
    (to_asm (cadr expr) env top_level envPre)
    (concat (list 'PUSH 'R0))
    (to_asm (caddr expr) env top_level envPre)
    (concat (list 'PUSH 'R0))
    (concat (list 'POP 'R1))
    (concat (list 'POP 'R0))
    (concat (list 'CMP 'R0 'R1))))

(defun comp_to_compil (expr etiq)
  (case (car expr)
    (= (concat (list 'JEQ (list '@ etiq))))
    (EQ   (concat (list 'JEQ (list '@ etiq))))
    (EQL     (concat (list 'JEQ (list '@ etiq))))
    (EQUAL     (concat (list 'JEQ (list '@ etiq))))
    (<     (concat (list 'JL (list '@ etiq))))
    (>     (concat (list 'JG (list '@ etiq))))
    (!=     (concat (list 'JNEQ (list '@ etiq))))
    (<=     (concat (list 'JLE (list '@ etiq))))
    (>=     (concat (list 'JGE (list '@ etiq))))))

(defun neg_comp_to_compil (expr etiq)
  (case (car expr)
    (=     (concat (list 'JNEQ (list '@ etiq))))
    (EQ     (concat (list 'JNEQ (list '@ etiq))))
    (EQL     (concat (list 'JNEQ (list '@ etiq))))
    (EQUAL     (concat (list 'JNEQ (list '@ etiq))))
    (<     (concat (list 'JGE (list '@ etiq))))
    (>    (concat (list 'JLE (list '@ etiq))))
    (!=   (concat (list 'JEQ (list '@ etiq))))
    (<=     (concat (list 'JG (list '@ etiq))))
    (>=  (concat (list 'JL (list '@ etiq))))))

;Creer l'environnement a partir des parametres
(defun make_param_env (lpara env)
  (if (atom lpara)
      env
    (make_param_env (cdr lpara) (add_to_env (car lpara) (list (- 0 (+ (length lpara) 1)) 'FP) env) )))

(let ((liste ()))
(defun compiler (expr &optional fichier)
  (setf liste ())
  (if (atom expr)
      ()
    (to_asm expr () t ()))
  (concat '(HALT)) 
  (reverse liste))

;;;
(defun to_asm (expr env top_level envPre)

  (if (atom expr)
      (exp_to_compil expr env envPre)
    (cond
     ((EQL 'QUOTE (car expr))
	 
      (if (listp (cadr expr))
	  (exp_to_compil (cadr expr) env envPre)
	(exp_to_compil (cadr expr) env envPre)))
     ((EQ 'defun (car expr))
(concat (list 'LABEL (cadr expr)))

      ;créer l'environnement à/p des parametres
      (let ((env (make_param_env (caddr expr) env)))
(to_asm (cadddr expr) env () envPre)
	(concat (list 'RTN))))
     ((is_op_arith (car expr))
      (to_asm (cadr expr) env top_level envPre)
      (concat (list 'PUSH 'R0))
(to_asm (caddr expr) env top_level envPre)
      (concat (list 'PUSH 'R0))
      (concat (list 'POP 'R1))
      (concat (list 'POP 'R0))
      (case (car expr )
	(+ (concat (list 'ADD 'R1 'R0)))
	(-	 (concat (list 'SUB 'R1 'R0)))
(*	 (concat (list 'MUL 'R1 'R0)))
	(/	 (concat (list 'DIV 'R1 'R0)))))
((EQ 'if (car expr))
	
      (let ((saut1 (compteur++)))
	(let ((saut2 (compteur++)))
(op_to_compil (cadr expr) saut1 env top_level envPre)
	  (to_asm (cadddr expr) env top_level envPre)
(concat (list 'JMP (list '@ saut2)))
	  (concat (list 'LABEL saut1))
	  (to_asm (caddr expr) env top_level envPre)
(concat (list 'LABEL saut2)))))
     ((EQ 'progn (car expr))	
      (loop for elmt in (cdr expr) 
	    do
	    (to_asm elmt env top_level envPre)))
     ((EQ 'cond (car expr))  
      
      (let ((fin (compteur++)))	  
	(loop for val in (cdr expr)
	      do
	      (let ((saut1 (compteur++)))
(let ((saut2 (compteur++)))
		  (op_to_compil (car val) saut1 env top_level envPre)
	  (concat (list 'JMP (list '@ saut2)))
		  (concat (list 'LABEL saut1))
(loop for elt in (cdr val) 
			do
		(to_asm elt env top_level envPre))
		  (concat (list 'JMP (list '@ fin)))
(concat (list 'LABEL saut2)))))
	(concat (list 'LABEL fin))))
     (t
 (if (atom (car expr))
	  (progn
(par_to_compil (cdr expr) env top_level envPre )
	    (concat (list 'MOVE (list '$ (length (cdr expr))) 'R0))
	   (concat (list 'PUSH 'R0))
	(concat (list 'INCR 'R0))
	(concat (list 'MOVE 'FP 'R1))
	    (concat (list 'MOVE 'SP 'FP))
	(concat (list 'MOVE 'SP 'R2))
	    (concat (list 'SUB 'R0 'R2))
(concat (list 'PUSH 'R2))
(concat (list 'PUSH 'R1))
	    (let ((etiq (cadr (assoc (car expr) env :test #'eql))))
	      (if (null etiq)
		  (concat (list 'JSR (list '@ (car expr))))
(concat (list 'JSR (list '@ etiq)))))
(concat (list 'POP 'R1))
	(concat (list 'POP 'R2))
	    (concat (list 'MOVE 'R1 'FP))
(concat (list 'MOVE 'R2 'SP)))
	(error "Erreur: ~S ne peut pas etre compilerr." (car expr)))))))

;fction de concaténation
(defun concat (code_asm)
   (setf liste (cons code_asm liste))))

;compiler les expressions passées dans le let
(defun let_to_compil (lexpr env top_level envPre)
  (if (atom lexpr)
      ()
    (progn
      (to_asm (car lexpr) env top_level envPre)
      (let_to_compil (cdr lexpr) env top_level envPre))))


;compiler les expressions passées en parametre
(defun par_to_compil (lexpr env top_level envPre) 
  (if (atom lexpr)
      ()
    (progn
      (to_asm (car lexpr) env top_level envPre)
      (concat (list 'PUSH 'R0))
      (par_to_compil (cdr lexpr) env top_level envPre))))

;expression atomique connu dans l'environnement ou label
(defun exp_to_compil (var env envPre)
  (if (AND
       (symbolp var)
       (NOT (EQL var 'nil)))
      (let ((res (cadr (assoc var env :test #'eql))) (res2 (cadr (assoc var envPre :test #'eql))))
	(if (null res)
	    (if (null res2)
		(concat (list 'MOVE (list '$ var) 'R0))
	      (progn
		(concat (list 'MOVE 'FP 'R2))
		(concat (list 'MOVE (list 3 'FP) 'FP))
		(concat (list 'MOVE res2 'R0))
		(concat (list 'MOVE 'R2 'FP)) 
		))
	  (if (atom res)
	      (concat (list 'MOVE res 'R0))
	    (if (EQ (car res) '@)
		(concat (list 'LOAD res 'R0))
	      (concat (list 'MOVE res 'R0))))))	
    (concat (list 'MOVE (list '$ var) 'R0))))