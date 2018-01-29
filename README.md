# Virtual-machine-registre
Compilateur et Générateur VM
# Execution du compilateur
1. (load "compilateur.lisp")
2. (compiler '(defun fibo (n) (cond ((= n 0) 0) ((= n 1) 1) (m ( + (fibo1 (- n 1)) (fibo1 (- n 2)))))))
3. (compiler '(fibo 6))
# Execution de la VM
1. (load "vm.lisp")
2. (make_vm 'essai 1048576)
3. (charger_fichier_vm 'essai "fibo.lisp.asm")
4. (apply_vm 'essai '((MOVE ($ 5) R0) (PUSH R0) (MOVE ($ 1) R0) (PUSH R0) (INCR R0) (MOVE FP R1) (MOVE SP FP) (MOVE SP R2) (SUB R0 R2) (PUSH R2) (PUSH R1) (JSR (@ FIBO5)) (POP R1) (POP R2) (MOVE R1 FP) (MOVE R2 SP) (HALT)) 1)
