#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;;Guita Vahdatinia
;; 1482836
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an
;;    SBIR program, which is the executed. Converts the SBIR program
;;    and prints out the results 

;;Initializing the label table
(define *label-table* (make-hash))
(define (label-get key)
    (hash-ref *label-tabel* key))
(define (label-put! key value)
    (hash-set! *label-table* key value))


;; Initializing function table
(define *function-table* (make-hash))
(define (function-get key)
    (hash-ref *function-table* key))
(define (function-put! key value)
    (hash-set! *function-table* key value))

(for-each
 (lambda (item) (hash-set! *function-table* (car item) (cadr item)))
 `(
        (+      ,+)
        (-      ,-)
        (*      ,*)
        (/      ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        (%      ,(lambda (x y) (- x (* (/ x y) y))))
        (div    ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (print  ,print)
        (atan   ,atan)
        (acos   ,acos)
        (asin   ,asin)
        (tan    ,tan)
        (cos    ,cos)
        (sin    ,sin)
        (exp    ,exp)
        (log    ,log)
        (sqrt   ,sqrt)
        (^      ,expt)
        (<      ,<)
        (>      ,>)
        (<=     ,<=)
        (>=     ,>=)
        (=      ,=)
        (<> ,(lambda (x y) (not (= x y))))
        (abs    ,abs)
        (ceil   ,ceiling)
        (floor  ,floor)
        (round  ,round)
        (trunc  ,truncate)
        (log2 ,(lambda (x) (/ (log (+ x 0.0)) (log 2))))
        (log    ,(lambda (x) (log ( + x 0.0))))
   )
 )

;;Initialize Variable table
(define *variable-table* (make-hash))
(for-each
 (lambda (item) (hash-set! *variable-table* (car item) (cadr item)))
 `(
        (e  2.718281828459045235360287471352662497757247093)
        (pi 3.141592653589793238462643383279502884197169399)
 )
)


(define (variable-get key)
    (hash-ref *variable-table* key))
(define (variable-put! key value)
    (hash-set! *variable-table* key value))

(define *stderr* (current-error-port))

(define *run-file*
  (let-values
      (((dirpath basepath root?)
        (split-path (find-system-path 'run-file))))
    (path->string basepath))
  )

(define (die list)
  (for-each (lambda (item) (display item *stderr*)) list)
  (newline *stderr*)
  (exit 1)
  )

(define (usage-exit)
  (die `("Usage: " ,*run-file* " filename"))
  )
(define (readlist-from-inputfile filename)
  (let ((inputfile (open-input-file filename)))
    (if (not (input-port? inputfile))
        (die `(,*run-file* ": " ,filename ": open failed"))
        (let ((program (read inputfile)))
          (close-input-port inputfile)
          program))))
;;======================================

;;return value of vector at slot 
(define (vectorref ops)
    (vector-ref (variable-get (car ops))
        (- (expression-function (cadr ops)) 1))
)
;;evaluating the expression line 
(define (expression-function line)
  (cond 
        ((string? line) line)
        ((number? line) line)
        ((not (pair? line)) 
            (when (hash-has-key? *variable-table* line)
                (variable-get line)
            )
        )
        ((and (hash-has-key? *variable-table* (car line))
              (vector? (variable-get (car line)))
            )
            (vectorref line)
        )
        ((function-get (car line))
         (apply (function-get (car line))
           (map expression-function (cdr line)))
        )
        (else  (die(printf "failed in expression-function") #f))
    )
)

;;Take input in while decreasing line currently at
(define (input-function line)
    (let ((innum 0))
    (variable-put! `inputcount innum)
        (map (lambda (var)
            (let ((input(read (current-input-port))))
               (if (number? input)
                  (begin
                    (variable-put! var input) 
                    (variable-put! `inputcount (+ innum 1))
                     (set! innum (+ innum 1)))
                    (if (not (eof-object? input))
                     (die `(,*run-file* " incorrect input"))
                        (variable-put! `inputcount -1)

                    )
                )
             )
            )
           line 
        )
    ) #f
)
;;Check if goto is in label and then reference it to go there
(define (goto-function ops)
    (define goto (car ops ))
    (cond
        ((not(pair? goto ))
            ( if (not(hash-has-key? *label-table* goto ))
                (#f)  
                (hash-ref! *label-table* goto 0)
            )
        )
        (else #f)
    )
)
;;Print function ,if not string evaluate
(define (print-function ops)
  (map (lambda (ops)(if (string? ops)
                      (display ops)
                      (display (expression-function ops)))
                      )ops)
  (newline)
  #f
)
;;Insert vector with given variable name
(define (dim-function ops)
    (let* ((vec-name (caar ops))
      (size (expression-function(cadar ops)))
    (vec (make-vector size 0)))
    (variable-put! vec-name vec)
    )
    #f
)
;;All part of let function ==============================
;;Handling list insertion 
(define (put-vector! ops)
           (vector-set! (variable-get (car(car ops)))
                        (- (expression-function (car(cdr(car ops )))) 1)
                        (expression-function (car (cdr ops )))
    )
)
;;Check if let not null then insert if variable
;;or insert if list
(define (help-let ops)
    (if (not (pair? (car ops)))
                (begin        
                (variable-put! (car ops) 
                (expression-function (cadr ops)))
                )
        (if (and ;(evalvect (car ops))
                (hash-has-key? *variable-table* (caar ops))
                (vector? (variable-get (caar ops)))
                ;(evalvect (car ops))
            ) (put-vector! ops)
            (#f))
    )
)
;;take in let function and make sure not null
;;then call help-let to evaluate
(define (let-function ops)
  (cond
   ((and (null? (cdr (cdr ops)))
        (pair? ops)
        (expression-function (cadr ops)))
        (help-let ops)
    )
   (else #f)
  ) 
  
#f)
;;End of let function ================================
;; If function, looks up operation in functiontable
;; and then handles that operation
(define (if-function stmt )
  (define line (car stmt ))
  (define label (cdr stmt ))
  (define ops (car line))
   (if (hash-has-key? *function-table* ops)
        (if (expression-function line )
            (goto-function label)
            #f)
        ( #f)
    )
)
;;Evaluate the given statement and goes to corresponding
;;function
(define (evaluate stmt )
    (let ((expr (car stmt )) (args (cdr stmt )))
  (cond
    ((eqv? expr 'if) (if-function args ))
    ((eqv? expr 'let) (let-function args))
    ((eqv? expr 'dim) (dim-function args))
    ((eqv? expr 'print) (print-function args))
    ((eqv? expr 'goto) (goto-function args))
    ((eqv? expr 'input) (input-function args))
    )
))

;; Parses the line to get line number label and statement
(define (evaluate-program prgm)
  (unless (null? prgm) 
  (define line (car prgm))
  (define (run-line)
    (if (not (null? (cdr line)))
        (if (not(pair? (cadr line)))
            (if (not (null? (cddr line)))
                (evaluate(caddr line)) #f)
            (evaluate(cadr line))) #f))
  (let ((x (run-line)))
    (if (not x) (evaluate-program(cdr prgm)) (evaluate-program x)))))

;;adds labels to the labeltable
(define (evallabels prgm)
  (when (not(null? prgm))
  (define ln (car prgm))
  (when (not(null? (cdr ln)))
    (when (not(pair? (cadr ln)))
      (when (not(hash-has-key? *label-table* (cadr ln)))
        (label-put! (cadr ln) prgm))))
    (evallabels (cdr prgm))))


(define (main arglist)
  (if (or (null? arglist) (not (null? (cdr arglist))))
    (usage-exit)
    (let* ((sbprogfile (car arglist))
           (program (readlist-from-inputfile sbprogfile)))
      (evallabels program)
      (evaluate-program program))))

(main (vector->list (current-command-line-arguments)))


