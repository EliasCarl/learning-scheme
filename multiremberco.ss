;
; This is the multirember&co procedure from The Little Schemer page
; 137. This procedure marks a sharp increase in the difficulty in
; the book. What does this procedure do?
;

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? a (car lat))
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat) seen)))))
      (else
        (multirember&co a
                        (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car lat) newlat) seen)))))))

; Let's begin by expanding the following call to multirember&co. We
; call the procedure with the following arguments.
;
; a   = 'tuna
; lat = (tuna)
; col = list

(multirember&co 'tuna (tuna) list)

; Since the list of atoms, (tuna) in this case, is not null,
; the first cond will fail. But since (eq? 'tuna (car (tuna))) 
; is #t,the second cond will succeed, and we recur with three args.
; Below I is the exact call.

(multirember&co 'tuna (cdr (tuna)) (lambda (newlat seen)
                                     (list newlat 
                                           (cons (car (tuna)) 
                                                 seen))))

; The first argument is still 'tuna, and will always be. The second
; argument is (cdr (tuna)), which is '(). The third argument is a
; procedure. Expanding this call as much as we can by replacing all
; (car (tuna))'s with 'tuna, and (cdr (tuna)) with '(), we get this.

(multirember&co 'tuna '() (lambda (newlat seen)
                            (list newlat (cons 'tuna seen))))

; As you probably figured out, the interesting part is the third
; argument. It's important to understand that this is a closure,
; all that means is that, in this case, this anonymous function is
; defined "around" (closes over) the value 'tuna, which it gets from
; it's outer lexical environment at the moment of the definition.
;
; In this call the first cond checking the null list will succeed, 
; and all we do is call the passed closure with two empty lists.
; Note that this is just the closure called with '() and '().

(lambda (newlat seen)
  (list newlat (cons 'tuna seen)))
('() '())

; Substituting the parameters newlat and seen for '(), we get.

(lambda (list '() (cons 'tuna '())))

; Consing the 'tuna onto '() we get this.

(lambda (list '() (tuna)))

; Just to really understand this, lets expand this call again but
; this time with three arguments, so we get to try out that else
; case as well. The initial call now looks like this.
;
; a   = 'tuna
; lat = (berries tuna fish)
; col = list

(multirember&co tuna (berries tuna fish) list)

; The first time around, (car lat) is not eq to tuna, so we move
; on to the else case. This looks pretty similar to what we did
; before, but this closure is not exactly the same, and the recurring
; call differs as well. Note that in this closure, berries is consed
; onto newlat, whereas in the eq cond, the closure will be defined so
; that it's consed onto seen.

; The first time around, (car lat) eq 'berries, which will put us
; at the else case right away. The else case is not very different
; from the eq case. The recursion will look like this.

(multirember&co 'tuna (cdr (berries tuna fish)) (lambda (newlat seen)
                                                  (list (cons (car lat) 
                                                              newlat) 
                                                        seen)))

; And expanded:

(multirember&co 'tuna (tuna fish) (lambda (newlat seen)
                                    (list (cons 'berries newlat) seen)))

; This time (car (tuna fish)) will eq 'tuna, and so we go into the eq
; cond. But note that this time we need to use our passed down closure 
; in the body of the third function argument and we have to nest our
; lambdas. Now I skip the intermediate step of writing out the cdr's 
; and car's.
;
; What is happening here is that we're building onto the closure, so
; we get a stack of functions where the arguments are consed before
; feeding into the next function, waiting to be unraveled. Also,
; remember that the outer 'tuna comes from the outer lexical
; environment, and the innermost 'berries comes from the inital call;
; these are the closed over values we carry with us every time we
; define one of these "third-argument functions".

(multirember&co 'tuna (fish) (lambda (newlat seen)
                               (lambda (newlat seen)
                                 (list (cons 'berries newlat) seen))
                               (newlat (cons 'tuna seen))))

; In the next recursion, 'fish wont be eq to 'tuna, so we go into the
; else again and recur. In the process we will of course define a new
; lambda that closes over the current value, now we have three nested
; functions waiting to be unraveled.

(multirember&co 'tuna '() (lambda (newlat seen)
                            (lambda (newlat seen)
                              (lambda (newlat seen)
                                (list (cons 'berries newlat) seen))
                              (newlat (cons 'tuna seen)))
                            ((cons ('fish newlat)) seen)))

; In the next and final recursion, we will be stopped in the null? check.
; This is our base case; the list is now empty. Now we call the passed in
; closure with two empty lists. That call will look like this.

(lambda (newlat seen)
  (lambda (newlat seen)
    (lambda (newlat seen)
      (list (cons 'berries newlat) seen))
    (newlat (cons 'tuna seen)))
  ((cons ('fish newlat)) seen))
('() '())

; Now this is the moment we have been waiting for. This is where we
; unwind this stack of lambdas, and somehow we should have separated
; the list into two. We first substitute the null lists and get this.

(lambda (newlat seen)
  (lambda (newlat seen)
    (list (cons berries newlat) seen))
  (newlat (cons 'tuna seen)))
((cons ('fish '())) '())

; Note that we completely remove the first outer lambda, and substitute
; the empty lists into the first inner lambda, which is the lambda that
; makes use of the outer lambdas arguments. We substitute further.

(lambda (newlat seen)
  (list (cons berries newlat) seen))
((fish) (tuna))

; And further.

(list (berries fish) (tuna))

