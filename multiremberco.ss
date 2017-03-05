; This is the multirember&co procedure from The Little Schemer page
; 137. This procedure marks a sharp increase in difficulty in
; the book. This document walks thourgh this procedure and expands
; the recursion. This is only for learning purposes.

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

; Since the list of atoms, (tuna) in this case, is not null, the first 
; cond will fail. But since (eq? 'tuna (car (tuna))) is #t, the second 
; cond will succeed, and we recur with three args. Below is the exact 
; call.

(multirember&co 'tuna (cdr (tuna)) (lambda (newlat seen)
                                     (list newlat (cons (car (tuna)) seen))))

; Expanding the car's and cdr's.

(multirember&co 'tuna '() (lambda (newlat seen)
                            (list newlat (cons 'tuna seen))))

; The third argument is of course where it gets tricky. This lambda is
; a closure. The variable it closes over, as seen in the definition, is
; (car lat), which in our particlar case is the atom 'tuna. The two other 
; variables in the closure are not yet defined, because the closure is 
; not called at this moment, it's passed on to the next recursion.
;
; In the original function definition, this closure is bound to the 
; variable "col". This name is short for "collector". The closed over 
; variable 'tuna is the part we're collecting. If you're unsure about 
; how a function can "store" a value like this, you need to learn how 
; closures work.
;
; It's also important to understand that the (list) function we passed
; is _not_ the closure. The function (list) will _only be called once_.
; We'll see below when it's actually called.
;
; Going into the first recursive call, understand that "col", is now
; bound to the closure we discussed above, and the 'tuna comes with it.
; Since we passed a '(), the (null?) check will be #t. Now we actually
; call the closure (not the list function!), and we call it with two 
; null lists. And the closed over variable is already there inside 
; of the closure.

(lambda (newlat seen)
  (list newlat (cons 'tuna seen)))
('() '())

; Substituting the parameters newlat and seen for '(), we get.

(lambda (list '() (cons 'tuna '())))

; Consing the 'tuna onto '() we get this.

(lambda (list '() (tuna)))

;;;;;;;;;;;;;;;;;;;;;;;;;;   EXPLAINATION   ;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; And what did we accomplish? We did one of two things everytime we
; came across an atom. If the atom was equal to 'tuna, we recurred 
; with a closure that _when called_, will cons the atom onto whatever 
; "seen" is. If the atom was not equal to 'tuna, we recurred with a 
; closure that _when called_, will cons the atom onto whatever "newlat"
; is. 
;
; If you look closely, you'll notice that newlat and seen are not so
; mysterious actually. These arguments are where the next recursion 
; will put it's result. Because we're a recursive function, we need 
; to know what the next iterations result is and combine that with our 
; own result. So we're asking a future computation to define newlat and
; seen for us.
;
; Now, each iteration may ask the next iteration for newlat and seen,
; because it wants to cons something onto one of them. The only case
; where it doesn't, finally, is the base case. 
;
; When we no longer have any atoms to process we call the closure 
; passed to us most recently and define newlat and seen as two null 
; lists. The stack of closures asking for newlat and lat will unwind, 
; and the closed over variable will be consed onto one of the two list 
; in each call. Finally we get to the innermost body, where our (list) 
; is still waiting around, we pass it the two now complete lists and 
; return the result.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Just to really understand this, lets expand this call again but
; this time with three arguments, so we get to try out that else
; case as well. The initial call now looks like this.
;
; a   = 'tuna
; lat = (berries tuna fish)
; col = list

(multirember&co 'tuna (berries tuna fish) list)

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

; Expanding the car's and cdr's.

(multirember&co 'tuna (tuna fish) (lambda (newlat seen)
                                    (list (cons 'berries newlat) seen)))

; This time (car (tuna fish)) will eq 'tuna, and so we go into the eq
; cond. But note that this time we need to use our passed down closure 
; in the body of the third function argument and we have to nest our
; closures. Remember, the inner Closure 1 is asking the outer Closure
; 2 to give it newlat and seen, but Closure 2 doesn't know what newlat
; or seen is, it will have to ask the next iteration. However, Closure
; 2 does know that whatever seen will be, 'tuna should be consed with it.

(multirember&co 'tuna (fish) (lambda (newlat seen) ; Closure 2
                               (lambda (newlat seen) ; Closure 1
                                 (list (cons 'berries newlat) seen))
                               (newlat (cons 'tuna seen))))

; In the next recursion, 'fish wont be eq to 'tuna, so we go into the
; else again and recur. In the process we will of course define a new
; lambda that closes over the current value, now we have three nested
; closures waiting to be unraveled.

(multirember&co 'tuna '() (lambda (newlat seen) ; Closure 3
                            (lambda (newlat seen) ; Closure 2
                              (lambda (newlat seen) ; Closure 1
                                (list (cons 'berries newlat) seen))
                              (newlat (cons 'tuna seen)))
                            ((cons ('fish newlat)) seen)))

; In the next and final recursion, we will be stopped in the (null? ...) 
; check. This is our base case; the list is now empty. Now we call the 
; passed closure with two empty lists. That call will look like this.

(lambda (newlat seen) ; Closure 3
  (lambda (newlat seen) ; Closure 2
    (lambda (newlat seen) ; Closure 1
      (list (cons 'berries newlat) seen))
    (newlat (cons 'tuna seen)))
  ((cons ('fish newlat)) seen))
('() '())

; Now this is the moment we have been waiting for. This is where we
; unwind this stack of closures, and each closed over variable will
; finally actually be consed. We first substitute the null lists and 
; get this, i.e. we answer Closure 3's question with '() and '().

(lambda (newlat seen) ; Closure 2
  (lambda (newlat seen) ; Closure 1
    (list (cons 'berries newlat) seen))
  (newlat (cons 'tuna seen)))
((cons ('fish '())) '())

; Substitute further.

(lambda (newlat seen) ; Closure 1
  (list (cons 'berries newlat) seen))
((fish) (tuna))

; And further.

(list (berries fish) (tuna))
