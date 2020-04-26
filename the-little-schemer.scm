(define (atom? x) (not (pair? x)))


(define L `(("a" "b") ("c" "d")))
(define lat `(1 1 2 3 3 4 4 5 5 5))

(define firsts
  (lambda (x)
    (cond
      ((null? x) (quote ()))
      (else
        (cons (car (car x)) (firsts (cdr x) )) 
      )
    )
  )
)


(define insertR
  (lambda (x y lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? x (car lat)) (cons x (cons y (cdr lat))))
      (else (cons (car lat) (insertR x y (cdr lat))
      ))
    )  
  )
)

(define insertL
  (lambda (x y lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? x (car lat)) (cons y lat))
      (else (cons (car lat) (insertL x y (cdr lat))
      ))
    )  
  )
)


(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))

      ; ((eq? o1 (car lat)) (cons new (cdr lat)))
      ; ((eq? o2 (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat))
      ))
    )  
  )
)

(define multiinsertR
(lambda (new old lat)
(cond
  ((null? lat) (quote ()))
  ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat) )))) 
  (else (cons (car lat) (multiinsertR new old (cdr lat)) ))
)
))


(display (subst2 "a" 22 5 lat))
(display "\n")
(display (subst2 "a" 5 2 lat))
(display "\n")


(define add1
  (lambda (m)
  (+ 1 m)
))

(define sub1
  (lambda (m)
  (- m 1)
))


(define add
  (lambda (m n)
    (cond
      ((zero? m) n)
      ((zero? n) m)
      (else (add (sub1 m) (add1 n)))
    )
))

(define sub
  (lambda (m n)
    (cond
      ((zero? m) n)
      ((zero? n) m)
      (else (sub (sub1 m) (sub1 n)))
    )
))


(define sum
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (sum (cdr tup))))
    )
  )
)


(define make_latte
  (lambda ()
    `(steamed-milk)
  )
)


(define make_special_latte
  (lambda (special_ingredient)
    (cons special_ingredient (make_latte)) 
  )
)

(define xor
  (lambda (p q)
    (and (or p q) (or (not p) (not q)))
  )
)


(define mult
 (lambda (m n)
  (_mult (abs (max m n)) (abs (min m n)) (xor (> m 0) (> n 0)))
 )
)

(define _mult
  (lambda (m n predicate)
    (cond
      (predicate (- 0 (_mult_pos m n)))
      (else (_mult_pos m n))
    )
  )
)


(define _mult_pos
  (lambda (m n)
  ; Note that m and n should be positive.
    (cond
      ((eq? n 0) 0)
      ((eq? n 1) m)
      (else (add m (_mult_pos m (sub1 n)))) 
    )
  )
)


(define tupsum 
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tupsum (cdr tup1) (cdr tup2))))
    )
  )
)

(define greater
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (greater (sub1 x) (sub1 y)))
    )
  )
)

(define lesser
  (lambda (x y)
    (greater y x)
  )
)

(define equal
  (lambda (x y)
    (and 
      (not (greater x y)) 
      (not (lesser x y))
    )
  )
)

(define equal2
  (lambda (x y)
  (cond 
    ((greater x y) #f)
    ((lesser x y) #f)
    (else #t)
  )
))

(define pow
  (lambda (base n)
    (cond
      ((eq? n 0) 1)
      ((eq? n 1) base)
      (else (* base (pow base (sub1 n))))
    )
  )
)



(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat))))
    )  
  )
)

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) -1)
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))) 
    )
  )
)

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) lat)
      ((eq? n 1) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat))))
    )
  )
)


(define no-num
 (lambda (lat)
  (cond
    ((null? lat) (quote ()))
    ((number? (car lat)) (no-num (cdr lat)))
    (else (cons (car lat) (no-num (cdr lat))))
  )
 )
)

(define mixed_lat `(a 1 b 2 c 3 33 44 b z))



(define rember*
  (lambda (a l)
    (cond 
      ((null? l) (quote ()))
      ((atom? (car l)) 
        (cond 
          ((eq? (car l) a) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))
        )
      )
      (else (cons (rember* a (car l)) (rember* a (cdr l))))
    )  
  )
)

(define another_list `((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond 
      ((null? l) (quote ()))
      ((atom? (car l)) 
        (cond 
          ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))
        )
      )
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
    )  
  )
)

(define long_list `((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))


; page 84

(define occur*
  (lambda (a l )
    (cond 
      ((null? l) 0)
      ((atom? (car l)) 
        (cond
          ((eq? a (car l)) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))
        )
      )
      (else (+ (occur* a (car l)) (occur* a (cdr l))))
    )  
  )
)

(define l `((banana) (split ((((banana ice))) (banana cream (banana)) sherbet))
(banana) (banana banana)
(bread banana)
(banana banana brandy)))

(display (occur* `banana l))
(display "\n")



(define subst*
  (lambda (new old l)
    (cond 
      ((null? l) (quote ()))
      ((atom? (car l)) 
        (cond 
          ((eq? (car l) old) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))
        )
      )
      (else (cons (subst* new old (car l)) (subst* new old (cdr l))))
    )  
  )
)

(define insertL*
  (lambda (new old l)
    (cond 
      ((null? l) (quote ()))
      ((atom? (car l)) 
        (cond 
          ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l))))
        )
      )
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
    )  
  )
)

(define leftmost
  (lambda (l)
    (cond 
      ((atom? (car l)) (car l)) 
      (else (leftmost (car l)))
    )  
  )
)


(define eqlist?
  (lambda (m n)
    (cond 
      ((and (null? m) (null? n) #t))
      ((or (null? m) (null? n)) #f)
      ((and (atom? (car m)) (atom? (car n)))
        (cond
          ((eq? (car m) (car n)) (eqlist? (cdr m) (cdr n)))
          (else #f)
        )
      )
      ((and (not (atom? (car m))) (not (atom? (car n)))) 
        (and (eqlist? (car m) (car n)) (eqlist? (cdr m) (cdr n))))
      (else #f)
    )  
  )
)

(define equal?
  (lambda (x y)
    (cond
      ((and (atom? x) (atom? y)) (eq? x y))
      ((or (atom? x) (atom? y)) #f)
      (else (eqlist? x y))
    )
  )
)

(define a (quote (1)))
(define b `(1))

(define rember
(lambda (s l)
(cond
((null? l) (quote ()))
(else (cond
((equal? (car l) s) (cdr l))
(else (cons (car l)
(rember s
(cdr l))))))))) 

(+ 3 (* 4 5))

; chapter 6

(define numbered_long?
  (lambda (aexp)
    (cond 
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr (aexp)))))))
      ((eq? (car (cdr aexp)) (quote *))(and (numbered? (car aexp)) (numbered? (car (cdr (cdr (aexp)))))))
      ((eq? (car (cdr aexp)) (quote up))(and (numbered? (car aexp)) (numbered? (car (cdr (cdr (aexp)))))))
    )
  )
)

(define numbered?
  (lambda (aexp)
    (cond 
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car (aexp))) (numbered? (car (cdr (cdr aexp))))))
     
    )
  )
)

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((and (atom? (car nexp)) (atom? (car (cdr (cdr nexp))))) 
        (cond 
          ((eq? (car (cdr nexp)) (quote +)) (+ (car nexp) (car (cdr (cdr nexp)))))
          ((eq? (car (cdr nexp)) (quote *)) (* (car nexp) (car (cdr (cdr nexp)))))
          (else (pow (car nexp) (car (cdr (cdr nexp)))))
        )   
      )
      ((atom? (car nexp)) 
        (cond 
          ((eq? (car (cdr nexp)) (quote +)) (+ (car nexp) (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote *)) (* (car nexp) (value (cdr nexp))))
          (else (pow (car nexp) (value (cdr nexp))))
        ) 
      )
      ((atom? (car (cdr (cdr nexp)))) 
        (cond 
          ((eq? (car (cdr nexp)) (quote +)) (+ (value (car nexp)) (car (cdr (cdr nexp)))))
          ((eq? (car (cdr nexp)) (quote *)) (* (value (car nexp)) (car (cdr (cdr nexp)))))
          (else (pow (value (car nexp)) (car (cdr (cdr nexp)))))
        ) 
      )
      (else
        (cond 
          ((eq? (car (cdr nexp)) (quote +)) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
          ((eq? (car (cdr nexp)) (quote *)) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
          (else (pow (value (car nexp)) (value (car (cdr (cdr nexp))))))
        ) 
    )
  )
))


(define ssum
  (lambda (n)
    (cond 
      ((null? n) 0)
      (else (add1 (ssum (cdr n))))
    )
  )
)


(define sero? (lambda (n) (null? n)))


(define sadd1
  (lambda (n)
    (cons (quote ()) n)
  )
)


(define ssub1
  (lambda (n)
    (cdr n)
  )
)

(define my_add
  (lambda (n m) 
    (cond 
      ((sero? m) n)
      (else (sadd1 (my_add n (ssub1 m))))
    
    )
  )
)

; Chapter 7

(define is_member?
  (lambda (element lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) element) (is_member? element (cdr lat))) )
    )
  )
)


(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else (and (not (is_member? (car lat) (cdr lat))) (set? (cdr lat))))
    )
  )
)

(define makeset
  (lambda (lat)
    (cond 
      ((null? lat) (quote ()))
      ((is_member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat))))
    )
  )
)


; beautiful
(define makeset_rem
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat) (makeset_rem (rember* (car lat) (cdr lat)))))
    )
  )
)

(eqlist? (makeset_rem lat) (makeset lat))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else 
        (cond
          ((and (is_member? (car subset1) set2) (subset? (cdr set1) set2)))
        ))
    )
  )
)

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((is_member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f)
   )
  )
)

(subset? `(5 chicken wings) `(5 ham 2 pi fri chicken duck light wings))



; one liner nice solution
(define eqset? 
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))
  )  
)

; interesting solution
(define eqset1? 
  (lambda (set1 set2)
    (cond
      ((null? set2) #t)
      ((is_member? (car set1) set2) (eqset1? (cdr set1) (rember* (car set1) set2)))
      (else #f)
    )
  ) 
)

(eqset? `(5 chicken wings) `(5 ham 2 pi fri chicken duck light wings))

(define set1 `(5 ham 2 pi fri chicken duck light wings))
(define set2 `(5 ham 2 fri pi duck  light chicken wings))


(eqset? set1 set2)

