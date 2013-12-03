#lang racket


(require racket/gui/base)

(define (speel audiofile)
 (play-sound audiofile #t))


(define fragmenten '("1.wav" "2.wav" "3.wav" "4.wav" "5.wav"))

(define nummers '(1 2 3 4 5))

(define (speelnummer n) (speel (list-ref fragmenten (- n 1))))

(define (schuif-naar-links x) (append (rest x) (list (first x))))

(define (schuif x n) 
  
  (if (equal? n 0)
      x
      (schuif (schuif-naar-links x) (- n 1))
  
  )
)


(define (permutatie l x) 
     
  (if (empty? l)
     
      x

      (let 
          ((r (random (length l))))

            (permutatie (rest (schuif l r)) (cons (list-ref l r) x) )
       )   
   )
  
)

(define (raadvolgorde l score)

   (if (empty? l)
       
       (begin (display "SCORE: ") (display (number->string score)) (display "/5"))
       
       (begin
         (speelnummer (first l))
         (if (= (string->number (read-line)) (first l)) 
             
             
             (begin (displayln "GOED") (raadvolgorde (rest l) (+ score 1)) )
             (begin (displayln "FOUT") (raadvolgorde (rest l) score) )
             
         )
   
       )
   )

)


(raadvolgorde (permutatie nummers '()) 0)