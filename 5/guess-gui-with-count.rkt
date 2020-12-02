#lang racket
(require 2htdp/image 2htdp/universe)

(define TEXT-SIZE 11)
(define HELP-TEXT (text "↑ for larger numbers, ↓ for smaller ones" 
                        TEXT-SIZE 
                        "blue"))
(define HELP-TEXT2 (text "Press = when your number is guessed; q to quit." 
                         TEXT-SIZE 
                         "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define SIZE 72)
(define COLOR "red")
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define MT-SC
  (place-image/align HELP-TEXT
                     TEXT-X
                     TEXT-UPPER-Y
                     "left"
                     "top"
                     (place-image/align HELP-TEXT2
                                        TEXT-X
                                        TEXT-LOWER-Y
                                        "left"
                                        "bottom"
                                        (empty-scene WIDTH HEIGHT))))
(define GUESS-COUNT-SIZE 10)
(define GUESS-COUNT-COLOR "green")

(struct interval (small big) #:transparent)
(struct guess-state (count interval)  #:transparent)

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w))
            2))

(define (bigger intrvl)
  (interval (min (add1 (guess intrvl))
                 (interval-big intrvl))
            (interval-big intrvl)))

(define (smaller intrvl)
  (interval (interval-small intrvl)
            (max (interval-small intrvl)
                 (sub1 (guess intrvl)))))

(define (guess-and-count f-guess w)
  (local ((define intrvl (guess-state-interval w)))
    (guess-state (+ 1 (guess-state-count w))
                 (f-guess intrvl))))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (guess-and-count bigger w)]
        [(key=? key "down") (guess-and-count smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (render-guess-count w)
  (text (string-append "#Guesses: " (number->string (guess-state-count w))) GUESS-COUNT-SIZE GUESS-COUNT-COLOR))

(define (render w)
  (above (render-guess-count w)
         (overlay (above (text (number->string (guess (guess-state-interval w))) SIZE COLOR)
                         (render-guess-count w))
           MT-SC)))

(define (render-last-scene w)
  (overlay (above (text "End" SIZE COLOR)
                  (render-guess-count w))
           MT-SC))

(define (single? w)
  (local ((define intrvl (guess-state-interval w)))
    (= (interval-small intrvl) (interval-big intrvl))))

(define (start lower upper)
  (big-bang (guess-state 1 (interval lower upper))
    (on-key deal-with-guess)
    (to-draw render)
    (stop-when single? render-last-scene)))