;; Please run the code first, (main START) to play 

(require 2htdp/image)
(require 2htdp/universe)
;; =================
;; Constants:

(define BOARD-SIZE 20)   ; The board is 10 cells by 10 cells

(define CELL-PIXELS 14)                         ; cells are square
(define BOARD-WIDTH (* BOARD-SIZE CELL-PIXELS)) ;
(define BOARD-HEIGHT BOARD-WIDTH)               ;

(define EYES (beside (circle (/ CELL-PIXELS 8) "solid" "black")
                     (square (/ CELL-PIXELS 8) "solid" "green")
                     (circle (/ CELL-PIXELS 8) "solid" "black")))
(define HEAD (overlay/offset EYES
                             0 (/ CELL-PIXELS 3.5)
                      (square CELL-PIXELS "solid" "green")))
(define BODY (square CELL-PIXELS "solid" "red"))
(define FOOD (square CELL-PIXELS "solid" "blue"))
(define BOMB (circle (/ CELL-PIXELS 2) "solid" "black"))

(define MTS (square BOARD-WIDTH "solid" (make-color 200 200 200)))

(define MESSAGE (text "GAME OVER" 40 "purple"))

(define SCOREBOARD (overlay/align "right" "top" 
                                  (text "SCORE" 25 "black")
                                  (rectangle BOARD-SIZE 60 "outline" "white")))

(define SPEED 0.3)
;; =================
;; Data Definitions:

;; Direction is one of: 
;;  - "U"
;;  - "D"
;;  - "L"
;;  - "R"
;; interp. the four directions a snake could travel

(define D1 "U")
(define D2 "D")
(define D3 "L")
(define D4 "R")

#;
(define (fn-for-direction d)
  (cond [(string=? "U" d) (...)]
        [(string=? "D" d) (...)]
        [(string=? "L" d) (...)]
        [(string=? "R" d) (...)]))

;; -----------------------------------------------------------------------------
(define-struct cell (c r))   ; c and r stand for column and row
;; Cell is  (make-cell Integer[-1, BOARD-SIZE] Integer[-1, BOARD-SIZE])
;; interp. a cell position on the board from top-left corner
;;         -1 and BOARD-SIZE are on the edges of the board and indicate
;;         "going out of bounds"/game-over condition

(define C1 (make-cell -1 200))
(define C2 (make-cell 15 BOARD-SIZE))
(define C3 (make-cell BOARD-SIZE -1))

#;
(define (fn-for-cell cl)
  (... (cell-c cl)
       (cell-r cl)))

;; -----------------------------------------------------------------------------
;; Body is one of:
;;  - (cons Cell empty)
;;  - (cons Cell Body)
;; interp. the body of a snake 

(define B1 (cons (make-cell -1 200) empty))
(define B2 (cons (make-cell BOARD-SIZE 0) (cons (make-cell BOARD-SIZE 1) empty))) 
(define B3 (cons (make-cell 14 20) (cons (make-cell 13 20) (cons (make-cell 12 20) empty))))
(define B4 (cons (make-cell 25 3) (cons (make-cell 26 3) (cons (make-cell 27 3) (cons (make-cell 28 3) empty)))))

#;
(define (fn-for-body b)
  (cond [(empty? (rest b)) (... (fn-for-cell (first b)))]
        [else
         (... (fn-for-cell (first b)) 
              (fn-for-body (rest b)))]))

;; -----------------------------------------------------------------------------
(define-struct snake (dir head body)) 
;; Snake is (make-snake Direction Cell Body)
;; interp. a snake with a head and body moving in some direction

(define S1 (make-snake "U" (make-cell -1 200) (cons (make-cell -1 199) empty)))
(define S2 (make-snake "R" (make-cell 15 BOARD-SIZE) 
                       (cons (make-cell 14 BOARD-SIZE) 
                             (cons (make-cell 13 BOARD-SIZE) 
                                   (cons (make-cell 12 BOARD-SIZE) empty)))))
(define S3 (make-snake "D" (make-cell BOARD-SIZE -1) (cons (make-cell BOARD-SIZE 0)(cons (make-cell BOARD-SIZE 1) empty))))

#;
(define (fn-for-snake s)
  (... (fn-for-direction (snake-dir s))
       (fn-for-cell (snake-head s))
       (fn-for-body (snake-body s)))) 

;; -----------------------------------------------------------------------------
;; Food is Cell 
;; interp. a food blob in a cell on the board 

(define F1 (make-cell 50 25))
(define F2 (make-cell 0 BOARD-SIZE))
(define F3 (make-cell BOARD-SIZE 5))

#;
(define (fn-for-food f)
  (... (fn-for-cell f)))

;; -----------------------------------------------------------------------------
(define-struct bomb (dir cell))
;; Bomb is (make-bomb Direction Cell)
;; interp. a bomb with a direction and a starting cell 

(define BMB0 (make-bomb "L" (make-cell BOARD-SIZE 5))) ; bomb at right of board
(define BMB1 (make-bomb "D" (make-cell 6 -1))) ;bomb at top at board

#;
(define (fn-for-bomb bmb)
  (... (fn-for-direction (bomb-dir bmb))
       (fn-for-cell (bomb-cell bmb))))

;; -----------------------------------------------------------------------------
(define-struct game (food snake score bomb))
;; Game is (make-game Food Snake Score Bomb) 
;; interp. the game state with the snake

(define G1 (make-game F1 S1 0 BMB0))
(define G2 (make-game F2 S2 3 BMB1))
(define G3 (make-game F3 S3 5 BMB0))

#;
(define (fn-for-game g)
  (... (fn-for-cell (game-food g)) 
       (fn-for-snake (game-snake g))
       (fn-for-score (game-score g))
       (fn-for-bomb (game-bomb g))))

(define START (make-game (make-cell 5 10) 
                         (make-snake "R" (make-cell 0 5) 
                                     (list (make-cell 0 6)
                                           (make-cell 0 7)
                                           (make-cell 0 8))) 
                         0 
                         (make-bomb "U" (make-cell 2 (- BOARD-SIZE 1)))))

;; =================
;; Functions:

;; Game -> Game 
;; start the world with (main START)

(define (main g)
  (big-bang g                         ; WS
            (on-tick   tock SPEED)    ; WS -> WS
            (to-draw   render)        ; WS -> Image
            (on-key    handle-key)    ; WS KeyEvent -> WS
            (stop-when last-world? last-picture))) ; WS -> Boolean 

;; -----------------------------------------------------------------------------
;; Game -> Game
; produce the next game 

(define (tock g)
  (make-game 
   (if (same-cell-head? (game-food g) (game-snake g))
       (random-food (game-food g))
       (game-food g))
   (cond [(same-cell-head? (game-food g) (game-snake g)) 
          (tock-snake-add-body (game-snake g))];add a body cell when snake eats food
         [(same-cell-body? (game-bomb g) (game-snake g))
          (injure-snake (game-bomb g) (game-snake g))]
         [else (tock-snake (game-snake g))])
   (if (same-cell-head? (game-food g) (game-snake g))
       (+ 1 (game-score g))
       (game-score g))
   (if (outside-board--bomb? (game-bomb g))
       (random-bomb (game-snake g))
       (tock-bomb (game-bomb g))))) 

;; -----------------------------------------------------------------------------
;; Cell Snake -> Boolean 
;; produce true if Cell is in the same cell as the head of the snake 

(define (same-cell-head? cl s)
  (and (= (cell-c cl) (cell-c (snake-head s))) 
       (= (cell-r cl) (cell-r (snake-head s)))))

;; -----------------------------------------------------------------------------
;; Cell -> Cell
;; Given current location of snake's head produce a different random location for food
(define (random-food cl)
  (random-food-check cl (make-cell (random BOARD-SIZE) (random BOARD-SIZE))))

;; -----------------------------------------------------------------------------
;; Cell Cell -> Cell
;; generative recursion
;; if cl is different from candidate produce candidate; otherwise try a new candidate
(define (random-food-check cl candidate)
  (if (equal? cl candidate) 
      (random-food cl) 
      candidate))   

;; -----------------------------------------------------------------------------
;; Snake -> Snake 
;; given a snake, produce the next snake 

(define (tock-snake s) 
  (make-snake (snake-dir s)
              (next-cell (snake-dir s) (snake-head s))
              (advance-body (snake-head s) (snake-body s)))) 

(define (tock-snake-add-body s)
  (make-snake (snake-dir s)
              (next-cell (snake-dir s) (snake-head s))
              (cons (snake-head s) (snake-body s)))) ;; add body by not removing the 
;; former position of the snake head

;; -----------------------------------------------------------------------------
;; Direction Cell -> Cell 
;; given a direction and a cell, produce the next cell 

(define (next-cell d cl)
  (cond [(string=? "U" d) (if (= -1 (cell-r cl))
                              cl
                              (make-cell (cell-c cl) (- (cell-r cl) 1)))]
        [(string=? "D" d) (if (= BOARD-SIZE (cell-r cl))
                              cl
                              (make-cell (cell-c cl) (+ (cell-r cl) 1)))]
        [(string=? "L" d) (if (= -1 (cell-c cl))
                              cl
                              (make-cell (- (cell-c cl) 1) (cell-r cl)))]
        [(string=? "R" d) (if (= BOARD-SIZE (cell-c cl))
                              cl
                              (make-cell (+ (cell-c cl) 1) (cell-r cl)))]))

;; -----------------------------------------------------------------------------
;; Body -> Body 
;; produce the body without the last item 

(define (remove-last b)
  (cond [(empty? (rest b)) empty]
        [else
         (cons (first b)
               (remove-last (rest b)))]))

;; -----------------------------------------------------------------------------
;; Cell Body -> Body
;; given a snake and a body, produce the new body 

(define (advance-body cl b)
  (cons cl (remove-last b)))

;; -----------------------------------------------------------------------------
;; Bomb -> Boolean
;; produce true if the bomb is outside the board 

(define (outside-board--bomb? bmb)
  (outside-board--cell? (bomb-cell bmb)))

;; -----------------------------------------------------------------------------
;; Cell -> Boolean 
;; produce true if the cell is outside the game board

(define (outside-board--cell? cl)
  (or (= -1 (cell-c cl))
      (= BOARD-SIZE (cell-c cl))
      (= -1 (cell-r cl))
      (= BOARD-SIZE (cell-r cl))))

;; -----------------------------------------------------------------------------
;; Snake -> Bomb
;; produce a random bomb that starts at the edge of the board 
;(define (random-bomb s)
;  (make-bomb "U" (make-cell 3 BOARD-SIZE)))
(define (random-bomb s)
  (local [(define random-number (random 4))]
    (list-ref (list (make-bomb "L" (make-cell (- BOARD-SIZE 1) (random (- BOARD-SIZE 1))))
                    (make-bomb "R" (make-cell 0 (random (- BOARD-SIZE 1))))
                    (make-bomb "U" (make-cell (random (- BOARD-SIZE 1)) (- BOARD-SIZE 1)))
                    (make-bomb "D" (make-cell (random (- BOARD-SIZE 1)) 0)))
              random-number))) 

;; -----------------------------------------------------------------------------
;; Bomb -> Bomb
;; produce the next bomb

(define (tock-bomb bmb)
  (make-bomb (bomb-dir bmb)
             (next-cell-bomb (bomb-dir bmb) (bomb-cell bmb))))

;; -----------------------------------------------------------------------------
;; Direction Cell -> Cell
;; produce the next bomb cell 

(define (next-cell-bomb d cl)
  (cond [(string=? "U" d) ;(if (= -1 (cell-r cl))
         ;(bomb-cell (random-bomb cl))
         (make-cell (cell-c cl) (- (cell-r cl) 1))]
        [(string=? "D" d) ;(if (= BOARD-SIZE (cell-r cl))
         ;(bomb-cell (random-bomb cl))
         (make-cell (cell-c cl) (+ (cell-r cl) 1))]
        [(string=? "L" d) ;(if (= -1 (cell-c cl))
         ;(bomb-cell (random-bomb cl))
         (make-cell (- (cell-c cl) 1) (cell-r cl))]
        [(string=? "R" d) ;(if (= BOARD-SIZE (cell-c cl))
         ;(bomb-cell (random-bomb cl))
         (make-cell (+ (cell-c cl) 1) (cell-r cl))]))

;; -----------------------------------------------------------------------------
;; Game -> Image
;; render the head of the snake onto the a cell position on the board 

(define (render g)
  (above/align "right" 
               (render-score (game-score g))
               (rectangle 10 10 "solid" "white")
               (render-snake (game-snake g) (render-bomb (game-bomb g) (render-food (game-food g)))))) 

;; -----------------------------------------------------------------------------
;; Bomb Snake -> Boolean
;; produce true if the bomb hits any cell in the body of the snake 

(define (same-cell-body? bmb s)
  (same-cell-body-helper? (bomb-cell bmb) (snake-body s)))

;; -----------------------------------------------------------------------------
;; Cell Body -> Boolean
;; produce true if given cell matches any of the body cells

(define (same-cell-body-helper? cl b)
  (cond [(empty? b) false]
        [else
         (or (equal? cl (first b)) 
             (same-cell-body-helper? cl (rest b)))]))

;; -----------------------------------------------------------------------------
;; Bomb Snake -> Snake
;; produce the new snake without all body cells after the one hit by bomb 

(define (injure-snake bmb s)
  (make-snake (snake-dir s) (snake-head s) (injure-snake-body-bmb bmb (snake-body s))))

;; -----------------------------------------------------------------------------
;; Bomb Body -> Body
;; produce the new body without all body cells after the one hit by bomb

(define (injure-snake-body-bmb bmb b)
  ;; rsf: (listof Cell) ; list of cells that were before the one hit by bomb
  (local [(define (injure-snake-body-bmb-rsf bmb b rsf)
            (cond [(empty? b) rsf] 
                  [else
                   (if (equal? (bomb-cell bmb) (first b))
                       rsf
                       (injure-snake-body-bmb-rsf bmb (rest b) 
                                                  (append rsf (list (first b)))))]))]
    (injure-snake-body-bmb-rsf bmb b empty)))  


;; -----------------------------------------------------------------------------
;; Snake -> Image 
;; Given a snake, render it onto the board at x,y 

(define (render-snake s img) 
  (place-in-cell (rotate-head (snake-dir s) HEAD) 
                 (snake-head s) 
                 (render-body (snake-body s) img)))    

;; -----------------------------------------------------------------------------
;; Direction Image -> Image 
;; rotate the image given based on the direction 

(define (rotate-head d img)
  (cond [(string=? "U" d) img]
        [(string=? "D" d) (rotate 180 img)]
        [(string=? "L" d) (rotate 90 img)]
        [(string=? "R" d) (rotate 270 img)]))

;; -----------------------------------------------------------------------------
;; Body -> Image
;; render the body onto the board at x,y

(define (render-body b img)
  (cond [(empty? b) empty-image]
        [(empty? (rest b)) (place-in-cell BODY (first b) img)]
        [else
         (place-in-cell BODY (first b) 
                        (render-body (rest b) img))]))

;; -----------------------------------------------------------------------------
;; Food -> Image 
;; render image of food onto image of snake on board 

(define (render-food f)
  (place-in-cell FOOD f MTS)) 

;; -----------------------------------------------------------------------------
;; Natural - Image 
;; render the score onto a scoreboard 

(define (render-score n)
  (overlay/align "right" "bottom" 
                 (text (number->string n) 30 "black")
                 SCOREBOARD))

;; -----------------------------------------------------------------------------
;; Bomb Image -> Image 
;; render image of bomb onto given image 

(define (render-bomb bmb img)
  (place-in-cell BOMB (bomb-cell bmb) img)) 


;; -----------------------------------------------------------------------------
;; Image Cell Image -> Image
;; place img in cell c given the board scene scn

(define (place-in-cell img c scn)
  (place-image img 
               (+ (* (cell-c c) CELL-PIXELS) (/ CELL-PIXELS 2))
               (+ (* (cell-r c) CELL-PIXELS) (/ CELL-PIXELS 2))
               scn))

;; -----------------------------------------------------------------------------
;; Game KeyEvent -> Game 
;; given a game and a keyevent, produce the new game 

(define (handle-key g ke)
  (cond [(key=? ke "up") (make-game (game-food g) (turn (game-snake g) "U") (game-score g) (game-bomb g))]
        [(key=? ke "down") (make-game (game-food g) (turn (game-snake g) "D") (game-score g) (game-bomb g))]
        [(key=? ke "left") (make-game (game-food g) (turn (game-snake g) "L") (game-score g) (game-bomb g))]
        [(key=? ke "right") (make-game (game-food g) (turn (game-snake g) "R") (game-score g) (game-bomb g))]
        [else g]))

;; -----------------------------------------------------------------------------
;; Snake Direction -> Snake 
;; given a snake and a keyevent, produce the new snake 

(define (turn s d)
  (make-snake d (snake-head s) (snake-body s))) 

;; -----------------------------------------------------------------------------
;; Game -> Boolean
;; stop game when head of the snake touches one of the walls, and display message

(define (last-world? g)
  (last-snake? (game-snake g) (game-bomb g)))

;; -----------------------------------------------------------------------------
;; Snake Bomb -> Boolean  
;; produce true if head of snake is touching the wall, or if the bomb 
;;     hit the snake's head, false otherwise 

(define (last-snake? s bmb)
  (or (last-cell? (snake-head s))
      (equal? (snake-head s) (bomb-cell bmb))
      (empty? (snake-body s))
      (hit-itself? (snake-head s) (snake-body s)))) 

;; -----------------------------------------------------------------------------
;; Cell Body -> Boolean
;; produce true if the snake ran into itself

(define (hit-itself? cl b)
  (same-cell-body-helper? cl b))

;; -----------------------------------------------------------------------------
;; Cell -> Boolean
;; produce true if the cell is on the wall of the box, false otherwise

(define (last-cell? cl)
  (or (= -1 (cell-c cl))
      (= BOARD-SIZE (cell-c cl))
      (= -1 (cell-r cl))
      (= BOARD-SIZE (cell-r cl))))

;; -----------------------------------------------------------------------------
;; Game -> Image
;; produce the message if the head is touching a wall of the box 

(define (last-picture g)
  (if (last-snake? (game-snake g) (game-bomb g))
      (overlay MESSAGE
               (render g))
      (render g))) 
;; -----------------------------------------------------------------------------
