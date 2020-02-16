(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders
;; By: Salar Salahshoor
;; Designed for DrRacket BSL (Beginning Student with List Abbreviations)

;; Constants:

(define WIDTH  500)
(define HEIGHT 750)

(define UFO [ADD IMAGE HERE)
(define LUFO (rotate 20 UFO))
(define RUFO (rotate 340 UFO))
(define TANK [ADD IMAGE HERE])
(define TANK-Y (- HEIGHT 26))
(define MISSILE (ellipse 5 15 "solid" "red"))

(define UFO-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define UFO-Y-SPEED 2)
  
(define TANK-SPEED 10)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 10)

(define BACKGROUND (empty-scene WIDTH HEIGHT))


;; Data Definitions:

(define-struct game (ufos missiles tank))
;; Game is (make-game  (ListOfUFOS) (ListOfMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loufo (game-ufos s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, TANK-Y in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct ufo (x y dx))
;; UFO is (make-ufo Number Number Number)
;; interp. the UFO is at (x, y) in screen coordinates
;;         the UFO along x by dx pixels per clock tick

(define U1 (make-ufo 150 100 UFO-X-SPEED))              ;not landed, moving right
(define U2 (make-ufo 150 HEIGHT (- UFO-X-SPEED)))       ;exactly landed, moving left
(define U3 (make-ufo 150 (+ HEIGHT 10) (- UFO-X-SPEED))) ;> landed, moving left

#;
(define (fn-for-ufo ufo)
  (... (ufo-x ufo) (ufo-y ufo) (ufo-dx ufo)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (ufo-x U1) (+ (ufo-y U1) 10)))  ;exactly hit U1
(define M3 (make-missile (ufo-x U1) (+ (ufo-y U1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list U1) (list M1) T1))
(define G3 (make-game (list U1 U2) (list M1 M2) T1))


;; Functions:

;; Game -> Game
;; start the world with (main empty)
;; 
(define (main game)
  (big-bang game                        ; Game
            (on-tick   run-game)        ; Game -> Game
            (to-draw   render-game)     ; Game -> Image
            (on-key    play-game)       ; Game KeyEvent -> Game
            (stop-when end-game?)))     ; Game -> Boolean


;; Game -> Game
;; Randomly spawn new UFO, move UFO's down the screen in a z pattern, and shoot missles upward from the tank in a linear trajectory 

(define (run-game g)
  (spawn-ufos (advance-objects (remove-off-screen-mis (remove-collisions g)))))


;; Game -> Game
;; Remove UFOs and Missiles that collide in the game

(define (remove-collisions g)
  (make-game
   (filter-ufos (game-ufos g) (game-missiles g))
   (filter-missiles (game-missiles g) (game-ufos g))
   (game-tank g)))


;; ListOfUFOS ListOfMissiles -> ListOfUFOS
;; Given a ListOfUFOS and ListOfMissiles, check if there is a hit and return a list of remaining UFOs that were not hit by a Missiles
(check-expect (filter-ufos (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 20 20 UFO-X-SPEED)) (list (make-missile 10 10) (make-missile 40 40))) empty)
(check-expect (filter-ufos (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 200 20 UFO-X-SPEED)) (list (make-missile 40 40) (make-missile 10 10))) (list (make-ufo 200 20 UFO-X-SPEED)))
(check-expect (filter-ufos (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 20 20 UFO-X-SPEED)) (list (make-missile 20 20) (make-missile 10 10))) empty)

(define (filter-ufos lofufo lofm)
  (cond [(empty? lofufo) empty]
        [else
         (cond [(find-ufo-strike? (first lofufo) lofm) (filter-ufos (rest lofufo) lofm)]
               [else
                (cons (first lofufo) (filter-ufos (rest lofufo) lofm))])]))


;; UFO ListOfMissiles -> Boolean
;; Given a UFO and ListOfMissiles, check the list of missiles to see if the UFO was hit, if hit return true, if not return false.
(check-expect (find-ufo-strike? (make-ufo 10 10 UFO-X-SPEED) (list (make-missile 10 10) (make-missile 30 30))) true)
(check-expect (find-ufo-strike? (make-ufo 10 10 UFO-X-SPEED) (list (make-missile 30 10) (make-missile 10 10))) true)
(check-expect (find-ufo-strike? (make-ufo 10 10 UFO-X-SPEED) (list (make-missile 30 10) (make-missile 40 10))) false)

(define (find-ufo-strike? ufo lofm)
  (cond [(empty? lofm) false]
        [else
         (if (and (<= (abs (- (ufo-x ufo) (missile-x (first lofm)))) HIT-RANGE)
                  (<= (abs (- (missile-y (first lofm)) (ufo-y ufo))) HIT-RANGE))
                  true
                  (find-ufo-strike? ufo (rest lofm)))]))


;; ListOfMissiles ListOfUFOS -> ListOfMissiles
;; Given a ListOfMissiles, return a new ListOfMissiles that have not struck a UFO
(check-expect (filter-missiles (list (make-missile 10 10) (make-missile 40 40)) (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 20 20 UFO-X-SPEED))) (list (make-missile 40 40)))
(check-expect (filter-missiles (list (make-missile 40 40) (make-missile 10 10)) (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 20 20 UFO-X-SPEED))) (list (make-missile 40 40)))
(check-expect (filter-missiles (list (make-missile 20 20) (make-missile 10 10)) (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 20 20 UFO-X-SPEED))) empty)
(check-expect (filter-missiles (list (make-missile 40 40) (make-missile 10 10) (make-missile 10 0)) (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 20 20 UFO-X-SPEED))) (list (make-missile 40 40)))

(define (filter-missiles lofm lofufo)
  (cond [(empty? lofm) empty]
        [else 
         (cond [(find-mis-strike? (first lofm) lofufo) (filter-missiles (rest lofm) lofufo)]
               [else
                (cons (first lofm) (filter-missiles (rest lofm) lofufo))])]))


;; MISSILE ListOfUFOS -> Boolean
;; Given a MISSILE and ListOfUFOs, check the list of UFO to see if the MISSILE was hit, if hit return true, if not return false.
(check-expect (find-mis-strike? (make-missile 10 10) (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 30 30 UFO-X-SPEED))) true)
(check-expect (find-mis-strike? (make-missile 30 15) (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 30 10 UFO-X-SPEED))) true)
(check-expect (find-mis-strike? (make-missile 40 10) (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 30 30 UFO-X-SPEED))) false)

(define (find-mis-strike? mis lofufo)
  (cond [(empty? lofufo) false]
        [else
         (if (and (<= (abs (- (missile-x mis) (ufo-x (first lofufo)))) HIT-RANGE)
                  (<= (abs (- (ufo-y (first lofufo)) (missile-y mis))) HIT-RANGE))
                  true
                  (find-mis-strike? mis (rest lofufo)))]))


;; Game -> Game
;; Given a ListOfMissiles, return a list of MISSILES that have not left the screen
(check-expect (remove-missiles (list (make-missile 10 10) (make-missile 20 0))) (list (make-missile 10 10)))
(check-expect (remove-missiles (list (make-missile 10 -5) (make-missile 20 0))) empty)
(check-expect (remove-missiles (list (make-missile 10 10) (make-missile 20 20))) (list (make-missile 10 10) (make-missile 20 20)))

(define (remove-off-screen-mis g)
  (make-game (game-ufos g) (remove-missiles (game-missiles g)) (game-tank g)))

(define (remove-missiles lofm)
  (cond [(empty? lofm) empty]
        [else
         (cond [(<= (missile-y (first lofm)) 0) (remove-missiles (rest lofm))]
               [else
                (cons (first lofm) (remove-missiles (rest lofm)))])]))

;; Game -> Game
;; Move ListOfUFOS downward and ListOfMissiles upward

(define (advance-objects g)
  (make-game (advance-ufos (game-ufos g))
             (advance-missiles (game-missiles g))
             (game-tank g)))


;; ListOfUFOS -> ListOfUFOS
;; Given a ListOFUFOs, adjust the x y position of each UFO in the list according to the game rules
(check-expect (advance-ufos (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 20 20 UFO-X-SPEED)))
              (list (make-ufo (+ 10 UFO-X-SPEED) (+ 10 UFO-Y-SPEED) UFO-X-SPEED) (make-ufo (+ 20 UFO-X-SPEED) (+ 20 UFO-Y-SPEED) UFO-X-SPEED)))
(check-expect (advance-ufos (list (make-ufo (- WIDTH 10) 50 10))) (list (make-ufo WIDTH (+ UFO-Y-SPEED 50) -10))) ; UFO reaches edge
(check-expect (advance-ufos (list (make-ufo 10 50 -10))) (list (make-ufo 0 (+ UFO-Y-SPEED 50) 10)))
(check-expect (advance-ufos (list (make-ufo (- WIDTH 5) 50 10))) (list (make-ufo WIDTH (+ UFO-Y-SPEED 50) -10))) ; UFO goes beyond edge
(check-expect (advance-ufos (list (make-ufo 5 50 -10))) (list (make-ufo 0 (+ UFO-Y-SPEED 50) 10)))

(define (advance-ufos lofufo)
  (cond [(empty? lofufo) empty]
        [else
         (cons (move-ufo (first lofufo))
               (advance-ufos (rest lofufo)))]))

;; UFO -> UFO
;; Took template from UFO

(define (move-ufo ufo)
    (cond [(>= (+ (ufo-x ufo) (ufo-dx ufo)) WIDTH) (make-ufo WIDTH (+ (ufo-y ufo) UFO-Y-SPEED) (- (ufo-dx ufo)))]
          [(<= (+ (ufo-x ufo) (ufo-dx ufo)) 0)     (make-ufo 0     (+ (ufo-y ufo) UFO-Y-SPEED) (- (ufo-dx ufo)))]
          [else
           (make-ufo (+ (ufo-x ufo) (ufo-dx ufo))
                     (+ (ufo-y ufo) UFO-Y-SPEED)
                     (ufo-dx ufo))]))


;; ListOfMissiles -> ListOfMissiles
;; Given a ListOFMissiles, adjust the x y position of each MISSILE in the list according to the game rules
(check-expect (advance-missiles (list (make-missile 10 10) (make-missile 10 100) (make-missile 10 HEIGHT))) (list (make-missile 10 0) (make-missile 10 90) (make-missile 10 740)))

(define (advance-missiles lofm)
  (cond [(empty? lofm) empty]
        [else
         (cons (move-missile (first lofm))
               (advance-missiles (rest lofm)))]))


;; Missile -> Missile
;; Took template from UFO

(define (move-missile mis)
    (make-missile (missile-x mis)
                  (- (missile-y mis) MISSILE-SPEED)))


;; ListOfUFOS -> ListOfUFOS
;; Given a ListOfUFOs, add new UFOs to the list at a random position x at a rate of INVADE-RATE

(define (spawn-ufos g)
  (cond [(< (random 500) INVADE-RATE) (make-game (cons (make-ufo (random WIDTH) 0 (random-direction UFO-X-SPEED)) (game-ufos g)) (game-missiles g) (game-tank g))]
        [else g]))


;; Integer -> Integer
;; Given an integer, return the negative or positive value of the integer randomly

(define (random-direction i)
  (if (= (modulo (random 10) 2) 1)
      (- i)
      i))


;; Game -> Image
;; Render the an image of the current state of the Space Invaders game
(check-expect (render-game (make-game (list (make-ufo 10 10 UFO-X-SPEED) (make-ufo 200 200 UFO-X-SPEED)) (list (make-missile 10 20) (make-missile 300 300)) T0))
              (place-image RUFO 200 200 (place-image RUFO 10 10 (place-image MISSILE 300 300 (place-image MISSILE 10 20 (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))))))

(define (render-game g)
  (render-ufos (game-ufos g)
               (render-missiles (game-missiles g)
                                (render-tank (game-tank g)))))

;; Tank -> Image
;; Render the an image of the current state of the Space Invaders game

(define (render-tank tank)
  (place-image TANK (tank-x tank) TANK-Y BACKGROUND))


;; ListOfMissiles -> Image
;; Render the an image of ListOFMissiles

(define (render-missiles lofm img)
  (cond [(empty? lofm) img]
        [else
         (place-image MISSILE
                      (missile-x (first lofm))
                      (missile-y (first lofm))
                      (render-missiles (rest lofm) img))]))


;; ListOfUFOS -> Image
;; Render the an image of ListOFUFOS

(define (render-ufos lofufo img)
  (cond [(empty? lofufo) img]
        [else
         (place-image (if (positive? (ufo-dx (first lofufo)))
                          RUFO
                          LUFO)
                      (ufo-x (first lofufo))
                      (ufo-y (first lofufo))
                      (render-ufos (rest lofufo) img))]))


;; Game -> Game
;; Right and Left arrow keys result in Tank x position adjusting right or left on the screen
;; Space Bar results in new missile being fired from the current position of the tank

;(define (play-game game) game) ;stub

(define (play-game g ke)
  (cond
    [(key=? ke "left")  (make-game (game-ufos g) (game-missiles g) (turn-left (game-tank g)))]
    [(key=? ke "right") (make-game (game-ufos g) (game-missiles g) (turn-right (game-tank g)))]
    [(key=? ke " ")     (make-game (game-ufos g) (fire-missile (game-missiles g) (game-tank g)) (game-tank g))]
    [else g]))


;; Tank -> Tank
;; Move the tank direction left by TANK-SPEED

(define (turn-left tank)
  (make-tank (- (tank-x tank) TANK-SPEED) -1))


;; Tank -> Tank
;; Move the tank direction right by TANK-SPEED

(define (turn-right tank)
  (make-tank (+ (tank-x tank) TANK-SPEED) 1))


;; ListOfMissiles -> ListOfMissiles
;; Given ListOfMissiles add a new MISSILE at x position of tank-x position and Y position of (- HEIGHT 50)
(check-expect (fire-missile (list (make-missile 10 10) (make-missile 20 20)) T0) (list (make-missile 250 700) (make-missile  10 10) (make-missile 20 20)))

(define (fire-missile lofm tank)
  (cond [(empty? lofm) (cons (make-missile (tank-x tank) (- TANK-Y 24)) empty)]
        [else
         (cons (make-missile (tank-x tank) (- TANK-Y 24)) lofm)]))


;; Game -> Boolean
;; Stop the game when the first UFO reaches y coordinate of 0
(check-expect (end-game? (make-game (list (make-ufo 10 HEIGHT UFO-X-SPEED)) empty T0)) true)
(check-expect (end-game? (make-game (list (make-ufo 10 700 UFO-X-SPEED) (make-ufo 10 HEIGHT UFO-X-SPEED)) empty T0)) true)
(check-expect (end-game? (make-game (list (make-ufo 10 700 UFO-X-SPEED) (make-ufo 10 20 UFO-X-SPEED)) empty T0)) false)

(define (end-game? g)
  (ufo-breach? (game-ufos g)))


(define (ufo-breach? lofufo)
  (if (<= (length lofufo) 0)
      false
      (if (ufo-strike? (first lofufo))
          true
          (ufo-breach? (rest lofufo)))))


(define (ufo-strike? ufo)
  (>= (ufo-y ufo) HEIGHT))
