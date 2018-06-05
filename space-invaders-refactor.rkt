;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname space-invaders-refactor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

;; A List<X> is one of  
;; - empty  
;; - (cons X List<X>)

;;An Invader is a Posn
;;An IB is Posn
;;INTERP: represents the (x,y) coordinate of an invader bullet
;;A SB is Posn
;;INTERP: represents a spaceship bullet

;;A Position is Posn
;;INTERP: represents the (x,y) location of the spaceship in the game
(define-struct spaceship (sp-direction position))
;;A Spaceship is (make-spaceship SP-Direction Position)
;;INTERP: represents a spaceship with its direction and position

(define UP 'up)
(define DOWN 'down)
(define LEFT 'left)
(define RIGHT 'right)
(define WIDTH 500)
(define HEIGHT 500)
(define BG (empty-scene WIDTH HEIGHT))
 

(define INVADER-SIDE 20)
(define INVADER-IMAGE (square INVADER-SIDE "solid" "red"))
(define INVADER-RIGHT-BOUND 410)
(define SPACE 20)

(define INVADER1 (make-posn 90 20))
(define INVADER2 (make-posn 90 50))
(define INVADER3 (make-posn 90 80))
(define INVADER4 (make-posn 90 110))

(define SPEED 10)

(define SPSHIP-HEIGHT 20)
(define SPSHIP-WIDTH 40)
(define SP-RIGHT-BOUND 480)
(define SP-LEFT-BOUND 20)
(define SPSHIP-IMAGE (rectangle SPSHIP-WIDTH SPSHIP-HEIGHT "solid" "green"))
(define SPSHIP-INIT-DIR RIGHT)
(define SPSHIP-INIT (make-spaceship
                     SPSHIP-INIT-DIR
                     (make-posn(/ WIDTH 2) (- HEIGHT (/ SPSHIP-HEIGHT 2)))))
(define SPSHIP-INIT1 (make-spaceship
                     LEFT
                     (make-posn(/ WIDTH 2) (- HEIGHT (/ SPSHIP-HEIGHT 2)))))
(define BULLET-SIDE 5)
(define IB-IMAGE (square BULLET-SIDE 'solid 'red))
(define SB-IMAGE (square BULLET-SIDE 'solid 'green))

(define SCORE-INIT 0)
(define LIVES-INIT 3)
(define score-x-position 250)
(define score-y-position 10)
(define live-x-position 480)
(define live-y-position 480)

(define-struct world (spaceship loi loib losb time score live))

;;;;Signature
;;new-invader: Invader -> Invader
;;;;Purpose
;; GIVEN: an invader
;; RETURNS: next invader in the same row

(define(new-invader invader)
  (make-posn
   (+ INVADER-SIDE SPACE (posn-x invader))
   (posn-y invader)))

;;;; Signature
;; make-loi: Invader -> LoI
;;;; Purpose
;; GIVEN: first invader in each row
;; RETURNS: a list of LoI

(define(make-loi invader)
  (if
   (<= (posn-x invader) INVADER-RIGHT-BOUND)
   (cons invader (make-loi (new-invader invader)))
   empty))

(define LOI1 (make-loi INVADER1))
(define LOI2 (make-loi INVADER2))
(define LOI3 (make-loi INVADER3))
(define LOI4 (make-loi INVADER4))
(define LOI-INIT (append LOI1 LOI2 LOI3 LOI4))


;;A World is (make-world Spaceship LoI LoIB LoSB Natural Natural Natural)
;; WHERE live is 0 to 3
;; INTERP: represents a world with a spaceship, invader bullets,
;; spaceship bullets, play time, scores and spaceship lives
(define WORLD-INIT (make-world SPSHIP-INIT LOI-INIT empty empty 0 0 3))
(define INVADER-TEST (make-posn 90 20) )
(define LOI-TEST (cons INVADER-TEST empty))
(define SB-TEST (make-posn 250 480) )
(define LOSB-TEST (cons SB-TEST empty))

;;;; Signature:
;; list-draw: List<X> (Image of X) (Image of gackground)
;; -> Image of the list and the background 
(define(list-draw list img-x img-b)
  (cond
    [(empty? list) img-b]
    [else (place-image img-x
                      (posn-x(first list))
                      (posn-y(first list))
                      (list-draw (rest list) img-x img-b))]))
  
;;;;Signature
;;ibullet-draw: LoIB Image -> Image
;;;; Purpose
;; GIVEN: a list of invader bullets and an image
;; RETURNS: a new image with invader bullets drawn on the image

(define(ibullet-draw loib img)
  (list-draw loib IB-IMAGE img))

;;;;Signature
;;sbullet-draw: LoSB Image -> Image
;;;; Purpose
;; GIVEN: a list of spaceship bullets and an image
;; RETURNS: a new image with a list of spaceship bullets drawn on the image
(define(sbullet-draw losb img)
  (list-draw losb SB-IMAGE img))

;;;;Signature
;;loi-draw:LoI -> Image
;;;;Purpose
;;GIVEN: a list of invaders 
;;RETURNS:  the list of invaders drawn on the background

(define(loi-draw loi)
  (list-draw loi INVADER-IMAGE BG))

;;;; Signature
;;;; live-draw: Natural Image -> Image
;;;; Purpose
;; GIVEN: a live and an image 
;; RETURNS: a new image with spaceship's lives number drawn on the given image

(define (live-draw live img)
  (place-image (text (number->string live) 20 "black")
               live-x-position
               live-y-position
               img))
;;;; Signature
;; score-draw: Natural Image -> Image
;;;; Purpose
;; GIVEN: a natural number and an image
;; RETURNS: a new image with the score on the image

(define (score-draw score img)
   (place-image (text (number->string score) 20 "black")
                score-x-position
                score-y-position
                img))


;;;; Signature
;;spaceship-draw: Spaceship Image -> Image
;;;; Purpose
;; GIVEN: a spaceship and an image 
;; RETURNS: new image with each spaceship drawn on the image
(define (spaceship-draw spaceship img)
  (place-image
   SPSHIP-IMAGE
   (posn-x (spaceship-position spaceship))
   (posn-y (spaceship-position spaceship))
   img))

;;;;Signature
;; world-draw: World -> Image
;;;; Purpose
;; GIVEN: a world
;; RETURNS: the image of the world

(define(world-draw w)
  (score-draw(world-score w)
             (live-draw(world-live w)
                       (sbullet-draw (world-losb w)
                                     (ibullet-draw
                                      (world-loib w)
                                      (spaceship-draw
                                       (world-spaceship w)
                                       (loi-draw(world-loi w))))))))

;;;; Tests
(check-expect (world-draw WORLD-INIT)
               (place-image
               (text "0" 20 "black")
               250
               10
               (place-image
                (text "3" 20 "black")
                480
                480
               (place-image SPSHIP-IMAGE
                            (/ WIDTH 2)
                            (- HEIGHT (/ SPSHIP-HEIGHT 2))
                            (loi-draw LOI-INIT)))))

;;;;Signature
;;spaceship-move: Spaceship LoIB -> Spaceship
;;;;Purpose
;;GIVEN: a spaceship and a list of invader bullets
;;RETURNS: a new spaceship with location changed in corresponding direction

(define(spaceship-move s loib)
  (cond
    [(ship-is-hit? s loib) SPSHIP-INIT] 
    [(not-hit-left? s)
       (make-spaceship (spaceship-sp-direction s)
                  (make-posn
                   (- (posn-x (spaceship-position s))
                      SPEED)
                   (posn-y (spaceship-position s))))]
      [(not-hit-right? s)
       (make-spaceship (spaceship-sp-direction s)
                  (make-posn
                   (+ SPEED
                      (posn-x (spaceship-position s)))
                   (posn-y (spaceship-position s))))]
      [else s]))

;;;;Test
(define SP-TEST1 (make-spaceship 'right (make-posn 250 490)))
(check-expect (spaceship-move SP-TEST1 LOIB-TEST)
              (make-spaceship 'right (make-posn 260 490)))
(define SP-TEST2 (make-spaceship 'left (make-posn 250 490)))
(check-expect (spaceship-move SP-TEST2 LOIB-TEST)
              (make-spaceship 'left (make-posn 240 490)))
(define SP-TEST3 (make-spaceship 'right (make-posn 490 490)))
(check-expect (spaceship-move SP-TEST3 LOIB-TEST)
              (make-spaceship 'right (make-posn 490 490)))
(define SP-TEST4 (make-spaceship 'left (make-posn 250 490)))
(define IB-HIT (make-posn 250 485))
(define LOIB-HIT (cons IB-HIT empty))
(check-expect (spaceship-move SP-TEST4 LOIB-HIT)
              SPSHIP-INIT)

;;;;Signature
;;not-hit-left?: Spaceship -> Boolean
;;GIVEN: a spaceship
;;RETURNS true if it hits the left bound, else false

(define (not-hit-left? s)
  (and (symbol=? LEFT (spaceship-sp-direction s))
          (>(posn-x (spaceship-position s)) SP-LEFT-BOUND)))


;;;;Signature
;;not-hit-left?: Spaceship -> Boolean
;;GIVEN: a spaceship
;;RETURNS true if it hits the left bound, else false
(define(not-hit-right? s)
  (and (symbol=? RIGHT (spaceship-sp-direction s))
            (<(posn-x (spaceship-position s)) SP-RIGHT-BOUND)))

;;;;Signature
;;ib-hit?: IB Spaceship -> Boolean
;;;;Purpose
;;GIVEN: an invader bullet and a spaceship
;;RETURNS: true if the invader bullet hit the spaceship, false otherwise

(define(ib-hit? s ib)
  (and (<= (- (posn-x (spaceship-position s)) (/ SPSHIP-WIDTH 2))
                               (posn-x ib)
                               (+ (posn-x (spaceship-position s))
                                  (/ SPSHIP-WIDTH 2)))
       (<= (- (posn-y (spaceship-position s)) (/ SPSHIP-HEIGHT 2))
                               (posn-y ib)
                               (+ (posn-y (spaceship-position s))
                                  (/ SPSHIP-HEIGHT 2)))))

;;;; Signature
;; ship-is-hit?: Spaceship LoIB -> Boolean
;;;; Purpose
;; GIVEN: a spaceship and a list of invador bullets
;; RETURNS: true if any bullet from the list hits the ship, false otherwise


(define (ship-is-hit? s loib)
  (cond
    [(empty? loib) #false]
    [(cons? loib) (if (ib-hit? s (first loib))
                      #true
                      (ship-is-hit? s (rest loib)))]))

(define (general-bullets-move list)
  (cond
    [(empty? list)empty]
    [(cons? list)(cons
                  (make-posn
                   (posn-x (first list))
                   (- (posn-y (first list)) SPEED))
                  (general-bullets-move (rest list)))]))


;;;;Signature
;;sp-bullets-move: LoSB -> LoSB
;;;; Purpose
;; GIVEN:  a list of spaceship bullets
;; RETURNS: the updated list of spaceship bullets
;; after moving upwards in a speed of 10 units

(define (sp-bullets-move losb)
  (map (λ(x) (make-posn
              (posn-x x)
              (- (posn-y x) SPEED))) losb))

;;;;Signature
;;iv-bullets-move: LoIB -> LoIB
;;;; Purpose
;; GIVEN:  a list of invader bullets
;; RETURNS: the updated list of invader bullets
;; after moving downwards in a speed of 10 units

(define (iv-bullets-move loib)
  (map (λ(x) (make-posn
              (posn-x x)
              (+ (posn-y x) SPEED))) loib))


;;gen-ibullet: LoI LoIB-> LoIB
;;;;Purpose
;; GIVEN: an list of invaders and a list of invader bullets
;; RETURNS: a new list of randomly generated invader bullets added with
;; original list of invader bullets
(define (gen-ibullet loi loib)
  (cond
    [(empty? loi) loib]
    [(= (random 36) 0)
     (cons (make-posn(posn-x (first loi))
                     (+(posn-y (first loi))
                       (/ INVADER-SIDE 2))) loib)]
    [else (gen-ibullet (rest loi) loib )]))

;;;;Signature
;;invaders-fire: LoIB LoI -> LoIB
;;;;Purpose
;;GIVEN: a list of invader bullets and a list of invaders
;;RETURNS: a new list of invader bullets with fired bullets added
;; if the number of bullets are fewer than 10 
              
(define(invaders-fire loib loi)
  (cond
    [(empty? loi) loib]
    [(cons? loi)
     (if
       (< (length loib) 10)
      (gen-ibullet loi loib)
       loib)]))

(define IB-TEST (make-posn 90 20) )
(define LOIB-TEST (cons IB-TEST empty))
(define LOIB-TEST1 (list IB-TEST IB-TEST IB-TEST IB-TEST IB-TEST IB-TEST
                         IB-TEST IB-TEST IB-TEST IB-TEST))
(check-expect (invaders-fire LOIB-TEST1 empty) LOIB-TEST1)
(check-expect (invaders-fire LOIB-TEST1 LOI-TEST) LOIB-TEST1)
(define LOIB-TEST2 (list IB-TEST IB-TEST IB-TEST IB-TEST IB-TEST IB-TEST
                         IB-TEST IB-TEST IB-TEST))

;;;;Signature
;;gen-sbullet: Spaceship -> SB
;;;;Purpose
;; GIVEN: a spaceship
;; RETURNS: a randomly generated spaceship bullet

(define (gen-sbullet spaceship)
  (make-posn (posn-x (spaceship-position spaceship))
             (-(posn-y (spaceship-position spaceship))
               (/ SPSHIP-HEIGHT 2))))

;;;;Signature
;;spaceship-fire: Spaceship LoSB-> LoSB
;;;;Purpose
;;GIVEN: a spaceship and a list of spaceship bullets
;;RETURNS: a new list of spaceship bullets with fired bullets added

;;;; Function
(define (spaceship-fire spaceship losb)
  (cond
    [(< (length losb) 3) (cons (gen-sbullet spaceship) losb)]
    [else losb]))

(define LOSB-TEST1 (list SB-TEST SB-TEST))
(define LOSB-TEST2 (list SB-TEST SB-TEST SB-TEST))
(check-expect (spaceship-fire SPSHIP-INIT LOSB-TEST)
              LOSB-TEST1)
(check-expect (spaceship-fire SPSHIP-INIT LOSB-TEST2)
              LOSB-TEST2)

;;;;Signature
;; iv-is-hit?: Invader LoSB -> Boolean
;;;;Purpose
;; GIVEN: an invader and a list of spaceship bullets
;; RETURNS: true if an invader is hit by any of the list of spaceship bullets,
;; else false

(define (iv-is-hit? i losb)
  (cond
    [(empty? losb) #false]
    [(cons? losb) (or (iv-sb-hit? i (first losb))
                      (iv-is-hit? i (rest losb)))]))

(check-expect (iv-is-hit? INVADER1 empty) #false)
(define SB-TEST2(make-posn 90 20))
(define LOSB-TEST3(list SB-TEST2))
(check-expect (iv-is-hit? INVADER1 LOSB-TEST3)
              #true)


;;;; Signature
;; iv-sb-hit?: Invader SB -> Boolean
;;;; Purpose
;; GIVEN: an invader and a spaceship bullet
;; RETURNS: true if the spaceship bullet hits the invader, else false
(define(iv-sb-hit? i sb)
  (and(<= (- (posn-x i) (/ INVADER-SIDE 2)) (posn-x sb) (+ (posn-x i)
                                                           (/ INVADER-SIDE 2)))
      (>= (+ (posn-y i) (/ INVADER-SIDE 2)) (posn-y sb))))

;;;; Signature
;; sb-hit-any?: LoI SB-> Boolean
;;;; Purpose
;; GIVEN: a spaceship bullet and a list of invaders
;; RETURNS: true if the bullet hits any of the invader,
;; else false
(define (sb-hit-any? loi sb)
  (cond
    [(empty? loi) #false]
    [(cons? loi) (or(iv-sb-hit? (first loi) sb)
                    (sb-hit-any? (rest loi) sb))]))

;;;; Signature
;; remove-spaceship-bullets: LoSB LoI -> LoSB
;;;; Purpose
;; GIVEN: a list of spaceship bullets and a list of invaders
;; RETURNS: an updated list of spaceship bullets with
;; bullets out of bounds or hit invader removed

(define(remove-spaceship-bullets losb loi)
  (cond
    [(empty? losb) empty]
    [(cons? losb) (if
                      (or(< (posn-y (first losb)) 0)
                        (sb-hit-any? loi (first losb)))
                     (remove-spaceship-bullets (rest losb) loi)
                     (cons (first losb)
                           (remove-spaceship-bullets (rest losb) loi)))]))

(check-expect (remove-spaceship-bullets LOSB-TEST3 empty) LOSB-TEST3)
(check-expect (remove-spaceship-bullets LOSB-TEST3 LOI-TEST) empty)
(check-expect (remove-spaceship-bullets LOSB-TEST LOI-TEST) LOSB-TEST)

;;;; Sigature
;; remove-hit-invader: LoI LoSB -> LoI
;;;; Purpose
;; GIVEN: a list of invaders and a list of spaceship bullets
;; RETURNS: a new list of invaders with the hitted invader removed
(define(remove-hit-invader loi losb)
  (cond
    [(empty? loi) empty]
    [(cons? loi) (if (iv-is-hit? (first loi) losb)
                     (remove-hit-invader (rest loi) losb)
                     (cons (first loi)
                           (remove-hit-invader (rest loi) losb)))]))

(check-expect (remove-hit-invader empty LOSB-TEST3) empty)
(check-expect (remove-hit-invader LOI-TEST LOSB-TEST3) empty)
(check-expect (remove-hit-invader LOI-TEST LOSB-TEST) LOI-TEST)



;;;; Signature
;; remove-invader-bullets: LoIB Spaceship -> LoIB
;;;; Purpose
;; GIVEN: a list of invader bullets and a spaceship
;; RETURNS: an updated list of invader bullets with
;; bullets out of bounds or hit the spcaeship removed

(define(remove-invader-bullets loib s)
  (cond
    [(empty? loib) empty]
    [(cons? loib)
       (if
         (or(>(posn-y (first loib)) HEIGHT)
            (ib-hit? s (first loib)))
         (remove-invader-bullets (rest loib) s)
         (cons(first loib) (remove-invader-bullets (rest loib) s)))]))


(check-expect(remove-invader-bullets LOIB-TEST SPSHIP-INIT)
             LOIB-TEST)

;;;; Signature
;; invaders-move-a-tick: Natural LoI -> LoI

;;;; Purpose
;; GIVEN: the play time and a list of invaders
;; RETURNS: a new list of invaders after they move down by the unit step
;; after n ticks
(define (invaders-move-a-tick duration loi)
  (cond
    [(= (remainder duration 50) 0)
     (invaders-move loi)]
    [else loi]))

(check-expect (invaders-move-a-tick 99 LOI-TEST)
              LOI-TEST)

;;;; Signature
;; invaders-move: LoI -> LoI

;;;; Purpose
;; GIVEN: the play time and a list of invaders
;; RETURNS: a new list of invaders after they move down by the unit step

(define (invaders-move loi)
  (map (λ(x) (make-posn (posn-x x)(+ (posn-y x) SPEED))) loi))

(define LOI-TEST1(list (make-posn 90 30)))
(check-expect (invaders-move LOI-TEST)
              LOI-TEST1)

;;;; Signature
;; update-score: LoI -> Natural
;;;; Purpose
;; GIVEN: the list of invaders exist in the game
;; RETURNS: the score after a clock tick

(define (update-score loi)
  (* (- 36 (length loi)) 3))

;;;; Signature
;; update-live: World -> Natural
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the spaceship's live left after a clock tick

(define (update-live world)
  (cond
    [(ship-is-hit? (world-spaceship world) (world-loib world))
     (- (world-live world) 1)]
    [else (world-live world)]))

(define WORLD5 (make-world SPSHIP-INIT LOI-TEST LOIB-TEST LOSB-TEST3 0 105 3))
(check-expect (update-live WORLD5)
              3)

;;;;Signature
;;world-step: World -> World
;;;;Purpose
;;GIVEN: the current world
;;RETURNS:  the next world after one clock tick
(define(world-step w)
  (make-world
   (spaceship-move (world-spaceship w) (world-loib w))
   (invaders-move-a-tick (world-time w)
                  (remove-hit-invader (world-loi w) (world-losb w)))
   (iv-bullets-move
     (remove-invader-bullets
       (invaders-fire (world-loib w) (world-loi w))
       (world-spaceship w)))
   (sp-bullets-move(remove-spaceship-bullets (world-losb w)
                                                    (world-loi w)))
   (+ (world-time w) 1)
   (update-score (world-loi w))
   (update-live w)))

(define LOIB-TEST4 (cons (make-posn 90 40) empty))
(define WORLD3 (make-world SPSHIP-INIT LOI-TEST LOIB-HIT LOSB-TEST3 0 105 3))
(define WORLD4 (make-world SPSHIP-INIT empty empty empty 1 105 2))
(check-expect (world-step WORLD3)
              WORLD4)

;;;; Signature
;; key-handler: World Key-Event -> World
;;;; Purpose
;; GIVEN: the current world and a key event
;; RETURNS: a new world with direction updated according to the key event
(define(key-handler world ke)
  (cond
    [(or (key=? ke "left")
         (key=? ke "right"))
     (make-world (make-spaceship (string->symbol ke)
                                 (spaceship-position(world-spaceship world)))
                 (world-loi world)
                 (world-loib world)
                 (world-losb world)
                 (world-time world)
                 (world-score world)
                 (world-live world))]
    [(key=? ke " ")
     (make-world (world-spaceship world)
                 (world-loi world)
                 (world-loib world)
                 (spaceship-fire (world-spaceship world) (world-losb world))
                 (world-time world)
                 (world-score world)
                 (world-live world))]
    [else world]))

(check-expect (key-handler WORLD-INIT "left")
              (make-world SPSHIP-INIT1 LOI-INIT empty empty 0 0 3))
(define WORLD1 (make-world SPSHIP-INIT LOI-INIT empty LOSB-TEST 0 0 3))

(check-expect (key-handler WORLD-INIT " ")
              WORLD1)
(check-expect (key-handler WORLD-INIT "up")
              WORLD-INIT)

;;;;Siganture
;;iv-reach-btm?: Invader Spaceship-> Boolean
;;;;Purpose
;;Given: an invader and a spaceship
;;RETURNS: true if invader reach the bottom, false otherwise
(define(iv-reach-btm? invader spaceship)
  (>= (+ (posn-y invader) (/ INVADER-SIDE 2))
      (- (posn-y (spaceship-position spaceship)) (/ INVADER-SIDE 2))))
      

;;;;Siganature
;;loi-reach-btm?: loi Spaceship -> Boolean
;;;;Purpose
;;GIVEN:a list of invaders and a spaceship
;;RETURNS: true if any invader in the list reaches the bottom, false otherwise
(define(loi-reach-btm? loi spaceship)
  (cond
    [(empty? loi)#false]
    [(cons? loi)
     (or(iv-reach-btm? (first loi) spaceship)
        (loi-reach-btm? (rest loi) spaceship))]))


;;;;Signature
;;world-end?: World -> Boolean
;;;;Purpose
;;GIVEN: the current world
;;RETURNS: true if one of the condition that end the game has been met
;;         fasle otherwise.

(define (world-end? world)
  (or (= (world-live world) 0)
      (empty? (world-loi world))
      (loi-reach-btm? (world-loi world)
                      (world-spaceship world))))

(define WORLD2 (make-world SPSHIP-INIT1 empty empty LOSB-TEST 0 0 3))
(check-expect (world-end? WORLD-INIT) #false)
(check-expect (world-end? WORLD2) #true)

(big-bang WORLD-INIT
        (to-draw world-draw)
        (on-tick world-step 0.15) 
        (on-key key-handler)
        (stop-when world-end?))



                                  
