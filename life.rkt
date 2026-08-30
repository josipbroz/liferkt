#lang racket/gui
(require racket/list)

(define CELL-SIZE 6)
(define STATUS-BAR-HEIGHT 25)

(define-values (display-width display-height) (get-display-size))
(define frame-width (quotient display-width 2))
(define frame-height (quotient display-height 2))
(define GRID-WIDTH (quotient frame-width CELL-SIZE))
(define GRID-HEIGHT (quotient (- frame-height STATUS-BAR-HEIGHT) CELL-SIZE))

(define status-canvas #f)
(define generation-count 0)
(define paused? #f)

;; Vectors give O(1) cell access, unlike list-ref's O(n) walk
(define (map-with-index f vec)
  (build-vector (vector-length vec) (lambda (i) (f i (vector-ref vec i)))))

(define (grid-height grid) (vector-length grid))
(define (grid-width grid) (vector-length (vector-ref grid 0)))

;; Reads a cell, wrapping coordinates toroidally around the grid's own size
(define (cell-alive-at? x y grid)
  (define wrapped-y (modulo y (grid-height grid)))
  (define wrapped-x (modulo x (grid-width grid)))
  (vector-ref (vector-ref grid wrapped-y) wrapped-x))

(define (get-surrounding-coordinates x y)
  (define offsets '((-1 . -1) (-1 . 0) (-1 . 1)
                     (0 . -1)          (0 . 1)
                     (1 . -1)  (1 . 0)  (1 . 1)))
  (map (lambda (offset) (list (+ x (car offset)) (+ y (cdr offset)))) offsets))

;; Returns the count of living neighbors around a specific coordinate
(define (count-living-neighbors x y grid)
  (define living-neighbors
    (filter (lambda (coord) (cell-alive-at? (first coord) (second coord) grid))
            (get-surrounding-coordinates x y)))
  (length living-neighbors))

;; Determines the next state of an individual cell
(define (evaluate-cell-rules x y alive? grid)
  (define neighbors (count-living-neighbors x y grid))
  (cond
    [(and alive? (< neighbors 2)) #f]           ; Underpopulation: Cell dies
    [(and alive? (member neighbors '(2 3))) #t] ; Survival: Cell lives
    [(and alive? (> neighbors 3)) #f]           ; Overpopulation: Cell dies
    [(and (not alive?) (= neighbors 3)) #t]     ; Reproduction: Cell becomes alive
    [else alive?]))                             ; No change

;; Updates the entire grid by applying the rules to every single cell
(define (next-generation current-grid)
  (map-with-index
   (lambda (y row)
     (map-with-index
      (lambda (x cell-alive?)
        (evaluate-cell-rules x y cell-alive? current-grid))
      row))
   current-grid))

(define (toggle-cell grid x y)
  (map-with-index
   (lambda (row-y row)
     (map-with-index
      (lambda (col-x cell) (if (and (= row-y y) (= col-x x)) (not cell) cell))
      row))
   grid))

;; All (x y) coordinates covering the grid, in row-major order
(define (all-coordinates grid)
  (append-map (lambda (y) (map (lambda (x) (list x y)) (range (grid-width grid))))
              (range (grid-height grid))))

(define (alive-coordinates grid)
  (filter (lambda (coord) (cell-alive-at? (first coord) (second coord) grid))
          (all-coordinates grid)))

;; Random start where each cell independently has a chance of being alive
(define (make-random-grid width height density)
  (build-vector height (lambda (y) (build-vector width (lambda (x) (< (random) density))))))

;; ---- GUI ----

(define life-canvas%
  (class canvas%
    (init-field grid-width grid-height)
    (super-new)
    ;; Randomly seed 30%-37% of cells alive for an unpredictable default start
    (define grid
      (make-random-grid grid-width grid-height (+ 0.30 (* (random) 0.07))))

    (define/override (on-event event)
      (when (send event button-down?)
        (define x (quotient (send event get-x) CELL-SIZE))
        (define y (quotient (send event get-y) CELL-SIZE))
        (when (and (>= x 0) (< x grid-width) (>= y 0) (< y grid-height))
          (set! grid (toggle-cell grid x y))
          (send this refresh)))
      (super on-event event))

    (define/public (get-grid) grid)
    (define/public (set-grid! g) (set! grid g))
    (define/public (get-grid-width) grid-width)
    (define/public (get-grid-height) grid-height)))

(define frame
  (new (class frame%
         (super-new)
         ;; Terminate the process when the window's close button is pressed
         (define/augment (on-close) (exit 0))
         ;; Terminate the process when Escape is pressed anywhere in the window
         (define/override (on-subwindow-char receiver event)
           (cond
             [(eq? (send event get-key-code) 'escape) (exit 0)]
             [(memv (send event get-key-code) '(#\p #\P)) (set! paused? (not paused?))]
             [else (super on-subwindow-char receiver event)])))
       [label "Conway's Game of Life"] [width frame-width] [height frame-height]))

(define main-canvas
  (new life-canvas%
       [parent frame]
       [grid-width GRID-WIDTH]
       [grid-height GRID-HEIGHT]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-background "black")
          (send dc clear)
          (send dc set-brush (make-object color% 204 85 0) 'solid)
          (send dc set-pen "black" 1 'transparent)
          (define grid (send canvas get-grid))
          (for-each
           (lambda (coord)
             (send dc draw-rectangle
                   (* (first coord) CELL-SIZE) (* (second coord) CELL-SIZE)
                   (- CELL-SIZE 1) (- CELL-SIZE 1)))
           (alive-coordinates grid)))]))

; Create a custom canvas for drawing the status bar
(set! status-canvas
  (new canvas%
       [parent frame]
       [stretchable-height #f]
       [min-height STATUS-BAR-HEIGHT] ; Force a specific status bar height
       [paint-callback
        (lambda (canvas dc)
          ; Draw a light grey background
          (send dc set-background (make-object color% 240 240 240))
          (send dc clear)
          
          ; Draw a top border line to separate it from the main content
          (send dc set-pen "gray" 1 'solid)
          (send dc draw-line 0 0 (send canvas get-width) 0)
          
              ; Draw status text
              (send dc draw-text
                    (format "Generation: ~a" generation-count)
                5 4)

              ; Draw key hints right-aligned
              (define hint-text "Press ESC to exit, press P to pause")
              (define-values (hint-width hint-height _1 _2) (send dc get-text-extent hint-text))
              (send dc draw-text hint-text
                    (- (send canvas get-width) hint-width 5) 4))]))

;; Animation timer advancing one generation at ~200ms intervals, unless paused
(define timer
  (new timer%
       [interval 200]
       [notify-callback
        (lambda ()
          (unless paused?
            (send main-canvas set-grid! (next-generation (send main-canvas get-grid)))
            (set! generation-count (add1 generation-count))
            (send main-canvas refresh)
            (send status-canvas refresh)))]))

(send frame center 'both)
(send frame show #t)
