;CS-353 Project 1 / Ryan Lee / February 11th, 2024 / Made in conjuncture with ChatGPT

#lang racket
(require racket/string)

(define input-file (open-input-file "scores.txt"))

;; Convert score symbols to numbers, handling strikes, spares, and numeric scores
(define (string->number-safe str last-score)
  (cond
    [(string=? str "X") 10] ; Strike
    [(string=? str "/") (- 10 last-score)] ; Spare, based on last score
    [(regexp-match? #rx"^[0-9]$" str) (string->number str)] ; Numeric score
    [else 0])) ; Default to avoid contract violation

;Calculate bowling scores from a list of score strings. If there are strikes or spares handles them accordingly
(define (calc-scores scores)
  (let loop ([scores scores]
             [total 0]
             [last-score 0])
    (cond
      [(null? scores) total]  ;Base case: return the total score when no more scores are left
      [(string=? (car scores) "X") ;Strike
       (let* ([next-scores (cdr scores)]
              [next-score1 (if (null? next-scores) 0 (string->number-safe (car next-scores) 10))]
              [next-score2 (if (< (length next-scores) 2) 0 (string->number-safe (cadr next-scores) 10))])
         (loop (cdr scores) (+ total 10 next-score1 next-score2) 10))]
      [(string=? (car scores) "/") ; Spare
       (let* ([next-scores (cdr scores)]
              [next-score1 (if (null? next-scores) 0 (string->number-safe (car next-scores) 0))])
         (loop (cdr scores) (+ total (- 10 last-score) next-score1) 10))] ;Calculate spare as 10 - last score
      [else ;Numeric score
       (loop (cdr scores) (+ total (string->number-safe (car scores) last-score)) (string->number-safe (car scores) last-score))])))

;Displays team names, full names, and calculates scores
(define (process-line line highest-player-score player-with-highest-score highest-team-score team-with-highest-score)
  (define elements (string-split line))
  (cond
    [(= (length elements) 1)
     (if (= highest-team-score 0) ;If this is the first team name encountered
         (begin
           (display "\nTeam name: ")
           (displayln line)
           (values highest-player-score player-with-highest-score highest-team-score line)) ;Return the line number
         (begin ;If it's the second team name encountered
           (display "\nSecond team name: ")
           (displayln line)
           (values highest-player-score player-with-highest-score highest-team-score team-with-highest-score)))] ;Return the current highest score and team with highest score
    [(>= (length elements) 2)
     (define full-name (string-join (take elements 2) " ")) ;Join the first two elements to form the full name
     (let* ([team-name (car elements)]
            [scores (drop elements 2)]
            [total-score (calc-scores scores)])
       (display full-name)
       (displayln (format ": Total Score: ~a" total-score))
       (values (if (> total-score highest-player-score) total-score highest-player-score)
               (if (> total-score highest-player-score) full-name player-with-highest-score)
               (if (> total-score highest-team-score) total-score highest-team-score)
               (if (> total-score highest-team-score) team-name team-with-highest-score)))]))

;Reads each line of the file and displays the player with the highest score
(define (process-file input-file)
  (let loop ([input-file input-file]
             [highest-player-score 0]
             [player-with-highest-score ""]
             [highest-team-score 0]
             [team-with-highest-score ""]
             [line-count 0])
    (define line (read-line input-file))
    (if (eof-object? line)
        (begin
          (close-input-port input-file) ;; Close the file when reaching the end
          (display "\nPlayer with the highest score: ")
          (displayln player-with-highest-score)
          (display "Team with the highest score: ")
          (displayln team-with-highest-score))
        (let-values ([(new-highest-player-score new-player-with-highest-score
                       new-highest-team-score new-team-with-highest-score)
                      (process-line line
                                    highest-player-score
                                    player-with-highest-score
                                    highest-team-score
                                    team-with-highest-score)])
          (loop input-file
                new-highest-player-score
                new-player-with-highest-score
                new-highest-team-score
                new-team-with-highest-score
                (add1 line-count))))))

;Main execution, calls 
(process-file input-file)