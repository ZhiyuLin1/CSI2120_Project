#lang racket

;; Function to read lines from a file and convert them into a list of numbers.
(define (read-and-process-file filename normalize?)
  (with-input-from-file filename
    (lambda ()
      (let read-loop ((line (read-line)) (acc '()))
        (if (eof-object? line)
            (if normalize? 
                (normalize-histogram acc (apply + acc))  ; Normalize if required
                (reverse acc))
            (read-loop (read-line) (cons (safe-string->number line) acc)))))))

(define (safe-string->number s)
  (let ((num (string->number s)))
    (if num num 0)))


(define (normalize-histogram hist total)
  (if (null? hist)
      '()
      (cons (/ (car hist) 172800)
            (normalize-histogram (cdr hist) total))))



(define (calculate-similarity hist1 hist2)
  (if (or (null? hist1) (null? hist2))
      0
      (+ (min (car hist1) (car hist2))
         (calculate-similarity (cdr hist1) (cdr hist2)))))


(define (is-txt-file? filename)
  (let* ((lowercase-filename (string-downcase filename))
         (suffix ".txt"))
    (and (>= (string-length lowercase-filename) (string-length suffix))
         (string=? (substring lowercase-filename (- (string-length lowercase-filename) (string-length suffix)))
                   suffix))))


(define (process-directory query-histogram directory)
  (define (process-file file)
    (if (null? file)
        '()
        (let* ((filename (path->string (car file)))
               (rest (process-file (cdr file))))
          (if (is-txt-file? filename)
              (let* ((full-path (build-path directory filename))  ; Correctly build full path
                     (hist (read-and-process-file full-path #f))  ; Assume normalization needed
                     (similarity (calculate-similarity query-histogram hist)))
                (cons (cons filename similarity) rest))
              rest))))

  (sort (process-file (directory-list directory)) 
        (lambda (x y) (> (cdr x) (cdr y)))))



(define (display-similarities similarities count max-count)
  (when (and (not (null? similarities)) (< count max-count))
    (let ((pair (car similarities)))
      (display "Top ") (display (+ count 1))
      (display " = ") (display (car pair))
      (display " | Similarity: ") (display (cdr pair)) (newline)
      (display-similarities (cdr similarities) (+ count 1) max-count))))


;; Main function to execute the processing and display results
(define (main query-histogram-file directory)
  (let* ((query-histogram (read-and-process-file query-histogram-file #f))  ; No need to drop the first element
         (similarities (process-directory query-histogram directory)))
    (display "5 most similar images to the query image:\n")
    (display-similarities similarities 0 5)))

;; Usage example, replace the placeholders with actual paths
(main "q01.ppm_normalized.hist.txt" 
      "imageDataset2_15_20")
