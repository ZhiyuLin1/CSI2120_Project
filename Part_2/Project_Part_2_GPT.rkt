#lang racket

;; Reads all lines from a file and returns a list of strings.
(define (read-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line)) (acc '()))
        (if (eof-object? line)
            (reverse acc)
            (loop (read-line) (cons line acc)))))))

;; Safely converts a string to a number, returning 0 on failure.
(define (safe-string->number s)
  (let ((num (string->number s)))
    (if num num 0)))

;; Recursively converts a list of strings to a list of numbers, using safe conversion.
(define (strings-to-numbers strings)
  (if (null? strings)
      '()
      (cons (safe-string->number (car strings))
            (strings-to-numbers (cdr strings)))))

;; Recursively normalizes a histogram, eliminating the use of 'map'.
(define (normalize-histogram hist total)
  (if (null? hist)
      '()
      (let ((normalized-value (if (= total 0) 0 (/ (car hist) total))))
        (cons normalized-value
              (normalize-histogram (cdr hist) total)))))

;; Calculates the similarity between two histograms recursively.
(define (calculate-similarity hist1 hist2)
  (if (or (null? hist1) (null? hist2))
      0
      (+ (min (car hist1) (car hist2))
         (calculate-similarity (cdr hist1) (cdr hist2)))))

;; Process each file in the directory, comparing histograms, and returns similarities.
(define (process-directory query-histogram directory)
  (letrec ((ends-with? (lambda (string suffix)
                         (and (>= (string-length string) (string-length suffix))
                              (string=? (substring string (- (string-length string) (string-length suffix))) suffix))))
           (process-files (lambda (files)
                            (if (null? files)
                                '()
                                (let* ((file (car files))
                                       (filename (path->string file))
                                       (rest (cdr files))
                                       (lines (read-lines filename))
                                       (hist (strings-to-numbers (cdr lines)))
                                       (total (apply + hist))
                                       (normalized-hist (normalize-histogram hist total))
                                       (similarity (calculate-similarity query-histogram normalized-hist)))
                                  (if (and (ends-with? filename ".txt")  ; Process only .txt files
                                           (not (ends-with? filename ".jpg.txt")))
                                      (cons (cons filename similarity)
                                            (process-files rest))
                                      (process-files rest)))))))
    (process-files (directory-list directory))))

;; Displays the top 5 similarities using recursion.
(define (display-top-5 similarities count)
  (when (and (not (null? similarities)) (< count 5))
    (let ((pair (car similarities)))
      (display "Top ")
      (display (+ count 1))
      (display " = ")
      (display (car pair))  ; File name
      (display " | Similarity: ")
      (display (cdr pair))  ; Similarity value
      (newline)
      (display-top-5 (cdr similarities) (+ count 1)))))

;; Main function to perform similarity search.
(define (similarity-search query-histogram-file directory)
  (let* ((query-lines (read-lines query-histogram-file))
         (query-hist (strings-to-numbers (cdr query-lines)))  ; Skip the first line assuming it's the total count
         (total (apply + query-hist))
         (normalized-query-hist (normalize-histogram query-hist total))
         (similarities (sort (process-directory normalized-query-hist directory) 
                             (lambda (x y) (> (cdr x) (cdr y))))))
    (display "5 most similar images to the query image:\n")
    (display-top-5 similarities 0)))




;; Specify the correct paths to the histogram file and the directory containing your dataset.
(similarity-search "D:/My_Things/University/2024_Winter/CSI2120/CSI2120_Project/Part_2/q00.ppm_normalized.hist.txt" "D:/My_Things/University/2024_Winter/CSI2120/CSI2120_Project/Part_2/imageDataset2_15_20")
