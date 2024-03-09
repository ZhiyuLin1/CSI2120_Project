#lang racket
; Zhiyu Lin 300255509, Yitao Cui 300345709

; helper function used to read all numbers from the query histogram and store them in a list
; returns a list that stores the histogram data
(define (read-helper p)
  (let ((x (read p)))
    (if (eof-object? x)
        '()
        (cons x (read-helper p))
        )
    )
  )


; read query histogram file and store it in a list, ex: (0.0029 0.0157 ...), already normalized.
; returns a list that stores the normalized histogram data.
(define (readQuery queryFilename)
   (let ((p (open-input-file queryFilename)))
     (read-helper p)
    )
  )


; =============================================================================================
; ==== for dataset image only (read dataset histogram and normalize) =======

; helper function used to read all numbers from the dataset histogram, and normalize each histogram data.
; returns a list that stores the histogram data
(define (readDataset-helper p)
  (let ((x (read p)))
    (if (eof-object? x)
        '()
        (cons (/ x 172800) (readDataset-helper p))
        )
    )
  )


; read dataset histogram file and store it in a list. 
; returns a list that stores the normalized histogram data.
(define (readDatasetHistogram datasetFilename)
   (let ((p (open-input-file datasetFilename)))
     (let ((first (read p))) 
       (cons first (readDataset-helper p))))) 

; ============================================================================================



; ============================================================================================
; read dataset files name to a list ex: (1001.jpg.txt 1003.jpg.txt 1004.jpg.txt ... )
; ============================================================================================

; read dataset files names
; returns a list that stores dataset files name ex: (1001.jpg.txt 1002.jpg.txt 1003.jpg.txt ... )
(define (get-txt-files directory)
  (foldr (lambda (file acc)
           (let ((filename (path->string file)))
             (if (string-suffix? filename ".txt")
                 (cons filename acc)
                 acc)))
         '()
         (directory-list directory)))



; ============================================================================================
; Make a list with sublists containing each dataset image's histogram and their filename
; ============================================================================================

; processes each file in a list by constructing sublists that contain each dataset image's filename and its histogram.
; returns a list that stores dataset files names and histograms. ex: ((1001.jpg.txt (histogram)) ... )

(define (process-files file-list directory)
  (if (null? file-list)
      '()  ; return empty list if there is no more files
      (let* ((filename (car file-list))
             (full-path (string-append directory "/" filename))
             (histogram (readDatasetHistogram full-path)))
        (cons (list (list filename histogram))
              (process-files (cdr file-list) directory)))))



; ============================================================================================
; histogram comparison
; ============================================================================================

; helper function used to calculate the similarity score between a query histogram and a dataset histogram.
; query-hist (list), dataset-hist (list) -> Output: similarity score (number)

(define (compare-histograms query-hist dataset-hist)
  (letrec ((helper (lambda (q-hist d-hist sum-min sum-query)
                     (if (null? q-hist) 
                         (/ sum-min sum-query) 
                         (let ((q-head (car q-hist))
                               (d-head (car d-hist)))
                           (helper (cdr q-hist) (cdr d-hist)
                                   (+ sum-min (min q-head d-head)) 
                                   (+ sum-query q-head))))))) 
    (helper query-hist dataset-hist 0 0)))


; ==== processing

; helper function used to process and compare each histogram in the dataset with the query histogram.
; returns a list of similarity scores paired with filenames.
; file-list (list), directory (string), query-histogram (list) -> Output: (list)

(define (process-and-compare-files file-list directory query-histogram)
  (if (null? file-list)
      '()
      (let* ((filename (car file-list))
             (full-path (string-append directory "/" filename))
             (dataset-histogram (readDatasetHistogram full-path))
             (similarity (compare-histograms query-histogram dataset-histogram)))
        (cons (list similarity filename)
              (process-and-compare-files (cdr file-list) directory query-histogram)))))



; compares the query histogram to all histograms in the dataset.
; returns a list of similarity scores and corresponding filenames.
; directory (string), queryFilename (string) -> Output: (list)

(define (compare-query-with-dataset directory queryFilename)
  (let* ((query-histogram (readQuery queryFilename))
         (files (get-txt-files directory)))
    (process-and-compare-files files directory query-histogram)))



;============================================================================
; sort similarity results list (from biggest to smallest)
;============================================================================

; inserts an item into a sorted list based on the item's first element. Sort from biggest to smallest.
; returns the sorted list

(define (insert-sorted result item)
  (if (null? result)
      (list item)
      (if (> (car item) (car (car result)))
          (cons item result)
          (cons (car result) (insert-sorted (cdr result) item)))))



; sort the similarity results list (from biggest to smallest)
; returns the sorted list

(define (sort-results results)
  (if (null? results)
      '()
      (insert-sorted (sort-results (cdr results)) (car results))))



;==========================================================================
; Output Result
;==========================================================================

; helper function for display the final results.
; returns the first 5 elements from a list.

(define (take-first-5 lst)
  (define (helper lst count)
    (if (or (null? lst) (= count 5))
        '() ; If list is null or count = 5, return empty list
        (cons (car lst) (helper (cdr lst) (+ count 1))))) 
  (helper lst 0))



; helper function for display the final results.
; Output: displays the Top 1 to Top 5 file names and similarity.

(define (display-results results count)
  (if (or (null? results) (> count 5))
      (void) ; stop if count > 5
      (begin
        (let ((result (car results)))
          (display (format "Top ~a = ~a | Similarity: ~a" count (cadr result) (car result)))
          (display "\n"))
        (display-results (cdr results) (+ count 1)))))



; main function, get the query histogram file name and the dataset directory.
; Output: displays the final results.

(define (similaritySearch queryHistogramFilename imageDatasetDirectory)
  (let ((sorted-results (sort-results (compare-query-with-dataset imageDatasetDirectory queryHistogramFilename))))
    (let ((top-5 (take-first-5 sorted-results))) 
      (display "5 most similar images to the query image:\n")
      (display-results top-5 1))))


(similaritySearch "q00.ppm_normalized.hist.txt" "imageDataset2_15_20")


