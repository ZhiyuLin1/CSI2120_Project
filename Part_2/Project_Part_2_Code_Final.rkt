#lang racket
; for query image only (read query image histogram)
(define (read-helper p)
  (let ((x (read p)))
    (if (eof-object? x)
        '()
        (cons x (read-helper p))
        )
    )
  )

; read query image and store histogram to a list ex: (512 ..), already normalized
(define (readQuery queryFilename)
   (let ((p (open-input-file queryFilename)))
     (read-helper p)
    )
  )


; =============================================================================================
; ==== for dataset image only (read dataset histogram and normalize) =======
(define (readDataset-helper p)
  (let ((x (read p)))
    (if (eof-object? x)
        '()
        (cons (/ x 172800) (readDataset-helper p))
        )
    )
  )


(define (readDatasetHistogram datasetFilename)
   (let ((p (open-input-file datasetFilename)))
     (let ((first (read p))) 
       (cons first (readDataset-helper p))))) 

; ============================================================================================



; ============================================================================================
; read dataset files name to a list ex: (1001.jpg.txt 1003.jpg.txt 1004.jpg.txt ... )
; ============================================================================================

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

(define (process-and-compare-files file-list directory query-histogram)
  (if (null? file-list)
      '()
      (let* ((filename (car file-list))
             (full-path (string-append directory "/" filename))
             (dataset-histogram (readDatasetHistogram full-path))
             (similarity (compare-histograms query-histogram dataset-histogram)))
        (cons (list similarity filename)
              (process-and-compare-files (cdr file-list) directory query-histogram)))))

(define (compare-query-with-dataset directory queryFilename)
  (let* ((query-histogram (readQuery queryFilename))
         (files (get-txt-files directory)))
    (process-and-compare-files files directory query-histogram)))



;============================================================================
; sort similarity results list (from biggest to smallest)
;============================================================================

(define (insert-sorted result item)
  (if (null? result)
      (list item)
      (if (> (car item) (car (car result)))
          (cons item result)
          (cons (car result) (insert-sorted (cdr result) item)))))

(define (sort-results results)
  (if (null? results)
      '()
      (insert-sorted (sort-results (cdr results)) (car results))))



;==========================================================================
; Output Result
;==========================================================================

(define (take-first-5 lst)
  (define (helper lst count)
    (if (or (null? lst) (= count 5))
        '() ; If list is null or count = 5, return empty list
        (cons (car lst) (helper (cdr lst) (+ count 1))))) 
  (helper lst 0)) 


(define (display-results results count)
  (if (or (null? results) (> count 5))
      (void) ; stop if count > 5
      (begin
        (let ((result (car results)))
          (display (format "Top ~a = ~a | Similarity: ~a" count (cadr result) (car result)))
          (display "\n"))
        (display-results (cdr results) (+ count 1)))))

(define (similaritySearch queryHistogramFilename imageDatasetDirectory)
  (let ((sorted-results (sort-results (compare-query-with-dataset imageDatasetDirectory queryHistogramFilename))))
    (let ((top-5 (take-first-5 sorted-results))) 
      (display "5 most similar images to the query image:\n")
      (display-results top-5 1))))



(similaritySearch "q00.ppm_normalized.hist.txt" "imageDataset2_15_20")


