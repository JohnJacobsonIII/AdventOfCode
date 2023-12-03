#lang racket

(require megaparsack megaparsack/text)
(require rebellion/streaming/reducer)
(require rebellion/streaming/transducer)

(define file-contents
  (port->string (open-input-file "tests/input2.txt") #:close? #t))

(define (convert inp)
  (string-replace (string-replace (string-replace (string-replace (string-replace (string-replace (string-replace (string-replace (string-replace inp "nine" "n9e" #:all? #true)
                                                                                                                                  "eight" "e8t" #:all? #true)
                                                                                                                  "seven" "7n" #:all? #true)
                                                                                                  "six" "6" #:all? #true)
                                                                                  "five" "5e" #:all? #true)
                                                                  "four" "4" #:all? #true)
                                                  "three" "t3e" #:all? #true)
                                  "one" "o1e" #:all? #true)
                  "two" "t2o" #:all? #true))

(define (trebuchet1 string)
  (transduce (map (lambda (line)
                    (string->number (list->string (list (first (filter char-numeric? (string->list line)))
                                                        (last (filter char-numeric? (string->list line)))))))
                  (string-split file-contents)) #:into into-sum))

(define (trebuchet2 string)
  (transduce (map (lambda (line)
                    (string->number (list->string (list (first (filter char-numeric? (string->list (convert line))))
                                                        (last (filter char-numeric? (string->list (convert line))))))))
                  (string-split file-contents)) #:into into-sum))

(trebuchet2 file-contents)
