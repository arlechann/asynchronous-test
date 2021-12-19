(import (scheme base)
        (scheme list)
        (scheme write)
        (scheme list-queue)
        (gauche base)
        (gauche process)
        (gauche partcont)
        (data queue)
        (text console)
        (lib coroutine)
        (lib micro-thread)
        )

(define *path-list* '())

(define *path-generator*
        (let ((in (open-input-process-port '(find /) :error "/dev/null")))
          (make-generator (lambda (yield)
            (let rec ((line (read-line in)))
              (if (eof-object? line)
                  #f
                  (begin
                    (yield line)
                    (rec (read-line in)))))))))

(define (char-list->string ls)
  (list->string (reverse ls)))

(define (main)
  (call-with-console (make-default-console) (lambda (console)
    (define *input-char-list* '())

    (define (get-path-thread yield)
      (let loop ()
        (let ((path (*path-generator*)))
          (if (eof-object? path)
              #f
              (begin
                (set! *path-list* (cons path *path-list*))
                (yield #f)
                (loop))))))

    (define *input-queue* (make-queue))
    (define (input-thread yield)
      (let loop ()
        (if (chready? console)
            (enqueue! *input-queue* (getch console))
            (yield #f))
        (move-cursor-to console 0 0)
        (clear-to-eol console)
        (putstr console (char-list->string *input-char-list*))
        (loop)))

    (define (show-list row max-row ls)
      (if (and (not (null? ls)) (<= row max-row))
          (begin
            (move-cursor-to console row 0)
            (putstr console (car ls))
            (show-list (+ row 1) max-row (cdr ls)))))

    (define (filter-string-list yield input path-list)
      (reverse (filter (lambda (str) (string-scan str input)) path-list)))

    (define *query-queue* (make-queue))
    (define (output-thread yield)
      (let loop ()
        (if (queue-empty? *query-queue*)
            (yield #f)
            (begin
              (let ((query (dequeue! *query-queue*)))
                (move-cursor-to console 1 0)
                (clear-to-eos console)
                (show-list 1 12 (filter-string-list yield query *path-list*)))
                (yield #f)))
        (loop)))

    (define (eval-thread yield)
      (let loop ()
        (if (queue-empty? *input-queue*)
            (yield #f)
            (begin
              (let ((c (dequeue! *input-queue*)))
                (cond ((or (eq? c #\delete)
                           (eq? c #\backspace)
                           (eq? c #\x04)
                           (eq? c 'KEY_DEL))
                         (unless (null? *input-char-list*)
                           (set! *input-char-list* (cdr *input-char-list*))
                           (enqueue! *query-queue* (char-list->string *input-char-list*))))
                      ((eq? c #\return) (exit))
                      ((eq? c #\newline) (exit))
                      ((char? c) (begin
                        (set! *input-char-list* (cons c *input-char-list*))
                        (enqueue! *query-queue* (char-list->string *input-char-list*))))))))
        (loop)))

    (clear-screen console)
    (fork get-path-thread)
    (enqueue! *query-queue* "")
    (main-loop input-thread eval-thread output-thread)
    (display "exit"))))

(main)

