(define-library (olite cache)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      make-cache
      load-cache)

   (import
      (otus lisp))

(begin
   (define (make-cache name loader)
      (coroutine [name "cache"] (lambda ()
         (let loop ((this #empty))
            (let*((envelope (wait-mail))
                  (sender msg envelope))

               (define id (cond
                  ((symbol? msg) msg)
                  ((string? msg) (string->symbol msg))
                  (else
                     (runtime-error (string-append "invalid " name " name ") msg))))

               (define object (this id #false))

               (if object then
                  (mail sender object)
                  (loop this)
               else
                  (define object (loader (symbol->string id)))
                  (mail sender object)
                  (loop (put this id object))))))))

   (define (load-cache cache)
      ; id is a string or symbol
      (case-lambda
         ((id) (await (mail cache id)))
         ((id default) (or
               (await (mail cache id)) default))))

))
