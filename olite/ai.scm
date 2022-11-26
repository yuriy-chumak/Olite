(define-library (olite ai)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      load-ai ; загрузить ai из файла (или кеша)
      dumb-ai ; ничего не делающий ai

      ; запустить новый инстанс загруженного ai
      run-ai

      ; общение с другими ai
      notify handle
   )

   (import
      (otus lisp)
      (olite math)
      ;; (scheme dynamic-bindings)
      
      (owl parse) (lang sexp) (lang eval) (scheme repl) ; почтистить
      (file sexp)

      ; ai script libraries (to be used in ai file)
      (scheme inexact)
      (scheme srfi-27))

   ; game ai finite-state machine
   (include "olite/ai/state-machine.scm")

(begin
   (import (olite config))

   ; ----------------------------------------
   ; -- ai files ----------------------------
   ; ...
   (define (read-ai-file name)
      (define filename (string-append "gamedata/ai/" name ".ai"))
      (print "Loading ai " filename " ...")

      (define sexp (read-sexp-file filename))
      (unless sexp
         (runtime-error "invalid ai file" filename))

      (define ai (eval sexp (interaction-environment)))
      (unless (ff? ai)
         (runtime-error "invalid ai" sexp))
      ai)

   ; -=( cache )=-----------------------------------
   (import (olite cache))
   (define cache (make-cache "ai" read-ai-file))
   (define load-ai (load-cache cache))

   ; ------------------------------------------------------

   ; ai, который перманентно спит и вообще ничего не делает
   (define (dumb-ai)
      (define (dumb)
         (let*((envelope (wait-mail))
               (sender msg envelope))
            (dumb)))
   
      (actor ['dumb] dumb))

   ; отправить сообщение (просто)
   (define (notify ship name . args) ; todo: проверить используется ли где-то
      (mail ship ['notify-ai name args]))
   ; отправить сообщение и подождать
   (define (handle ship name . args) ; todo: проверить используется ли где-то
      (print)
      (print "sending event " name " for " ship)
      (await
         (mail ship ['handle-ai name args]))
      (print "received answer")
      (print))
))