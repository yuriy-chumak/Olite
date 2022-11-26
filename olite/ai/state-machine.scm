; state-machine: state-machine
; name: entity

; a hack
(define (get-arity func)
   (case (type func)
      (type-bytecode
         (if (eq? (ref func 0) 11) ; JAF
            (-- (ref func 1))))
      (type-procedure
         (get-arity (ref func 1)))
      (type-closure
         (get-arity (ref func 1)))))

; handle state
(define (process-state state event default api args)
   (define handler (if (function? state) state (state event default)))
   (cond
      ((value? handler) handler)
      ((symbol? handler) handler)
      ((number? handler) handler)
      (else
         (define arity (get-arity handler))
         (case arity
            (0 (handler))
            (1 (handler api))
            (else (apply handler
               (cons* api (take args (-- arity)))))))))

; run new ai
(define (run state-machine functions)
   (define initial-state-name #false)
   (define initial-state {
      #false (state-machine #T 'main) ; конструктор, если есть
   })

   (actor ['ai] (lambda ()
      (let loop ((state initial-state)
                 (state-name initial-state-name)
                 (alarm 0))
         (define time (time-ms)) ; текущее время

         ; надо проверить входящие, если что-то произошло,
         ; то надо тут же дернуть ai (разбудить если спит)
         (define envelope (check-mail))
         ; если событий нет и время просыпаться не пришло:
         (if (and (not envelope) (< time alarm))
         then ; то ничего не делаем
            (sleep 0)
            (loop state state-name alarm)
         else ; иначе дергаем событие
            (vector-apply (if envelope (ref envelope 2) [#false #false #null]) (lambda (event-type event args)
               (define api (put functions 'event event)) ; сохраним ивент доступным для сырого анализа ai

               (define answer (process-state state event (lambda ()
                     (print-to stderr "Unhandeled event " event " for state " state-name ", sleep for 30 sec")
                     30000)
                  api args))
               (if (eq? event-type 'handle) (mail (ref envelope 1) 'ok)) ; если отправитель ждет ответа, отправим
               (let subloop ((state state) (state-name state-name) (answer answer))
                  (cond
                     ; надо сменить стейт? (можно на себя же)
                     ((symbol? answer)
                        (define new-state-name answer)
                        (define new-state (or
                           (state-machine answer #false)
                           (begin
                              (print-to stderr "No state " new-state-name " found.")
                              {})))

                        ; call the 'leave
                        (define ignorethis
                           (process-state state 'leave (lambda () #f) api new-state-name))
                        ; call the 'enter
                        (define new-answer
                           (process-state new-state 'enter (lambda () #f) api state-name))
                        ; change the state
                        (subloop new-state new-state-name new-answer))
                  ; спим
                  ((number? answer)
                     (loop state state-name (+ time (floor (exact answer)))))
                  (else
                     ; все остальные ответы мы игнорируем, будь то #false, #true или ":)"
                     (loop state state-name alarm)))))))))))

; todo: case-lmabda
(define run-ai run)
