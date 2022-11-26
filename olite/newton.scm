(define-library (olite newton) ; TODO: rename to physics
(import
   (otus lisp)
   (lib newton-dynamics)
   (olite math))

(export
   newtonian-world
   newtonian-world-update
   (exports
      (lib newton-dynamics))

   ship->userdata
   userdata->ship
)

(begin
   (define newtonian-world (or
      (NewtonCreate)
      (runtime-error "Can't create newtonian world" #f)))

   (print "NewtonGetMemoryUsed = " (NewtonGetMemoryUsed))
   (print "NewtonGetSolverIterations = " (NewtonGetSolverIterations newtonian-world))

   ; init
   ;(NewtonSetSolverIterations newtonian-world 4)
   ;(NewtonSetThreadsCount newtonian-world 1)

   (define ship->userdata vm:pin)
   (define userdata->ship vm:deref)

   (define (newtonian-world-update delta-t)
      (NewtonUpdate newtonian-world delta-t))


   (define Null (NewtonCreateNull newtonian-world))
   ;; (actor 'newton (lambda ()
   ;;    (let loop ()
   ;;       (let*((envelope (wait-mail))
   ;;             (sender msg envelope))
   ;;          (case msg
   ;;             (['update delta_t]
   ;;                (mail sender #true)
   ;;                (loop))
               
   ;;             (['disable body]
   ;;                (print "DISABLE")
   ;;                ;(NewtonBodySetCollidable body 0) ; не работает
   ;;                ;(NewtonBodySetCollision body Null)
   ;;                ;(NewtonBodySetSimulationState body 0) ; падает
   ;;                (print "DISABLE OK")
   ;;                (loop))

   ;;             ;; (['move body matrix]
   ;;             ;;    (NewtonBodySetMatrix body matrix)
   ;;             ;;    (loop))

   ;;             ;; (['hide body]
   ;;             ;;    (mail sender (this msg #false))
   ;;             ;;    (loop this id))
   ;;             ; TODO: возвращать весь список есть #true или что-то такое
   ;;             (else
   ;;                (loop)))))))

))