(define-library (olite ship)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      create-ship

      set-ship-position
      set-ship-orientation
      set-ship-hull
      set-ship-autopilot

      ;; add-ship-to-scene

      get-ship-position
   )

   (import
      (otus lisp)
      (olite math)
      (scheme dynamic-bindings)

      (olite ai)
      (olite hull)
      
      (olite newton)
      (lib newton-dynamics))

(begin
   (import (olite config))

   ;; ; *HULLS*
   ;; (import (olite hulls))

   ;; ; ----------------------------------------
   ;; ; ПОКА ЧТО у нас будет один список для ВСЕХ кораблей
   ;; ; потом можем сделать отдельные для каждой звездной системы
   ;; (define *SHIPS* (make-parameter {})) ; TODO: change to *SHIPS*

   ;; ; todo: enable and disable ship

   ;; (define (add-ship-to-scene ship)
   ;;    (*SHIPS* (put (*SHIPS*) ship #true)))

   ; == newton-dynamics ===================================================

   ; apply-gravity callback:
   ; (низкоуровневое управление кораблем)
   ; todo: перепроверить, исправить, доделать
   (define ApplyGravityCallback (NewtonApplyForceAndTorque
      (lambda (body timestep threadIndex)
         (define ship (userdata->ship (NewtonBodyGetUserData body)))
         (when ship
            (define this (await (mail ship 'this)))
            ;; (print "this: " this)

            ; обрабатываем управление только в полете
            (when (eq? (this 'state) 'flight)
               ; матрица транформации
               (define matrix [#i1 #i0 #i0 #i0
                               #i0 #i1 #i0 #i0
                               #i0 #i0 #i1 #i0
                               #i0 #i0 #i0 #i1])
               (NewtonBodyGetMatrix body matrix)

               ; желаемая скорость движения (считаем сами)
               ;  реальная скорость будет несколько меньше
               (define speed (this 'speed))
               (NewtonBodySetForce body
                  (vector-map (lambda (v) (* v speed))
                     (mat4*vec3 matrix [0 0 1])))

               ;; (define velocity '(#i0 #i0 #i0))
               ;; (NewtonBodyGetVelocity body velocity)

               ;; (print "speed: " (inexact speed))
               ;; (print "velocity: " (sqrt (+ (* (lref velocity 0) (lref velocity 0))
               ;;                              (* (lref velocity 1) (lref velocity 1))
               ;;                              (* (lref velocity 2) (lref velocity 2)))))

               ; ====================
               ; roll, pitch, and yaw
               ;; (define omega [#i0 #i0 #i0])
               ;; (NewtonBodyGetOmega body omega)
               ;; (if (eq? ship 'commander)
               ;;    (print omega))

               (define torgues [
                  (this 'pitch #i0)
                  (this 'yaw #i0)
                  (this 'roll #i0)
               ])
               ;(NewtonBodySetTorque body [0 0 0]) TODO: проверить не будет ли мешать после соударений
               (NewtonBodyAddTorque body (mat4*vec3 matrix torgues)))

               ; done.
               #true))))

   ; --------------------------------------------------------------------------------------------------
   ; пересечение бандинг-боксов
   ; перенести часть этого кода в OnSubShapeAABBOverlapTest (?)
   (define OnBodyAABBOverlap (NewtonOnAABBOverlap
      (lambda (contact timestep threadIndex)
         (call/cc (lambda (return)
            (define body1 (NewtonJointGetBody0 contact))
            (define body2 (NewtonJointGetBody1 contact))

            (define name1 (userdata->ship (NewtonBodyGetUserData body1)))
            (define name2 (userdata->ship (NewtonBodyGetUserData body2)))

            ;; (print (time-ms) ": OnBodyAABBOverlap " name1 " x " name2)

            (define this1 (await (mail name1 'this)))
            (define this2 (await (mail name2 'this)))

            ; 1. Пристыкованные корабли на столкновения не обрабатываем!
            ;    (воркераунд, так как NewtonBodySetSimulationState крашится,
            ;     а NewtonBodySetCollidable не работает)
            ; todo: пофиксить креш в newton-dynamics либе, сделать пул-реквест
            (when (or
                     (not (eq? (this1 'state) 'flight))
                     (not (eq? (this2 'state) 'flight)))
               (return 0))

            ; 2. Один из кораблей - станция? Возможен вход в docking-area...
            ;; (when (or (this1 'station #f) (this2 'station #f))
            ;;    (define hull1 (this1 'hull {}))
            ;;    (define hull2 (this2 'hull {}))

            ;;    ; вариант станция-станция обрабатывается как станция-корабль, кому первому повезет
            ;;    (define-values (station station-body station-hull
            ;;                   ship ship-body)
            ;;       (if (this1 'station #f)
            ;;          (values name1 body1 hull1 name2 body2)
            ;;          (values name2 body2 hull2 name1 body1)))

            ;;    ;; (define v [#i0 #i0 #i0])
            ;;    ;; (NewtonBodyGetPosition ship-body v)
            ;;    ;; (print "v: " v)

            ;;    ; вычислим ближайшее положение от нашего корабля к сфере стыковки
            ;;    (define matrix [#i1 #i0 #i0 #i0
            ;;                    #i0 #i1 #i0 #i0
            ;;                    #i0 #i0 #i1 #i0
            ;;                    #i0 #i0 #i0 #i1])
            ;;    (NewtonBodyGetMatrix station-body matrix)
            ;;    (define dock-point (matrix*vertex matrix (station-hull 'dock-point)))

            ;;    (NewtonBodyGetMatrix ship-body matrix)
            ;;    (define collision (NewtonBodyGetCollision ship-body))

            ;;    (define vertex [#i0 #i0 #i0])
            ;;    (define normal [#i0 #i0 #i0])
            ;;    (define calculated (NewtonCollisionPointDistance
            ;;       (NewtonBodyGetWorld station-body) dock-point
            ;;       (NewtonBodyGetCollision ship-body) matrix
            ;;       vertex normal 0)) ; normal нас не интересует
               
            ;;    ; да, мы смогли найти ближайшую точку к док-поинту
            ;;    (when calculated
            ;;       ;(define square-distance (vector-fold (lambda (s a b) (+ s (abs (- a b)))) 0 vertex dock-point))
            ;;       (define square-distance (vector-fold (lambda (s a b)
            ;;             (let ((ab (abs (- a b))))
            ;;                (+ s (* ab ab))))
            ;;          0 vertex dock-point))
            ;;       (define dock-radius (station-hull 'dock-radius))

            ;;       (when (< square-distance (* dock-radius dock-radius))
            ;;          ; ты смотри, таки задокались и не убились по дороге...
            ;;          (print "   dock-point: " dock-point)
            ;;          (print "   vertex: " vertex)
            ;;          (print "   normal: " normal)
            ;;          (print "   square-distance: " square-distance)
            ;;          (print "   dock-radius^2: " (* dock-radius dock-radius))

            ;;          ; сообщим станции что к ней пришвартовался кораблик
            ;;          ; (и подождем пока сообщение будет отработано!)
            ;;          (event-ai station 'dock ship)

            ;;          ; возвращаем 0 и уже не проверяем на столкновения
            ;;          (return 0))))

            ; 3. дальнейшие возможные проверки
            ; ...

            ; специальные случаи проверены, совпадений нет,
            ; продолжаем тестировать столкновения
            1)))))

   ; а нахера этот сабшейп?? хотя может тут проверять докинг?
   ; ААААААА. это как бы уточнение к OnBodyAABBOverlap выше!!
   ; todo: проверить
   (define OnSubShapeAABBOverlapTest (NewtonOnCompoundSubCollisionAABBOverlap
      (lambda (contact timestep body1 collisionNode1 body2 collisionNode2 threadIndex)
         (define name1 (userdata->ship (NewtonBodyGetUserData body1)))
         (define name2 (userdata->ship (NewtonBodyGetUserData body2)))
         ; что такое collisionNode1?

         ;(print (time-ms) ": OnSubShapeAABBOverlapTest   " name1 " x " name2)

         1)))

   ; сюда мы попадаем когда уже есть реальный контакт между шейпами
   ; этот колбек вызывается для каждого контакта между мешами!
   ; (а не один раз)
   (define OnContactCollision (NewtonContactsProcess
      (lambda (joint timestep threadIndex)
         (define body1 (NewtonJointGetBody0 joint))
         (define body2 (NewtonJointGetBody1 joint))

         (define name1 (userdata->ship (NewtonBodyGetUserData body1)))
         (define name2 (userdata->ship (NewtonBodyGetUserData body2)))

         ;(print (time-ms) ": OnContactCollision " name1 "->" name2)

         ; есть пересечение неких соллайдеров,
         ; давайте проверять каких и кем.
         (define-values (alert1 alert2 crash)
            (let cycle ((contact (NewtonContactJointGetFirstContact joint))
                        (alert1 #false) (alert2 #false) (crash #false))
               (if contact
               then
                  (define id1 (NewtonContactGetCollisionID0 contact))
                  (define id2 (NewtonContactGetCollisionID1 contact))
                  (define next (NewtonContactJointGetNextContact joint contact))
                  ;(print id1 " - " id2)

                  ;; setProximityAlert:
                  (cond
                     ; если столкнулись только сферы "интереса", то кораблики еще далековато
                     ;  друг от друга, игнорируем
                     ((and (eq? id1 PROXIMITY-SPHERE-ID) (eq? id2 PROXIMITY-SPHERE-ID))
                        (NewtonContactJointRemoveContact joint contact)
                        (cycle next alert1 alert2 crash))

                     ; кто-то вошел в опасную зону первого
                     ((eq? id1 PROXIMITY-SPHERE-ID)
                        (NewtonContactJointRemoveContact joint contact)
                        (cycle next #true alert2 crash))

                     ; кто-то вошел в опасную зону второго
                     ((eq? id2 PROXIMITY-SPHERE-ID)
                        (NewtonContactJointRemoveContact joint contact)
                        (cycle next alert1 #true crash))

                     ; todo: док?

                     ; неопознанный контакт, видимо настоящее столкновение
                     (else
                        (cycle next alert1 alert2 #true)))
               else
                  (values alert1 alert2 crash))))

         ; сообщим об опасности столкновения, если есть
         (if alert1
            (mail name1 ['add-proximity-alert name2]))
         (if alert2
            (mail name2 ['add-proximity-alert name1]))

         ; и финальным аккордом,
         ; todo: послать обоим столкнувшимся "соударение"
         (if crash
            (print "crash!")
            #true))))

   ; жаль, эту функцию нельзя использовать так как мы не умеем из лиспа
   ; генерировать contactBuffer из NewtonUserContactPoint
   ; todo: проапгрейдить newton-dynamics библиотеку, создать pull-request

   ;; (define OnContactGeneration (NewtonOnContactGeneration
   ;;    (lambda (material body1 collision1 body2 collision3 contactBuffer maxCount threadIndex)
   ;;       (define name1 (userdata->ship (NewtonBodyGetUserData body1)))
   ;;       (define name2 (userdata->ship (NewtonBodyGetUserData body2)))

   ;;       (print (time-ms) ": OnContactGeneration   " name1 " x " name2)

   ;;       ;NewtonCollisionGetUserID (we can add unique ID to the collision)
   ;;       ; и это надо сделать для док станции и для всех сфер корабликов (?)

   ;;       ; если мы пересекаем сферу "интереса" кораблика, то надо вернуть 0
   ;;       ; и сообщить кораблику что "чувак, ты в опасносте"
   ;;       ; иначе сложная вещь:
   ;;       dExcavatorModel* const excavator = (dExcavatorModel*)NewtonCollisionGetUserData(collision0);
   ;;       dAssert (excavator);
   ;;       return excavator->CollideLink(material, body0, body1, contactBuffer);

   ;;       1)))

   ; парочка служебных объектов:
   (define sphere-collision
      (NewtonCreateSphere newtonian-world  0.5  0  #f)) ; todo: проверить размеры

   (define sphere-collision-big
      (NewtonCreateSphere newtonian-world  200  0  #f)) ; todo: проверить размеры

   (define null-collision
      (NewtonCreateNull newtonian-world))

   (define defaultMaterialID (NewtonMaterialGetDefaultGroupID newtonian-world))

   ; подключаем логику обработки столкновений
   ; по шагам:
   ;  1. OnBodyAABBOverlap, if ok
   ;  2. OnSubShapeAABBOverlapTest, if ok
   ;  3. OnContactGeneration
   (NewtonMaterialSetCollisionCallback newtonian-world defaultMaterialID defaultMaterialID OnBodyAABBOverlap OnContactCollision)
   (NewtonMaterialSetCompoundCollisionCallback newtonian-world defaultMaterialID defaultMaterialID OnSubShapeAABBOverlapTest) ; уточнение к выше
   ; эту не подключаем, todo: проверить
   ;; (NewtonMaterialSetContactGenerationCallback newtonian-world defaultMaterialID defaultMaterialID OnContactGeneration)

   ; --------------------------------------------------------------------------------------------------
   ; рейкаст стрелялки лазером
   (define RayCastFilter (NewtonWorldRayFilterCallback
      (lambda (body shapeHit hitContact hitNormal collisionID userData intersectParam)
         (define ship (userdata->ship (NewtonBodyGetUserData body)))
         (define self (userdata->ship userData))

         (print self " -> " ship "   :" intersectParam)
         (mail ship ['hit self]) ; todo: заменить на эвент для ai
         intersectParam)))

   ; ---------------------------------------------------------------------------------------------
   ; математика корабликов (todo: проверить, переделать)
   ;; (define (TrackDestination this destination options)
   ;;    (define body (this 'body))

   ;;    (define matrix [#i1 #i0 #i0 #i0
   ;;                    #i0 #i1 #i0 #i0
   ;;                    #i0 #i0 #i1 #i0
   ;;                    #i0 #i0 #i0 #i1])
   ;;    (NewtonBodyGetMatrix body matrix)

   ;;    (define position [#i0 #i0 #i0])
   ;;    (NewtonBodyGetPosition body position)

   ;;    ; options
   ;;    (define retreat (options 'retreat #f))
   ;;    (define pitching_over (options 'pitching_over #f))

   ;;    ; ?? _destination = OOHPVectorInterpolate(our position, their position, 0.5);
   ;;    ; ?? desired_range = prox_ship->collision_radius * PROXIMITY_AVOID_DISTANCE_FACTOR;
   ;;    ; trackDestination
   ;;    ;; desired_range = prox_ship->collision_radius * PROXIMITY_AVOID_DISTANCE_FACTOR;
   ;;    ;; _destination = prox_ship->position;

   ;;    ;double dq = [self trackDestination:delta_t:YES]; // returns 0 when heading towards prox_ship
   ;;    ;; Heading towards target with desired_speed > 0, avoids collisions better than setting desired_speed to zero. ??
   ;;    ;; (tested with boa class cruiser on collisioncourse with buoy) ??
   ;;    ; desired_speed = maxFlightSpeed * (0.5 * dq + 0.5);

   ;;    ; todo: оформить отдельно функцией
   ;;    ; -----------------------------------
   ;;    ; trackDestination: (большая функция)
   ;;    ;; double reversePlayer = 1.0;

   ;;    (define min_d   0.004)
   ;;    (define MAX_COS 0.995)
   ;;    (define MAX_COS2 (* MAX_COS MAX_COS))
   ;;    (define max_cos MAX_COS) ; should match default value of max_cos in behaviour_fly_to_destination!

   ;;    (define +min_d min_d)
   ;;    (define -min_d (negate min_d))
   ;;    (define -max_cos (negate max_cos))
   ;;    ;; double precision = we_are_docking ? 0.25 : 0.9025; // lower values force a direction closer to the target. (resp. 50% and 95% within range)

   ;;    ;(define retreat #true)        ; TODO! use as parameter! (хотим ли мы лететь ОТ цели или К цели)
   ;;    ;(define pitching_over #true)  ; TODO! use as parameter!

   ;;    (define reverse (if retreat -1 1)) ; deprecated

   ;;    (define sign+ (if retreat negate idf))
   ;;    (define sign- (if retreat idf negate))
   ;;    ;; if (isPlayer) ???
   ;;    ;; {
   ;;    ;;    reverse = -reverse;
   ;;    ;;    reversePlayer = -1;
   ;;    ;; }


   ;;    ;; (define range2 distance2)

   ;;    ;; ????????????????????????????
   ;;    ;; if (range2 > desired_range2)
   ;;    ;; {
   ;;    ;;    max_cos = sqrt(1 - precision * desired_range2/range2);  // Head for a point within 95% of desired_range.
   ;;    ;;    if (max_cos >= 0.99999)
   ;;    ;;    {
   ;;    ;;       max_cos = 0.99999;
   ;;    ;;    }
   ;;    ;; }

   ;;    (define rel-pos (vector-vector destination position))
   ;;    ;(define rel-pos-norm (if (equal? rel-pos [#i0 #i0 #i0]) [#i0 #i0 #i1] (normalize rel-pos)))
   ;;    (define rel-pos-norm (normalize rel-pos))

   ;;    (define d_right   (dot-product rel-pos-norm (matrix*vector matrix [1 0 0]))) ;v_right
   ;;    (define d_up      (dot-product rel-pos-norm (matrix*vector matrix [0 1 0]))) ;v_up
   ;;    (define d_forward (dot-product rel-pos-norm (matrix*vector matrix [0 0 1]))) ;v_forward);	// == cos of angle between v_forward and vector to target

   ;;    (define hull (this 'hull {}))

   ;;    (define max_flight_pitch (hull 'max_flight_pitch))
   ;;    (define max_flight_roll (hull 'max_flight_roll))
   ;;    (define max_flight_yaw max_flight_pitch) ; unused
   ;;    (define max_flight_speed (hull 'max_flight_speed))

   ;;    (define-values (roll pitch)
   ;;       ;; check if we are flying toward (or away from) the destination..
   ;;       (if (or (< d_forward max_cos) retreat)

   ;;          (let*(; hack to avoid just flying away from the destination
   ;;                (d_up (if (or (<= d_forward -max_cos) (and retreat (>= d_forward max_cos)))
   ;;                         (* min_d 2.0)
   ;;                         d_up))
   ;;                (stick_roll (or
   ;;                         (when (> d_up +min_d)
   ;;                            (define factor (min 8 (sqrt (abs (/ d_right min_d)))))
   ;;                            (cond
   ;;                               ((> d_right +min_d)
   ;;                                  (- (* max_flight_roll 1/8 factor))) ; - max_flight_roll * reversePlayer * 0.125 * factor
   ;;                               ((< d_right -min_d)
   ;;                                  (+ (* max_flight_roll 1/8 factor))) ; + max_flight_roll * reversePlayer * 0.125 * factor
   ;;                               ;if (fabs(d_right) < fabs(stick_roll) * delta_t) 
   ;;                               ; stick_roll = fabs(d_right) / delta_t * (stick_roll<0 ? -1 : 1);
   ;;                            ))
   ;;                         (when (< d_up -min_d)
   ;;                            (define factor (min 8 (sqrt (abs (/ d_right min_d)))))
   ;;                            (cond
   ;;                               ((> d_right +min_d)
   ;;                                  (+ (* max_flight_roll 1/8 factor)))
   ;;                               ((< d_right -min_d)
   ;;                                  (- (* max_flight_roll 1/8 factor)))
   ;;                            ))
   ;;                         0.00))
                  
   ;;                (stick_pitch (or
   ;;                         (when (zero? stick_roll)
   ;;                            (define factor (min 8 (sqrt (/ (abs d_up) (abs min_d)))))
   ;;                            (cond
   ;;                               ((> d_up +min_d)
   ;;                                  (sign- (* max_flight_pitch 1/8 factor)))
   ;;                               ((< d_up -min_d)
   ;;                                  (sign+ (* max_flight_pitch 1/8 factor))) ))
   ;;                         ; pitching-over: резкий тангаж, даже если крен еще не выставлен.
   ;;                         ;  отрабатываем, если преграда впереди по курсу
   ;;                         (when (and
   ;;                                  pitching_over
   ;;                                  (< (sign+ d_forward) 0.707))
   ;;                            (cond
   ;;                               ((> d_up 0)
   ;;                                  (sign- max_flight_pitch))
   ;;                               ((< d_up 0)
   ;;                                  (sign+ max_flight_pitch)) ))
   ;;                         ;; not sufficiently on course yet, but min_d is too high
   ;;                         ;; turn anyway slightly to adjust
   ;;                         0.01))
   ;;                ;; (_ (print "stick_pitch: " stick_pitch))
   ;;                ; ...
   ;;                ;; if (we_are_docking && docking_match_rotation && (d_forward > max_cos))
   ;;                ;; {
   ;;                ;;    /* we are docking and need to consider the rotation/orientation of the docking port */
   ;;                ;;    StationEntity* station_for_docking = (StationEntity*)[self targetStation];

   ;;                ;;    if ((station_for_docking)&&(station_for_docking->isStation))
   ;;                ;;    {
   ;;                ;;       stick_roll = [self rollToMatchUp:[station_for_docking portUpVectorForShip:self] rotating:[station_for_docking flightRoll]];
   ;;                ;;    }
   ;;                ;; }
   ;;                (ok #true))
   ;;             (values stick_roll stick_pitch))
   ;;       else
   ;;          (values #i0 #i0)))

   ;;    (define dq (let*(
   ;;          (d_forward (if retreat (* d_forward d_forward) d_forward))
   ;;          (d_forward (max d_forward #i0)))
   ;;       (if (and (zero? roll) (zero? pitch)) #i1 d_forward)))

   ;;    ; applySticks: todo, тоже сделать отдельной функцией
   ;;    (values roll pitch
   ;;       (* max_flight_speed (+ (/ dq 2) 1/2))))

   ; ---------------------------------------------------------------------------------------------
   ; ------------------------------
   ; создать новый именной кораблик
   (define (create-ship name) ; todo: а может ship:new ?
      ; матмодель newton-dynamics
      (define body (NewtonCreateDynamicBody newtonian-world #false
            '(;x y z w
               1 0 0 0 ; front
               0 1 0 0 ; up
               0 0 1 0 ; right
               0 0 0 1)))

      ; this
      (define defaults {
         ; обязательные поля, используемые функциями и т.д.
         'name name ; actor, уникальное имя корабля
         'body body ; newtonian body

         ;; 'location      [#i0 #i0 #i0] ; позиция в мировых координатах, метры
         ;; 'rotation  [#i1 #i0 #i0 #i0] ; кватернион вращения

         'state 'flight ; текущее глобальное состояние (todo: сменить дефолтное состояние на что-то другое)
         'model 'nonenonenone2 ; модель для рендеринга (десериализуем из 'hull в целях оптимизации доступа)
         'visible #true ; видимость

         ; управление кораблем
         'speed #i0 ; 0 .. maximal-speed
         'roll #i0 'pitch #i0 'yaw #i0 ; -maximal .. maximal

         'ai (dumb-ai) ; state machine (по умолчанию ничего не делающая)
         'autopilot #true
         'behaviour 'idle  ; текущее поведение корабля:
                           ; 'stop-still 'idle 'tumble 'tractored 'track_target 'intercept_target
                           ; 'attack_target 'attack_slow_dogfight 'evasive_action 'attack_break_off_target
                           ; 'fly_to_target_six 'attack_mining_target 'attack_fly_to_target 'attack_fly_from_target
                           ; 'running_defense 'flee_target 'attack_broadside 'attack_broadside_left
                           ; 'attack_broadside_right 'close_to_broadside_range 'close_with_target
                           ; 'attack_broadside_target 'attack_sniper 'fly_range_from_destination
                           ; 'face_destination 'land_on_planet 'formation_form_up 'fly_to_destination
                           ; 'fly_from_destination 'avoid_collision 'track_as_turret 'fly_thru_navpoints 'scripted_ai
         ;; 'proximity-ship #false  ; угрожающий столкновением корабль
         ;; 'primary-target #false  ; основная цель: корабль или точка в пространстве

         'saved-behaviours #false ; предыдущее поведение, todo: организовать стеком?


         ; TODO: установить какие-то дурацкие минимальные настройки
         'hull {
            'name "Cobra Mark III"
            'model "Battackbomber"
            'max-flight-speed #i70

            'max_flight_roll #i2
         }
      })

      ; --------------------------------------------------
      ; сервисный API кораблика
      (define functions {
         ; актор, чтобы можно было с ним общаться напрямую
         'name name

         ; =======================================
         ; ориентирование в мире
         ;
         ; todo: определиться, position/orientation или location/rotation ?
         ;; ; получить положение в пространстве
         ;; 'position (lambda ()
         ;;       (await (mail name 'location))) ; свое положение
         ;; 'ship-position (lambda (name)
         ;;       (await (mail name 'location))) ; положение (другого) корабля

         ;; ; получить ориентацию в пространстве
         ;; 'orientation (lambda ()
         ;;       (await (mail name 'rotation)))
         ;; 'ship-orientation (lambda (name)
         ;;       (await (mail name 'rotation)))

         ;; ; получить пространственную матрицу положение/лриентация 
         ;; 'matrix (lambda ()
         ;;       (await (mail name 'matrix)))
         ;; 'ship-matrix (lambda (name)
         ;;       (await (mail name 'matrix)))

         ; =======================================
         ; модификаторы
         ;
         ;; ;; ; поместить актора по координатам x,y,z
         ;; ;; 'locate (lambda (x y z)
         ;; ;;    (mail name ['set 'location [x y z]]))
         ;; ;; 'rotate (lambda (a b c) ; временная функция, для дебага
         ;; ;;    (mail name ['set 'rotation [#i1 a b c]]))
         ;; ;...
         ;; ;; 'teleport (lambda (x y z) ; TODO: rename!!
         ;; ;;    (mail name ['moveto [x y z]]))

         ;; ; назначить новый корпус
         ;; ; TODO: change to "set-hull"
         ;; 'set-hull (lambda (hullname)
         ;;    (mail name ['set-hull (string->symbol hullname)]))

         ; =======================================
         ; генераторы
         ;
         ;; ; создать новый кораблик (станция будет запускать, например, паки полицейских)
         ;; 'create-ship (lambda (name hull ai)
         ;;    (define ship (create-ship name))
         ;;    ; назначим корпус и запустим ai
         ;;    (mail ship ['set-hull (string->symbol hull)])
         ;;    (mail ship ['run (*AI* ai)])
         ;;    ; все, можно рисовать
         ;;    (add-ship-to-scene ship) ; todo: сделать это автоматическим, но манипулировать флажком 'visible

         ;;    ship)

         ; =======================================
         ; целеполагание
         ;
         ; установить текущее поведение
         'set-behaviour (lambda (behaviour)
            (mail name ['set-behaviour behaviour])
            #false)
         ; save previous behaviour
         'push-behaviour (lambda ()
            (mail name ['push-behaviour])
            #false)
         ; restore saved behaviour or
         'pop-behaviour-or (lambda (default)
            (mail name ['pop-behaviour-or default])
            #false)

         ;; ; задать новую цель
         ;; 'add-target (lambda (target) ; addTarget
         ;;    (unless (eq? target name)
         ;;       (mail name ['set-primary-target target])
         ;;       #false))
         ;; ;; removeTarget
         ;; 'set-target-station (lambda (station) ; setTargetStation
         ;;    ; todo: assert target is station
         ;;    (mail name ['set-target-station station])
         ;;    #false)
         ;; ;; setTargetToSystemStation
         ;; ;; setTargetToNearestStation
         ;; ;; setTargetToNearestFriendlyStation
         ;; ;; primaryTarget
         ;; ;; primaryTargetWithoutValidityCheck ;?

      })

      ; пусть все наши кораблики будут иметь сферический момент инерции (временно)
      (NewtonBodySetMassProperties body 1 sphere-collision)
      (NewtonBodySetForceAndTorqueCallback body ApplyGravityCallback)

      ; интернируем себя для доступа из newton-dynamics
      (NewtonBodySetUserData body (ship->userdata name))

      ; глобальное "воздушно-безвоздушное" трение, препятствует движению без прикладывания сил
      ;  default: 0.1
      ;  todo: брать из настроек худа корабля
      (NewtonBodySetLinearDamping body DEFAULT-LINEAR-DAMPING)
      (NewtonBodySetAngularDamping body DEFAULT-ANGULAR-DAMPING)

      ; ---------------------------------------------------
      ; -=( космический корабль с ai )=-----------------------------------------
      (actor name (lambda ()
         (let loop ((this defaults))
            (let*((envelope (wait-mail))
                  (sender msg envelope))
               ;; (print name ": " envelope)

               ; internal functions
               (define (notify-ai event args)
                  (mail (this 'ai) ['notify event args]))
               (define (handle-ai ev args) ; event and wait
                  (await (mail (this 'ai) ['handle ev args])))
               (define (update ff) ; todo: remove
                  (if ff
                     (ff-replace this ff)
                     this))
               (define (reloop ff) ; todo: remove
                  (loop (ff-replace this ff)))
               (define continue reloop)

               ;(print "actor " name " <- " msg " from " sender)
               (case msg
                  ('this ; оптимизация - возвращает все параметры пачкой
                         ;               не позволяет их менять, так что безопасно
                     (mail sender this)
                     (loop this))

                  (['set key value] ; Временный хак. TODO: удалить.
                     (loop (put this key value)))

                  ; -=( ai )=---------
                  ; запустить новый ai
                  ; старый ai убить с помощью kill
                  ; TODO: старый ai поставить на паузу и сохранить в стеке (?)
                  (['run ai]
                     (define oldai (this 'ai))
                     (when oldai (kill oldai)) ; стопнем old ai
                     (loop
                        (put this 'ai (run-ai ai functions))))

                  ; ретрансляция сообщений к ai корабля:
                  ; (сам ai инкапсулирован и недоступен)
                  (['notify-ai event args]
                     (notify-ai event args))
                  (['handle-ai name args]
                     (mail sender (await
                        (mail (this 'ai) ['handle name args])))
                     (loop this))

                  ;; (['die] ; умереть
                  ;;    (define oldai (this 'ai))
                  ;;    (when oldai (kill oldai))
                  ;;    (NewtonDestroyBody body)
                  ;;    (*SHIPS* (del (*SHIPS*) name))) ; ?

                  ; ------------------------------------------
                  ; movements
                  ; ... NewtonApplyForceAndTorque
                  ; ... NewtonSetTransform
                  ;
                  ; ... NewtonBodyGetSimulationState
                  ; ... NewtonBodyAddForce
                  ; ... NewtonBodyAddTorque
                  ; ... NewtonBodySetCentreOfMass
                  ; ... NewtonBodySetMassMatrix
                  ; ... NewtonBodySetFullMassMatrix
                  ; ...

                  ; ... NewtonBodySetVelocity
                  ; ... NewtonBodySetForce (use in NewtonApplyForceAndTorque callback)
                  ; ... NewtonBodySetTorque

                  ; ... NewtonBodySetCollision
                  ; ... NewtonBodySetUserData
                  ;
                  ; ... NewtonBodyGetPosition
                  ; ... NewtonBodyGetRotation
                  ; ... NewtonBodyGetMass
                  ; ... NewtonBodyGetVelocity
                  ; ... NewtonBodyGetAcceleration
                  ; ... NewtonBodyGetForce
                  ; ... NewtonBodyGetTorque
                  ; ... NewtonBodyGetPointVelocity
                  ; ... 

                  ; можно вручную погонять тест пересечения: NewtonCollisionIntersectionTest

                  ;; (['moveto xyz]
                  ;;    (print "*** OBSOLETE *** use 'set-position!")
                  ;;    (vector-apply xyz (lambda (x y z)
                  ;;       (define matrix '(#i1 #i0 #i0 #i0
                  ;;                        #i0 #i1 #i0 #i0
                  ;;                        #i0 #i0 #i1 #i0
                  ;;                        #i0 #i0 #i0 #i1))
                  ;;       (NewtonBodyGetMatrix body matrix)
                  ;;       (NewtonBodySetMatrix body (append
                  ;;          (take matrix 12)
                  ;;          (list x y z 1)))))
                  ;;    (loop this))

                  (['set-position xyz]
                     ;; (print name "> set-position: x,y,z: " (vector-map inexact xyz))
                     (vector-apply xyz (lambda (x y z)
                        (define matrix (zero-matrix))
                        (NewtonBodyGetMatrix body matrix)
                        (NewtonBodySetMatrix body (append
                           (take (vector->list matrix) 12)
                           (list x y z 1)))))
                     (loop this))

                  (['set-orientation rot]
                     (define matrix (rotation-matrix rot))
                     (define position (zero-vector))
                     (NewtonBodyGetPosition body position)
                     (vector-apply position (lambda (x y z)
                        (NewtonBodySetMatrix body (append
                              (take (vector->list matrix) 12)
                              (list x y z 1)))))
                     (loop this))

                  ; set-rotation ; todo?: set-orientation ?
                  ; todo: ['rotate a b c w]
                  ; ...
                  ; set-position/rotation
                  ; ...

                  ; --------------------------------------
                  ; -=( GETters )=------------------------
                  ; geometry:
                  ('matrix
                     ;; (define matrix (zero-matrix)) ; оптимизация: использовать просто [.....]
                     (define matrix [#i1 #i0 #i0 #i0
                                     #i0 #i1 #i0 #i0
                                     #i0 #i0 #i1 #i0
                                     #i0 #i0 #i0 #i1])
                     (NewtonBodyGetMatrix body matrix)
                     (mail sender matrix) (loop this))
                  ('location ; current location
                     ;(define position (zero-vector))
                     (define position [#i0 #i0 #i0])
                     (NewtonBodyGetPosition body position)
                     (mail sender position) (loop this))
                  ('rotation ; current rotation
                     ;(define rotation (zero-quaternion))
                     (define rotation [#i0 #i0 #i0 #i1])
                     (NewtonBodyGetRotation body rotation)
                     (mail sender rotation) (loop this))
                  ('velocity ; current velocity
                     (define velocity (zero-vector))
                     (NewtonBodyGetVelocity body velocity)
                     (mail sender velocity) (loop this))

                  ; compatibility api
                  ('position     ; same as 'location
                     (define position (zero-vector))
                     (NewtonBodyGetPosition body position)
                     (mail sender position) (loop this))
                  ('orientation  ; same as 'rotation
                     (define rotation (zero-quaternion))
                     (NewtonBodyGetRotation body rotation)
                     (mail sender rotation) (loop this))

                  ; текущее глобальное состояние корабля
                  ('state
                     (mail sender (this 'state 0))
                     (loop this))
                  ; корпус (что рендерим, как летаем, etc.)
                  ('hull
                     (mail sender (this 'hull {}))
                     (loop this))
                  ; желаемая скорость передвижения
                  ('speed
                     (mail sender (this 'speed 0))
                     (loop this))


                  ; -------
                  ;; ('think
                  ;;    (loop this))

                  ;; ('show
                  ;;    (NewtonBodySetCollidable body 1)
                  ;;    (loop (put this 'visible #true)))
                  ;; ('hide
                  ;;    (NewtonBodySetCollidable body 0)
                  ;;    (loop (put this 'visible #false)))

                  ;; ('die2
                  ;;    (loop (ff-replace this {
                  ;;       'state 'dead ; die!!
                  ;;    })))

                  ;; ; TODO: временный код, устарел, удалить(?)
                  ;; (['dock station]
                  ;;    (print "DOCK")
                  ;;    ; отключим обработку столкновений (вовне потока проверки столкновений)
                  ;;    ; это надо сделать вовне цикла проверки столкновений

                  ;;    ; Стыкуем:
                  ;;    ; todo: забрать в (mail 'newton ['dock body station])
                  ;;    (NewtonBodySetMatrix body (await (mail station 'matrix)))
                  ;;    (NewtonBodySetVelocity body [0 0 0])
                  ;;    (NewtonBodySetForce body [0 0 0])
                  ;;    (NewtonBodySetTorque body [0 0 0])

                  ;;    ; и сообщим станции, что мы пристыковались
                  ;;    ;(mail station ['docked name])

                  ;;    ; перейдем в состояние "пристыкован"
                  ;;    (loop (ff-replace this {
                  ;;       'state 'docked
                  ;;       'docked-in station ; где пристыковались
                  ;;       'visible #false

                  ;;       'speed 0
                  ;;    })))

                  ;; ('undock ;launch
                  ;;    ; переместить кораблик в точку андокинга,
                  ;;    ; задать ему правильную скорость
                  ;;    ; перейти в состояние "вылетаю"
                  ;;    ; todo: перенести эту логику в ai космической станции
                  ;;    (print "UNDOCK from " (this 'docked-in))

                  ;;    (define matrix (await (mail (this 'docked-in) 'matrix)))
                  ;;    ; todo: добавить такое же вращение кораблю, как и у станции

                  ;;    (define station (await (mail (this 'docked-in) 'this)))
                  ;;    (define undock-point ((station 'hull) 'undock-point))

                  ;;    ; переместим кораблик в положение для андокинга
                  ;;    (vector-apply (matrix*vertex matrix undock-point) (lambda (x y z)
                  ;;       (NewtonBodySetMatrix body (append
                  ;;          (take (vector->list matrix) 12)
                  ;;          (list x y z 1)))))

                  ;;    ; set 'speed to 80% of max
                  ;;    (loop (ff-replace this {
                  ;;       'state 'flight
                  ;;       'docked-in #false
                  ;;       'visible #true

                  ;;       'speed (* #i0.5 ((this 'hull {}) 'max_flight_speed #i100)) ; 50% of the max speed

                  ;;    })))

                  ; ========================================
                  ; hull
                  ; назначить новый корпус
                  ; todo: перепроверить
                  (['set-hull hullname]
                     (define hull (load-hull hullname (defaults 'hull)))
                     ;; (define modelname (string->symbol (hull 'model ((default 'hull) 'model))))
                     (define mass 1)

                     ;; (define model (get-model modelname))
                     ;; (define collision (ref model 3))
                     ;; (NewtonBodySetCollision body collision)
                     ;(NewtonBodySetMassProperties body 1 collision)

                     ; конфигурируем управляемость:
                     (NewtonBodySetLinearDamping body (hull 'linear-damping DEFAULT-LINEAR-DAMPING))
                     (NewtonBodySetAngularDamping body (hull 'angular-damping DEFAULT-ANGULAR-DAMPING))

                     ; TODO: добавить объект стыковки коллайдером
                     ; 1. высислим базовую сферу-коллайдер
                     (define AA [#i0 #i0 #i0])
                     (define BB [#i0 #i0 #i0])
                     (NewtonBodyGetAABB body AA BB)

                     ; оставим сообщение ai что мы меняем hull
                     ;; (let ((ai (this 'ai)))
                     ;;    (if ai
                     ;;       (mail ai 'hull-changed))) ; todo?: rename to hull-assigned
                     ; готово.
                     (loop (ff-replace this {
                        'hull hull ; установим новый корпус
                        'station (hull 'station #false) ; флажок орбитальной станции

                        ;; ; и зададим модель для отрисовки (оптимизация)
                        ;; 'model model
                     })))

                  ; ручное управление кораблем
                  ; todo: на скрине скорости показывать полоской желаемую и прямоугольничком текущую
                  (['manual-control pitch_back pitch_forward
                                    roll_left  roll_right
                                    yaw_left   yaw_right
                                    increase_speed decrease_speed]
                     (define hull (this 'hull {}))

                     (define max_flight_pitch (hull 'max_flight_pitch #i0.1))
                     (define max_flight_roll (hull 'max_flight_roll #i0.2))
                     (define max_flight_yaw max_flight_pitch)

                     ; триггеры крен/тангаж/рыскание
                     (loop (ff-replace this {
                        'roll (* (+ (if roll_right 1 0)
                                    (if roll_left -1 0))
                                 max_flight_roll)

                        'pitch(* (+ (if pitch_forward 1 0)
                                    (if pitch_back -1 0))
                                 max_flight_pitch)

                        'yaw  (* (+ (if yaw_right 1 0)
                                    (if yaw_left -1 0))
                                 max_flight_yaw)
                     })))

                  ; попытка добиться желаемой скорости (todo: переделать)
                  (['boost-slowdown delta_t increase_speed decrease_speed]
                     (define hull (this 'hull {}))
                     (define max_flight_speed (hull 'max_flight_speed #i140))
                     (define thrust (hull 'thrust #i5))

                     (loop (ff-replace this {
                        'speed (minmax
                                 (+ (this 'speed)
                                    (* (+ (if increase_speed 1 0)
                                          (if decrease_speed -1 0))
                                       thrust delta_t
                                       (/ max_flight_speed 100)))
                                 0 max_flight_speed)
                     })))

                  ;; ; пришла команда стрелять
                  ;; (['fire arg1]
                  ;;    (define matrix [#i1 #i0 #i0 #i0
                  ;;                    #i0 #i1 #i0 #i0
                  ;;                    #i0 #i0 #i1 #i0
                  ;;                    #i0 #i0 #i0 #i1])
                  ;;    (NewtonBodyGetMatrix body matrix)

                  ;;    (define userdata (vm:pin name))
                  ;;    (NewtonWorldRayCast newtonian-world
                  ;;       (matrix*vertex matrix [0 -3 5]) ; laser start (todo: get from hull)
                  ;;       (matrix*vertex matrix [0 0 300]) ; laser end (todo: get from hull)
                  ;;       RayCastFilter userdata
                  ;;       #false 0)
                  ;;    (vm:unpin userdata)
                  ;;    (loop (update {
                  ;;       'laser (time-ms)
                  ;;       'laser-from (matrix*vertex matrix [0 -3 5])
                  ;;       'laser-to (matrix*vertex matrix [0 0 300])
                  ;;    })))

                  ;; ; takeDamage
                  ;; ; takeHeatDamage
                  ;; ; takeScrapeDamage

                  ;; (['hit from]
                  ;;    (mail name ['die])
                  ;;    (loop this))

                  ;; ; рендерер (TODO: забрать отдельно!)
                  ;; ('draw
                  ;;    (define matrix [#i1 #i0 #i0 #i0
                  ;;                    #i0 #i1 #i0 #i0
                  ;;                    #i0 #i0 #i1 #i0
                  ;;                    #i0 #i0 #i0 #i1])
                  ;;    (NewtonBodyGetMatrix body matrix)
                  ;;    ; рисуем все, что не корпус корабля (ракеты, лазер, и т.д.)

                  ;;    (glDisable GL_TEXTURE_2D)

                  ;;    ; луч лазера
                  ;;    (define now (time-ms))
                  ;;    (define old (this 'laser 0))
                  ;;    (define delta (- now old))
                  ;;    (when (< delta 500) ; менее секунды
                  ;;       (define q (- 1 (/ delta 500)))
                  ;;       (define from (this 'laser-from)) ; todo: get from hull and laser
                  ;;       (define to (this 'laser-to)) ; todo: get from hull and laser
                  ;;       (glColor3f q q 0.2)
                  ;;       (glBegin GL_LINES)
                  ;;          (glVertex3fv from)
                  ;;          (glVertex3fv to)
                  ;;       (glEnd))

                  ;;    ; ok.
                  ;;    (mail sender 'ok)
                  ;;    (loop this))

                  ;; ;; целеполагание
                  ;; ; addTarget
                  ;; (['set-primary-target target]
                  ;;    ;set primaryTarget to target
                  ;;    ;startTrackingCurve/calculateTrackingCurve/updateTrackingCurve
                  ;;    ; todo: установить цель и для всех подчиненных кораблей/модулей
                     
                  ;;    ; сообщить ИИ что цель установлена (только для себя, не для подчиненных модулей)
                  ;;    (notify-ai 'ship-target-acquired target)
                  ;;    (loop (update {
                  ;;       'primary-target target
                  ;;    })))
                  ;; ; setTargetStation
                  ;; (['set-target-station station]
                  ;;    (loop (update {
                  ;;       'target-station station
                  ;;    })))
                  ;; ; setProximityAlert
                  ;; (['add-proximity-alert target]
                  ;;    ; тут целая пачка условий, которые мы скипаем.
                  ;;    ; и если ничего не скипнули - сообщаем стейт-машине
                  ;;    (notify-ai 'proximity-alert target)
                  ;;    (loop (update {
                  ;;       'proximity-ship target
                  ;;    })))
                     

                  ;; автопилот
                  (['set-autopilot flag]
                     ;(notify-ai 'autopilot-changed flag) ;?
                     (loop (ff-replace this {
                        'autopilot flag
                     })))
                  
                  ;; (['set-behaviour behaviour]
                  ;;    ; поведение корабля может измениться в процессе
                  ;;    (unless (eq? behaviour (this 'behaviour))
                  ;;       ; поэтому стоит об этом сообщить ai
                  ;;       (notify-ai 'behaviour-changed behaviour (this 'behaviour)))
                  ;;    (loop (ff-replace this {
                  ;;       'behaviour behaviour
                  ;;    })))
                  ;; (['push-behaviour]
                  ;;    (loop (update {
                  ;;       'saved-behaviours (cons {
                  ;;          'behaviour (this 'behaviour)
                  ;;          'primary-target (this 'primary-target)
                  ;;       } (this 'saved-behaviours))
                  ;;    })))
                  ;; (['pop-behaviour-or default]
                  ;;    (define saved-behaviours (this 'saved-behaviours #false))
                  ;;    (if saved-behaviours
                  ;;       (loop (update
                  ;;          (ff-union (lambda (a b) b)
                  ;;             (car saved-behaviours)
                  ;;             {
                  ;;                'saved-behaviours (cdr saved-behaviours)
                  ;;             })))
                  ;;    else
                  ;;       (loop (update {
                  ;;          'behaviour default
                  ;;       }))))

                  ;; (['process-autopilot]
                  ;;    (let ((this (update (if (this 'autopilot)
                  ;;       (case (this 'behaviour 'idle)
                  ;;          ('idle
                  ;;             {
                  ;;                ; todo: сделать отдельное поведение "роллинг"
                  ;;                'roll (if (not (this 'station #f)) ; and not a BUOY
                  ;;                   0
                  ;;                   (this 'roll))
                  ;;                'pitch ; if a BUOY - (this 'roll)
                  ;;                   0
                  ;;                'yaw #i0
                  ;;                'speed #i0
                  ;;                ;; 'speed 0
                  ;;                ;; 'pitch 0 'roll 0 'yaw 0
                  ;;             })
                  ;;          ('avoid-collision
                  ;;             (call/cc (lambda (ret)
                  ;;                (define proximity-ship (this 'proximity-ship))
                  ;;                (unless proximity-ship ; нету угрозы - ничего не делаем
                  ;;                   (ret #false))

                  ;;                (define collision-radius 12) ; TODO: брать из свойств корабля
                  ;;                (define desired-range (* collision-radius PROXIMITY-AVOID-DISTANCE-FACTOR))
                  ;;                (define desired-range2 (* desired-range desired-range))

                  ;;                (define position [#i0 #i0 #i0])
                  ;;                (NewtonBodyGetPosition body position)
                  ;;                (define destination (if (eq? (size proximity-ship) 3) ; a vector, not a ship
                  ;;                   proximity-ship
                  ;;                else
                  ;;                   (await (mail proximity-ship 'position))))

                  ;;                (define distance2 (distance-between2 destination position))
                  ;;                (when (> distance2 desired-range2)
                  ;;                   ; успешно удалились от слишком близко находившегося корабля
                  ;;                   (notify-ai 'collision-avoided proximity-ship)
                  ;;                   (ret {
                  ;;                      'proximity-ship #false
                  ;;                   }))

                  ;;                ; пытаемся улететь
                  ;;                (let* ((roll pitch speed (TrackDestination this destination {
                  ;;                         'retreat #true
                  ;;                         'pitching_over #true
                  ;;                      })))
                  ;;                   {
                  ;;                      'roll roll
                  ;;                      'pitch pitch
                  ;;                      'yaw 0 ; no yaw
                  ;;                      'speed speed
                  ;;                   }))))
                  ;;          ('fly-to-target ; ??
                  ;;             (call/cc (lambda (ret)
                  ;;                (define primary-target (this 'primary-target))
                  ;;                (unless primary-target ; нету цели - ничего не делаем
                  ;;                   (notify-ai 'target-lost #false) ; todo: 'primary-target-lost ?
                  ;;                   (ret #false))

                  ;;                (define dest-radius 12) ; TODO: брать из свойств корабля
                  ;;                (define desired-range (* dest-radius PROXIMITY-AVOID-DISTANCE-FACTOR))
                  ;;                (define desired-range2 (* desired-range desired-range))

                  ;;                (define position [#i0 #i0 #i0])
                  ;;                (NewtonBodyGetPosition body position)
                  ;;                (define destination (if (eq? (size primary-target) 3)
                  ;;                   primary-target
                  ;;                else
                  ;;                   (await (mail primary-target 'position))))

                  ;;                (define distance2 (distance-between2 destination position))
                  ;;                (when (< distance2 desired-range2)
                  ;;                   ; приблизились на нужное расстояние от нужнойго места (или корабля)
                  ;;                   (notify-ai 'target-achieved primary-target)
                  ;;                   (ret #false))

                  ;;                (let* ((roll pitch speed (TrackDestination this destination {
                  ;;                         'retreat #false
                  ;;                         'pitching_over #true
                  ;;                      })))
                  ;;                   {
                  ;;                      'roll roll
                  ;;                      'pitch pitch
                  ;;                      'yaw 0 ; no yaw
                  ;;                      'speed speed
                  ;;                   }))))
                              
                  ;;          ('stop-still
                  ;;             {
                  ;;                'roll #i0
                  ;;                'pitch #i0
                  ;;                'yaw #i0
                  ;;             }))))))
                  ;;       (mail sender 'ok)
                  ;;       (loop this)))

                  ; ====================================================================
                  (else
                     ; DEBUG:
                     ;  you can start olite using `,l "olite.lisp"` from the Ol session, then
                     ;  you'll be able to get all game variables (and send game commands) interactively
                     (if (symbol? msg)
                     then
                        (mail sender (this msg #f))
                        (loop this)
                     else
                        (print-to stderr name ": Unhandled message " msg " from " sender)
                        (loop this)))))))))

   ; функции-хелперы
   (define (set-ship-position ship position)
      (mail ship ['set-position position]))
   (define (set-ship-orientation ship set-orientation)
      (mail ship ['set-orientation set-orientation]))
   (define (set-ship-hull ship hull-name)
      (mail ship ['set-hull hull-name]))
   (define (set-ship-autopilot ship flag)
      (mail ship ['set-autopilot flag]))


   (define (get-ship-position ship)
      (await (mail ship 'position)))

))