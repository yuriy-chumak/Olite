#!/usr/bin/env ol

(import (lib gl-2)
   (scheme inexact))
(import (lib soil))
;(gl:set-window-size 900 900)

(import (olite config))
(import (olite newton)) ; newton-dynamics
(import (lib keyboard)) ; клавиатура

(import (olite math)) ; math

; init
; ....
; temp setup -->
;  planet_distance = 452760.000000
(define planet_distance #i452760.000000)
(define planet_radius #i4116)

;  sun_radius = 84449.095459 (default is 2.5 * planet_radius)
;  sun_vector = 0.865 0.376 -0.334
;  sun_distance = 905520 (default is planet_radius*20)

(define sun_dir [#i0.865 #i0.376 #i-0.334])
;; (define sun_distance (/ #i905520 100))
;; (define sun_radius (/ #i84449.095459 #i3.33))
(define sun_distance #i905520)
(define sun_radius #i84449.095459)

; safeDistance=36 * sun_radius * sun_radius ???

(define sunPos (vec3scale sun_dir sun_distance))
; <-- temp setup


;; ; temp again
;; (import (olite model))
;; (define mmm (load-model "Attack Bomber-T"))


; Модельное (локальное) пространство -
;  положения вертексов при размещении модели как в редакторе (обычно в точке [0,0,0])
; Мировое пространство -
;  положение вертексов после размещения (и поворотов) модели в мире
; Видовое пространство (ModelView, название сложилось исторически):
;  положение вертексов после перемещения мира так, чтобы камера
;  разместилась в точке [0,0,0] и смотрела в [0,0,-1] с верхом в [0,1,0]
; Проективное пространство:
;  итоговая проекция на монитор того, что мы видим (с перспективными искажениями)

; 1. Прочитаем мир
; 2. Декодируем текущую звездную систему
; 3. Создадим протагониста
(import (olite ship))

(define hero (create-ship 'hero))
(set-ship-position hero [0 0 0]) ;(- planet_distance (/ planet_distance 4))])
(set-ship-hull hero "Cobra Mark III")
(set-ship-autopilot hero #false) ; отключим автопилот


;; (define ship (create-ship 'ship))
;; (set-ship-position ship (+ (get-ship-position 'hero) [0 0 30]))
;; (set-ship-hull ship "Cobra Mark III")
;; (set-ship-autopilot ship #true)

;; ; -- menu -----------
;; (import (olite menu))

; --------------------
; environment
(import (prefix (olite skybox) skybox:))
(import (prefix (olite stardust) stardust:))
(import (prefix (olite planet) planet:))
(import (prefix (olite planet sun) sun:))

(skybox:generate! 8887 129) ; todo: get from planet system setup
; FYI: после резкого изменения положения камеры надо перегенерировать космическую пыль
(stardust:generate! (get-ship-position 'hero))

; -------------------------------------------------------
; -=( вся математика (=----------------------------------
; -- 
(import (scheme dynamic-bindings))

(define time (make-parameter (time-ms))) ; время последнего рендеринга
(define camera-shift (make-parameter [#i0 #i0 #i0]))

; ai + physics
(gl:set-calculator (lambda ()

   ; - timer ----------------------------------------
   ; delta_t - дельта по времени (seconds)
   (define now (time-ms))
   (define old (time now))
   ; временной интервал с прошлой математики
   (define delta_t (inexact
      (min 1/60 (/ (- now old) 1000))))

   ; - ai -------------------------------------------
   ;; ; handle ALL autopilot events
   ;; (ff-for-each (lambda (ship t)
   ;;       (await (mail ship ['process-autopilot])))
   ;;    (*SHIPS*))

   ; - обработчик ручного управления протагониста ---
   (mail 'hero ['manual-control
      (key-pressed? KEY_UP)    (key-pressed? KEY_DOWN)
      (key-pressed? KEY_RIGHT) (key-pressed? KEY_LEFT)
      #false #false
   ])
   (mail 'hero ['boost-slowdown delta_t ; todo: попытаться обойтись без временного интервала
      (key-pressed? KEY_W)     (key-pressed? KEY_S)])  ; W, S

   ; - пришло время симуляции мира ------------------
   ; сохраним старое положение кораблика чтобы рисовать звездную пыль
   ; причем сохраним как копию, так как newtonian-world-update может изменить
   ; внутреннее состояние запрошенных векторов
   (define pre-position (get-ship-position 'hero))
   (define pre-position-copy (deep-vector-copy pre-position))
   ; обновим физику текущего мира
   (newtonian-world-update delta_t)
   ; сохраним новое положение кораблика чтобы рисовать звездную пыль
   (define new-position (get-ship-position 'hero))
   (camera-shift (vector-map - new-position pre-position-copy))

   ; обновление для звездной пыли
   (stardust:update! new-position)

   ; один раз отрисуем меню в текстуру
   ; да и вообще, здесь можно рендерить то, что нам надо только в плоском варианте (например, зеркало заднего вида)
   ;; (render-menu)

   #true
))

;;    ; придется в качестве мировой матрицы использовать gl_TextureMatrix
;;    ; ну не хочется свои функции работы с матрицами городить...

;; (define cockpit (let ((buffer (file->bytevector "media/cockpit.tga")))
;;    (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS)))
;; (define quad (gl:create-program
;;    "  #version 120 // OpenGL 2.1
;;    #define my_ModelMatrix gl_TextureMatrix[0]
;;    void main()
;;    {
;;       //gl_Position = gl_Vertex;
;;       gl_Position = gl_ModelViewMatrix * my_ModelMatrix * gl_Vertex;
;;       gl_TexCoord[0] = gl_MultiTexCoord0;
;;    }"

;;    "  #version 120 // OpenGL 2.1
;;    uniform sampler2D tex0;
;;    void main()
;;    {
;;       gl_FragColor = texture2D(tex0, gl_TexCoord[0].st);
;;    }"))

;; ; простое текстурирование, без теней, освещения и т.д.
;; (define shader-program (gl:create-program
;;    "  #version 120 // OpenGL 2.1
;;    #define my_ModelMatrix gl_TextureMatrix[0]
;;    void main()
;;    {
;;       gl_Position = gl_ModelViewProjectionMatrix * my_ModelMatrix * gl_Vertex;
;;       gl_TexCoord[0] = gl_MultiTexCoord0;
;;    }"

;;    "  #version 120 // OpenGL 2.1
;;    uniform sampler2D tex0;
;;    void main()
;;    {
;;       gl_FragColor = texture2D(tex0, gl_TexCoord[0].st);
;;    }"))

; --------------------------------------------------------------
; draw
(import (lib GLU))
(gl:set-renderer (lambda ()
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; матрицы вида/проецирования, TODO: поворачивать мышкой
   (define FOVY 45.0) ; todo: взять из настроек текущего корабля
   (define ASPECT (/ (gl:get-window-width) (gl:get-window-height)))

   ; перспективная матрица (в VR режиме игнорируется)
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective FOVY ASPECT RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)

   ; матрица вида
   (glMatrixMode GL_MODELVIEW)
   (define matrix (await (mail 'hero 'matrix)))

   ; сохраним. оптимизация.
   ; (не вычисляем из матрицы, будут проблемы с точностью)
   (define position (get-ship-position 'hero))

   ; надо сделать lookat по векторам
   (glLoadIdentity)
   (apply gluLookAt (append
      (vector->list position)
      (vector->list (mat4*vert matrix [0 0 1])) ; вертекс
      (vector->list (mat4*vec3 matrix [0 1 0])) ; вектор!
   ))

   ; операции с миром мы проводим в модельном пространстве
   ;  (за которое мы используем текстурную матрицу 0)
   (glMatrixMode GL_TEXTURE) ; модельная матрица (матрицу вида больше не трогаем!)
   (glActiveTexture GL_TEXTURE0) ; active texture держим по дефолту: 0

   ; ------ рендерим обычный мир и его объекты ---------------------

   ;; TEMP>
   (define degrees (/ (mod (time-ms) 360000) 1000)) ; 0..359 degrees
   (define PI #i3.14159265)
   ;; <----

   (glEnable GL_BLEND) ; полупрозрачность
   (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

   ;; --------------------------------------
   ;; 1. Скайбокс (с Cолнцем)
   (glLoadIdentity) ; двигаем вместе с собой (но не вращаем)
   (vector-apply position (lambda (x y z)
      (glTranslatef x y z)))

   (glDisable GL_DEPTH_TEST)
   (skybox:draw)
   (glEnable GL_DEPTH_TEST)
   (sun:draw sun_radius sunPos degrees)

   ; все, бесконечные объекты нарисованы
   ;  начинаем рендеринг мира как бы "с нуля"
   (glClear GL_DEPTH_BUFFER_BIT)

   ;; --
   ;; 2.1 Точки, создающие ощущение скорости
   (glLoadIdentity)
   (stardust:draw (camera-shift))

   ;; 2.2 Планеты
   (glLoadIdentity)

   ; 0,0,0 - точка входа в систему, видимо

   ; Lave:
   ;  radius = 4116
   ;  rotation_speed = 0.004504
   ;  sky_n_blurs = 129
   ;  sky_n_stars = 8887
   ;  station_vector = "0.601 -0.738 0.305"
   ;  sun_color = 1 1 1 1
   ;  sun_distance = 905520
   ;  sun_radius = 84449.095459
   ;  sun_vector = 0.865 0.376 -0.334
   ;  planet_distance = 452760.000000
   ;  coordinates = 20 173

   ; борьба с z-figting:
   ; теоретически, должна позволить нарисовать тени от спутников на поверхности
   ; идея - если планета дальше чем MAX-PLANET-RENDER-DISTANCE, то корректно
   ;  перенести ее на MAX-PLANET-RENDER-DISTANCE с сохранением пропорций.
   (define camera position)
   (define rel-pos (vec3-vec3 [0 0 planet_distance] camera))
   (define multiplier (min 1 (/ MAX-PLANET-RENDER-DISTANCE (magnitude rel-pos))))
   (define pos (vec3+vec3 camera (vec3scale rel-pos multiplier)))

   ;; (print "camera: " camera ", multiplier: " multiplier ", rel-pos: " rel-pos ", pos: " pos)
   ;; (print "mag/mag: " (magnitude rel-pos) " / " (magnitude (vec3-vec3 camera pos)))
   ; for-each planet and satellites
   (planet:draw (* planet_radius multiplier) pos (/ degrees 20) sunPos)

   ;; 3. Кораблики
   ;; tbd.
   (glLoadIdentity)

   ;; ;; ; temp
   ;; ;; (define one (normalize pos)) ; направление на планету
   ;; ;; (define two (+ position (* one 80))) ;[0 -3 8])); 
   ;; ;; (apply glTranslated (vector->list two))
   ;; ;; ; размеры будут проблемой, да
   ;; ;; ; человек видит полноценное 3д на довольно небольшом расстоянии
   ;; ;; ; и придется уменьшать мир?
   ;; ;; (glScalef 0.1 0.1 0.1)

   ;; ;; (glUseProgram shader-program)
   ;; ;; (glUniform1i (glGetUniformLocation shader-program "tex0") 0)

   ;; ;; (for-each (lambda (submodel)
   ;; ;;       (vector-apply submodel (lambda (name groups)
   ;; ;;          (for-each (lambda (group)
   ;; ;;                (vector-apply group (lambda (i map_kd)
   ;; ;;                   (glBindTexture GL_TEXTURE_2D map_kd)
   ;; ;;                   (glCallList i))))
   ;; ;;             groups))))
   ;; ;;    (ref mmm 1))

   ;; ;; ;; ;; текстура поверх
   ;; ;; ;; ;; игнорирует любые матрицы, так что просто рисуем
   ;; ;; ;; (glUseProgram quad)
   ;; ;; ;; (define tex0 (glGetUniformLocation quad "tex0"))
   ;; ;; ;; (glUniform1i tex0 0)
   ;; ;; ;; (glBindTexture GL_TEXTURE_2D cockpit)

   ;; ;; ;; (define matrix (await (mail 'hero 'matrix)))
   ;; ;; ;; ;; (glLoadIdentity)
   ;; ;; ;; ;; (glLoadMatrixf matrix)

   ;; ;; ;; ; todo: перемещать углы текстуры с помощью mat4*vert
   ;; ;; ;; ; (или сделать это в шейдере?)

   ;; ;; ;; (glDisable GL_DEPTH_TEST)
   ;; ;; ;; (glBegin GL_QUADS)
   ;; ;; ;;    (glTexCoord2f 0 1)
   ;; ;; ;;    ;; (glVertex3f -1 -1 0.1)
   ;; ;; ;;    (glVertex3fv (mat4*vec3 matrix (+ position [-1 -1 0.1])))
   ;; ;; ;;    (glTexCoord2f 0 0)
   ;; ;; ;;    ;; (glVertex3f -1 +1 0.1)
   ;; ;; ;;    (glVertex3fv (mat4*vec3 matrix (+ position [-1 +1 0.1])))
   ;; ;; ;;    (glTexCoord2f 1 0)
   ;; ;; ;;    ;; (glVertex3f +1 +1 0.1)
   ;; ;; ;;    (glVertex3fv (mat4*vec3 matrix (+ position [+1 +1 0.1])))
   ;; ;; ;;    (glTexCoord2f 1 1)
   ;; ;; ;;    ;; (glVertex3f +1 -1 0.1)
   ;; ;; ;;    (glVertex3fv (mat4*vec3 matrix (+ position [+1 -1 0.1])))
   ;; ;; ;; (glEnd)

   #true
))
