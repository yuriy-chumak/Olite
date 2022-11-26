(define-library (olite stardust)
   (version 1.1)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      SKYDUST-RADIUS
      
      generate!
      draw
      update!)

   (import
      (otus lisp)
      (scheme srfi-27)
      (lib gl-2)
      (OpenGL EXT geometry_shader4))
(begin
   ; эти параметры дают нам приблизительно адекватное ощущение скорости
   (define SKYDUST-RADIUS 9)
   (define SKYDUST-COUNT 30)

   (define R SKYDUST-RADIUS)
   (define R2 (* R R))
   (define D (* R 2))
   (define -R (negate R))

   ; раскидаем равномерно в шаре R
   (define (i0) (inexact 0))
   (define particles (list->vector (map (lambda (i) [(i0) (i0) (i0)]) (iota SKYDUST-COUNT))))

   (define shader-program (gl:create-program
   "  #version 120 // OpenGL 2.1
   #define my_ModelMatrix gl_TextureMatrix[0]

   uniform float R;
   varying float r;
   void main()
   {
      gl_Position = gl_ModelViewProjectionMatrix * my_ModelMatrix * gl_Vertex;
      vec3 pos = (gl_ModelViewMatrix * my_ModelMatrix * gl_Vertex).xyz;
      r = length(pos) / R;
      gl_PointSize = 1.78;
   }"

   "  #version 120 // OpenGL 2.1
   varying float r;
   void main()
   {
      float color = r;
      gl_FragColor = vec4(
         0.6, 0.6, 0.6, // todo: менять цвет в зависимости от скорости
         1.0 - color * color);
      //gl_FragColor = vec4(len, len, len, 1);
   }"))

   ; обновить космическую пыль вокруг игрока
   (define (generate! player)
      (vector-apply player (lambda (ox oy oz)
         (let loop ((N SKYDUST-COUNT))
            (unless (eq? N 0)
               (let ((x (- (* (random-real) D) R))
                     (y (- (* (random-real) D) R))
                     (z (- (* (random-real) D) R)))
                  (define r (+ (* x x) (* y y) (* z z)))
                  (if (< r R2)
                  then
                     (define dest (ref particles N))
                     (vm:set! (ref dest 1) 0 (+ x ox) 0 (size x))
                     (vm:set! (ref dest 2) 0 (+ y oy) 0 (size y))
                     (vm:set! (ref dest 3) 0 (+ z oz) 0 (size z))
                     (loop (-- N))
                  else
                     (loop N))))))))


   (define (draw shift)
      (glUseProgram shader-program)
      (glUniform1f (glGetUniformLocation shader-program "R") R)

      (define shift2 (vector-fold + 0 (vector-map * shift shift)))

      ; контрольные точки (чтобы пыль висела когда мы не двигаемся)
      (if (< shift2 0.0008) ; когда точки делать линиями
      then
         (glBegin GL_POINTS)
         (glColor4f 1 1 1 0.4)
         (vector-for-each (lambda (v)
               (glVertex3fv v))
            particles)
         (glEnd)
      ; линии, если быстро движемся
      else
         (glLineWidth 1.78)
         (glBegin GL_LINES)
         (glColor4f 1 1 1 0.4)
         (vector-for-each (lambda (v)
               (define o (vector-map - v shift)) ; линия сдвига
               (glVertex3fv v)
               (glVertex3fv o))
            particles)
         (glEnd)))

   (define (update! center)
      (vector-apply center (lambda (ox oy oz)
         (vector-for-each (lambda (v) (vector-apply v (lambda (x y z)
               (define dx (- ox x)) (define dx2 (* dx dx))
               (define dy (- oy y)) (define dy2 (* dy dy))
               (define dz (- oz z)) (define dz2 (* dz dz))

               (when (> (+ dx2 dy2 dz2) R2)
                  (vm:set! x 0 (+ ox dx) 0 (size x))
                  (vm:set! y 0 (+ oy dy) 0 (size y))
                  (vm:set! z 0 (+ oz dz) 0 (size z)) ))))
            particles))))
))