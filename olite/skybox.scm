(define-library (olite skybox)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))
   (description
      "Skybox library")

   (export
      generate!
      draw)

   (import
      (otus lisp)
      (lib gl-2)
      (olite math))

(begin
   ; config
   (import (olite config))

   ; number of images in atlas
   (define SKYBOX-ATLASSIZE 16)

   (define NEBULAS-SCALE #i1.4)
   (define STARS-SCALE #i0.021)

   (define NEBULAS-ALPHA #i0.1)
   (define STARS-ALPHA #i0.812)

   ; textures
   (import (lib soil))
   (define sky-atlas (let ((buffer (file->bytevector "media/sky-atlas.png")))
      (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGB SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS)))

   (define skybox (glGenLists 1))

   (define R SKYBOX-RADIUS)
   (define -R (negate R))
   (define N (/ #i1 SKYBOX-ATLASSIZE))

   ; todo: move to external json config
   (define stars-probabilities '(1 0.00002 0.0005 0.00005 0.05 0.0005 0.00005 0.00005 0.9 0.9 0.0005 1))
   (define stars-probabilities-sum (fold + 0 stars-probabilities))
   (define nebula-probabilities '(1 0.5 0.1 1))
   (define nebula-probabilities-sum (fold + 0 nebula-probabilities))

   (define (random-star-texture)
      (let cycle ((n 0)
                  (id (* (random-real) stars-probabilities-sum))
                  (stars stars-probabilities))
         (if (< id (car stars))
            (* n N)
         else
            (cycle (++ n) (- id (car stars)) (cdr stars)))))
   (define (random-nebula-texture)
      (let cycle ((n (length stars-probabilities))
                  (id (* (random-real) nebula-probabilities-sum))
                  (nebulas nebula-probabilities))
         (if (< id (car nebulas))
            (* n N)
         else
            (cycle (++ n) (- id (car nebulas)) (cdr nebulas)))))

   (define (random-xyz) (values
      (- (* (random-real) 2.0) 1.0)
      (- (* (random-real) 2.0) 1.0)
      (- (* (random-real) 2.0) 1.0)))
   (define (random-rgb) (values
      (+ (* (random-real) 0.6) 0.4)
      (+ (* (random-real) 0.6) 0.4)
      (+ (* (random-real) 0.6) 0.4)))

   (define PI #i3.14159265)

   ; -=( generate )=---------------------
   (import (olite math))
   (import (scheme inexact))

   ; с геометрическими шейдерами под GLES у нас проблемы,
   ;  поэтому будем пока считать ручками.. в общем отработаем алгоритм
   ;  и снова попробуем засунуть в геометрию...

   ; поворот вокруг произвольной оси:
   ; 1. вычисляем кватернион поворота R от [0 0 -1] к вектору "вперед" (правосторонняя система координат)
   ; 2. вычисляем матрицу поворота M
   ; 3. поворачиваем вокруг оси Oz квадратик для текстуры
   ; 4. передвигаем матрицей поворота в правильное место на сфере

   ; todo: добавить зерно генератора
   (define (generate! stars nebulas)
      (define up (vec3 0 1 0))

      (define (spawn count SCALE ALPHA texture)
         (for-each (lambda (i)
            (define-values (x y z) (random-xyz))
            (define-values (r g b) (random-rgb))
            (define a (* 2 PI (random-real))) ; rotation

            (define xyz (normalize (vec3 x y z)))
            (define orientation (rotation-between xyz [0 0 -1]))
            (define M (rotation-matrix3 orientation))

            (define scale (* SCALE (random-real)))
            (define texa (texture))
            (define texb (+ texa N))

            (define +sa (* scale (sin a))) (define -sa (negate +sa))
            (define +ca (* scale (cos a))) (define -ca (negate +ca))

            (glColor4f r g b ALPHA)

            (glTexCoord2f texa 0)
            (glVertex3fv (mat3*vec3 M [+ca +sa -R]))
            (glTexCoord2f texa 1)
            (glVertex3fv (mat3*vec3 M [-sa +ca -R]))
            (glTexCoord2f texb 1)
            (glVertex3fv (mat3*vec3 M [-ca -sa -R]))
            (glTexCoord2f texb 0)
            (glVertex3fv (mat3*vec3 M [+sa -ca -R])))
         (iota count)))

      (glNewList skybox GL_COMPILE)
      (glBegin GL_QUADS)
         ; nebulas (они размытые фоновые, поэтому рисуем первыми)
         (spawn nebulas (* NEBULAS-SCALE SKYBOX-RADIUS) NEBULAS-ALPHA random-nebula-texture)
         ; stars
         (spawn stars (* STARS-SCALE SKYBOX-RADIUS) STARS-ALPHA random-star-texture)
      (glEnd)
      (glEndList))

   ; -=( draw )=----------------------------
   (define shader-program (gl:create-program
   "  #version 120 // OpenGL 2.1
   #define my_ModelMatrix gl_TextureMatrix[0]
   void main()
   {
      gl_Position = gl_ModelViewProjectionMatrix * my_ModelMatrix * gl_Vertex;
      gl_FrontColor = gl_Color;
      gl_TexCoord[0] = gl_MultiTexCoord0;
   }"
   "  #version 120 // OpenGL 2.1
   uniform sampler2D tex0;
   void main()
   {
      vec3 color = texture2D(tex0, gl_TexCoord[0].st).rgb;
      float alpha = (color.r + color.g + color.b) / 3.0;
      gl_FragColor = vec4(gl_Color.rgb, gl_Color.a * alpha);
   }"))
   (define tex0 (glGetUniformLocation shader-program "tex0"))

   (define (draw)
      ; position: положение глаза игрока
      (glUseProgram shader-program)

      (glUniform1i tex0 0)
      (glBindTexture GL_TEXTURE_2D sky-atlas)

      (glCallList skybox))

))