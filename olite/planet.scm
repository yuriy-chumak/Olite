(define-library (olite planet)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      ; todo: new -> [radius rotation testure ...]

      geoid
      draw)

   (import
      (otus lisp)
      (lib gl-2)
      (olite math))

(begin
   ; config
   ; ...

   ; программа рендеринга поверхности планеты с освещением
   ; todo: сделать поверхность процедурной, а не текстурой
   (define shader-program (gl:create-program
   "  #version 120 // OpenGL 2.1
   // разделяем видовую и модельную матрицы
   #define my_ModelMatrix gl_TextureMatrix[0]

   varying vec4 vertexPosition;
   varying vec3 vertexNormal;

   void main()
   {
      vec4 vertex = gl_Vertex;
      // нормаль в нашем случае совпадает
      vec3 normal = normalize(vertex.xyz);

      vertexPosition = my_ModelMatrix * vertex;
      // нормаль тоже трансформируем в мировое пространство (но не пространство вида!)
      vertexNormal = mat3(my_ModelMatrix) * normal;

      gl_Position = gl_ModelViewProjectionMatrix * vertexPosition;
      // положение вершины в базовом пространстве (для текстурирования)
      gl_TexCoord[0] = vertex;
   }"

   "  #version 120 // OpenGL 2.1
   #define PI 3.1415927410125732421875 // IEEE754 Pi Approximation

   uniform sampler2D tex0;
   uniform vec3 lightPosition; // солнце в мировом пространстве

   varying vec4 vertexPosition;
   varying vec3 vertexNormal;  // ok

   void main()
   {
      float x = gl_TexCoord[0].x,
            y = gl_TexCoord[0].y,
            z = gl_TexCoord[0].z;
      float phi = 1.0 - (1.0 + atan(x, -z)/PI) / 2.0;
      float theta = acos(y) / PI;

      vec3 vertex = vertexPosition.xyz;
      vec3 normal = normalize(vertexNormal); // надо повторно нормализовать

      vec3 lightPos = lightPosition.xyz;
      // солнце - направленный источник, а не точечный
      vec3 lightDir = lightPos; // - vertex * lightPos.w;
      vec3 unitLightDir = normalize(lightDir);

      float diff = dot(normal, unitLightDir);
      //float diff = max(0.1, dot(normal, unitLightDir));
      vec3 color = texture2D(tex0, vec2(phi, theta)).rgb;

      float light = 0.1 + 0.9 * smoothstep(0.0, 1.0, diff);
      
      gl_FragColor = vec4(color * light, 1.0);
   }"))
   (setq tex0 (glGetUniformLocation shader-program "tex0"))
   (setq lightPosition (glGetUniformLocation shader-program "lightPosition"))

   ; textures
   (import (lib soil))
   (define mercury (let ((buffer (file->bytevector "media/solarsystemscope/2k_mercury.jpg")))
         (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGB SOIL_CREATE_NEW_ID 0))) ; SOIL_FLAG_MIPMAPS
   ; настроим бесшовность текстуры и сглаживание
   (glBindTexture GL_TEXTURE_2D mercury)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glBindTexture GL_TEXTURE_2D 0)

   ; процедурный геоид (медленнее, но гибче) / todo: перенести в отдельную библиотеку?
   (define geoid (glGenLists 1))
   (glNewList geoid GL_COMPILE)
   (begin
      (glBegin GL_TRIANGLES)

      ; todo: проверить:
      ; если не нормализовать точки - страдает разбиение
      (define (loop a b c n)
         (vector-apply a (lambda (ax ay az)
         (vector-apply b (lambda (bx by bz)
         (vector-apply c (lambda (cx cy cz)
            (if (> n 0)
            then
               (define d (normalize [(/ (+ ax bx) 2) (/ (+ ay by) 2) (/ (+ az bz) 2)]))
               (define e (normalize [(/ (+ bx cx) 2) (/ (+ by cy) 2) (/ (+ bz cz) 2)]))
               (define f (normalize [(/ (+ cx ax) 2) (/ (+ cy ay) 2) (/ (+ cz az) 2)]))

               (loop a d f (-- n))
               (loop d b e (-- n))
               (loop e c f (-- n))
               (loop d e f (-- n))
            else
               (glVertex3fv a)
               (glVertex3fv b)
               (glVertex3fv c)) )))))))

      ; сгенерируем нужной точности сферу
      (define DIVISIONS 5) ; TODO: move to json config
      ; "нулевой" тетраэдр
      (define r (/ (sqrt 6) 4))
      (define m (negate r))
      (define vertices [
         [m m m] [r m r] [m r r] [r r m] ])
      ; генератор
      (for-each (lambda (a b c)
            (loop (normalize (ref vertices a))
                  (normalize (ref vertices b))
                  (normalize (ref vertices c)) DIVISIONS))
         '(1 2 3 4)
         '(2 3 4 1)
         '(3 4 1 2))
   (glEnd))
   (glEndList)

   (define PI #i3.14159265)

   ; -=( generate )=---------------------
   ;(import (OpenGL version-2-1))
   (define (list* . args) args) ; todo: make standard

   ; -=( draw )=------------------------------
   (import (lib GLU))
   (define (draw radius position rotation sun)
      (glPushMatrix)

      ; подготовим модельную матрицу
      (vector-apply position (lambda (x y z)
         (glTranslatef x y z)))
      (glScalef radius radius radius)
      ; todo: сделать правильный rotation из кватерниона
      (glRotatef (* rotation -10) 0 1 0)

      (glUseProgram shader-program)
      (glColor3f 1 1 1)
      (glActiveTexture GL_TEXTURE0)
      (glUniform1i tex0 0)
      (glUniform3fv lightPosition 1 sun)
      (glBindTexture GL_TEXTURE_2D mercury)
      (glCallList geoid)

      (glPopMatrix))
))