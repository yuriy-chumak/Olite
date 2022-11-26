(define-library (olite planet sun)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      draw)

   (import
      (otus lisp)
      (lib gl-2)
      (olite math))

(begin
   (import (olite config))
   ; config
   ; ...

   ; программа рендеринга поверхности солнца
   ; todo: сделать поверхность процедурной
   (define shader-program (gl:create-program
   "  #version 120 // OpenGL 2.1
   #define my_ModelMatrix gl_TextureMatrix[0]

   varying vec4 vertexPosition;
   void main()
   {
      // наша сфера не сферична (чтобы быстрее считать)
      // поэтому мы делаем ее сферичной тут:
      vec4 vertex = vec4(normalize(gl_Vertex.xyz), 1.0);

      vertexPosition = my_ModelMatrix * vertex;

      gl_Position = gl_ModelViewProjectionMatrix * vertexPosition;
      gl_TexCoord[0] = vertex; // положение вершины в модельном пространстве (для текстурирования)
   }"

   "  #version 120 // OpenGL 2.1
   #define PI 3.1415927410125732421875 // IEEE754 Pi Approximation

   uniform sampler2D tex0;
   varying vec4 vertexPosition;
   varying float depth;
   void main()
   {
      float x = gl_TexCoord[0].x,
            y = gl_TexCoord[0].y,
            z = gl_TexCoord[0].z;
      float phi = 1.0 - (1.0 + atan(x, -z)/PI) / 2.0;
      float theta = acos(y) / PI;

      gl_FragColor = texture2D(tex0, vec2(phi, theta));
   }"))

   ; textures
   (import (lib soil))
   (define texture (let ((buffer (file->bytevector "media/solarsystemscope/2k_sun.jpg")))
         (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGB SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS)))
   ; настроим бесшовность текстуры и сглаживание
   (glBindTexture GL_TEXTURE_2D texture)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glBindTexture GL_TEXTURE_2D 0)

   (define PI #i3.14159265)
   (import (only (olite planet) geoid))

   ; -=( draw )=------------------------------
   (import (lib GLU))
   (define (draw radius position rotation)
      (glPushMatrix)
      ; todo: position - вектор и rotation - кватернион

      ; так как мы не делаем z-буфер линейным (мы запускаемся и на мобильных девайсах),
      ;  и солнце у нас всегда "далеко-далеко", то поместим его поближе. так, чтобы не
      ;  калечилась при выводе моделька:
      (define multiplier (/ SUN-RENDER-DISTANCE (magnitude position)))
      (define loc (vec3scale position multiplier))
      (define r (* radius multiplier))

      ; подготовим модельную матрицу
      
      (vector-apply loc (lambda (x y z)
         (glTranslatef x y z)))
      (glScalef r r r)
      ; todo: сделать правильный rotation из кватерниона
      (glRotatef (* rotation -3) 0 1 0)

      (glUseProgram shader-program)
      (glColor3f 1 1 1)
      (glUniform1i (glGetUniformLocation shader-program "tex0") 0)
      (glBindTexture GL_TEXTURE_2D texture)
      (glCallList geoid)
      
      (glPopMatrix))
))