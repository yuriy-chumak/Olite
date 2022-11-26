(define-library (olite menu)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))
   (description
      "Olite menu library")

(export
   ;; show-menu
   ;; hide-menu

   render-menu
   get-menu-texture)

(import
   (otus lisp)
   (scheme inexact)
   ;; (scheme dynamic-bindings)
   (lib gl-2)
   (lib soil))

(cond-expand
   (Android
      (import (OpenGL OES framebuffer_object)))
   (else
      (import (OpenGL EXT framebuffer_object))) )

(begin
   ; config
   (setq resources "media/fonts/")
   ; размеры окна вывода
   (setq WIDTH 40)
   (setq HEIGHT 20)

   ; opengl
   (import (OpenGL version-2-1))
   ;; (import (OpenGL SGIS generate_mipmap))

   ; словарь { буква -> глиф на текстуре }
   (import (file json))
   (define font (read-json-file (string-append resources "Anonymous Pro.json")))
   (define font (put font 'characters
      (ff-fold (lambda (ff key value)
                  (put ff (string-ref (symbol->string key) 0) value))
         #empty
         (get font 'characters #empty))))

   (define buffer (file->bytevector (string-append resources (font 'file))))
   (define atlas (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS))

   ; TODO: создать вертексный буфер и писать текст через индексный буфер
   ; font precompilation:
   (define charset
      (ff-map (lambda (key glyph) [
            ; texture coordinates
            (inexact (/    (glyph 'x)                  (font 'width)))
            (inexact (/    (glyph 'y)                  (font 'height)))
            (inexact (/ (+ (glyph 'x) (glyph 'width) ) (font 'width)))
            (inexact (/ (+ (glyph 'y) (glyph 'height)) (font 'height)))
            
            ; glyph coordinates
            (glyph 'originX)
            (glyph 'originY)
            (- (glyph 'originX) (glyph 'width))
            (- (glyph 'originY) (glyph 'height))
            ])
         (font 'characters)))

   ; размеры шрифта для окошка 16/9
   (define font-size (font 'size 16))
   (define font-width (floor (* font-size 9/16)))

   ;; ----------------
   ;; Рендерер

   (define TEXW 1024) ; todo: change to config window size??
   (define TEXH 1024)

   (define texture (let* ((id (box 0))
                          (ok (glGenTextures 1 id)))
                      (unbox id)))
   (glBindTexture GL_TEXTURE_2D texture)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR) ; nearest??
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA TEXW TEXH 0 GL_RGBA GL_UNSIGNED_BYTE 0)
   (glBindTexture GL_TEXTURE_2D 0)

   ; текстурный буфер нам нужен, а вот буфер глубины - нет
   (define framebuffer (let* ((id (box 0))
                              (ok (glGenFramebuffers 1 id)))
                          (unbox id)))
   (glBindFramebuffer GL_FRAMEBUFFER framebuffer)
   (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D texture 0)
   (glBindFramebuffer GL_FRAMEBUFFER 0)

   ; --- internal functions
   (setq cursor '(0 . 0)) ; текущее положение курсора
   (setq drawing-area [0 0 0 0]) ; виртуальное окно

   (define (move-to x y)
      (set-car! cursor x)
      (set-cdr! cursor y))

   (define (set-color rgb)
      (glColor3fv rgb))

   ; яркостные компоненты палитры
   (setq q #x55/#i255) (setq a #xAA/#i255) (setq f #xFF/#i255)

   ; full CGA 16-color palette
   (define BLACK (list 0 0 0))
   (define BLUE (list 0 0 a))
   (define GREEN (list 0 a 0))
   (define CYAN (list 0 a a))
   (define RED (list a 0 0))
   (define MAGENTA (list a 0 a))
   (define BROWN (list a q 0))
   (define GRAY (list q q q))
   (define YELLOW (list f f q))
   (define WHITE (list f f f))
   (define LIGHTGRAY (list a a a))
   (define LIGHTBLUE (list q q f))
   (define LIGHTGREEN (list q f q))
   (define LIGHTCYAN (list q f f))
   (define LIGHTRED (list f q q))
   (define LIGHTMAGENTA (list f q f))

   (define (writer . text)
      (define (echo text)
         (let do ((x (+ (ref drawing-area 1) (car cursor)))
                  (y (+ (ref drawing-area 2) (cdr cursor)))
                  (text (string->runes text)))
            (if (null? text)
               (move-to (- x (ref drawing-area 1)) (- y (ref drawing-area 2))) ; закончили печатать, переместим курсор
            else ; иначе отрендерим символ
               (let ((char (car text)))
                  (case char
                     (#\space
                        (do (+ x 1) y (cdr text)))
                     (#\newline
                        (do (ref drawing-area 1) (+ y 1) (cdr text)))
                     (else
                        (let ((tc (charset char #false))
                              (x (+ (* x font-width) 2))
                              (y (+ (* y font-size) font-size -12)))
                           (if tc (vector-apply tc
                              (lambda (l t r b  dx dy xw yh)
                                 (let*((lx (- x dx))
                                       (ty (- y dy))
                                       (rx (- x xw))
                                       (by (- y yh)))
                                    (glTexCoord2f l t)
                                    (glVertex2f lx ty)
                                    (glTexCoord2f r t)
                                    (glVertex2f rx ty)
                                    (glTexCoord2f r b)
                                    (glVertex2f rx by)

                                    (glTexCoord2f l t)
                                    (glVertex2f lx ty)
                                    (glTexCoord2f r b)
                                    (glVertex2f rx by)
                                    (glTexCoord2f l b)
                                    (glVertex2f lx by))))))
                        (do (+ x 1) y (cdr text))))))))

      (for-each (lambda (text)
            (cond
               ; строка - текст
               ((string? text)
                  (echo text))
               ((number? text)
                  (echo (number->string text)))
               ; список - цвет
               ((list? text)
                  (set-color text))

               ((symbol? text) #t) ; игнорировать, символы обратавывает другая функция
               ((function? text) #t) ; аналогично

               (else
                  #false)))
         text))
   ; ---

   ; шейдер текстурирования текста
   (define program (gl:create-program
   "  void main()
      {
         gl_Position = gl_ModelViewMatrix * gl_Vertex;
         gl_FrontColor = gl_Color;
         gl_TexCoord[0] = gl_MultiTexCoord0;
      }"
   "  uniform sampler2D atlas;
      void main()
      {
         gl_FragColor = vec4(gl_Color.rgb, texture2D(atlas, gl_TexCoord[0].st).a);
      }"))

   (define (render-menu)
      (glBindFramebuffer GL_FRAMEBUFFER framebuffer)
      (glViewport 0 0 TEXW TEXH)
      (glClearColor 0.2 0.2 0.2 1)
      (glClear GL_COLOR_BUFFER_BIT)

      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (glOrtho 0 (* WIDTH font-width) -0 (* HEIGHT font-size) 0 1) ; размер нашей консольки

      (glDisable GL_CULL_FACE)
      (glEnable GL_BLEND)
      (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
      (glDisable GL_DEPTH_TEST)

      (glUseProgram program)

      (glActiveTexture GL_TEXTURE0)
      (glUniform1i (glGetUniformLocation program "tex0") 0)
      (glBindTexture GL_TEXTURE_2D atlas)

      ; -- вот тут наконец рисуем контент окна
      (move-to 0 0)
      (glBegin GL_TRIANGLES)
      (writer WHITE "HELLO!!!" "\n" GREEN "green text")
      (glEnd)

      (glBindFramebuffer GL_FRAMEBUFFER 0))
      
   (define (get-menu-texture)
      texture)
))