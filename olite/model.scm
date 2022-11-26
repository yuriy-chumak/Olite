(define-library (olite model)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      load-model)

   (import
      (otus lisp)
      (file wavefront obj)
      (file wavefront mtl)
      (lib gl-2))

(begin
   (import (olite config))

   (import (lib soil))
   (import (olite newton))

   (define checker
      (define id '(0))
      (glGenTextures (length id) id)
      (glBindTexture GL_TEXTURE_2D (car id))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
      (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA 2 2 0 GL_RGBA GL_FLOAT
         (cons (fft* fft-float)
               (list 1 1 1 1
                     0 0 0 0
                     1 1 1 1
                     0 0 0 0))) ; RGBA
      (glBindTexture GL_TEXTURE_2D 0)
      (car id))


   (import (owl parse))
   (define (read-model-file name)
      ;; LOAD

      (define resources "media/models/")
      ; todo: (or fasl-load (string-append "cache/" filename ".fasl")

      ; Load geometry,
      (define obj-filename (string-append resources name ".obj"))
      (print "Loading object file " obj-filename "...")
      (define obj (parse wavefront-obj-parser (file->bytestream obj-filename) obj-filename #t #empty))
      ; Load materials
      (define mtl-filename (string-append resources (obj 'mtllib "")))
      (print "Loading materials file " mtl-filename "...")
      (define mtl (parse wavefront-mtl-parser (file->bytestream mtl-filename) mtl-filename #t #empty))

      ; (todo: let's convert into inexacts)
      (define vertices (list->vector (obj 'v #null)))
      (define normals (list->vector (obj 'vn #null)))
      (define tcoords (list->vector (obj 'vt #null)))

      ; materials (list of )
      (define mtls (map (lambda (material) [
            (material 'name)     ; name
            (material 'kd)       ; diffuse color
            (material 'map_kd)]) ; diffuse color texture file
         mtl))

      ; geometry
      (define objects (map (lambda (object) [
            (object 'name)
            (object 'facegroups)])
         (obj 'o)))
      ;; todo: (fasl-save model (string-append "cache/" name ".fasl"))

      ;; COMPILE
      ; map of name -> [name Kd map_Kd]
      (define materials (fold (lambda (materials material)
            (vector-apply material (lambda (name kd map_kd)
               (define sname (string->symbol name))
               (put materials sname [
                  sname
                  (vector-map inexact kd)
                  (if map_kd then
                     (define id (let ((buffer (file->bytevector map_kd)))
                        (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID SOIL_FLAG_INVERT_Y))) ; invert y for bmp
                     (print "map_kd: " map_kd ", id: " id)
                        ; SOIL_FLAG_MIPMAPS
                     (glBindTexture GL_TEXTURE_2D id)
                     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
                     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
                     ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                     ;; (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                     (glBindTexture GL_TEXTURE_2D 0)
                     id
                  else
                     checker)
               ]))))
         {}
         mtls))

      (define mesh (NewtonMeshCreate newtonian-world))
      (NewtonMeshBeginBuild mesh)

      (define submodels (reverse
      (fold (lambda (submodels o)
            (vector-apply o (lambda (name facegroups)
               (define index (glGenLists (length facegroups)))
               (print "compiling object " name "...")

               ;; (define mesh (NewtonMeshCreate newtonian-world))
               ;; (NewtonMeshBeginBuild mesh)

               (define gllists (reverse
                  (fold (lambda (o group i)
                           (vector-apply (materials (string->symbol (car group))) (lambda (name kd map_kd)
                              (print map_kd)
                              (glNewList i GL_COMPILE)

                              (glColor4fv kd)
                              (glBegin GL_TRIANGLES)
                              (for-each (lambda (face)
                                    (NewtonMeshBeginFace mesh)
                                    (for-each (lambda (vertex)
                                          (vector-apply vertex (lambda (xyz uv n)
                                             (define vertex (ref vertices xyz))
                                             (define normal (ref normals n))

                                             (glTexCoord2fv (ref tcoords uv))
                                             (glNormal3fv normal)
                                             (glVertex3fv vertex)

                                             (apply NewtonMeshAddPoint (cons mesh (vector->list vertex)))
                                             (NewtonMeshAddMaterial mesh 0)
                                             (apply NewtonMeshAddNormal (cons mesh (vector->list normal)))
                                          )))
                                          face)
                                    (NewtonMeshEndFace mesh))
                                 (cdr group))
                              (glEnd)
                              (glEndList)
                              (cons [i map_kd] o)))) ; [list texture]
                        #null
                        facegroups
                        (iota (length facegroups) index))))

               ;; (NewtonMeshEndBuild mesh)

               ;; (define convex (NewtonCreateConvexHullFromMesh newtonian-world mesh 0.0 0))
               ;; (NewtonCompoundCollisionAddSubCollision collision convex)
               ;; (NewtonDestroyCollision convex)

               (cons [name gllists] submodels))))
         #null
         objects)))

      ;; (NewtonCompoundCollisionEndAddRemove collision)

      (NewtonMeshEndBuild mesh)
      (define collision (NewtonCreateCompoundCollisionFromMesh newtonian-world mesh 0.0 0 0))

      ; NewtonCollisionCreateInstance + NewtonCollisionGetUserData?
      (NewtonCompoundCollisionBeginAddRemove collision)
      (define collision-radius 12) ; todo: брать из свойств корабля
      (define sphere (NewtonCreateSphere newtonian-world
            (* collision-radius PROXIMITY-WARNING-DISTANCE)
            PROXIMITY-SPHERE-ID #f)) ; это некая большая сфера,
            ; которая должна вызывать "avoid-collision" событие
      (NewtonCompoundCollisionAddSubCollision collision sphere)
      (NewtonCompoundCollisionEndAddRemove collision)

      [submodels collision]) ; todo: add materials? add something else?

   ; -=( cache )=-----------------------------------
   (import (olite cache))
   (define cache (make-cache "model" read-model-file))
   (define load-model (load-cache cache))
))
