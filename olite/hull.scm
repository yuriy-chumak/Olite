(define-library (olite hull)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))

   (export
      load-hull
   )

   (import
      (otus lisp)
      (file json))
(begin
   (define (read-hull-file name)
      (define filename (string-append "gamedata/hull/" name ".json"))
      (print "Loading hull " filename " ...")

      (read-json-file filename))

   ; -=( cache )=-----------------------------------
   (import (olite cache))
   (define cache (make-cache "hull" read-hull-file))
   (define load-hull (load-cache cache))
))
