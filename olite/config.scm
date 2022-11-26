(define-library (olite config)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol))
   (description
      "Skybox library")

   (export
      ; camera setup
      RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR

      ; environment render settings
      SKYBOX-RADIUS
      SUN-RENDER-DISTANCE
      MAX-PLANET-RENDER-DISTANCE

      ; вязкоcть космической среды
      ; (влияет на физику полета)
      DEFAULT-LINEAR-DAMPING
      DEFAULT-ANGULAR-DAMPING

      ; настройки коллайдера ai
      PROXIMITY-SPHERE-ID
      PROXIMITY-WARNING-DISTANCE
      PROXIMITY-AVOID-DISTANCE-FACTOR
   )
   (import
      (scheme core))

(begin
   ; camera setup
   ; 0.2 уже слишком близко и некомфортно (у самого носа) и по краям экрана клиппинг.
   (define RL_CULL_DISTANCE_NEAR 0.1) ; как в VR
   ; максимальное, которое не портится во float
   (define RL_CULL_DISTANCE_FAR 10000000000)

   ; config
   (define SKYBOX-RADIUS 1000000)

   ; расстояние, на котором солнце можно безопасно для
   ;  графики рендерить (ввиду z-figting на 24 битах)!
   (define SUN-RENDER-DISTANCE 32000)
   (define MAX-PLANET-RENDER-DISTANCE 24000)

   (define DEFAULT-LINEAR-DAMPING 0.9)
   (define DEFAULT-ANGULAR-DAMPING [0.9 0.9 0.9])

   (define PROXIMITY-SPHERE-ID 1001)
   (define PROXIMITY-WARNING-DISTANCE 4)
   (define PROXIMITY-AVOID-DISTANCE-FACTOR 10)

))