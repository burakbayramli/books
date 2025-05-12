The program displays four squares.  The left-most square has a single texture
of a door.  The square to its right shows the light map treated as the base
texture using standard texturing.  The next square to the right shows the
door texture modulated (multiplied) by the light map.  The right-most square
shows the light map subtracted from the door texture using a combine mode.

To navigate the scene by moving the camera:
  UP ARROW    = translate camera along view direction (move forward)
  DOWN ARROW  = translate camera along view direction (move backward)
  LEFT ARROW  = rotate camera about up vector (turn to left)
  RIGHT ARROW = rotate camera about up vector (turn to right)
  HOME        = translate camera along up vector (move up)
  END         = translate camera along up vector (move down)
  PAGE UP     = rotate camera about right vector (look up)
  PAGE DOWN   = rotate camera about right vector (look down)

To tumble the scene (camera remains stationary):
  F1 / F2 KEYS = rotate about global X-vector
  F3 / F4 KEYS = rotate about global Y-vector
  F5 / F6 KEYS = rotate about global Z-vector

Keyboard controls.
q, Q, ESC:  quit the application

