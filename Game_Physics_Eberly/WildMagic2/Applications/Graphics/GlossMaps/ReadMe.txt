Objects often don't have a uniformly specular surface (e.g., water on a
counter), and gloss mapping modulates specular contribution by the alpha
channel of a gloss map to achieve this effect.  Run the program to see
two squares.  The one on the right has "magic" written on it.  Rotate the
squares using the F1/F2 keys to see how the gloss map affects the letters.

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

