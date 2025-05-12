This is a simple demonstration.  Four squares are displayed, two on the left
and two on the right.  The red squares and green squares are coplanar.  The
right pair of squares has a polygon offset render state attached.  When you
move the camera forward/backward with the up/down arrow keys, the squares on
the left show z-buffer fighting.  The squares on the right do not because the
polygon offset biases the z-values so the two squares are just slightly not
coplanar.

Camera controls.
Up/Down arrow:     translate along camera direction vector
Left/Right arrow:  rotate about camera up vector (yaw)
Home/End:          translate along camera up vector
PageUp/PageDown:   rotate about camera left vector (pitch)

Keyboard controls.
q, Q, ESC:  quit the application
0:          reset frame rate counter
w,W:        toggle wireframe
g:          animate a frame (hold 'g' down for full cycle)
G:          reset the animation (to time zero)

