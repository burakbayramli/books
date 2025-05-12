Irridescence Demo
=================

Camera controls.
Up/Down arrow:     translate along camera direction vector
Left/Right arrow:  rotate about camera up vector (yaw)
Home/End:          translate along camera up vector
PageUp/PageDown:   rotate about camera left vector (pitch)

Keyboard controls.
q, Q, ESC:  quit the application
+: increase the amount that the irridescent texture is shown
-: increase the amount that the underlying texture is shown

Explanation
-----------
This shader works in very much the same way that the Fresnel shader works.
It calculates a per pixel viewing direction and normal in the vertex shader.
In the pixel shader, it uses the dot product of the viewing direction and
normal as an input to a 1D gradient texture lookup.  When viewed straight on,
the gradient texture is green.  When viewed at an angle it is more blueish.
Then this lookup is blended with the original texture, it gains an
irridescent sheen.

A possible extension to this would be to use a 2D gradient texture instead of
the 1D one in this example and use part of a texture coordinate for the other
dimension.