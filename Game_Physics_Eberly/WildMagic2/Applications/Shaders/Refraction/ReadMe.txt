Refraction Demo
===============

Camera controls.
None.  Camera is fixed.

Object controls.
F1/F2:             rotate the object along camera direction vector
F3/F4:             rotate the object along camera up vector
F5/F6:             rotate the object along camera left vector

Keyboard controls.
q, Q, ESC:  quit the application
Space bar:  enable/disable reflection
+/-      :  increase/decrease the index of refraction

Explanation
-----------
This pair of vertex and pixel shaders calculates environment refraction and
reflection.  To do this, it calculates the view direction and then using the
normal and the index of refraction calculaes the refraction vector.  If the
environment map is a cube map, a shader can use this directly as the
coordinates for the environmental texture lookup.  If the environment map
is a sphere map, then a few more calculations need to be done to get the
correct texture coordinates.  A reflection vector is calculated in the exact
same manner.  A Fresnel factor is calculated to determine the strength of
the reflection versus the refraction.  To make the refraction appear more
real, a quad (that represents the environment map in that direction) is put
up behind the object.
