Vertex Noise Demo
=================

Camera controls.
Up/Down arrow:     translate along camera direction vector
Left/Right arrow:  rotate about camera up vector (yaw)
Home/End:          translate along camera up vector
PageUp/PageDown:   rotate about camera left vector (pitch)

Object controls.
F1/F2:             rotate the object along camera direction vector
F3/F4:             rotate the object along camera up vector
F5/F6:             rotate the object along camera left vector

Keyboard controls.
q, Q, ESC:  quit the application
Space bar:  enable/disable the vertex shader
+/-      :  increase/decrease the noise frequency
1/2      :  increase/decrease the noise amplitude

Explanation
-----------
This shader permutes the vertex positions based on three dimensional Perlin
noise.  The table permutations and gradients is precomputed at start up time
and is passed as a uniform constant to the vertex shader.  This same sort of
noise could equally be used to create procedural textures.

(Used directly with very minimal changes from NVIDIA's Vertex Noise shader
on the cgshaders.org website)
