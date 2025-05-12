Ripple Demo
===========

Camera controls.
Up/Down arrow:     translate along camera direction vector
Left/Right arrow:  rotate about camera up vector (yaw)
Home/End:          translate along camera up vector
PageUp/PageDown:   rotate about camera left vector (pitch)

Object controls.
F1/F2:             rotate the object along camera left vector
F3/F4:             rotate the object along camera forward vector
F5/F6:             rotate the object along camera up vector

Keyboard controls.
q, Q, ESC:  quit the application
Space bar:  enable/disable the animation

Wave controls.
W/w:               increase/decrease wave height
S/s:               increase/decrease wave speed
R/r:               increase/decrease ripple frequency
T/t:               increase/decrease ripple texture repeat
A/a:               increase/decrease ambient addition
Space bar:         stop/start movement

Explanation
-----------
This shader is from the ideas in the ShaderX book's article called "Rendering
Ocean Water" by John Isodoro, Alex Vlachos, and Chris Brennan.  The sky
texture is edited from Jeremy Engleman's page of public textures
(http://art.net/~jeremy/photo/public_texture/).  The "plasma" height map was
made with the GIMP, using the render sky plasma2 effect (with tile
horizontally and vertically turned on).

The vertex shader is responsible for creating the wave displacement effect.
The pixel shader is responsible for calculating the water color, the diffuse
lighting, the specular reflection, and putting it all together.

The wave effect is created by adding together four sine waves that propogate
in 2D tangent space.  They have some specific height along the normal as well
as a speed, direction, and offset.  Using only one or two waves makes the
water appear very sinusoidal.  Four gives it a very undulating effect.  You
could easily add another four waves if you wanted even more control.  The
other thing that the vertex shader does is calculate a number of vectors that
the pixel shader uses.  It calculates the new normal, tangent, and binormal
vectors (based on the cosine of the wave) in order to create a vector space
for the pixel shader.  It also calculates a view vector.  Finally, it creates
two texture coordinates.  These texture coordinates scroll with different
speeds and with one of them flipped so that the two textures never line up.

The pixel shader first samples the plasma height map (which has been turned
into a normal map) with both texture coordinates.  These bump maps are
averaged together to get a new normal for this particular pixel.  Then, using
the normal, tangent, and binormal vectors the bumpmap value is transformed
into world space.  This is the new normal for the pixel with the ripple
effect.

The water color is calculated by using the dot product of the (old) normal
and the view direction and looking up into a gradient.  This way the water
is green when looked down into and more blue when you look out at it.  The
old normal is used because it the new normal is too high frequency for the
colors to look right.  Because of the bumpmapping, blue and green patches
appear almost equally all over the water patch.

The diffuse color is calculated a dot product of the normal with a
directional light.  Finally, the specular reflection is calculated.  The
view direction is bounced off of the new normal and a color is gotten from
the background image.  The magnitude of this color is squared (to emphasize
the bright parts) and then multiplied by the background color to get a
color.  This specular color is then multiplied by a Fresnel factor (dot
product of the view direction and the normal).  Water behaves by reflecting
when the view direction is close to the surface and less when the view
direction is looking along the surface normal.  The Fresnel factor
approximates this.  Finally, they are all added together to get the final
water color.

A few side notes: There were a lot of tweaks to get this shader to look
right.  The most important part of the entire shader are the specular
reflections.  This is really what makes the water look like water.  Otherwise,
the shadowing from the bumpmapping looks very strange.  Also, to offset the
amount of shadowing from the bumpmapping, a certain amount of ambience was
added to the diffuse color term.  Playing with the ambient color makes the
water look anywhere from sunset to full noon on a clear day.

I recommend playing with the parameters, especially ambient, texture repeat,
and ripple speed.  Using these you can create very different looking water
scenes.
