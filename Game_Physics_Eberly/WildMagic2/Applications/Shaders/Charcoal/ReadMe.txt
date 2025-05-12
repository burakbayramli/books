Charcoal Demo
=============

Camera controls.
Up/Down arrow:     translate along camera direction vector
Left/Right arrow:  rotate about camera up vector (yaw)
Home/End:          translate along camera up vector
PageUp/PageDown:   rotate about camera left vector (pitch)

Keyboard controls.
q, Q, ESC:  quit the application
Space bar:  enable/disable the animation
p: enable/disable the blending with the background
l: enable/disable the diffuse lighting contribution
s: enable/disable the smudging of lighting and charcoal texturing (invalid if
   lighting off)

Explanation
-----------
This is an implementation of Charcoal-style NPR rendering written by Michael
Riegger (mikeriegger@yahoo.ca) for cgshaders.org.  This implementation is
based on the paper "Hardware Accelerated Real Time Charcoal Rendering" by
Aditi Majumder, Department of Computer Science, University of North Carolina
at Chapel Hill (majumder@cs.unc.edu).

There are three parts to creating the charcoal effect: diffuse lighting, a
constrast map, and blending with a paper background.  The first part is that
the diffuse lighting for two directional lights is calculated.  The contrast
of the lighting is strengthened with a exponential operator to enchance the
shadows.  The pixel shader gets a random value (by looking up in a random
texture map).  It is not good enough to simply pass a random texture
coordinate to the pixel shader from the application because it creates
banding effects.  Thus, to simulate a random number the texture coordinate
of the original object is used as a lookup into a random map, which has been
filled at run-time with random data.  This random number as well as the
diffuse lighting are used as coordinates into a contrast map.

The contrast map is a white texture filled at run-time with black dots.
However, by use of an exponential operator on one of the inputs, the contrast
map is an approximation of a gradient from black to white along one texture
coordinate.  Finally, the value from the contrast map and the value from the
diffuse lighting are "smudged" (averaged) to give a much smoother final image.

Finally, the model is blended with the paper texture.  However, just alpha
blending the model does not look right because parts of the model that were
self-obscured are now visible.  To fix this, the pixel shader itself alpha
blends with the background texture.  It looks up the background color using
screen space dependent texture coordinates.  This way the z-buffer can be
used to determine which pixels are on top and then only those pixels will
be blended with the background.

This demo uses a vertex shader for the drawing of the background itself.
Because it's easiest to describe screen polygons in projected world space,
this shader simply passes through vertex values.   It automatically calculates
the correct texture coordinates for the screen polygon so that the texture
fills the screen.  This was necessary because WildMagic does not support
shaders on screen polygons and shows how you can use multiple shaders in the
same program.

This shader also uses uniform variables passed in by the user as boolean
variables.
