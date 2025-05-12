Portal Test Demonstration

A top view of the collection of convex regions is shown in Layout.png.  The
floor is z = 0 and the ceiling is z = 1.  Each cubic region is 2x2x2.  The
13 regions are named as shown in the image.  The portal indices for each
region are also shown.

The convex region manager uses a BSP tree to partition space into the
appropriate regions.  The BSP tree for this layout is in BspTree.txt.

The tessellation for the center cube is shown in WallTessellate.png.  This is
a top view with the ceiling removed.  The four portals are shown.  The end
cubes centered at (4,0), (-4,0), (0,4), and (0,-4) have three portals each.
The tessellation is shown in EndTessellate.png.  The 8 connector regions have
two portals each.  The tessellation is shown for the axis-aligned connectors
in ConnectorTessellate.png.  The connectivity array is the same for the
diagonal connectors; only the vertex geometry is different.

Yes, the artwork is awful.  I am an engineer, not an artist :)

----------
Camera controls.
Up/Down arrow:     translate along camera direction vector
Left/Right arrow:  rotate about camera up vector (yaw)
Home/End:          translate along camera up vector
PageUp/PageDown:   rotate about camera left vector (pitch)

Keyboard controls.
q, Q, ESC:  quit the application
0:          reset frame rate counter
w,W:        toggle wireframe
