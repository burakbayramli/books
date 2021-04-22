% Mesh2D for 1 element

function  mesh2d_1ele
include_flags;


% Node:  1    2    3    4
%--------------------------
x   =  [0.0  0.0  2.0  2.0];     % X coordinate
y   =  [1.0  0.0  0.5  1.0];     % Y coordinate

IEN =  [2    3    4    1  ]';     % connectivity array


plotmesh;


