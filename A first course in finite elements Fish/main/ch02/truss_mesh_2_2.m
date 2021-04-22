function  truss_mesh_2_2
include_flags;


% Node:  1    2    3   (origin placed at node 2) 
%--------------------
x   =  [1.0  0.0  1.0  ];     % X coordinate  
y   =  [0.0  0.0  1.0  ];     % Y coordinate

% connectivity array
IEN =  [1    2        
        3    3];     

% plot truss
plottruss;

