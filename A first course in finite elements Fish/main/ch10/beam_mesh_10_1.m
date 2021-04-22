function  beam_mesh_10_1
include_flags;


% Node:  1    2    3   (origin placed at node 2) 
%--------------------
x   =  [0.0  8.0  12.0  ];     % X coordinate  
y   =  [0.0  0.0  0.0   ];     % Y coordinate

% connectivity array
IEN =  [1    2        
        2    3];     

% plot beam
plotbeam;

