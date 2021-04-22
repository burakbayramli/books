function  truss_mesh_2_8;
include_flags;


% Node:  1    2    3   (origin placed at node 2) 
%--------------------
x   =  [0.0  1.0  2.0  ];     % X coordinate  
y   =  [0.0  0.0  0.0  ];     % Y coordinate

% connectivity array
IEN =  [1    2          % element 1
        2    3]';       % element 2

% plot truss
plottruss;

% plot mesh variables
fprintf(1,'  Truss Params \n');
fprintf(1,'--------------- \n');
fprintf(1,'No. of Elements  %d \n',nel);
fprintf(1,'No. of Nodes     %d \n',nnp);
fprintf(1,'No. of Equations %d \n\n',neq);


