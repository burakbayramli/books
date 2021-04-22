function  bar_mesh5_2_2ele
include_flags;

% Node:  1    2    3    4    5      
%----------------------------------
x   =  [2.0  3.5  5.0  5.5  6.0  ];        % x coordinate  
y   =  2*x;                                % y is used only for plotting 

% connectivity array
IEN =  [1  3
        2  4
        3  5];
plotbar;


