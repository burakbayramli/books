function  bar_mesh5_2_1ele
include_flags;


% Node:  1    2    3    
x   =  [2.0  4.0  6.0  ];        % x coordinate  
y   =  2*x;                      % y is used only for plotting the bar 

% connectivity array
IEN =  [1    2    3]';           % 1 element

plotbar;


