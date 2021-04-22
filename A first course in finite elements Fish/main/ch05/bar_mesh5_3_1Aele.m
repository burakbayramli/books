function  bar_mesh5_3_1Aele
include_flags;



% Node:  1    2    3   (coordinate systems attached to node 2) 
%--------------------
x   =  [2.0  5.0  6.0  ];        % x coordinate  
y   =  2*x;                      % y is used only for the bar plot 

% connectivity array
IEN =  [1    2    3]';           % 1 element

plotbar;

