function  bar_mesh5_2_4ele
include_flags;


% Node:  1    2    3    4    5    6    7    8    9     
%---------------------------------------------------
x   =  [2.0  2.5  3.0  3.5  4.0  4.5  5.0   5.5  6.0];        % x coordinate  
y   =  2*x;                                                   % y is used only for plotting 

% connectivity array
IEN =  [1  3  5  7; 
        2  4  6  8; 
        3  5  7  9];

plotbar;
