function [p,e,t] = bsp10
% Example: Flow past cylinder
% Coarse mesh

p =  [0, 10, 10,  0,  7,  3, 20, 20,  5,  5;
      0,  0, 10, 10,  5,  5,  0, 10,  3,  7];
% move cavity to right      
p =  [0, 14, 14,  0,  9,  5, 20, 20,  7,  7;
      0,  0, 10, 10,  5,  5,  0, 10,  3,  7];

      
t =  [4     6     2     5     3     1     3     2     2     4;
      1     1     3     3     4     2     2     7     5     6;
      6     9     5    10    10     9     8     8     9    10;
      1     1     1     1     1     1     2     2     1     1];
      
e = [1  2  3  4    5    9    6    10  2  7  8; % indices of starting points
     2  3  4  1    9    6   10     5  7  8  3; % indices of ending points 
     0  0  0  0    0  0.5    0   0.5  0  0  0; % left parameter value in edge
     1  1  1  1  0.5    1  0.5     1  1  1  1; % right parameter value in edge 
     1  2  3  4    5    5    6     6  7  8  9; % segment number
     1  1  1  1    1    1    1     1  2  2  2;% left subdomain number
     0  2  0  0    0    0    0     0  0  0  0];
