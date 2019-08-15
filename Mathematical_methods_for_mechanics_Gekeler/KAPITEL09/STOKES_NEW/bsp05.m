function [p,e,t] = bsp05
% Example after Boukir with exact solution
% Square, Coarse triangular mesh
% p/e/t : nodes/edges/triangles

p = [ 0,    1,    1,   0,   0.5,  0.5   1  0.5  0;
     -0.5, -0.5,  0.5, 0.5, 0  , -0.5,  0  0.5  0]; 

t = [1, 6, 6, 2, 5, 7,  5, 4;
     6, 5, 2, 7, 7, 3,  8, 9;
     9, 9, 5, 5, 8, 8,  4, 5;
     1, 1, 1, 1, 1, 1,  1, 1]; % Subdomain indices
     
e = [1,   6,   2,   7    3    8    4    9 ;  % indices of starting points
     6,   2,   7,   3    8    4    9    1 ;  % indices of ending points
     0,   0.5, 0,   0.5  0    0.5  0    0.5; % left parameter value in edge
     0.5, 1,   0.5, 1    0.5  1    0.5  1;   % right parameter value in edge 
     1,   1,   2,   2,   3,   3,   4    4;   % segment number
     1,   1,   1,   1,   1,   1,   1    1;   % left subdomain number
     0,   0,   0,   0,   0,   0    0    0];  % right subdomain number (exterior domain)

