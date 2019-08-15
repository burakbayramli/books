function [p,e,t] = bsp01
% Example: LID DRIVEN CAVITY
% Square, Coarse triangular mesh
% p/e/t : nodes/edges/triangles

p = [0, 1, 1, 0, 0.5;
     0, 0, 1, 1, 0.5]; 

t = [1, 2, 3, 4;
     2, 3, 4, 1;
     5, 5, 5, 5;
     1, 1, 1, 1]; % Subdomain indices
     
e = [1, 2, 3, 4;  % indices of starting points
     2, 3, 4, 1;  % indices of ending points
     0, 0, 0, 0;  % left parameter value in edge
     1, 1, 1, 1;  % right parameter value in edge 
     1, 2, 3, 4;  % segment number
     1, 1, 1, 1;  % left subdomain number
     0, 0, 0, 0]; % right subdomain number (exterior domain)

