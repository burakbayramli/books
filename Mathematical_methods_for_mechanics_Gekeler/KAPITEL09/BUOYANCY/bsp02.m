function [p,e,t] = bsp02
% convection in unit square
p = [0     1     1     0;
     0     0     1     1];
t = [1     2;
     2     3;
     4     4;
     1     1];
e = [1     2     3     4;
     2     3     4     1;
     0     0     0     0;
     1     1     1     1;
     1     2     3     4;
     1     1     1     1;
     0     0     0     0];