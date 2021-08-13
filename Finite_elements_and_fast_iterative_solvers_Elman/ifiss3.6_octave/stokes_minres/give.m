function g = give(x,y)
%GIVE computes Givens rotation matrix
%   IFISS function: DJS; 2 January 2011.
%
%
%Modification of old MathWorks function GIVENS.m
%  Copyright (c) 1986-93 by The MathWorks, Inc.
%GIVENS Givens rotation matrix.
%       G = GIVENS(x,y) returns the complex Givens rotation matrix
%
%           | c       s |                  | x |     | r | 
%       G = |           |   such that  G * |   |  =  |   |
%           |-conj(s) c |                  | y |     | 0 |
%                                       
%       where c is real, s is complex, and c^2 + |s|^2 = 1. 
 

absx = abs(x);
if absx == 0.0
        c = 0.0; s = 1.0;
else
%       nrm = sqrt(x^2+y^2);
        nrm = norm([x y]);
        c = absx/nrm;
        s = x/absx*(conj(y)/nrm);
end
g = [c s;-conj(s) c];
return
