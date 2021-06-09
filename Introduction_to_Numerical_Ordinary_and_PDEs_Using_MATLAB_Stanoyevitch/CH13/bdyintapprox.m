function lineint = bdyintapprox(fun, tri, redges)
% function M-file for EFR 13.13
% inputs will be 'fun', an inline function (or M-file) of vars x, y; a  matrix 'tri' of
% nodes of a triangle in the plane, and a 2-column matrix 'redges', possibly empty ([ ]), 
% containing, as rows,  the corresponding node indices (from 1 to 3 indicating nodes
% by their row in 'tri') of nodes which are endpoints of segments of the triangle which are part 
% of the 'Robin' boundary (for an underlying FEM problem).  Thus the rows of 'redges' can include
% only the following three vectors:  [1 2], [1 3], and [2 3].  (Or permutations of these.) 
% The output, 'lineint' will be the the Newton-Coates approx. ((31) of Chapter 13) line integral of
% 'fun' over the Robin segments of the triangle.
lineint=0;
[rn cn] = size(redges); %rn = number of Robin edges
if rn == 0
    return
end
for i=1:rn
    nodes = redges(i,:);
    N1=tri(nodes(1),:);, N2=tri(nodes(2),:);
    N1x=N1(1);, N1y=N1(2);, N2x=N2(1);, N2y=N2(2);
    vec = N2-N1;
    approx=norm(vec,2)/6*(feval(fun,N1x,N1y)+4*feval(fun,(N1x+N2x)/2,(N1y+N2y)/2)+feval(fun,N2x,N2y));
    lineint=lineint+approx;
end

    
    