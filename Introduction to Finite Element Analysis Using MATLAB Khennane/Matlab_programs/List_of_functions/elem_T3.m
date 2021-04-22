function[bee,g,A] = elem_T3(i)
%
% This function returns the coordinates of the nodes of element i
% and its steering vector
%
global nnd nel nne nodof eldof n 
global geom connec dee nf load
%
x1 = geom(connec(i,1),1);   y1 = geom(connec(i,1),2);
x2 = geom(connec(i,2),1);   y2 = geom(connec(i,2),2);
x3 = geom(connec(i,3),1);   y3 = geom(connec(i,3),2);
%
A = (0.5)*det([1   x1    y1; ...
               1   x2    y2; ...
               1   x3    y3]);
%
 m11 = (x2*y3 - x3*y2)/(2*A);
 m21 = (x3*y1 - x1*y3)/(2*A);
 m31 = (x1*y2 - y1*x2)/(2*A);
 m12 = (y2 - y3)/(2*A);
 m22 = (y3 - y1)/(2*A);
 m32 = (y1 - y2)/(2*A);
 m13 = (x3 - x2)/(2*A);
 m23 = (x1 - x3)/(2*A);
 m33 = (x2 -x1)/(2*A);
%
bee = [ m12   0    m22   0    m32      0; ...
         0   m13    0   m23    0     m33; ...
        m13  m12   m23  m22   m33    m32] ;
%
l=0;
for k=1:nne   
    for j=1:nodof
    l=l+1;
    g(l)=nf(connec(i,k),j);
    end
end
%
% End function elem_T3