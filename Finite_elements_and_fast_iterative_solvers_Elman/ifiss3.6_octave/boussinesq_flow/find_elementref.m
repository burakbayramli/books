function [e1,s1,t1,e2,s2,t2]=find_elementref(xyv,mv2,x1,y1,x2,y2);
%FIND_ELEMENTREF finds elements that contain reference points
%  [e1,s1,t1,e2,s2,t2]=find_elementref(xyv,mv2,x1,y1,x2,y2);							
%   input
%          xyv        Q2 nodal coordinate vector 
%          mv2        Q2 element mapping matrix
%          x1,y1      coordinates of first point
%          x2,y2      coordinates of second point
%   output
%          e1,s1,t1   first element and local coordinates
%          e2,s2,t2   second element and local coordinates
%   IFISS function: 29 April 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.

nel=length(mv2(:,1));      %  number of elements
d1_min=inf; d2_min=inf;    %  set the minimal distances
for i=1:nel                %  loop over all elements
   d1=0; d2=0; 
   for j=1:4               %  loop over all vertices
      d1=d1+(xyv(mv2(i,j),1)-x1)^2+(xyv(mv2(i,j),2)-y1)^2;
      d2=d2+(xyv(mv2(i,j),1)-x2)^2+(xyv(mv2(i,j),2)-y2)^2;
   end
   if(d1<d1_min)
      d1_min=d1; e1=i;
   end
   if(d2<d2_min)
      d2_min=d2; e2=i;
   end
end
s1=(2.0*x1-xyv(mv2(e1,1),1)-xyv(mv2(e1,2),1))/(xyv(mv2(e1,2),1)-xyv(mv2(e1,1),1));
t1=(2.0*y1-xyv(mv2(e1,1),2)-xyv(mv2(e1,3),2))/(xyv(mv2(e1,3),2)-xyv(mv2(e1,1),2));
s2=(2.0*x2-xyv(mv2(e2,1),1)-xyv(mv2(e2,2),1))/(xyv(mv2(e2,2),1)-xyv(mv2(e2,1),1));
t2=(2.0*y2-xyv(mv2(e2,1),2)-xyv(mv2(e2,3),2))/(xyv(mv2(e2,3),2)-xyv(mv2(e2,1),2));
return
