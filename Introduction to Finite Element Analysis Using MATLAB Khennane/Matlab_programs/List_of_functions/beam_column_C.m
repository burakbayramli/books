function[C] = beam_column_C(i)
%
% This function forms the transformation between 
% local and global coordinates
%
global nnd nel nne nodof eldof
global geom connec prop nf load
%
% retrieve the nodes of element i
%
node1=connec(i,1);
node2=connec(i,2);
%
%
% Retrieve the x and y coordinates of nodes 1 and 2
%
x1=geom(node1,1); y1=geom(node1,2);
x2=geom(node2,1); y2=geom(node2,2);
%
% Evaluate the angle that the memeber makes with the global axis X
%
if(x2-x1)==0
    if(y2>y1)
    theta=2*atan(1);
    else
    theta=-2*atan(1);
    end
else
    theta=atan((y2-y1)/(x2-x1));
end
%
% Construct the transformation matrix
%
C = [cos(theta)   -sin(theta)    0         0            0          0 ; ...
       sin(theta)    cos(theta)     0         0            0          0 ; ...
             0                0            1          0            0          0 ; ...
         0             0         0     cos(theta)   -sin(theta)    0 ; ...
         0             0         0     sin(theta)    cos(theta)    0 ; ...
         0             0         0          0                0             1 ];
%
% end function beam_column_C