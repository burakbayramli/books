function[g] = beam_column_g(i)
%
% This function forms the steering vector for element i
%
global nnd nel nne nodof eldof
global geom connec prop nf load
%
% retrieve the nodes of element i
%
node1=connec(i,1);
node2=connec(i,2);
%
% Retrieve the element degrees of freedom to be stored 
% in the steering vector
%
g=[nf(node1,1); ...
   nf(node1,2); ...
   nf(node1,3); ...
   nf(node2,1); ...
   nf(node2,2); ...
   nf(node2,3)];
%
% end function beam_column_g
