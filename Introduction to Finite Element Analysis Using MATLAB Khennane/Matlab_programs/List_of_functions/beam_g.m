function[g] = beam_g(i)
%
% This function forms the steering vector for element i
%
global connec nf 
%
% retrieve the nodes of element i
%
node_1=connec(i,1);
node_2=connec(i,2);
%
% Form the steering vector from element's degrees 
% of freedom
%
g=[nf(node_1,1); nf(node_1,2); nf(node_2,1);nf(node_2,2)];
%
%%%%%%%%%%%%%%%%%% end function beam_g %%%%%%%%%%%%%%%%