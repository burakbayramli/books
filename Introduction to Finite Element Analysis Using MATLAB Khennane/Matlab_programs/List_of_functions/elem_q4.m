function[coord,g] = elem_q4(i)
%
% This function returns the coordinates of the nodes of 
% element i and its steering vector g
%
global nnd nel nne nodof eldof n ngp
global geom connec dee nf load
%
l=0;
coord=zeros(nne,nodof);
for k=1: nne
    for j=1:nodof
    coord(k,j)=geom(connec(i,k),j);
    l=l+1;
    g(l)=nf(connec(i,k),j);
    end
end
%
% End function elem_q4