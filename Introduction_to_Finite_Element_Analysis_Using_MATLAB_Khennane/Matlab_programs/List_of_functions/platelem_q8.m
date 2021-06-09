function[coord,g] = platelem_q8(i)
%
% This function returns the coordinates of the nodes of element i
% and its steering vector
%
global nne nodof geom connec  nf  dim
%
coord=zeros(nne,dim);
for k=1: nne
    for j=1:dim
    coord(k,j)=geom(connec(i,k),j);
    end
end
%
l=0;
for k=1: nne
    for j=1:nodof
    l=l+1;
    g(l)=nf(connec(i,k),j);
    end
end
%
% End function platelem_q8