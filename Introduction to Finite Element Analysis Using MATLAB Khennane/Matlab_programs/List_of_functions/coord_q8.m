function[coord] = coord_q8(k,nne, geom, connec )
%
% This function returns the coordinates of the nodes of element k
%
coord=zeros(nne,2);
for i=1: nne
    coord(i,:)=geom(connec(k,i),:);
end
%
% End function coord_q8