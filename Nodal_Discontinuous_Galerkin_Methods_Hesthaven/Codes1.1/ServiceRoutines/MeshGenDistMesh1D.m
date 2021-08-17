function [Nv, VX, K, EToV] = MeshGenDistMesh1D()

% function [VX, K, EToV] = MeshGenDistMesh1D()
% Purpose  : Generate 1D mesh using DistMesh;

% distance function for a circle about xc=0
fd=inline('abs(p)-1','p'); 

% distribution weight function
fh=inline('abs(p)*0.075+0.0125','p'); 

% generate non-uniform example mesh in 1D using DistMesh
h0 = 0.025;            % chosen element spacing
[p,t]=distmeshnd(fd,fh,h0,[-1;1],[]); 
K = size(t,1); Nv = K+1;

% Sort elements in acending order
[x,i]=sort(p(t));   
t=t(i,:);
Met=sort(t','ascend')';      
EToV = t; VX = p(:,1);
return
