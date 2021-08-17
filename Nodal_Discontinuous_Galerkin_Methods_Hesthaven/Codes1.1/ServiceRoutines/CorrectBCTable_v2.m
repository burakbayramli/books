function BCType = CorrectBCTable_v2(EToV,VX,VY,BCType,fd,BCcode)

% function BCType = CorrectBCTable(EToV,BCType,fd,BCcode);
% Purpose: Setup BCType for boundary conditions in 2D
%
%    EToV   : Element-To-Vertice table
%    VX, VY : (x,y)-coordinates of mesh vertices
%    BCType : Table with types of faces for BC's
%    fd     : handle to distance function
%    BCcode : Integer for specific boundary type
%
% By Allan P. Engsig-Karup

Globals2D;

VNUM = [1 2;2 3;3 1]; % face orientations

pxc = 0.5*(VX(EToV)+VX(EToV(:,[2 3 1])));
pyc = 0.5*(VY(EToV)+VY(EToV(:,[2 3 1])));
dc  = abs(fd([pxc(:) pyc(:)])); % distances to boundaries from face centers
tol = 1e-4; % tolerance
idx = find(dc<tol);
BCType(idx) = BCcode;
return