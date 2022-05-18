%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%===============================================
% Computes [2*Aire(k)*grad(wi)] for the triangle k
% i local number of the vertex (i=1, 2 or 3)
%================================================
function [grd]=Gradwi(k,i)

global XYs I123

% Coordinates [x y] of the vertex
%q1=XYs(I123(k,i),:);
q2=XYs(I123(k,mod(i,3)+1),:);
q3=XYs(I123(k,mod(i+1,3)+1),:);

% Verification
%Q=[q1;q2;q3;q1];plot(Q(:,1),Q(:,2));hold on

qo=q3-q2;                           % opposed edge
grd=[-qo(2) qo(1)];                 % grd=1/(perpendicular on the opposed edge)
