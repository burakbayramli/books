%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [X,Y,Z,TRI]=CAGD_sbezier(T,XP,YP,ZP);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function [X,YZ,TRI]=CAGD_sbezier(T,XP,YP,ZP)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   Sampling of Bézier surface by de Boor - Coox algorithm
%%   
%%   Input :  T sampling values
%%            XP, YP, ZP control points coordinates 
%%
%%   Output : X, Y, Z sampling points coordinates
%%            TRI list of triangular faces of the surface
%%   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
n=size(T,2);n1=n-1;n2=n*n;
X=zeros(n2,1);Y=zeros(n2,1);Z=zeros(n2,1);
for k1=1:n
for k2=1:n
t1=T(k1);t2=T(k2);
[x,y,z]=CAGD_coox(t1,t2,XP,YP,ZP);
k=k1+(k2-1)*n;
X(k)=x;Y(k)=y;Z(k)=z;
end
end
%%  triangles (faces of surface)
m=n1*n1*2;
TRI=zeros(m,3);
for k1=1:n1
for k2=1:n1
k=k1+(k2-1)*n1;
kk=k1+(k2-1)*n;
%% 1st triangle
TRI(2*k-1,1)=kk;
TRI(2*k-1,2)=kk+1;
TRI(2*k-1,3)=kk+1+n;
%% 2nd  triangle
TRI(2*k,1)=kk;
TRI(2*k,2)=kk+n;
TRI(2*k,3)=kk+n+1;
end
end