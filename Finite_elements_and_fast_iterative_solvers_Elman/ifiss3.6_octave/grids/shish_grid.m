function [x,y] = shish_grid(nc,epsilon,beta_y)
%SHISH_GRID Shishkin grid generator on unit domain
%   [x,y]=shish_grid(nc,epsilon,beta_y);
%   input
%          nc         grid parameter: generates 2^nc * 2^nc grid
%          epsilon    diffusion parameter
%          beta_y     vertical wind magnitude 
%
%   called by ref_domain
%   IFISS function: DJS, AR; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nx=2^nc;
ny=2^nc;
disp('Calculating subdivision ...')
fprintf('%3.0f by %3.0f grid \n',nx,ny)

% uniform grid in x direction
x=0:1/nx:1;

% Shishkin grid in y direction
% changeover point: exponential layer
sigy=min(0.5,(2*epsilon*log(ny))/beta_y);

h1=(2*(1-sigy))/ny;
h2=(2*sigy)/ny;
y(1)=0;
for i=2:(ny/2)+1
 y(i)=y(i-1)+h2;
end
for i=(ny/2)+2:ny
   y(i)=y(i-1)+h1;
end
y(ny+1)=1.0;

% boundary layer is at the top
y=1.0-fliplr(y);

% plot mesh?
meshplot=0;
if meshplot~=0
   [X,Y]=meshgrid(x,y);
   Z=ones(size(X));
   figure
   surf(X,Y,Z)
   axis('square')
   view(0,90)
end
x=x';y=y';
return

