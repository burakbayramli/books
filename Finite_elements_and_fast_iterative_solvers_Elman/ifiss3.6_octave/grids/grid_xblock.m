function [xy,mv,left,right,bottom,top,mbound,x,y] = ...
                     grid_xblock(xx,yy) 
%GRID_XBLOCK horizontal domain Q2 grid generator
%  [xy,mv,left,right,bottom,top,mbound,x,y] = grid_xblock(xx,yy);
%   input
%          xx      horizontal coordinate vector
%          yy      vertical coordinate vector
%   output
%          xy      nodal coordinate vector
%          mv      Q2 macroelement mapping matrix
%        left      left edge boundary nodes
%       right      right edge boundary nodes
%      bottom      bottom edge boundary nodes
%         top      top edge boundary nodes
%      mbound      macroelement boundary edge vector
%         x,y      plotting vector coordinates 
% 
%   IFISS function: DJS; 6 November 2011. 
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('   Grid generation for x-channel ...')
%----- non-uniform grid
xstart=xx(1); xend=xx(end); nx=length(xx)-1; 
ystart=yy(1); yend=yy(end); ny=length(yy)-1;
xl = xend-xstart; yl = yend-ystart;
nxx=2*nx; nyy=2*ny;
nel=nx*ny;
% compute (x,y) coordinates of vertices
x=sort([xx,0.5*(xx(1:end-1)+xx(2:end))]); 
y=sort([yy,0.5*(yy(1:end-1)+yy(2:end))]);
nvtx=(nxx+1)*(nyy+1);
[X,Y]=meshgrid(x,y);
xx=reshape(X,nvtx,1);
yy=reshape(Y,nvtx,1);
xy=[xx(:),yy(:)];
%
% compute biquadratic element coordinates
mv=zeros(nel,9);
yloop=2*ny+1;
for ii=1:nx
      el=(ii-1)*ny+1:ii*ny; 
      mref=(ii-1)*2*yloop+1:2:(ii-1)*2*yloop+yloop-2; 
      mv(el,1) = mref;
      mv(el,2) = mref+2*yloop;
      mv(el,3) = mref+2*yloop+2;
      mv(el,4) = mref+2;
      mv(el,5) = mref+yloop;
      mv(el,6) = mref+2*yloop+1; 
      mv(el,7) = mref+yloop+2; 
      mv(el,8)=  mref+1;
      mv(el,9)=  mref+yloop+1;
end
%
% compute boundary vertices 
left=[1:yloop]';
right=[nvtx-yloop+1:nvtx]';
bottom=[1:yloop:nvtx]';
top=[yloop:yloop:nvtx]';
% compute boundary edges
mleft=[1:ny]';          ileft=4*ones(size(mleft));
mright=[nel-ny+1:nel]'; iright=2*ones(size(mright));
mbottom=[1:ny:nel]';    ibottom=1*ones(size(mbottom));
mtop=[ny:ny:nel]';      itop=3*ones(size(mtop));
mbound(:,1)=[mleft;mright;mbottom;mtop];
mbound(:,2)=[ileft;iright;ibottom;itop];
%
fprintf('done.\n')
return
