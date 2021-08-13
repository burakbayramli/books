function [xy,mv,left,right,bottom,top,mbound,x,y] = ...
                     grid_yblock(xx,yy) 
%GRID_YBLOCK vertical domain Q2 grid generator
%  [xy,mv,left,right,bottom,top,mbound,x,y] = grid_yblock(xx,yy);
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
% 
%   IFISS function: DJS; 23 April 2012. 
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('   Grid generation for y-channel ...')
%----- non-uniform grid
xstart=xx(1); xend=xx(end); nx=length(xx)-1; 
ystart=yy(1); yend=yy(end); ny=length(yy)-1;
xl = xend-xstart; yl = yend-ystart;
% compute (x,y) coordinates of vertices
nxx=2*nx; nyy=2*ny;
nel=nx*ny;
% compute (x,y) coordinates of vertices
x=sort([xx,0.5*(xx(1:end-1)+xx(2:end))]); 
y=sort([yy,0.5*(yy(1:end-1)+yy(2:end))]);
nvtx=(nxx+1)*(nyy+1);
[X,Y]=meshgrid(x,y);
xx=reshape(X',nvtx,1);
yy=reshape(Y',nvtx,1);
xy=[xx(:),yy(:)];
%
% compute biquadratic element coordinates
mv=zeros(nel,9);
xloop=2*nx+1;
for jj=1:ny
      el=(jj-1)*nx+1:jj*nx; 
      mref=(jj-1)*2*xloop+1:2:(jj-1)*2*xloop+xloop-2; 
      mv(el,1) = mref;
      mv(el,2) = mref+2;
      mv(el,3) = mref+2*xloop+2;
      mv(el,4) = mref+2*xloop;
      mv(el,5) = mref+1;
      mv(el,6) = mref+xloop+2; 
      mv(el,7) = mref+2*xloop+1; 
      mv(el,8)=  mref+xloop;
      mv(el,9)=  mref+xloop+1;
end
%
% compute boundary vertices 
left=[1:xloop:nvtx]';
right=[xloop:xloop:nvtx]';
bottom=[1:xloop]';
top=[nvtx-xloop+1:nvtx]';
% compute boundary edges
mleft=[1:nx:nel]';      ileft=4*ones(size(mleft));
mright=[nx:nx:nel]';    iright=2*ones(size(mright));
mbottom=[1:nx]';        ibottom=1*ones(size(mbottom));
mtop=[nel-nx+1:nel]';   itop=3*ones(size(mtop));
mbound(:,1)=[mleft;mright;mbottom;mtop];
mbound(:,2)=[ileft;iright;ibottom;itop];
%
fprintf('done.\n')
return
