%TWOSTEP_DOMAIN backward-facing double step domain Q2 grid generator
%   twostep_domain
% Prior to calling the script file the variable outbnd needs
% to be set to the outflow length.
% The grid definition data is saved to the file: step_grid.mat
%   IFISS function: DJS; 23 November 2014.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
fprintf('\nGrid generation for symmetric backward-facing step domain.\n')
nc=default('grid parameter (default is 4)',4);
stretch=default('grid stretch factor (default is 1)',1);
n=2^(nc-2); 
if n<2, error('illegal nc parameter, try again.'), end
%
% generate the inlet grid
if stretch==1,
xx=[-1:1/n:0];  yy=[-1/2:1/n:1/2];
elseif stretch>1
dy=(1-stretch)/(1-stretch^n);
dd(1)=dy;for k=1:n-1, dd(k+1)=dd(k)+dy*stretch^k; end, dd(n)=1;
dxd=[dd/2,1/2+1/n:1/n:1]; 
% sanity check
if any(diff(dxd)<0),
error('Ops.. this level grid cannot be stretched'), end
xx=sort([-dxd,0]);
dy=sort([1/2-dd(1:n-1)/2]); dyd = sort([dy,1/2,1-dy,1]);
yy=sort([-1/2,-dy,0,dy,1/2]);
else, error('illegal stretch parameter, try again.')
end
[xyl,mvl,leftl,rightl,bottoml,topl,mboundl,xl,yl] = grid_xblock(xx,yy);
refleft=leftl;refright=rightl; refbottom=bottoml;reftop=topl;
bound=unique([leftl;rightl;topl;bottoml]);
%macrogridplot(xyl,mvl,bound,mboundl), pause
%
if stretch==1; 
xx=[0:1/n:outbnd]; yy=[-1:1/n:1];
elseif stretch>1
%dd(1)=dy;for k=1:n-1, dd(k+1)=dd(k)+dy*stretch^k; end, dd(n)=1;
ddx=dd(n)-dd(n-1);ndx=ceil(1/ddx);
fprintf('   outlet subdivision parameter set to %d\n',ndx) 
fprintf('   ... associated uniform grid value is %d\n',n) 
xx=sort([0,dxd,[1+1/n:1/n:outbnd]]);
yy=sort([-dyd,0,dyd]);
end
[xyr,mvr,leftr,rightr,bottomr,topr,mboundr,xr,yr] = grid_xblock(xx,yy);
bound=unique([leftr;rightr;topr;bottomr]);
%macrogridplot(xyr,mvr,bound,mboundr), pause
%
% merge grids
[xy,mv,left,bottom,top,right,mbound,x,y] = ...
   grid_mergeleftright(xyl,mvl,leftl,rightl,bottoml,topl,mboundl,xl,yl,...
   xyr,mvr,leftr,rightr,bottomr,topr,mboundr,xr,yr);
bound=unique([left;top;bottom]);
%macrogridplot(xy,mv,bound,mbound);
%figure(1),pause(1),set(gcf,'Visible','off'), 
%figure(2), pause(1),set(gcf,'Visible','off'), 
fprintf('  All done.\n\n')
%
% specify boundary information for graphics
% bndxy: (x,y)-coordinates of vertices that define the domain and
%         obstacle(s) 
% bnde: boundary edges (node1 node2 1(for dirichlet)/0(for neumann))
% obs: obstacles (node1 node2 node3 node4)
% sbnde: boundary edges near which stretching is needed (edge1 edge2 ...)
% 'obs' and/or 'sbnde' can be absent if there is no obstacle in the problem
% and/or only uniform grid is needed
bndxy = [-1,-1; 0,-1; outbnd,-1; outbnd,1; 0,1; -1,1; ...
        -1,0.5; 0,0.5; -1,-0.5; 0,-0.5];
bnde = [2, 3, 1; 3, 4, 0; 4, 5, 1; 5,8,1; 8,7,1; 7,9,1; 9,10,1; 10,2,1];
obs = [1, 2, 10, 9; 7, 8, 5, 6; ];
sbnde = [5 7];
gohome 
cd datafiles
save step_grid.mat mv xy bound mbound outbnd stretch x y bndxy bnde obs sbnde
return
