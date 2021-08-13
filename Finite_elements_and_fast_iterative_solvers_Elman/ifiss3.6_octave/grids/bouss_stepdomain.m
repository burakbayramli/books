function bouss_stepdomain
%BOUSS_STEPDOMAIN step domain grid generator for Boussinesq
%   bouss_stepdomain;
%
%  Grid defining data is saved to two files: step_grid1h, step_grid
%   IFISS scriptfile: DJS; 3 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.

fprintf('\nGrid generation for Boussinesq backward facing step domain \n');
H=1;
L=default('Input the outlet length L (default 5)',5);
outbnd=L;
nc=default('grid parameter: 3 for underlying 8x24 grid (default is 4)',4);
stretch=default('grid stretch factor (default is 1)',1);
if stretch>1, gt=2; else gt=1; end
n=2^(nc-2); 
if n<1, error('illegal nc parameter, try again.'), end
%
% generate the inlet grid
if stretch==1,
xx=[-1:1/n:0];  yy=[0:1/n:1];
elseif stretch>1
dy=(1-stretch)/(1-stretch^n);
dd(1)=dy;for k=1:n-1, dd(k+1)=dd(k)+dy*stretch^k; end, dd(n)=1;
xx=sort([0,-dd]); 
yy=[0,dd];
else, error('illegal stretch parameter, try again.')
end
[xyl,mvl,leftl,rightl,bottoml,topl,mboundl,xl,yl] = grid_xblock(xx,yy);
refleft=leftl;refright=rightl; refbottom=bottoml;reftop=topl;
bound=unique([leftl;rightl;topl;bottoml]);
%
if stretch==1; 
xx=[0:1/n:outbnd]; yy=[-1:1/n:1];
elseif stretch>1
dd(1)=dy;for k=1:n-1, dd(k+1)=dd(k)+dy*stretch^k; end, dd(n)=1;
ddx=dd(n)-dd(n-1);ndx=ceil(1/ddx);
fprintf('   outlet subdivision parameter set to %d\n',ndx) 
fprintf('   ... associated uniform grid value is %d\n',n) 
xx=sort([0,dd,[1+1/ndx:1/ndx:outbnd]]);
yy=sort([-dd,0,dd]); 
end
[xyr,mvr,leftr,rightr,bottomr,topr,mboundr,xr,yr] = grid_xblock(xx,yy);
bound=unique([leftr;rightr;topr;bottomr]);
%
% merge grids
[xy,mv,left,bottom,top,right,mbound,x,y] = ...
   grid_mergeleftright(xyl,mvl,leftl,rightl,bottoml,topl,mboundl,xl,yl,...
   xyr,mvr,leftr,rightr,bottomr,topr,mboundr,xr,yr);
bound=unique([left;top;bottom]);
fprintf('  All done.\n\n')
%
% grid parameters
lh=1; gh=1; nh=1;   % single grid only
net=2;              % y-direction enumeration 
str=stretch; sx=str; sy=1;
npx=length(x)-1; npy=length(y)-1;
hty=1;
hbbc=1;
%end conditions
%fprintf('Type of BC for the temperature on vertical walls:');
%fprintf('\n   1   Adiabatic (Neumann, default)');
%fprintf('\n   2   Perfectly conducting (Dirichlet)');
%hbbc=default('',1);
nx=2*npx;             %  number of lines of nodes in x direction
ny=2*npy;             %  number of lines of nodes in y direction 
ne=length(mv(:,1));   %  number of elements
hxmin=min(diff(x)); hxmax=max(diff(x));
hymin=min(diff(y)); hymax=max(diff(y));
%
% Grid statistics
   fprintf('\nGrid statistics (level %2i):\n',lh);
   fprintf('   Number of elements: %5i (%3i x %3i)\n',ne,npx,npy);
   fprintf('   hxmin=%6.4f    hxmax=%6.4f\n',hxmin,hxmax);
   fprintf('   hymin=%6.4f    hymax=%6.4f\n',hymin,hymax);
%macrogridplot(xy,mv,bound,mbound); 
%
oldxy=xy;
% generate Q1 element coordinates for pressure
[xp,yp,xy,xyp,mp,map] = q2q1gridx(x,y,xy,mv,bound);
if norm(xy-oldxy)~=0 then
error('Error in setting up Q2 grid, try again.'), end
%
% set grid data
   grid(1).npx=npx;
   grid(1).npy=npy;
   grid(1).ne=ne;
   grid(1).mv2=mv;     
   grid(1).mp1=mp;
   grid(1).mt2=mv;
   grid(1).mt1=mp;
   grid(1).xy2=xy;
   grid(1).xy1=xyp;
   grid(1).x=x;
   grid(1).y=y;
   grid(1).bnd_d=bound;
   grid(1).bnd_dn2=unique([top;bottom]);
   grid(1).bnd_dn1=[NaN];
   grid(1).bnd_dv2=mbound;
   grid(1).bnd_dnt1=[NaN];
   kkbottom=find(mbound(:,2)==1); kktop=find(mbound(:,2)==3);
   grid(1).bnd_dnt2=mbound([kkbottom;kktop],:);   
%
%  Save the grid hierarchy to a relevant file
%
gohome; cd datafiles
% heat equation case
grid_type=gt; bound=unique([top;bottom]); 
mbound=mbound([kkbottom;kktop],:);
save step_grid.mat mv xy bound mbound stretch x y 
% Boussinesq case
save step_grid1h.mat grid str hbbc hty H L gh nh
fprintf('Grid data saved in step_grid1h.mat.\n')
return
