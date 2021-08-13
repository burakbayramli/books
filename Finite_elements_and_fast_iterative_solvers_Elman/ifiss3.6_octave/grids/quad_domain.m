function quad_domain
%QUAD_DOMAIN quadrilateral domain Q2 grid generator
%   quad_domain;
% 
% optional Neumann boundary condition on right-hand edge
% grid defining data is saved to the file: quad_grid.mat
%   IFISS function: DJS; 11 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
fprintf('\n\nGrid generation for quadrilateral domain.\n')
grid_type=1;
nc=default('grid parameter: 3 for underlying 8x8 grid (default is 16x16)',4);
if nc<2, error('illegal parameter choice, try again.'), end
aspect=default('aspect ratio (x:1) (default is 4:1)',4);
if aspect<1, error('illegal parameter choice, try again.'), end
deform=default('height contraction ratio (default is 4)',4);
if deform<1, error('illegal parameter choice, try again.'), end
ny=2^nc; nx=aspect*ny; n=nx*ny; 
npx=nx/2; nqx=n/4; npy=ny/2; nqy=ny/4;
%
%% compute (x,y) coordinates of vertices
% y-direction: left hand boundary
yy=[0:1/ny:1]; y=yy;
% x-direction
xx=[0:aspect/nx:aspect]; x=xx;
%
%% compute biquadratic element coordinates
nvtx=(nx+1)*(ny+1);
%% uniform grid
[X,Y]=meshgrid(x,y); 
if deform ~= 1
   slope=(1/aspect)*(1/deform-1);
   hts=slope*x+1;
   R=(hts'*ones(1,ny+1))';
   Y=Y.*R;
end
xx=reshape(X,nvtx,1);
yy=reshape(Y,nvtx,1);
xy=[xx(:),yy(:)];
%
kx = 1;
ky = 1;
mel=0;
for i=1:npx
   for j=1:npy
      mref=(ny+1)*(kx-1)+ky;
      mel=mel+1;
      nvv(1) = mref;
      nvv(4) = mref+2;
      nvv(3) = mref+2*ny+4;
      nvv(2) = mref+2*ny+2;
      nvv(8) = mref+1;
      nvv(7) = mref+ny+3; 
      nvv(6) = mref+2*ny+3; 
      nvv(5)=  mref+ny+1;
      nvv(9)=  mref+ny+2; 
      mv(mel,1:9)=nvv(1:9);
      ky = ky + 2;
   end
   kx = kx + 2; 
   ky = 1;
end
%
%% compute boundary vertices and edges
% four boundary edges 
k1=find( xy(:,2)==0 );
e1=[]; for k=1:mel, if any(mv(k,5)==k1), e1=[e1,k]; end, end
ef1=ones(size(e1));
%
k2=find( xy(:,1)==aspect  & xy(:,2)<1/deform   & xy(:,2) >0);
e2=[]; for k=1:mel, if any(mv(k,6)==k2), e2=[e2,k]; end, end
ef2=2*ones(size(e2));
%
slope=(1/aspect)*(1/deform-1);
k3=find( xy(:,2)==slope*xy(:,1) +1 );
e3=[]; for k=1:mel, if any(mv(k,7)==k3), e3=[e3,k]; end, end
ef3=3*ones(size(e3));
%
k4=find( xy(:,1)==0 & xy(:,2)<1   & xy(:,2) >0 );
e4=[]; for k=1:mel, if any(mv(k,8)==k4), e4=[e4,k]; end, end
ef4=4*ones(size(e4));
%
outbc_query=default('outflow boundary: natural/prescribed 1/2 (default is natural)',1);
outbc = outbc_query-1;
if outbc==1,
   bound=sort([k1;k2;k3;k4]);
   mbound=[e1',ef1';e2',ef2';e3',ef3';e4',ef4'];	
else
   bound=sort([k1;k3;k4]);
   mbound=[e1',ef1';e3',ef3';e4',ef4'];
end
%
% plotting of the grid 
adj=sparse(nvtx,nvtx);
for i=1:mel
adj(mv(i,1),mv(i,5))=1;
adj(mv(i,5),mv(i,2))=1;
adj(mv(i,2),mv(i,6))=1;
adj(mv(i,6),mv(i,3))=1;
adj(mv(i,3),mv(i,7))=1;
adj(mv(i,7),mv(i,4))=1;
adj(mv(i,4),mv(i,8))=1;
adj(mv(i,8),mv(i,1))=1;
adj(mv(i,9),mv(i,5))=1;
adj(mv(i,9),mv(i,6))=1;
adj(mv(i,9),mv(i,7))=1;
adj(mv(i,9),mv(i,8))=1;
end
fprintf('plotting grid points ...\n')
figure
gplot(adj,xy,'b')
axis('square')
hold on
xybd=xy(bound,:);
plot(xybd(:,1),xybd(:,2),'ro')
hold off
title('finite element subdivision')
drawnow
axis('equal'), axis('off')
%%
y=[0:1/(ny*deform):1];
gohome, cd datafiles
save quad_grid.mat mv xy bound mbound deform aspect x y grid_type
return
