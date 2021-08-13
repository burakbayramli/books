function plate_domain
%PLATE_DOMAIN slit shaped domain Q2 grid generator
%   plate_domain;
% 
% grid defining data is saved to the file: plate_grid.mat
%   IFISS function: DJS; 11 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
fprintf('\n\nGrid generation for a slit domain.\n')
nc=default('grid parameter: 3 for underlying 8x8 grid (default is 4)',4);
if nc<2, error('illegal parameter choice, try again.'), end
stretch=default('grid stretch factor (default is 1)',1);
n=2^nc; np=n/2; nx=n; ny=n; npx=np; npy=np;
%
%% compute (x,y) coordinates of vertices
% y-direction
if stretch >1
   dy=(1-stretch)/(1-stretch^np);
   yy(1)=dy;
   for k=1:np-1, yy(k+1)=yy(k)+dy*stretch^k; end
   yy(np)=1;
   else
   yy=[1/np:1/np:1];
   end
ypos=[0,yy];
yneg=-yy(length(yy):-1:1);
y=[yneg,ypos]';
% x-direction
xneg=yneg;
xpos=5*ypos;
x=[xneg,xpos]';
%
%% compute biquadratic element coordinates
nvtx=(nx+1)*(ny+1);
[X,Y]=meshgrid(x,y);
xx=reshape(X,nvtx,1);
yy=reshape(Y,nvtx,1);
xy=[xx(:),yy(:)];
%
mel=0; mref=-1; 
for i=1:np
for j=1:np
mref=mref+2;
mel=mel+1;
nvv(1) = mref;
nvv(2) = mref+2*ny+2;
nvv(3) = mref+2*ny+4;
nvv(4) = mref+2;
nvv(5) = mref+ny+1;
nvv(6) = mref+2*ny+3; 
nvv(7) = mref+ny+3; 
nvv(8)=  mref+1;
nvv(9)=  mref+ny+2; 
mv(mel,1:9)=nvv(1:9);
end
mref=mref+ny+2; 
end

%
%% compute boundary vertices
% five boundary edges 
%
%
k3=find( xy(:,2)==-1);
e3=[]; for k=1:mel, if any(mv(k,5)==k3), e3=[e3,k]; end, end
ef3=ones(size(e3));
%
k5=find( xy(:,2)==1 );
e5=[]; for k=1:mel, if any(mv(k,7)==k5), e5=[e5,k]; end, end
ef5=3*ones(size(e5));
%
k6=find( xy(:,1)==-1 & xy(:,2)<1   & xy(:,2) >-1 );
e6=[]; for k=1:mel, if any(mv(k,8)==k6), e6=[e6,k]; end, end
ef6=4*ones(size(e6));
%
k7=find( xy(:,2)==0 & xy(:,1)>=0 );
e7=[]; for k=1:mel, if any(mv(k,7)==k7), e7=[e7,k]; end, end
ef7=3*ones(size(e7));
e8=[]; for k=1:mel, if any(mv(k,5)==k7), e8=[e8,k]; end, end
ef8=ones(size(e8));
%
bound=sort([k3;k5;k6;k7]);
mbound=[e3',ef3';e5',ef5';e6',ef6';e7',ef7';e8',ef8'];
%
%
gohome 
cd datafiles
save plate_grid.mat mv xy bound mbound stretch x y
return
