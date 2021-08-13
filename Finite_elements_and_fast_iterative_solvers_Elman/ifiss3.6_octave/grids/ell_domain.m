function ell_domain
%ELL_DOMAIN L-shape domain Q2 grid generator
%   ell_domain;
% 
% grid defining data is saved to the file: ell_grid.mat
%   IFISS function: DJS; 6 May 2006.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

fprintf('\n\nGrid generation for a simple L-shaped domain.\n')
nc=default('grid parameter: 3 for underlying 8x8 grid (default is 4)',4);
if nc<2, error('illegal parameter choice, try again.'), end
stretch=default('grid stretch factor (default is 1)',1);
n=2^nc; np=n/2; nq=n/4;
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
y=[yneg,ypos];
% x-direction
xx=yy;
xpos=[0,xx];
xneg=-xx(length(xx):-1:1);
x=[xneg,xpos];
%
%% compute biquadratic element coordinates
nvtx= np*(np*1) + (np+1)*(n+1);
% negative x-values
[Xneg,Ypos]=meshgrid(xneg,ypos);
xx=reshape(Xneg',np*(np+1),1);
yy=reshape(Ypos',np*(np+1),1);
xyleft=[xx(:),yy(:)];
kx = 1;
ky = 1;
mel=0;
% symbolic assembly 
% loop over 2x2 macroelements
for j=1:nq
for i=1:nq
mref=np*(ky-1)+kx;
mel=mel+1;
nvv(1) = mref;
nvv(2) = mref+2;
nvv(3) = mref+2*np+2;
nvv(4) = mref+2*np;
nvv(5) = mref+1;
nvv(6) = mref+np+2; 
nvv(7) = mref+2*np+1; 
nvv(8)=  mref+np;
nvv(9)=  mref+np+1; 
mv(mel,1:9)=nvv(1:9);
kx = kx + 2;
end
ky = ky + 2;
kx = 1;
end
% correction along the internal boundary
mref=2*np*(np+1)+1;
for mel=nq:nq:nq*nq;
nvv=mv(mel,:);
nvv(2) = mref;
nvv(3) = mref+2*np+2;
nvv(6) = mref+np+1; 
mv(mel,1:9)=nvv(1:9);
mref=mref+2*np+2;
end	
%
% positive x_values
[Xpos,Y]=meshgrid(xpos,y);
xx=reshape(Xpos',(np+1)*(n+1),1);
yy=reshape(Y',(np+1)*(n+1),1);
xyright=[xx(:),yy(:)];
xy=[xyleft;xyright]; 
%
kx = 1;
ky = 1;
mel=nq*nq;
for j=1:np
for i=1:nq
mref = (np+1)*(ky-1)+kx + np*(np+1);
mel=mel+1;
nvv(1) = mref;
nvv(2) = mref+2;
nvv(3) = mref+2*np+4;
nvv(4) = mref+2*np+2;
nvv(5) = mref+1;
nvv(6) = mref+np+3; 
nvv(7) = mref+2*np+3; 
nvv(8)=  mref+np+1;
nvv(9)=  mref+np+2; 
mv(mel,1:9)=nvv(1:9);
kx = kx + 2;
end
ky = ky + 2;
kx = 1;
end
%
%% compute boundary vertices
% six boundary edges 
k1=find( xy(:,1) <0  & xy(:,2)==0 );
e1=[]; for k=1:mel, if any(mv(k,5)==k1), e1=[e1,k]; end, end
ef1=ones(size(e1));
%
k2=find( xy(:,1)==0  & xy(:,2)<=0 );
e2=[]; for k=1:mel, if any(mv(k,8)==k2), e2=[e2,k]; end, end
ef2=4*ones(size(e2));
%
k3=find( xy(:,1) >0  & xy(:,2)==-1);
e3=[]; for k=1:mel, if any(mv(k,5)==k3), e3=[e3,k]; end, end
ef3=ones(size(e3));
%
k4=find( xy(:,1)==1  & xy(:,2)<1   & xy(:,2) >-1);
e4=[]; for k=1:mel, if any(mv(k,6)==k4), e4=[e4,k]; end, end
ef4=2*ones(size(e4));
%
k5=find( xy(:,2)==1 );
e5=[]; for k=1:mel, if any(mv(k,7)==k5), e5=[e5,k]; end, end
ef5=3*ones(size(e5));
%
k6=find( xy(:,1)==-1 & xy(:,2)<1   & xy(:,2) >0 );
e6=[]; for k=1:mel, if any(mv(k,8)==k6), e6=[e6,k]; end, end
ef6=4*ones(size(e6));
%
bound=sort([k1;k2;k3;k4;k5;k6]);
mbound=[e1',ef1';e2',ef2';e3',ef3';e4',ef4';e5',ef5';e6',ef6'];
%
%
%%
outbc=1;
gohome 
cd datafiles
save ell_grid.mat mv  xy bound mbound stretch outbc x y
return
