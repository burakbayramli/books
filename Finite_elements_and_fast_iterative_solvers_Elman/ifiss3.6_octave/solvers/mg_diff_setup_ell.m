function Agal=mg_diff_setup_ell(x,y)
%MG_DIFF_SETUP_ELL GMG diffusion problem on L-shaped domain
%   Agal=mg_diff_setup_ell(x,y)
%   input
%          x       x coordinate vector for coarse grid
%          y       y coordinate vector for coarse grid
%   output
%          Agal    discrete diffusion operator
%
%   IFISS function: AR; 19 November, 2001.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage
n=length(x)-1; np=n/2; nq=n/4;
nvtx= np*(np*1) + (np+1)*(n+1);
% negative x-values
xneg=x(1:n/2);yneg=y(1:n/2);
xpos=x(n/2+1:end);ypos=y(n/2+1:end);
[Xneg,Ypos]=meshgrid(xneg,ypos);
xx=reshape(Xneg',np*(np+1),1);
yy=reshape(Ypos',np*(np+1),1);
xyleft=[xx(:),yy(:)];
% assembly process
kx = 1;
ky = 1;
mel=0;
% loop over 2x2 macroelements
for j=1:nq
   for i=1:nq
      mref=np*(ky-1)+kx;
      pref=nq*(j-1)+i;
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
      npp(1) = pref;
      npp(2) = pref+1;
      npp(3) = pref+nq+1;
      npp(4) = pref+nq;
      mv(mel,1:9)=nvv(1:9);
      mp(mel,1:4)=npp(1:4);
      kx = kx + 2;
   end
   ky = ky + 2;
   kx = 1;
end
%
% correction along the internal boundary
mref=2*np*(np+1)+1;
pref=2*nq*(nq+1)+1;
for mel=nq:nq:nq*nq;
   nvv=mv(mel,:);
   npp=mp(mel,:);
   nvv(2) = mref;
   nvv(3) = mref+2*np+2;
   nvv(6) = mref+np+1; 
   npp(2) = pref;
   npp(3) = pref+nq+1;
   mv(mel,1:9)=nvv(1:9);
   mp(mel,1:4)=npp(1:4);
   mref=mref+2*np+2;
   pref=pref+nq+1;
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
      pref = (nq+1)*(j-1)+i + nq*(nq+1);
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
      npp(1) = pref;
      npp(2) = pref+1;
      npp(3) = pref+nq+2;
      npp(4) = pref+nq+1;
      mv(mel,1:9)=nvv(1:9);
      mp(mel,1:4)=npp(1:4);
      kx = kx + 2;
   end
   ky = ky + 2;
   kx = 1;
end
%
% compute boundary vertices
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
% set up matrices for Q1 approximation
[ev,ebound]=mg_q1grid(x,y,xy,mv,bound,mbound);
[A,M,fdummy] = mg_q1diff(xy,ev);
%
% impose zero boundary conditions
[Agal] = mg_zerobc(A,xy,bound);
return
