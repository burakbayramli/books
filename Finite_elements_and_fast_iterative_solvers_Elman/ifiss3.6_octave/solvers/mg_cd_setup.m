function Asup = mg_cd_setup(x,y,viscosity,outbc)
%MG_CD_SETUP GMG convection-diffusion problem on square domain
%   Asup = mg_cd_setup(x,y,viscosity,outbc)
%   input
%           x          x-grid coordinates
%           y          y-grid coordinates
%           viscosity  diffusion coefficient
%           outbc      outflow boundary condition (for step) 
%                         1 for Dirichlet, 0 for Neumann
%   output
%           Asup      discrete convection-diffusion operator
%
%   IFISS function: AR, HCE; 21 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage

%   adapted from convection-diffusion generating routines
%   ref_domain, ref_cd, solve_cd
%
n=length(x)-1; np=n/2; nq=n/4;
nvtx=(n+1)*(n+1);
[X,Y]=meshgrid(x,y);
xx=reshape(X',nvtx,1);
yy=reshape(Y',nvtx,1);
xy=[xx(:),yy(:)];
% assembly process
kx = 1;
ky = 1;
mel=0;
for j=1:np
   for i=1:np
      mref=(n+1)*(ky-1)+kx;
      pref=(np+1)*(j-1)+i;
      mel=mel+1;
      nvv(1) = mref;
      nvv(2) = mref+2;
      nvv(3) = mref+2*n+4;
      nvv(4) = mref+2*n+2;
      nvv(5) = mref+1;
      nvv(6) = mref+n+3; 
      nvv(7) = mref+2*n+3; 
      nvv(8)=  mref+n+1;
      nvv(9)=  mref+n+2; 
      npp(1) = pref;
      npp(2) = pref+1;
      npp(3) = pref+np+2;
      npp(4) = pref+np+1;
      mv(mel,1:9)=nvv(1:9);
      mp(mel,1:4)=npp(1:4);
      kx = kx + 2;
   end
   ky = ky + 2; 
   kx = 1;
end
%
% compute boundary vertices and edges
% four boundary edges 
k1=find( xy(:,2)==-1 );
e1=[]; for k=1:mel, if any(mv(k,5)==k1), e1=[e1,k]; end, end
ef1=ones(size(e1));
%
%k2=find( xy(:,1)==1  & xy(:,2)<1   & xy(:,2) >-1);  % Old
k2=find( xy(:,1)==1 & xy(:,2)<=1   & xy(:,2) >-1);   % New (~ref_domain)
e2=[]; for k=1:mel, if any(mv(k,6)==k2), e2=[e2,k]; end, end
ef2=2*ones(size(e2));
%
k3=find( xy(:,2)==1 );
e3=[]; for k=1:mel, if any(mv(k,7)==k3), e3=[e3,k]; end, end
ef3=3*ones(size(e3));
%
%k4=find( xy(:,1)==-1 & xy(:,2)<1   & xy(:,2) >-1 ); % Old
k4=find( xy(:,1)==-1 & xy(:,2)<=1   & xy(:,2) >-1);  % New (~ref_domain)
e4=[]; for k=1:mel, if any(mv(k,8)==k4), e4=[e4,k]; end, end
ef4=4*ones(size(e4));
%
if outbc==1,  % Dirichlet on outflow
   bound=sort([k1;k2;k3;k4]);
   mbound=[e1',ef1';e2',ef2';e3',ef3';e4',ef4'];
else          % Neumann on outflow
   bound=sort([k1;k2;k4]);
   mbound=[e1',ef1';e2',ef2';e4',ef4'];
end
%
% set up matrices for Q1 approximation
[ev,ebound]=mg_q1grid(x,y,xy,mv,bound,mbound);
[A,N,M,epe,eph,epw] = mg_q1cd(xy,ev);
% SUPG
supg=0;
A = viscosity*A + N; f = zeros(size(xy(:,1)));
% compute element peclet numbers
epe = epe/viscosity;
% include streamline diffusion matrix (if necessary)
esupg=find(epe<=1); expe=epe;   %expe(esupg)=0;
if any(expe), 
   expe=0.5*(1-1./expe);
   expe(esupg)=inf;
   epp=expe; epp(esupg)=0; epp=epp.*eph./epw;
   S=mg_q1cd_supg(xy,ev,expe,eph,epw);A=A+S; 
end
%
%% impose boundary conditions
Asup = mg_zerobc(A,xy,bound);
