function [az,bz,mz,tz,fz,gz,hz] = ...
         dtboussbc(a,b,mm,tt,f,g,h,xyv,xyt,boundv,boundt,told,t)
%DTBOUSSBC imposes temperature BC at current timestep
%   [Ast,Bst,Mst,Tst,fst,gst,hst] = ...
%   dtboussbc(A,B,M,T,f,g,h,xyv,xyt,boundv,boundt,told,tcurrent);
%   input
%          A          vector diffusion matrix
%          B          divergence matrix
%          M          temperature coupling matrix
%          T          temperature diffusion matrix
%          f          velocity rhs vector
%          g          pressure rhs vector  
%          h          temperature rhs vector 
%          xy         vertex coordinate vector  
%          bound      boundary vertex vector
%          told       previous time
%          tcurrent   current time
%
%   calls functions: specific_flow, specific_bc
%   IFISS function: DJS; 3 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.
tout=0; %output parameter
nu = length(f); np = length(g); ntt=length(h); nvtx = nu/2; 
if ntt~=nvtx, error('Dimensions in conflict'), end 
fx=f(1:nvtx); fy=f(nvtx+1:nu); gz=g; hz=h(1:nvtx);

%% temperature 
nbdt=length(boundt); 
null_col=sparse(nvtx,nbdt); null_row=sparse(nbdt,nvtx);
Ax=tt(1:nvtx,1:nvtx); 
Mx=mm(1:nvtx,1:ntt); My=mm(nvtx+1:nu,1:ntt);

%% get boundary condition at current and previous times
xbd=xyt(boundt,1); ybd=xyt(boundt,2);
bc=specific_bc(xbd,ybd);
bcold=bc*(1-exp(-10*told)); 
bcnew=bc*(1-exp(-10*t));   
dt=t-told;
bc=2*(bcnew-bcold)/dt; 
if tout==1,
fprintf('\nupdated boundary values: timestep is %e\n',dt)
fprintf('temperature boundary change is %e\n',dt*norm(bc))
end

%% impose boundary condition
hz = h - tt(:,boundt)*bc; hz(boundt)=bc;
%% Note that coupling matrix is -M
fx = fx + Mx(:,boundt)*bc; fy = fy + My(:,boundt)*bc;
dA=zeros(nvtx,1); dA(boundt)=ones(nbdt,1);
Ax(:,boundt)=null_col;  Ax(boundt,:)=null_row;   
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  
Mx(:,boundt)=null_col; My(:,boundt)=null_col;
tz=Ax;

%% velocity
nbdv=length(boundv);
null_col=sparse(nvtx,nbdv); %null_row=sparse(nbd,nvtx);
null_pcol=sparse(np,nbdv);
null_tcol=sparse(ntt,nbdv);
Ax=a(1:nvtx,1:nvtx); Ay=a(nvtx+1:nu,nvtx+1:nu);
Bx=b(1:np,1:nvtx); By=b(1:np,nvtx+1:nu);


%% velocity : get boundary condition at current and previous times
xbd=xyv(boundv,1); ybd=xyv(boundv,2);
[bcx,bcy]=specific_flow(xbd,ybd);
bcxold=bcx*(1-exp(-10*told)); bcyold=bcy*(1-exp(-10*told));
bcxnew=bcx*(1-exp(-10*t));    bcynew=bcy*(1-exp(-10*t));
dt=t-told;
bcx=(bcxnew-bcxold)/dt; bcy=(bcynew-bcyold)/dt;
if tout==1,
fprintf('   velocity boundary change is %e\n',dt*norm([bcx;bcy]))
end

%% impose boundary condition
fx = fx - Ax(:,boundv)*bcx;  fy = fy - Ay(:,boundv)*bcy;  
gz = gz - Bx(:,boundv)*bcx;  gz = gz - By(:,boundv)*bcy;
dA=zeros(nvtx,1); dA(boundv)=ones(nbdv,1);  
Axt = Ax'; Axt(:,boundv) = null_col;
Ax = Axt'; Ax(:,boundv) = null_col;
Ax=Ax+spdiags(dA,0,nvtx,nvtx);  fx(boundv)=bcx;  
Ayt = Ay'; Ayt(:,boundv) = null_col;
Ay = Ayt'; Ay(:,boundv) = null_col;
Ay=Ay+spdiags(dA,0,nvtx,nvtx);  fy(boundv)=bcy; 
Bx(:,boundv)=null_pcol; By(:,boundv)=null_pcol;
Mxt = Mx'; Mxt(:,boundv) = null_tcol; Mx = Mxt';
Myt = My'; Myt(:,boundv) = null_tcol; My = Myt';
az=[Ax,sparse(nvtx,nvtx);sparse(nvtx,nvtx),Ay];
bz=[Bx,By];  mz=[Mx;My]; 
fz=[fx;fy];
return
