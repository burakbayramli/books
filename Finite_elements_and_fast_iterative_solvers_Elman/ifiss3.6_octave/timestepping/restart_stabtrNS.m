function [DT,U,Udot,time] = restart_stabtrNS(nonlin,qmethod,xy,ev,bound,A,B,C,G,AxB,...
                            initdata,tfinal,tol,nstar,tout,dpsi,restype)
%RESTART_STABTRNS restarted N-S integrator using stabilized TR
%   [DT,U,Udot,xtime] = restart_stabtrNS(nonlin,qmethod,xy,ev,bound,A,B,C,G,AxB,...
%                          initdata,tfinal,tol,nstar,1)
%   input
%          nonlin    number of nonlinear corrections
%          qmethod   approximation method
%          xy        vertex coordinate vector
%          ev        mv/ev  Q2/Q1 element mapping matrix
%          bound     boundary vertex vector 
%          A, B, C   matrices defining the saddle point system
%          G         mass matrix for velocity
%          AxB       saddle point system solver function_name
%          initdata  initial/restart data structure
%          tfinal    final time 
%          tol       local accuracy error tolerance   
%          nstar     averaging frequency   
%          tout      output data switch
%          dpsi      perturbation matrix (optional)
%          restype   hot/cold restart switch (optional, not used)
%   output
%          DT        timestep history
%          U         solution history
%          Udot      solution time derivative history
%          time      discrete time evolution
%
% calls functions: unsteadyflowbc, dtflowbc, resetflowbc, AxBhandle
%   IFISS function: DJS; 21 September 2016
% Copyright (c) 2016 D.J. Silvester

if nargin < 16, addpert=0; else, addpert=1; end
global viscosity
%%% set the time step limit
nnt=699;
AxBhandle=str2func(AxB);
[np,nuv]=size(B); nv=nuv/2;
T=tfinal;

%%% zeroth time step 
f=zeros(nuv,1);gzero=zeros(np,1);
n=initdata.n; t=initdata.time(end);
fprintf('\nTimestep %5i  Time %11.3e \n',n,t)
fprintf('Restarting integration ...\n')
if addpert,fprintf('perturbation injected for first timestep\n'), end
u=initdata.u;    ub=initdata.ub;  udot=initdata.udot; udotb=initdata.udotb;
dt=initdata.dt; dt0=initdata.dt0;
Ndof=length(u);
% preallocate array dimensions
DT = zeros(1,100); U = zeros(Ndof,100); time = zeros(1,100);
Udot = zeros(Ndof,100);

n=1; k=1;
udd = (udot-udotb)/dt0;
% debug
%fprintf('dt0= %g, dt=%g, norm(udot)= %g, norm(udotb)= %g, norm(udd)= %g', ...
%        dt0,dt, norm(udot), norm(udotb), norm(udd)),
time(1) = t;  DT(1) = dt;
U(:,1)  = u;  Udot(:,1) = udot;
acc = sqrt((udot'*(G*udot)));
if nonlin==0,
   fprintf('\n StabTR with no nonlinear corrections');
    else,fprintf('\n StabTR with %2i nonlinear corrections',nonlin); end
if tout==1,fprintf('\n   step  timestep       time        divresidual     acceleration'),
end
%
%
%%% loop until time limit is reached
flag = 0; nrej=0; avflag = 0;  nav=nstar; tstar=tfinal; %nav = 1e4;
while t <= T  & flag==0   
  if t+dt>T, dt = T-t; flag = 1; end          % fix final time step
  if n==nnt, flag=1; fprintf('\nToo slow -- step limit reached!')
  end
%%%%%%% general TR step  
ww = (1+(dt/dt0))*u - (dt/dt0)*ub; 
   if     qmethod>1,  N = navier_q2(xy,ev,ww,0);
   elseif qmethod<=1, N = navier_q1(xy,ev,ww,0); end
 addedp=0;
if addpert
if n<2, %(restype==0 & t<tstart + 0.00000001) | (restype==1 &  t<tstart + 0.00000001), %n<16)
Nxi = femq2_convection(xy,ev,dpsi,0); addedp=1; % add perturbation
N = N+Nxi; end
end
   Anst = 2*G + dt*viscosity*A + dt*[N, sparse(nv,nv); sparse(nv,nv), N];
   fnst = G*udot -(viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N])*u;
   gnst= -(B*u); 
 %  if t>0,gnst=gzero;end
   [Anst,Bst,fzz,gzz] = dtflowbc(Anst,B,fnst,(1/dt)*gnst,xy,bound,t,t+dt);
%
%---------- compute scaled pressure using AxB
    [v,pns] = AxBhandle(Anst,Bst,C,fzz,gzz,n);
   xns=[v;pns];                                % general TR step
%%%%% nonlinear iterations
     if nonlin>0 %& addedp==0,
        for itpic=1:nonlin    % perform Picard iteration(s)
        iuTR  = u + dt*v;
        N = navier_q2(xy,ev,iuTR,0);
        Anst = 2*G + dt*viscosity*A + dt*[N, sparse(nv,nv); sparse(nv,nv), N];
        fnst = G*udot -(viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N])*u;
        gnst= -(B*u);
        [Anst,Bst,fzz,gzz] = dtflowbc(Anst,B,fnst,(1/dt)*gnst,xy,bound,t,t+dt);
        %---------- compute scaled pressure using AxB
        [vpic,pns] = AxBhandle(Anst,Bst,C,fzz,gzz,n);
        fprintf('\n  %8.2e   --- nonlinear correction  ', norm(vpic-v));
        v=vpic;
        end
     end
   w = udot + (.5*dt)*udd;
   udiff = v - w;
   uAB2  = u + dt*w;
   upTR  = u + dt*v; 
% local truncation error estimate   
   d = (dt^2/(3*(dt+dt0)))*sqrt((udiff'*(G*udiff)));
% acceleration
   acc = sqrt((udot'*(G*udot)));
   if tout==1,
   fprintf('\n  %4i   %9.3e   %11.3e    %9.3e      %11.3e', n, dt, t+dt,norm(gnst),acc);
   if addedp==1, fprintf('**'); end
   end
%%%%%% time step rejection  
  if d < ((1/0.7)^3)*tol
%%%%% accepted step 		   
    if (t>tstar & avflag==0) | ~rem(n,nav)    % smooth by averaging
       ub  = .5*(u+ub);
       ub  = resetflowbc(ub,xy,bound,t-.5*dt0); % reset boundary values
       udotb = .5*(udot+udotb);
	   dt0 = .5*(dt+dt0);                     %% correct the old timestep
       u = .5*(u + upTR);
       udot = v;
	   t = t + .5*dt;                          % leave dt unchanged
       u  = resetflowbc(u,xy,bound,t);         % reset boundary values
       avflag=1; avstep=1;
       if nav == 1e4, nav = n; end
    if tout==1, fprintf('--- Averaging'), end
    else
%%%%%% regular step
       avstep=0;
       dt0 = dt;
       t = t+dt0;
       ub = u;
       u = upTR;
       udotb = udot;
       udot = 2*v - udot; 
    end
    udd = (udot-udotb)/dt0;
% debug
%fprintf('\ndt0= %g, norm(udot)= %g, norm(udotb)= %g, norm(udd)= %g', ...
%                      dt0, norm(udot), norm(udotb), norm(udd))
    n = n+1;                               

%%%%%% save solution data
       if n > length(time)		% need to allocate more memory
       DT   = [DT   zeros(1,100)];
       U    = [U zeros(Ndof,100)];
       Udot = [Udot zeros(Ndof,100)];
       time = [time zeros(1,100)];
       end    
    DT(n) = dt; U(:,n) = u;  time(n) = t;
    Udot(:,n) = udot;
       if avstep, % correct old values after averaging step
           U(:,n-1) = ub; Udot(:,n-1) = udotb; end
  else
%%%%%% rejected step     
  nrej = nrej + 1; 
  if tout==1, fprintf(' oops .. step was rejected'), end  
  end
  
%%%%%% compute the next timestep
dt = dt*(tol/d)^(1/3);
end
%%% end of timestep loop


%%% finishing touches
fprintf('\nfinished in %4i steps!\n',n)
DT = DT(1:n); U = U(:,1:n);  Udot = Udot(:,1:n); time = time(1:n);
if nrej>0, disp([int2str(nrej),' rejections in total: tol = ',num2str(tol)]), end
return
   

