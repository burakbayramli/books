function [DT,U,Udot,time] = stabtrNS(qmethod,xy,ev,bound,A,B,C,G,AxB,...
                                uzero,dtzero,tfinal,tol,nstar,tout,nonlin)
%STABTRNS Navier-Stokes integrator using stabilized TR 
%   [DT,U,Udot,time] = stabtrNS(qmethod,xy,ev,bound,A,B,C,G,AxB,...
%                          uzero,dtzero,tfinal,tol,nstar,0);
%   input
%          qmethod   approximation method
%          xy        vertex coordinate vector
%          ev        mv/ev  Q2/Q1 element mapping matrix
%          bound     boundary vertex vector 
%          A, B, C   matrices defining the saddle point system
%          G         mass matrix for velocity
%          AxB       saddle point system solver function_name
%          uzero     initial velocity condition
%          dtzero    initial timestep
%          tfinal    final time 
%          tol       local accuracy error tolerance   
%          nstar     averaging frequency   
%          tout      output data switch
%          nonlin    number of nonlinear corrections (optional, default is 0)
%   output
%          DT        timestep history
%          U         solution history
%          Udot      solution time derivative history
%          time      discrete time evolution
%
% calls functions: unsteadyflowbc, dtflowbc, resetflowbc, AxBhandle
%   IFISS function: DJS; 7 June 2010; ... 17 August 2016
% Copyright (c) 2006 D.J. Silvester, D.F. Griffiths, M. Schneider
if nargin < 16, nonlin=0; end
global viscosity
nnt=330; %1999; % maximum number of steps
warning('on','MATLAB:nearlySingularMatrix'); wchar=27;
fprintf('Solving DAE system using stabilized TR ...\n')
AxBhandle=str2func(AxB),
[np,nuv]=size(B); nv=nuv/2;
ub=uzero; 
Ndof=length(ub); T=tfinal; dt0=dtzero;
% preallocate array dimensions
DT = zeros(1,100); U = zeros(Ndof,100); time = zeros(1,100); 
Udot = zeros(Ndof,100);
%%% zeroth time step 
f=zeros(nuv,1);gzero=zeros(np,1);              

% initialization: potential flow solve
[fst,gst] = unsteadyflowbc(viscosity*A+(1/dt0)*G,B,f,gzero,xy,bound,dtzero);
bdy=fst(bound);
if tout==1,
fprintf('\n  initial nonlinear residual is %e \n',norm([fst;gst]))
fprintf('             boundary change is %e \n',norm(bdy)), end
  if     qmethod>1,  N = navier_q2(xy,ev,ub,1);
   elseif qmethod<=1, N = navier_q1(xy,ev,ub,1); end
Anst = viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N];
[Gst,Bst,fzz,gzz] = dtflowbc(G,B,-Anst*ub,gzero,xy,bound,0,dt0);
xpot = [Gst,Bst';Bst,-C]\[fzz;gzz];
udotb=xpot(1:nuv); pdotb=xpot(nuv+1:end); 
% compatibility check
divres_norm=norm(gzero-Bst*ub,inf);
if divres_norm>10*eps,
fprintf('initial divergence residual is %e',divres_norm)
error('incompatible  initial data!')
end
% take care of system singularity warnings
[lastmsg, msgid] = lastwarn; 
if length(msgid)==wchar,
 lastmsg, fprintf('This should not cause difficulty for enclosed flow problems.\n');
  end
warning('off','MATLAB:nearlySingularMatrix'); 
%
%%% first time step
n=1;                                              % time step index 
ww = ub+dt0*udotb;                                % w(dt0)
   if     qmethod>1,  N = navier_q2(xy,ev,ww,0);
   elseif qmethod<=1, N = navier_q1(xy,ev,ww,0); end
Anst = 2*G + dt0*viscosity*A + dt0*[N, sparse(nv,nv); sparse(nv,nv), N];
fnst = G*udotb - (viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N])*ub;
		gnst= -(B*ub); %fprintf('\ndivergence residual is %e',norm(gnst))
[Anst,Bst,fzz,gzz] = dtflowbc(Anst,B,fnst,(1/dt0)*gnst,xy,bound,0,dt0);
%---------- compute unscaled pressure using AxB
[v,pns] = AxBhandle(Anst,Bst,C,fzz,gzz,1);
xns=[v;pns];                                     % first TR step 
u = ub + dt0*v;                                  % u(dt0) 
udot = 2*v - udotb;	                             % du/dt(dt0)
udd = (udot-udotb)/dt0;		                     % second derivative
dt = dt0;    t = dt;
acc = sqrt((udot'*(G*udot)));
    if nonlin==0,
    fprintf('\n StabTR with no nonlinear corrections');
    else,fprintf('\n StabTR with %2i nonlinear corrections',nonlin); end
if tout==1,fprintf('\n   step  timestep       time        divresidual     acceleration'),
fprintf('\n  %4i   %9.3e   %11.3e    %9.3e      %11.3e', n, dt, t+dt,norm(gnst),acc);
end
%
%%% second time step
n=2;
DT(1:2) = [dt0, dt0];	
U(:,1) = ww; U(:,2) = u;
Udot(:,1) = udotb; Udot(:,2) = udot;
time(1:2) = [0,dt0];
%
%
%---------------- loop until time limit is reached
flag = 0; nrej=0; avflag = 0;  nav=nstar; tstar=tfinal; %nav = 1e4;
while t <= T  & flag==0   
  if t+dt>T, dt = T-t; flag = 1; end          % fix final time step
  if n==nnt, flag=1; fprintf('\nToo slow -- step limit reached!')
  end
%%%%%% general TR step
    ww = (1+(dt/dt0))*u - (dt/dt0)*ub;
    if     qmethod>1,  N = navier_q2(xy,ev,ww,0);
    elseif qmethod<=1, N = navier_q1(xy,ev,ww,0); end
    Anst = 2*G + dt*viscosity*A + dt*[N, sparse(nv,nv); sparse(nv,nv), N];
    fnst = G*udot -(viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N])*u;
    gnst= -(B*u);
 %  if t>0,gnst=gzero;end
    [Anst,Bst,fzz,gzz] = dtflowbc(Anst,B,fnst,(1/dt)*gnst,xy,bound,t,t+dt);
%---------- compute scaled pressure using AxB
    [v,pns] = AxBhandle(Anst,Bst,C,fzz,gzz,n);
    xns=[v;pns];                               % general TR step
%
%%%%% nonlinear iterations
    if nonlin>0
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
    end
%%%%%% time step rejection
   if d < ((1/0.7)^3)*tol
%     accepted step
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
         if tout==1,
         fprintf('\n  %4i   %9.3e   %11.3e  ', n, .5*dt, t);
         fprintf('--- Averaging step'),end
      else
%     regular step
         avstep=0;
         dt0 = dt;
         t = t+dt0;
         ub = u;
         u = upTR;
         udotb = udot;
         udot = 2*v - udot;
      end
      udd = (udot-udotb)/dt0;
%%% debug
%    if n>97,
%    fprintf('\n dt0= %g, norm(udot)= %g, norm(udotb)= %g, norm(udd)= %g', ...
%            dt0,norm(udot), norm(udotb), norm(udd)), end
      n = n+1;
% memory management
    if n > length(time)	% need to allocate more memory
       DT   = [DT   zeros(1,100)];
       U    = [U zeros(Ndof,100)];
       Udot = [Udot zeros(Ndof,100)];
       time = [time zeros(1,100)];
       end
%%%%%% save solution data
    DT(n) = dt0; U(:,n) = u;  time(n) = t;
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
%---------------- end of timestep loop


%%% finishing touches
fprintf('\nfinished in %4i steps!\n',n-1)
DT = [DT(1:n) dt]; U = U(:,1:n);  Udot = Udot(:,1:n); time = time(1:n);
if nrej>0, disp([int2str(nrej),' rejections in total: tol = ',num2str(tol)]), end
return
   
