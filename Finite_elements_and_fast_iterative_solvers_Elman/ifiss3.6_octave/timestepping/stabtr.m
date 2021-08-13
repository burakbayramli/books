function [DT,U,Udot,time] = stabtr(xy,bound,A,M,f,uzero,dtzero,tfinal,tol,nstar,info)
%STABTR standard integrator based on stabilized TR
%   [DT,U,Udot,time] = stabtr(xy,bound,A,M,f,uzero,dtzero,tfinal,tol,10,0);
%   input
%          xy        vertex coordinate vector
%          bound     boundary vertex vector 
%          A, M   specified ODE system: M udot + A u = f without BCs
%          uzero     initial condition
%          dtzero    initial timestep
%          tfinal    final time 
%          tol       local accuracy error tolerance   
%          nstar     averaging frequency   
%          info      output switch
%   output
%          DT        timestep history
%          U         solution history
%          Udot      solution time derivative history
%          time      discrete time evolution
%
% calls function: heatbc, dtheatbc, resetheatbc, xxheatbc
%   IFISS function: DJS; 7 December 2009.
% Copyright (c) 2006 D.J. Silvester, D.F. Griffiths 
fprintf('Solving ODE system using stabilized TR ...\n')
ub=uzero; 
N=length(ub); T=tfinal; dt0=dtzero;
%% preallocate array dimensions
DT = zeros(1,50); U = zeros(N,50); time = zeros(1,50); Udot = zeros(N,50); 

%%%zeroth time step
b=f;                                                    % f(0)  
% initialization 
%%udotb = M\(-Agal*ub+f);                               % du/dt(0)  
fgal = xxheatbc(M,(-A*ub+f),xy,bound,dt0);       
udotb = M\fgal;                                   
%
%%% first time step 
%u = (M+(.5*dt0)*Agal)\((M-(.5*dt0)*Agal)*ub+dt0*avb);  % first TR step 
[Hgal,fgal] = dtheatbc((M+(.5*dt0)*A), b + M*udotb - A*ub,xy,bound,0,dt0);  
v=Hgal\fgal;
u = ub + (.5*dt0)*v; 
udot = 2*(u-ub)/dt0 - udotb;	                       % du/dt(dt0)
udd = (udot-udotb)/dt0;		                           % second derivative
dt = dt0;    t=dt;     
n=2;                                                    % time step index 
DT(1:2) = [dt0, dt0];	
U(:,1) = ub; U(:,2) = u;
Udot(:,1) = udotb; Udot(:,2) = udot;
time(1:2) = [0,dt0];

%%% loop until time limit is reached
flag = 0; nrej=0; avflag = 0;  nav=nstar; tstar=tfinal; %nav = 1e4;
while t <= T  & flag==0   
  if t+dt>T, dt = T-t; flag = 1; end          % fix final time step
  if n==999, flag=1; fprintf('\nToo slow -- step limit reached!'), end
%%%%%%% general TR step  
%  v = (M+(.5*dt)*Agal)\(b - Agal*u + M*udot); 
  [Hgal,fgal] = dtheatbc((M+(.5*dt)*A),b + M*udot - A*u,xy,bound,t,t+dt); 
  v=Hgal\fgal;
  w = udot + (.5*dt)*udd;
  udiff = .5*v-w;
  upTR    = u + (.5*dt)*v; 
  d = (dt^2/(3*(dt+dt0)))*sqrt((udiff'*(M*udiff)));
    
%%%%%% time step rejection  
  if d < ((1/0.7)^3)*tol
%%%%% accepted step 
    if (t>tstar & avflag==0) | ~rem(n,nav) % smooth by averaging
       dt0 = .5*(dt+dt0);
       ub  = .5*(u+ub);
       ub  = resetheatbc(ub,xy,bound,t-.5*dt0);
       udotb = .5*(udot+udotb);
       u = .5*(u + upTR);
       udot = .5*v;
       t = t + .5*dt;                      % leave dt unchanged
       u  = resetheatbc(u,xy,bound,t);     % update boundary values
       avflag=1;
       if nav == 1e4, nav = n; end
    if info==1, disp(['Averaging: n = ' int2str(n) ', t = ',num2str(t)]), end
    else
%%%%%% regular step        
       dt0 = dt;
       t = t+dt0;
       ub = u;
       u = upTR;
       udotb = udot;
       udot = v - udot; 
    end
    udd = (udot-udotb)/dt0;
 %   r = norm(M*udot-Agal*u-b);
    n = n+1;                               

%%%%%% save solution data
       if n > length(time)		% need to allocate more memory
       DT   = [DT   zeros(1,100)];
       U    = [U zeros(N,100)];      Udot = [Udot zeros(N,100)];
       time = [time zeros(1,100)];
       end    
    DT(n) = dt; U(:,n) = u;  Udot(:,n) = udot; time(n) = t;
  else
%%%%%% rejected step     
  nrej = nrej + 1; 
  if info==1, disp(['oops .. step ', int2str(n),' rejected']), end  
  end
  
%%%%%% compute the next timestep
dt = dt*(tol/d)^(1/3);
end
%%% end of timestep loop


%%% finishing touches
fprintf('finished in %3i steps!\n',n)
DT = DT(1:n); U = U(:,1:n); Udot = Udot(:,1:n); time = time(1:n);
if nrej>0, disp([int2str(nrej),' rejections in total: tol = ',num2str(tol)]), end
return
   
