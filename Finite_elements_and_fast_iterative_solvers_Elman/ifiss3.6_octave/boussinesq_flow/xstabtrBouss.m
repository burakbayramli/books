function [DT,U,Udot,T,Tdot,time] = stabtrBouss(nonlin,qmethod,grid,spmat,...
                                       initdata,tfinal,tol,nstar,nmax,tout)
%XSTABTRBOUSS Boussinesq integrator based on stabilized TR (nonrobust version)
%   [DT,U,Udot,T,Tdot,time] = ...
%   xstabtrBouss(nonlin,qmethod,grid,spmat,initdata,tfinal,tol,nstar,2,1);
%   input
%          nonlin    number of nonlinear corrections (0 for linearized TR)
%          qmethod   approximation method
%          grid      grid data structure (see unpack_boussdata)
%          spmat     Galerkin matrix data structure (see unpack_boussdata)
%          initdata  initial/restart data structure 
%          tfinal    final time 
%          tol       local accuracy error tolerance   
%          nstar     averaging frequency  
%          nmax      number of itegration steps (*nnt defined below) 
%          tout      output data switch
%   output
%          DT        timestep history
%          U         velocity solution history
%          Udot      solution time derivative history
%          T         temperature solution history
%          Tdot      Temperature time derivative history
%          time      discrete time evolution
%
% calls functions:  dtboussbc, unsteadyflowbc, resetflowbc, xxheatbc, resetheatbc
% calls scriptfiles: unpack_boussdata, bousshistory
% hot restart datafile: stabtrBouss_restart
%   IFISS function: DJS; 10 May 2012; 24 June 2012
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.
global Pr Ra H L 
viscosity =sqrt(Pr/Ra);  viscosityT =1/sqrt(Pr*Ra);
fprintf('      fluid viscosity parameter is %8.2e \n',viscosity)
fprintf('temperature viscosity parameter is %8.2e \n',viscosityT)
warning('on','MATLAB:nearlySingularMatrix'); wchar=27;
%%% set the restart parameter
nnt=200;
%%% unpack grid and matrix data
unpack_boussdata
[nv,dd]=size(xyv(:,1)); nuv=2*nv;
[np,dd]=size(xyp(:,1)); [ntt,dd]=size(xyt(:,1));
Ndof=nuv; Tdof=ntt; 
% preallocate array dimensions
nntmax=nmax*nnt;
DT = zeros(1,nntmax);   time = zeros(1,nntmax); 
U = zeros(Ndof,nnt); Udot = zeros(Ndof,nnt);
T = zeros(Tdof,nnt); Tdot = zeros(Tdof,nnt);
KE = zeros(1,nntmax); VTY = zeros(1,nntmax); 
krestart=0;
%%% zeroth time step 
C=sparse(np,np);
f=zeros(nuv,1);gzero=zeros(np,1); h=zeros(ntt,1);
%%% factorize velocity mass matrix
[LQ,UQ]= lu(Qv(1:nv,1:nv));

%% unpack initialization structure
restart=initdata.restart;
if restart==0,
fprintf('Integrating using stabilized TR ...\n')
fprintf('intermediate (CheckPoint) datafile: stabtrBouss_restart\n')
ub=initdata.uzero; ttb=initdata.ttzero; 
dtzero=initdata.dtzero; dt0=dtzero;
n=0;
% initialization : potential flow solve
boundv=bnd_d;
[fst,gst] = unsteadyflowbc(viscosity*Av+(1/dt0)*Qv,B,f,gzero,xyv,boundv,dtzero);
bdy=fst(boundv);
% initialization : temp
boundt=bnd_dn2;                                         % Q2 Temperature
b=h;                                                    % f(0)   
%%udotb = M\(-Agal*ub+f);                               % du/dt(0)  
hgal = xxheatbc(Qt,(-viscosityT*At*ttb+h),xyt,boundt,dt0);       
tdotb = Qt\hgal;  
bdyt=hgal(boundt);
if tout==3,
fprintf('\n  initial nonlinear residual is %e \n',norm([fst;gst;hgal]))
fprintf('    velocity boundary change is %e \n',norm(bdy))
fprintf(' temperature boundary change is %e \n',norm(bdyt)), end
Nv = navier_q2(xyv,mv2,ub,0); %initial convection matrix 
Jnst = viscosity*Av + [Nv, sparse(nv,nv); sparse(nv,nv), Nv];
[Gbc,Bbc,fzz,gzz] = dtflowbc(Qv,B,-Jnst*ub,gzero,xyv,boundv,0,dt0);
xpot = [Gbc,Bbc';Bbc,-C]\[fzz;gzz];
udotb=xpot(1:nuv); pdotb=xpot(nuv+1:end); 
% compatibility check
divres_norm=norm(gzero-Bbc*ub,inf);
if divres_norm>10*eps,
fprintf('initial divergence residual is %e',divres_norm)
error('incompatible  initial data!')
end

%%% first time step
%%M=sparse(nuv,ntt);  %%%%%%%%%% debug: should switch off the flow!!
n=1;                                              % time step index 
ww = ub+dt0*udotb;                                % w(dt0)
Nv = navier_q2(xyv,mv2,ww,0);
Jnst = 2*Qv + dt0*viscosity*Av + dt0*[Nv, sparse(nv,nv); sparse(nv,nv), Nv];
fnst = Qv*udotb + M*ttb ...
     - (viscosity*Av + [Nv, sparse(nv,nv); sparse(nv,nv), Nv])*ub;
gnst= -(B*ub);
  Nt = conv_q2t(xyt,mt2,ww);
Tnst = Qt + .5*dt0*viscosityT*At + .5*dt0*Nt;
hnst = Qt*tdotb - (viscosityT*At + Nt)*ttb;
[Jbc,Bbc,Mbc,Tbc,fbc,gbc,hbc] = ...
       dtboussbc(Jnst,B,M,Tnst,fnst,(1/dt0)*gnst,hnst,xyv,xyt,boundv,boundt,0,dt0);
%xns = [Jbc,dt0*Bbc',-0.5*dt0*Mbc;dt0*Bbc,-dt0*dt0*C,sparse(np,ntt);...
%       sparse(ntt,nuv),sparse(ntt,np),Tbc]\[fbc;dt0*gbc;hbc];
s=Tbc\hbc; xns=[Jbc,dt0*Bbc';dt0*Bbc,-dt0*dt0*C]\[fbc+0.5*dt0*Mbc*s;dt0*gbc];
v=xns(1:nuv); pns=xns(nuv+1:nuv+np); % s=xns(nuv+np+1:end);   % first TR step 
   u = ub + dt0*v;          tt = ttb + (.5*dt0)*s;                              
udot = 2*v - udotb;	      tdot = s - tdotb;	                             
 udd = (udot-udotb)/dt0;   tdd = (tdot-tdotb)/dt0;		                
dt = dt0; t = dt; 

%%% second timestep
n=2;  k=2;                                           
time(1:2) = [0,dt0]; DT(1:2) = [dt0, dt0];	
U(:,1) = ww;  U(:,2) =  u; Udot(:,1) = udotb; Udot(:,2) = udot;
T(:,1) = ttb; T(:,2) = tt; Tdot(:,1) = tdotb; Tdot(:,2) = tdot;
%% take care of system singularity warnings
[lastmsg, msgid] = lastwarn; 
if length(msgid)==wchar,
   lastmsg, fprintf('This should not cause difficulty for enclosed flow problems.\n');
  end
warning('off','MATLAB:nearlySingularMatrix'); 

				
else %%%%%%%%%%%% hot restart
warning('off','MATLAB:nearlySingularMatrix'); 
n=initdata.n; t=initdata.time(end);
fprintf('\n\nTimestep %5i  Time %11.3e \n',n,t)
fprintf('Restarting integration ...\n ')
fprintf('intermediate (CheckPoint) datafile: stabtrBouss_restart')
u=initdata.u;    ub=initdata.ub;  udot=initdata.udot; udotb=initdata.udotb; 
tt=initdata.tt; ttb=initdata.ttb; tdot=initdata.tdot; tdotb=initdata.tdotb;
dt=initdata.dt; dt0=initdata.dt0;
boundv=bnd_d; boundt=bnd_dn2;   
n=1; k=1;
udd = (udot-udotb)/dt0;   tdd = (tdot-tdotb)/dt0;	
time(1) = t;  DT(1) = dt;	
U(:,1)  = u;  Udot(:,1) = udot; 
T(:,1)  = tt; Tdot(:,1) = tdot; 
end

%%% general time step 				
if nonlin==0,
   fprintf('\n StabTR with no nonlinear corrections');
else,fprintf('\n StabTR with %2i nonlinear corrections',nonlin); end
% print header as appropriate				
if tout==1,
  fprintf('\n\n   step  timestep    time    meanKE   vorticity'), 
elseif tout==2,
   fprintf('\n\n   step  timestep    time    meanKE   vorticity  skewness'), 
end
flag = 0; nrej=0; avflag = 0;  nav=nstar; tstar=tfinal; %nav = 1e4;
%
%%% loop until time limit is reached
while t <= tfinal  & flag==0   
  if t+dt>tfinal, dt = tfinal-t; flag = 1; end          % fix final time step
  if n==nntmax, flag=1; fprintf('\nTerminated -- step limit reached!'), end
%%%%%%% general TR step  
   ww = (1+(dt/dt0))*u - (dt/dt0)*ub;               
   Nv = navier_q2(xyv,mv2,ww,0);
   Jnst = 2*Qv + dt*viscosity*Av + dt*[Nv, sparse(nv,nv); sparse(nv,nv), Nv];
   fnst = Qv*udot + M*tt ...
          - (viscosity*Av + [Nv, sparse(nv,nv); sparse(nv,nv), Nv])*u;
   gnst= -(B*u);
   Nt = conv_q2t(xyt,mt2,ww);
   Tnst = Qt + .5*dt*viscosityT*At + .5*dt*Nt;
   hnst = Qt*tdot - (viscosityT*At + Nt)*tt;
   [Jbc,Bbc,Mbc,Tbc,fbc,gbc,hbc] = ...
       dtboussbc(Jnst,B,M,Tnst,fnst,(1/dt)*gnst,hnst,xyv,xyt,boundv,boundt,t,t+dt);
  %% xns = [Jbc,dt*Bbc',-0.5*dt*Mbc;dt*Bbc,-dt*dt*C,sparse(np,ntt);...
  %%        sparse(ntt,nuv),sparse(ntt,np),Tbc]\[fbc;dt*gbc;hbc];
   s=Tbc\hbc;  %%% if n==10, spparms('spumoni',2), end; %check logistics
   xns=[Jbc,Bbc';Bbc,-C]\[fbc+0.5*dt*Mbc*s;gbc];  %%% spparms('spumoni',0) 
   v=xns(1:nuv); pns=xns(nuv+1:nuv+np); % s=xns(nuv+np+1:end); % general TR step 
   %   if n==7 | n==27, keyboard, end 
   if nonlin>0
   %% test extra Picard iteration(s)
   for itpic=1:nonlin    %% perform iterations
   iuTR  = u + dt*v;  itTR  = tt + .5*dt*s; 
   Nv = navier_q2(xyv,mv2,iuTR,0);
   Jnst = 2*Qv + dt*viscosity*Av + dt*[Nv, sparse(nv,nv); sparse(nv,nv), Nv];
   fnst = Qv*udot + M*tt ...
          - (viscosity*Av + [Nv, sparse(nv,nv); sparse(nv,nv), Nv])*u;
   gnst= -(B*u);
   Nt = conv_q2t(xyt,mt2,iuTR);
   Tnst = Qt + .5*dt*viscosityT*At + .5*dt*Nt;
   hnst = Qt*tdot - (viscosityT*At + Nt)*tt;
   [Jbc,Bbc,Mbc,Tbc,fbc,gbc,hbc] = ...
       dtboussbc(Jnst,B,M,Tnst,fnst,(1/dt)*gnst,hnst,xyv,xyt,boundv,boundt,t,t+dt);
   spic=Tbc\hbc; xns=[Jbc,Bbc';Bbc,-C]\[fbc+0.5*dt*Mbc*spic;gbc];
   vpic=xns(1:nuv); pns=xns(nuv+1:nuv+np); % s=xns(nuv+np+1:end); % general TR step 
   fprintf('\n  %8.2e   %8.2e   --- nonlinear corrections  ', norm(spic-s),norm(vpic-v));
   s=spic; v=vpic;
   end
   end
   %%
   w = udot + (.5*dt)*udd; tw = tdot + (.5*dt)*tdd;          % AB2 increments 
   udiff = v - w;     ttdiff = .5*s - tw; 
   uAB2  = u + dt*w;  tAB2  = tt + .5*dt*tw; 
   upTR  = u + dt*v;  tpTR  = tt + .5*dt*s; 
% local truncation error estimates   
   erru2= udiff'*Qv*udiff; errt2= ttdiff'*Qt*ttdiff; 
   errtotal= sqrt(erru2+errt2);
% use combined truncation error    
   d = (dt^2/(3*(dt+dt0)))*errtotal;
% kinetic energy 
   ke =  sqrt(1/(2*L*H)*upTR'*Qv*upTR);
% average vorticity 
   fomega = -[BBy,-BBx]*upTR;
   omega = UQ\(LQ\fomega);
   vty =  sqrt(1/(2*L*H)*omega'*Qv(1:nv,1:nv)*omega);
%
% print current solution data
   if tout>=1,
   fprintf('\n  %4i   %8.2e  %7.2f  %7.4f  %7.4f   ', n, dt, t+dt, ke, vty); end
%%%%%% time step rejection  
  if d < ((1/0.7)^3)*tol
%%%%% accepted step 
    if (t>tstar & avflag==0) | ~rem(n,nav)    % smooth by averaging
       ub  = .5*(u+ub);   ttb  = .5*(tt+ttb); 
       ub  = resetflowbc(ub,xyv,boundv,t-.5*dt0);
       ttb = resetheatbc(ttb,xyt,boundt,t-.5*dt0);
       dt0 = .5*(dt+dt0);
       udotb = .5*(udot+udotb); tdotb = .5*(tdot+tdotb); 
       u = .5*(u + upTR);   tt = .5*(tt + tpTR);
       udot = v;          tdot = .5*s;
       t = t + .5*dt;                          % leave dt unchanged
       u  = resetflowbc(u,xyv,boundv,t);       % update boundary values
       tt = resetheatbc(tt,xyt,boundt,t);
       avflag=1;
       if nav == 1e4, nav = n; end
       if tout>=1, fprintf('--- Averaging'), end
    else
%%%%%% regular step        
       dt0 = dt;
       t = t+dt0;
       ub = u;       ttb = tt;
       u = upTR;     tt = tpTR;
       udotb = udot; tdotb = tdot;
       udot = 2*v - udot;  tdot = s - tdot; 
    end
    udd = (udot-udotb)/dt0;  tdd = (tdot-tdotb)/dt0;


%%%%%  generate point history data and check skewness if required
    if(tout==2)      
    ppsum=sum(pns); phyd=ppsum/np; pns=pns-phyd;   %%normalise pressure 
    [ttch,uxh,uyh,dph,Th]= datapoint_history(t,u(1:nv),u(nv+1:nuv),pns,tt,...
			eh1,eh4,phih1,psih1,phih4,mv2,mp1,mt2,mt1,1,ttch,uxh,uyh,dph,Th); 
    tskew=skewness_check(u(1:nv),u(nv+1:nuv),tt,eh1,eh2,psih1,psih2,mv2,mt2);
    if rem(n,nav), fprintf('  %8.2e', abs(tskew)), end
    end
    n = n+1;  k=k+1;                             

%%%%%% save solution data
       if k > nnt                 		% need to reorder memory
       timeptr=restart*nnt+1; 
       krestart=krestart+1; dateflag=date; restart=restart+1;
       initdata=struct('u',u,'ub',ub,'tt',tt,'ttb',ttb, ...
                       'udot',udot,'tdot',tdot, 'udotb',udotb,'tdotb',tdotb, ...
                       'dt0',dt,'dt',dt*(tol/d)^(1/3),'time',t,'n',n-1, ...
                       'restart',restart);
       
       gohome, cd datafiles
       longtime=time; longDT=DT; 
       kknz=[1,find(time)]; time=time(kknz); DT=DT(kknz);
       soltime=time(end-199:end); solDT=DT(end-199:end);
       save stabtrBouss_restart.mat U T Udot Tdot solDT soltime DT time L H initdata dateflag t tol Pr Ra
       time=longtime; DT=longDT;
       fprintf(' --- CheckPoint')
       % if tout>=1, fprintf('\n\n   step  timestep   time     meanKE   vorticity'), end
       k=1;      % reset storage counter  	   
       end 
    DT(n) = dt; time(n) = t;  KE(n) = ke;  VTY(n) = vty; 
    Udot(:,k) = udot; U(:,k) = u; Tdot(:,k) = tdot; T(:,k) = tt;
  else
%%%%%% rejected step     
  nrej = nrej + 1; 
  if tout>=1, fprintf(' oops .. step was rejected'), end  
  end
  
%%%%%% compute the next timestep
dt = dt*(tol/d)^(1/3);
end
%%% end of timestep loop


%%% finishing touches
plotbouss_pthistory    %% saves point history and plots pictures 
fprintf('\nfinished in %4i steps!\n',n)
DT = DT(1:n);  KE = KE(1:n); VTY = VTY(1:n); time = time(1:n);
%% solution is returned for time steps after the final check-point
U = U(:,1:k); Udot = Udot(:,1:k); 
T = T(:,1:k); Tdot = Tdot(:,1:k);
if nrej>0, disp([int2str(nrej),' rejections in total: tolerance is ',num2str(tol)]), end
%%resave the final solution
finalt=t;
initdata=struct('u',u,'ub',ub,'tt',tt,'ttb',ttb, ...
				'udot',udot,'tdot',tdot, 'udotb',udotb,'tdotb',tdotb, ...
				'dt0',dt,'dt',dt*(tol/d)^(1/3),'time',finalt,'n',n-1, ...
				'restart',restart);
gohome, cd datafiles
fprintf('final time solution data is saved in stabtrBouss_end.mat\n');
save stabtrBouss_end.mat KE VTY DT time L H initdata finalt tol Pr Ra
return
   
