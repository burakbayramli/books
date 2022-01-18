function BEconservation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Finite volume methods for Burgers equation
%
% w + w w =0
% t     x
%
% Domain: [a,b]
%
% Riemann Problem
% w0(x)=wl if x<xb=(b+a)/2; else w0(x)=wr
%
% Transmissive boundary conditions
% Deltat is obtained from CFL number
%
% Numerical methods:
% * Godunov
% * Lax Friedrichs
% * Q-scheme of van Leer / Roe
% * Non conservative
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  clear all
  clc
  disp('---------------------------------------------')
  disp('Finite volume methods for Burgers equation')
  disp('---------------------------------------------')
  disp('w + w w =0')
  disp(' t x')
  disp('Domain: [a,b]')
  disp('Transmissive boundary conditions')
  disp('Riemann problem for Burgers equation')
  disp('w0(x)=wl if x<xb=(b+a)/2; else w0(x)=wr')
  disp('Exact solution')
  disp('---------------------------------------------')
  a=0;
  disp(['Lower end of the interval a = ', num2str(a)])
  b=5;
  disp(['Upper end of the interval b = ', num2str(b)])
  m=100;
  disp(['Number of nodes m=', num2str(m)])
  deltax=(b-a)/(m);
  xb=(b+a)/2;
  disp(['Initial location of the discontinuity, xb = '...
	, num2str(xb)])
  disp(['deltax =', num2str(deltax)])
  x=[a:deltax:b];
  cfl=0.8;
  disp(['Courant number = ', num2str(cfl)])
				%
  wl=0.5;
  wr=0.1;
				%
				% Deltat from Courant number
  deltat=cfl*deltax/max(abs(wl),abs(wr));
				%
  disp(['Time step from Courant number = ',...
	num2str(deltat)])
  tmax=2;
  disp(['Time end = ', num2str(tmax)])
  mt=tmax/deltat;
  disp('---------------------------------------------')
  disp(['Initial condition:'])
  disp(['w_l= ',num2str(wl)])
  disp(['w_r= ',num2str(wr)])
  disp(['w0(x)=',num2str(wl),' if x<=',num2str(xb)])
  disp(['w0(x)=',num2str(wr),' if x>',num2str(xb)])
				%
  s=(wl+wr)/2;
  disp(['The speed of the shock'])
  disp(['from the Rankine Hugoniot conditions:',...
	num2str(s)])

  if wl>wr
    disp(['The exact solution is an entropy shock'])
    disp(['w(x,t)=',num2str(wl),...
	  ' if (x-',num2str(xb),')/t<=',num2str(s)])
    disp(['w(x,t)=',num2str(wr),...
	  ' if (x-',num2str(xb),')/t>',num2str(s)])
  elseif wl<wr
    disp(['The exact solution is a rarefaction wave'])
    disp(['w(x,t)=',num2str(wl),...
	  ' if (x-',num2str(xb),')/t<=',num2str(wl)])
    disp(['w(x,t)= (x-',num2str(xb),')/t',...
	  ' if ', num2str(wl),...
	  '<= (x-',num2str(xb),')/t<=',num2str(wr)])
    disp(['w(x,t)=',num2str(wr),...
	  ' if (x-',num2str(xb),')/t>',num2str(wr)])
  elseif wl==wr
    disp('The exact solution is')
    disp('a constant solution w(x,t)=wl')
  end
  disp('---------------------------------------------')
  disp(['Numerical methods:'])
  disp([' * Godunov'])
  disp([' * Lax Friedrichs'])
  disp([' * Q-scheme van Leer / Roe'])
  disp([' * Non conservative'])
  disp('---------------------------------------------')
				%
				% Plot the initial condition at [a,b]
				%
  for i=1:m+1
    if x(i)<=xb
      w0(i)=wl;
    else
      w0(i)=wr;
    end
  end
				%
  wmin=min(wl,wr)-0.2; %lower limit for the y axi
  wmax=max(wl,wr)+0.2; %upper limit for the y axi
  figure(1)
				%
  plot(x,w0,'ok')
  axis([a b wmin wmax])
  xlabel('x'); ylabel('w(x,0)');
  title('Initial condition');
  hold off
				%
				% Initialization
				%
  t=0;
			%
  we=w0;%Exact solution
  wagod=w0;% Godunov method
  walxf=w0;% Lax Friedrichs
  waq=w0;% Q-scheme (Roe=van leer for Burgers equation)
  wanc=w0;% Non conservative scheme
	  %
  dtdx=deltat/deltax;
				%
  for n=1:mt
				% Godunov method
    wngod=god_btbc(wagod,dtdx,m);
				% Lax Friedrichs scheme
    wnlxf=lxf_btbc(walxf,dtdx,m);
				% Q-scheme
    wnq=qscheme_btbc(waq,dtdx,m);
				% Non conservative scheme
    wnnc=ncon_btbc(wanc,dtdx,m);
				%
				% Exact solution
				%
    t=t+deltat;
    we=exact_burgers(x,t,xb,m,wl,wr,s);
				%
				%
    figure(2)
				%
    plot(x,abs(we-wngod),'-x',x,abs(we-wnlxf),'-x',...
	 x,abs(we-wnq),'-+',x,abs(we-wnnc),'-*')
    xlabel('x'); ylabel('|we(x,t)-w(x,t)|');
    axis([a b wmin wmax])
    title(['Burgers equation, t = ',num2str(t)]);
    legend('Error Godunov','Error Lax Friedrichs',...
	   'Error Q-scheme','Error Non conservative')
    pause(1)

    figure(3)

    plot(x,wngod,'-xb',x,wnlxf,'-xg',x,wnq,'-+r',...
				%
	 x,wnnc,'-*m',x,we,'-ok')
    plot(x,wngod,'-x',x,wnlxf,'-x',x,wnq,'-+',...
	 x,wnnc,'-*',x,we,'-ok')
    axis([a b wmin wmax])
    xlabel('x'); ylabel('w(x,t)');
    title(['Burgers equation, t = ',num2str(t)]);
    legend('Godunov','Lax Friedrichs','Q-scheme',...
	   'Non conservative','Exact solution')
    pause(1)
    
				% Update
    walxf=wnlxf;
    wagod=wngod;

    waq=wnq;
    wanc=wnnc;
  end
  disp('---------------------------------------------')
  disp(['Error at time t=',num2str(t)])
  disp(['* Godunov (máx(abs(we-wngod)))=',...
	num2str( max(abs(we-wngod)) )])
  disp(['* Lax Friedrichs (máx(abs(we-wnlxf)))=',...
	num2str( max(abs(we-wnlxf)) )])
  disp(['* Q-scheme (máx(abs(we-wnlxf)))=',...
	num2str( max(abs(we-wnq))
	       )])
  disp(['* Non conservative (máx(abs(we-wnnc)))=',...
	num2str( max(abs(we-wnnc)) )])
  disp('---------------------------------------------')
  disp('---------------------------------------------')
