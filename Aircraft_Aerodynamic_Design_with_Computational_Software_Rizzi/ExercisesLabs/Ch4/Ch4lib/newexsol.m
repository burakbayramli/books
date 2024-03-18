function [u,p,rho] = newexsol(p01,T01,p2,ar)
%close all
%clear all
%format compact
%p01 = 400000  % Pa
%p2  = 301000
%T01 = 400     % K
%A2  = 4;
%At  = 1;
%A0  = 2;
%ncon = 70
%nx  = 140;
%ar(1:ncon)    = (A0+At)/2 + (A0-At)/2*cos((0:ncon-1)/(ncon-1)*pi);
%ar(ncon+1:nx) = (At+A2)/2 + (At-A2)/2*cos((1:nx-ncon)/(nx-ncon)*pi);
ncon = find(ar==1);
%ar  = ar/At;
g   = 1.4;
A2  = ar(end);
A0  = ar(1);
R   = 287.3   % J/kg/K
rho0 = p01/T01/R
nx  = length(ar);
%figure(1)
%plot(ar)
%hold on

% try isentropic
q2 = (1-(p2/p01)^(1-1/g))*2*g*p01*rho0*A2^2/(g-1)*(p2/p01)^(2/g)
C  = 1/2*(g-1)/g*q2/rho0/p01./(ar.^2);
hh = 2/(g+1);
cmx = hh^(2/(g-1))*(g-1)/(g+1)
isen = max(C)<= cmx
if isen
% solve C = y^2 - y^(1+1/g), y = (p/p0)^(1/g)
% from A2 where y = (p2/p0)^(1/g)
%dC = (2y - (g+1) y^g) dy + 1/2(2 -g*(g+1)*y^(g-1))*dy^2
  yl = zeros(nx,1);
  yl(nx) = (p2/p01)^(1/g);
  tol = 1e-6;
  for k = nx-1:-1:1
    y  = yl(k+1);
    dC = C(k)-C(k+1);
    a = 1/2*(2-g*(g+1)*y^(g-1));
    b = 2*y-(g+1)* y^g;
    c = -dC;
    bo2a = b/2/a;
    dy2  = -c/b;
 %   dy2  = -bo2a + sqrt(bo2a^2-c/a);
    yt = dy2 + yl(k+1);
    while 1
      f = yt^2 - yt^(g+1) - C(k);
      df = 2*yt - (g+1)*yt^g;
      cr = f/df;
      yt = yt -cr ;
      if abs(cr) < tol
        break
      endif
    endwhile
    yl(k) = yt;
  end
  p   = yl.^(g)*p01;
  rho = rho0*(p/p01).^(1/g);
  u   = sqrt(q2)./ar'./rho;
  if u(1)/sqrt(g*p(1)/rho(1)) < 1
  %  plot(p/p01,'.-k')
  %  plot(u./sqrt(g*p./rho),'.-r')
'Subsonic'
    return
  else
    disp('all supersonic')
  end
end % if isen
 % supersonic
  rho01 = p01/R/T01
  S0    = p01/rho01^g
  Htot  = p01/rho01*g/(g-1)
% M2   = u2/(g p /rho) = 2(Htot- g/(g-1) p/rho)/(g p/rho) = -2/(g-1) + 2Htot/(g p/rho)
% S0   = p/rho^g
% M2   = -2/(g-1) + 2Htot/[(g rho^(g-1) S0]
% rho^(g-1)(M2 + 2/(g-1))= 2Htot/(g S0)
% exact solution
  n    = 40
  M0   = [linspace(0.01,1,n),linspace(1.05,5,n)];
  mu   = M0.^(2/(g+1));
  aoas = mu.^(1-g) + (g-1)/2*mu.^2;
  aoas = (aoas*2/(g+1)).^((g+1)/2/(g-1));
  isup = find(aoas == 1);
  isup = isup:2*n;
  isub = 1:min(isup)-1;
  M    = [interp1(aoas(isub),M0(isub),ar(1:ncon-1),'extrap'),interp1(aoas(isup),M0(isup),ar(ncon:nx),'extrap')];
  pause(1)
  rho = 2*Htot/(g*S0)./(M.^2 + 2/(g-1));
  rho = rho.^(1/(g-1));
  p   = S0*rho.^g;
  u   = M.*sqrt(g*p./rho);
  q   = ar(1).*rho(1).*u(1)

% RH: rho u A = const. = q  A unknown to match p at outflow A2 = p2
%  [rho A u g/(g-1)p/rho + 1/2 rho A u u2] = 0
%  g/(g-1) A [u+p+ - up]+1/2q[u+^2 - u2]=0
%  isentropic subsonic from A (u+, p+, rho+) to A2 (u2 p2 rho2) -- what A?
%  S1 = p+/rho+^g
% unknown: A u+ p+ rho+
% RH equations
% * q/A (u+ - u(A)) +u (p+ - p(A)] = 0: p+ = p - q/A(u+ - u)
% * g/(g-1) A [u+p+ - u(A)p(A)]+1/2q[u+^2 - u(A)^2]=0
% a quadratic for u+, then p+: p+ = p - q/A(u+ - u)
% * Htot = g/(g-1) p+/rho+ + 1/2 u+^2: rho+ = g/(g-1)*p+/(Htot -1/2 u+^2)
% with rho+,u+,p+(A)
%   rho2 A2 u2 = q: 
%   p+/rho+^g = p2/rho2^g: rho2 = rho+*(p2/p+)^(1/g)
%   u2 = q/(rho2 A2)
% *  Htot = g/(g-1) p2/rho2 + 1/2 u2^2
  nao = 50;
  aoassl = linspace(1.01,A2,nao);
  for k = 1:nao
    aoass = aoassl(k);
% isup = indices for supersonic branch
    ua = interp1(ar(ncon:nx), u(ncon:nx), aoass,'extrap'); 
    pa = interp1(ar(ncon:nx), p(ncon:nx), aoass,'extrap');
% * g/(g-1) aoass [u+(pa - q/aoass(u+ - ua)) - ua pa]+1/2q[u+^2 - ua^2]=0
% u+^2[g/(g-1)*-q + 1/2q] + u+(g/(g-1)*(aoass*pa + q*ua)) + g/(g-1)aoass(-ua*pa)-1/2 q ua^2))
    up = roots([(-g/(g-1)*q + 1/2*q),(g/(g-1)*(aoass*pa + q*ua)),(g/(g-1)*aoass*(-ua*pa)-1/2*q*ua^2)]);
    up = min(up);
    pp = pa - q/aoass*(up - ua);
    rhop = g/(g-1)*pp/(Htot - 1/2*up^2);
%  
    rho2 = rhop*(p2/pp)^(1/g);
    u2   = q/(rho2*A2);
    Ht(k) = g/(g-1)*p2/rho2 + 1/2*u2^2;
  end
  Htot
  min(Ht)
  max(Ht)
  if Htot < min(Ht) | max(Ht) < Htot
   % title('Supersonic out')
   % plot(M,'o-k')
   % plot(p/p01,'o-r') 
   'supersonic'
    return
  else
    isup = find(M>=1);
    nsup = min(isup);
    aosh = interp1(Ht,aoassl,Htot)
    iq   = find(ar(isup) >= aosh,1)
    ua   = interp1(ar(isup), u(isup), aosh)
    pa   = interp1(ar(isup), p(isup), aosh)
    rhoa = g/(g-1)*pa/(Htot-1/2*ua^2)
% * g/(g-1) aoass [u+(pa - q/aoass(u+ - ua)) - ua pa]+1/2q[u+^2 - ua^2]=0
% u+^2[g/(g-1)*-q + 1/2q] + u+(g/(g-1)*(aoass*pa + q*ua)) + g/(g-1)aoass(-ua*pa)-1/2 q ua^2))
    up = roots([(-g/(g-1)*q + 1/2*q),(g/(g-1)*(aosh*pa + q*ua)),(g/(g-1)*aosh*(-ua*pa)-1/2*q*ua^2)])
    up = min(up)
    pp = pa - q/aosh*(up - ua)
    rhop = g/(g-1)*pp/(Htot - 1/2*up^2)
    Sp   = pp/rhop^g;

% p/rho^g = pp/rhop^g; p = pp*(rho/rhop)^g
% rho aoas u = q: rho = q/(u aoas)
% g/(g-1)p/rho + 1/2 u2 = Htot
% g/(g-1) pp*rho^(g-1)/rhop^g + 1/2 (q/(rho aoas))^2 = Htot
% rho^(g-1) + rho^(-2) 1/2 (q/aoass)^2(g-1)rhop^g/(g*pp) = Htot*(g-1)*rhop^g/(g*pp)
% rho^(g-1) + a*(rho*aoas)^(-2) = rhs
    rmax = Htot*(g-1)/g/Sp;
    rmax = rmax^(1/(g-1))
    nr   = 20;
    rlist= linspace(rhop,rmax,nr);
    al   = q./(rlist.*sqrt(2*(Htot - g/(g-1)*Sp*rlist.^(g-1))));
    rs   = interp1(al,rlist,ar(nsup+iq-1:end));
    ps   = pp*(rs/rhop).^g;
    us   = q./(ar(nsup+iq-1:end).*rs);
    Ms   = us./sqrt(g*ps./rs);
    p (nsup+iq-1:nx)  = ps(1:end);
    u (nsup+iq-1:nx)  = us(1:end);
    rho(nsup+iq-1:nx) = rs(1:end);
    M (nsup+iq-1:nx)  = Ms(1:end);
    p = real(p);
    u = real(u);
    rho = real(rho);
    M   = real(M);
    'shock'
 %   plot(M,'o-k')
 %   plot(p/p01,'o-r')
 %   title('Subsonic out')
  end
%  xlabel('x')
%  ylabel('A,M,p/p01')
%  set(gca,'fontsize',16)






