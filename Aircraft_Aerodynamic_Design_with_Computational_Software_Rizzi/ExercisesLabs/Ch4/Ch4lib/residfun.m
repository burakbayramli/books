% nozzle equations dW/dt = residfun(W)
% aug 2017
function [W,rhs, p] = residfun(gamma, a, W, p01, p2, t01, vis2, vis4, R, vol, dt)
cpgas  = R*gamma/(gamma-1);
nnodes = size(W,1);
ib2    = nnodes-1;
% update p
rrho  = a./W(:,1);
rhou = W(:,2)./a;
rhoe = W(:,3)./a;
p    = (gamma-1).*(rhoe-0.5.*rhou.^2.*rrho);

% bcond
% Inlet.
u    = W(2,2)/W(2,1);
cs2  = gamma*p(2)*a(2)/W(2,1);
c02  = cs2 + 0.5*(gamma-1)*u^2;
rinv = u - 2*sqrt(cs2)/(gamma-1);
dis  = (gamma+1)*c02/((gamma-1)*rinv^2) - 0.5*(gamma-1);
if dis < 0
    dis = 1E-20;
end
cb   = -rinv*((gamma-1)/(gamma+1))*(1+sqrt(dis));
cc02 = min(cb^2/c02,1);
tb   = t01*cc02;
pb   = p01*(tb/t01)^(gamma/(gamma-1));
rhob = pb/(R*tb);
ub   = sqrt(2*cpgas*(t01-tb));
W(1,1) = rhob*a(2);
W(1,2) = rhob*a(2)*ub;
W(1,3) = (pb/(gamma-1)+0.5*rhob*ub^2)*a(2);
p(1)   = pb;

% Outlet.
rho = W(ib2,1)/a(ib2);
u   = W(ib2,2)/W(ib2,1);
cs  = sqrt(gamma*p(ib2)/rho);
if u >= cs
    % Supersonic.
    pb   = p(ib2);
    rhob = rho;
    ub   = u;
else
    % Subsonic.
    pb   = p2;
    rhob = rho + (p2-p(ib2))/cs^2;
    ub   = u - (p2-p(ib2))/(cs*rho);
end
W(end,1) = rhob*a(ib2);
W(end,2) = rhob*ub*a(ib2);
W(end,3) = (pb/(gamma-1)+0.5*rhob*ub^2)*a(ib2);
p(end)   = pb;

%dissipation
dp = abs((p(3:end)-2*p(2:end-1)+p(1:end-2))./(p(3:end)+2*p(2:end-1)+p(1:end-2)));
dp = [dp(1); dp; dp(end)];
dp = 0.5*(dp(2:end)+dp(1:end-1));

%Vectors of artificial viscosity contribution.
eps2 = vis2*dp(2:end-1)*ones(1,3);
eps4 = max(0,vis4-eps2);

%     eps2=[eps2 eps2 eps2];
%     eps4=[eps4 eps4 eps4];

evalflux=0.5*(vol(1:end-1)./dt(1:end-1)+vol(2:end)./dt(2:end));

% The artificial dissipation for the physical volumes
%d=diag(evalflux(2:end-1))*(eps2.*(W(3:end-1,:)-W(2:end-2,:))-eps4.*(W(4:end,:)-3*W(3:end-1,:)+3*W(2:end-2,:)-W(1:end-3,:)));
d=((evalflux(2:end-1))*ones(1,3)).*(eps2.*(W(3:end-1,:)-W(2:end-2,:))-eps4.*(W(4:end,:)-3*W(3:end-1,:)+3*W(2:end-2,:)-W(1:end-3,:)));

% Added dissipation for "ghost" volumes
diss = diff([d(2,:);d(1,:); d; d(end,:); d(end-1,:)]);

% flux_center
si   = 0.5*(a(2:end)+a(1:end-1));
rav  = 0.5*(W(2:end,1)./a(2:end)+W(1:end-1,1)./a(1:end-1));
ruav = 0.5*(W(2:end,2)./a(2:end)+W(1:end-1,2)./a(1:end-1));
reav = 0.5*(W(2:end,3)./a(2:end)+W(1:end-1,3)./a(1:end-1));

pav  = 0.5*(p(2:end)+p(1:end-1));
rhav = reav+pav;

qs = ruav.*si./rav;

% Matrix of flux
f(:,1)=rav.*qs;
f(:,2)=ruav.*qs+pav.*si;
f(:,3)=rhav.*qs;

% -dWdt = flux + dissipation + srcterm
rhs = zeros(nnodes,3);
rhs(2:end-1,:) = diff(f)-diss(2:end-1,:);

% srcterm
da = 0.5*(a(3:end)-a(1:(ib2-1)));
da = [da(1); da; da(end)];
rhs(:,2) = rhs(:,2)-p.*da;
% residual now in rhs
