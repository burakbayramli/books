function [ulimit] = WENOlimitDG(x,u,m,h,N,V,iV,Q,Xm,Xp);
% function ulimit = WENOlimitDG(x,u,m,h,N,V,iV,Q,Xm,Xp);
% Purpose: Apply WENO limiter by Zhong-Shu (2013) 
% to u - an m'th order polynomial    
eps0=1.0e-6;

% Set constants for limiting
eps1 = 1e-10; p=1;
gammam1 =0.001; gamma0 = 0.998; gammap1 = 0.001;

% Compute cell averages and cell centers
uh = iV*u; uh(2:(m+1),:)=0; uavg = V*uh; ucell = uavg(1,:); ulimit = u;

% Compute extended polynomials with zero cell averages
[ue] = extendDG(u,'P',0,'P',0);
Pm = Xp'*ue; Pp = Xm'*ue;
Ph = iV*Pm; Ph(1,:)=0; Pm = V*Ph;
Ph = iV*Pp; Ph(1,:)=0; Pp = V*Ph;

% Extend cell averages
[ve] = extendDG(ucell,'P',0,'P',0);

% extract end values and cell averages for each element
uel = u(1,:); uer = u(end,:); 
vj = ucell; vjm = ve(1:N); vjp = ve(3:N+2);

% Find elements that require limiting
vel = vj - minmod([(vj-uel)' (vj-vjm)' (vjp-vj)'])';
ver = vj + minmod([(uer-vj)' (vj-vjm)' (vjp-vj)'])';
ids = find(abs(vel-uel)>eps0 | abs(ver-uer)>eps0); 

% Apply limiting when needed
if(~isempty(ids))
    % Extract local polynomials
    pm1 = Pm(:,ids) + ones(m+1,1)*vj(ids);
    p0  =  u(:,ids);
    pp1 = Pp(:,ids+2) + ones(m+1,1)*vj(ids);
   
    % Compute smoothness indicators and WENO weights
    betam1 = diag(pm1'*Q*pm1); alpham1 = gammam1./(eps1 + betam1).^(2*p); 
    beta0 = diag(p0'*Q*p0); alpha0  =  gamma0./(eps1 + beta0).^(2*p);
    betap1 = diag(pp1'*Q*pp1); alphap1 = gammap1./(eps1 + betap1).^(2*p); 
  
    alphas = alpham1 + alpha0 + alphap1;
    omm1 = alpham1./alphas; om0  = alpha0./alphas; omp1 = alphap1./alphas;
        
    % Compute limited function
    ulimit(:,ids) = pm1*diag(omm1) + p0*diag(om0) + pp1*diag(omp1);
end
return  