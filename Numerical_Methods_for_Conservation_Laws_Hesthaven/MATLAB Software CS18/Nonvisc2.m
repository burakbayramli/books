function [nu] = Nonvisc2(x,u,iV,m,N,h,nu0);
% function [nu] = Nonvisc2(x,u,iV,m,N,h,nu0);
% Purpose: Compute nonlinear viscosity following Kloeckner et al (2013)
nu = zeros(m+1,N); S = zeros(1,N); nuh = zeros(1,N); onev = ones(m+1,1);
eps0 = 1e-10;

% Special case of m=1,2
if (m<3) 
    nuh = nu0*h/m*ones(1,N);
else
   % Extract coefficients and compute smoothness measure
   uh = iV*u; uh1 = uh(2:m+1,:);

   % Adjust for scaling 
   bh = [1:m]'.^(-m)./sqrt(sum([1:m].^(-2*m)));
   ut = sqrt(uh1.*uh1 + (bh.*bh)*sum(uh1.*uh1));
   
   % Adjust for non-monotone decay
   ub = ut;
   for i=1:m-1
       ub(i,:) = max(abs(ut(i:m,:)),[],1);
   end
   
   % Compute decay estimate by least squares fit
   b1 = log([1:m]); h1 = -sum(b1); h2 = -b1*log(ub+eps0);
   A = [m h1;h1 h1^2]; b = [-h1*ones(1,N);h2]; coef = A\b;
 
   % Compute elementwise viscosity
   nu1 = (coef(2,:)<=1); nu2 = (coef(2,:)>1).*(coef(2,:)<3);
   nuh = nu0*h/m*(nu1 + nu2.*(1-(coef(2,:)-1)/2));
end

% Compute continuous viscosity
nue = zeros(1,N+2); nue = [nuh(1) nuh nuh(N)];
maxL = max(nue(1:N),nue(2:N+1)); maxR = max(nue(2:N+1),nue(3:N+2));
nu = onev*maxL + (x-onev*x(1,:))/h.*(onev*(maxR-maxL));
return