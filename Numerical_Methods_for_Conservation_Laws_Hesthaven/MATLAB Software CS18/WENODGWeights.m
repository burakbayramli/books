function [Q,Xm,Xp] = WENODGWeights(m,iV);
% function [Q,Xm,Xp] = WENODGWeights(m,iV);
% Purpose: Compute operators to enable evaluation of WENO smoothness
% indicator and WENO polynomial of order m.
Q = zeros(m+1,m+1); Pmat = zeros(m+1,m+1); Xm = Pmat; Xp = Pmat;

% Compute quadrature points
[x,w] = LegendreGQ(m); Lambda = diag(w);

% Initial matrices of Legendre polynomails
for i=1:m+1;
    Pmat(i,:) = LegendreP(x,i-1)';
    Xm  (i,:) = LegendreP(x-2,i-1)';
    Xp  (i,:) = LegendreP(x+2,i-1)';
end

% Compute matrices corresponding to increasing order of derivative
for l=1:m    
    % Set up operator to recover derivaties
    A = zeros(m+2-l,m+2-l); A(1,1) = 1/sqrt((2*l+1)*(2*l-1)); 
    A(m+2-l,m+2-l) = 1/(sqrt(2*(m+2)+1)*sqrt(2*(m+2)-1));
    for i=2:m-l+1
        Ah = 1/(sqrt(2*(l-1+i)+1)*sqrt(2*(l-1+i)-1));
        A(i,i) = Ah; A(i+1,i-1) = -Ah;
    end
    
    % Recover derivatives at quadrature points
    Ph1 = A\Pmat(l:m+1,:);    
    Pmat(1:l,:)=0; Pmat(l+1:m+1,:) = Ph1(1:m-l+1,:);
   
    % Compute smoothness operator for order l and update
    Qh = Pmat*Lambda*Pmat';
    Q = Q + 2^(2*l-1)*Qh;
end

% Initialize operator for smoothness indicator in nodal space
Q = iV'*Q*iV;

% Initialize interpolation matrices 
Xp = iV'*Xp; Xm = iV'*Xm;
return