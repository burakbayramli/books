function x=pentaDiag_solve(A,b)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% pentsolve.m
%
% Solve a pentadiagonal system Ax=b where A is a strongly nonsingular matrix
% 
% If A is not a pentadiagonal matrix, results will be wrong
%
% Reference: G. Engeln-Muellges, F. Uhlig, "Numerical Algorithms with C"
%               Chapter 4. Springer-Verlag Berlin (1996)
%
% Written by Greg von Winckel 3/15/04
% Contact: gregvw@chtm.unm.edu
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[M,N]=size(A);

% Check dimensions
if M~=N
    error('Matrix must be square');
    return; 
end

if length(b)~=M
    error('Matrix and vector must have the same number of rows');
    return;
end

x=zeros(N,1);
    
% Check for symmetry
if A==A'    % Symmetric Matrix Scheme
    
    % Extract bands
    d=diag(A);
    f=diag(A,1);
    e=diag(A,2);
        
    alpha=zeros(N,1);
    gamma=zeros(N-1,1);
    delta=zeros(N-2,1);
    c=zeros(N,1);
    z=zeros(N,1);
    
    % Factor A=LDL'
    alpha(1)=d(1);
    gamma(1)=f(1)/alpha(1);
    delta(1)=e(1)/alpha(1);
    
    alpha(2)=d(2)-f(1)*gamma(1);
    gamma(2)=(f(2)-e(1)*gamma(1))/alpha(2);
    delta(2)=e(2)/alpha(2);
    
    for k=3:N-2
        alpha(k)=d(k)-e(k-2)*delta(k-2)-alpha(k-1)*gamma(k-1)^2;
        gamma(k)=(f(k)-e(k-1)*gamma(k-1))/alpha(k);
        delta(k)=e(k)/alpha(k);
    end
    
    alpha(N-1)=d(N-1)-e(N-3)*delta(N-3)-alpha(N-2)*gamma(N-2)^2;
    gamma(N-1)=(f(N-1)-e(N-2)*gamma(N-2))/alpha(N-1);
    alpha(N)=d(N)-e(N-2)*delta(N-2)-alpha(N-1)*gamma(N-1)^2;
    
    % Update Lx=b, Dc=z
    
    z(1)=b(1);
    z(2)=b(2)-gamma(1)*z(1);
    
    for k=3:N
        z(k)=b(k)-gamma(k-1)*z(k-1)-delta(k-2)*z(k-2);
    end
    
    c=z./alpha;
        
    % Backsubstitution L'x=c
    x(N)=c(N);
    x(N-1)=c(N-1)-gamma(N-1)*x(N);
    
    for k=N-2:-1:1
        x(k)=c(k)-gamma(k)*x(k+1)-delta(k)*x(k+2);
    end
    
else        % Non-symmetric Matrix Scheme
    
    % Extract bands
    d=diag(A);
    e=diag(A,1);
    f=diag(A,2);
    h=[0;diag(A,-1)];
    g=[0;0;diag(A,-2)];
        
    alpha=zeros(N,1);
    gam=zeros(N-1,1);
    delta=zeros(N-2,1);
    bet=zeros(N,1);
    
    c=zeros(N,1);
    z=zeros(N,1);
           
    % Factor A=LR
    alpha(1)=d(1);
    gam(1)=e(1)/alpha(1);
    delta(1)=f(1)/alpha(1);
    bet(2)=h(2);
    alpha(2)=d(2)-bet(2)*gam(1);
    gam(2)=( e(2)-bet(2)*delta(1) )/alpha(2);
    delta(2)=f(2)/alpha(2);
    
    for k=3:N-2
        bet(k)=h(k)-g(k)*gam(k-2);
        alpha(k)=d(k)-g(k)*delta(k-2)-bet(k)*gam(k-1);
        gam(k)=( e(k)-bet(k)*delta(k-1) )/alpha(k);
        delta(k)=f(k)/alpha(k);
    end
    
    bet(N-1)=h(N-1)-g(N-1)*gam(N-3);
    alpha(N-1)=d(N-1)-g(N-1)*delta(N-3)-bet(N-1)*gam(N-2);
    gam(N-1)=( e(N-1)-bet(N-1)*delta(N-2) )/alpha(N-1);
    bet(N)=h(N)-g(N)*gam(N-2);
    alpha(N)=d(N)-g(N)*delta(N-2)-bet(N)*gam(N-1);

    % Update b=Lc
    c(1)=b(1)/alpha(1);
    c(2)=(b(2)-bet(2)*c(1))/alpha(2);
    
    for k=3:N
        c(k)=( b(k)-g(k)*c(k-2)-bet(k)*c(k-1) )/alpha(k);
    end
    
    
    % Back substitution Rx=c
    x(N)=c(N);
    x(N-1)=c(N-1)-gam(N-1)*x(N);

    for k=N-2:-1:1
        x(k)=c(k)-gam(k)*x(k+1)-delta(k)*x(k+2);    
    end
   
end