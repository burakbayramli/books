function p = flow(s,p,rng)

% p = flow(s,p,rng)
% flow points p with deformation map given in s
% p must be 3*N
% s is the returned value of a call to match function

X = s.X;
mom = s.mom;
transmatrix = s.transmatrix;
transvector = s.transvector;
nx = s.nx;
sigmaV2 = s.sigmaV2;
tau = s.tau;
SobOrder = s.SobOrder;

if nargin == 2
    rng = 0:T-1;
end

N = size(p,2);

if rng(1) == 0
    % additional affine transformation
    p = transmatrix * p + repmat(transvector,1,size(p,2));
    rng = rng(2:end);
end

dp = zeros(size(p));
argin = zeros(N,nx);

for t = rng
    dp(:) = 0;
    for l = 1:N
        for k = 1:nx
            argin(l,k) = -( ...
                (X(1,k,t)-p(1,l))^2 + ...
                (X(2,k,t)-p(2,l))^2 + ...
                (X(3,k,t)-p(3,l))^2)/sigmaV2;
        end
    end
    argout = sobolev(SobOrder,argin);  %% BUILT IN KERNEL kerV, do not remove this comment
    for l = 1:N
        for k = 1:nx
            dp(1,l) = dp(1,l) + argout(l,k)*mom(1,k,t);
            dp(2,l) = dp(2,l) + argout(l,k)*mom(2,k,t);
            dp(3,l) = dp(3,l) + argout(l,k)*mom(3,k,t);
        end
    end

    %%%% BEGIN corrector scheme
    %%%% comment this block to use simple Euler scheme
    ptemp = p + tau * dp;
    for l = 1:N
        for k = 1:nx
            argin(l,k) = -( ...
                (X(1,k,t)-ptemp(1,l))^2 + ...
                (X(2,k,t)-ptemp(2,l))^2 + ...
                (X(3,k,t)-ptemp(3,l))^2)/sigmaV2;
        end
    end
    argout = sobolev(SobOrder,argin);  %% BUILT IN KERNEL kerV, do not remove this comment
    for l = 1:N
        for k = 1:nx
            dp(1,l) = dp(1,l) + argout(l,k)*mom(1,k,t);
            dp(2,l) = dp(2,l) + argout(l,k)*mom(2,k,t);
            dp(3,l) = dp(3,l) + argout(l,k)*mom(3,k,t);
        end
    end
    dp = dp / 2;
    %%%% END corrector scheme
    p = p + tau * dp;
end

function K = sobolev(k,r)

% Kernel for operator L=(I-Laplacian)^k in R^2
% expressed as a function of -|x-y|^2/sigma^2

s = sqrt(-r) + 1e-100;
order = k-1;
K = s.^order .* besselk(order,s);


