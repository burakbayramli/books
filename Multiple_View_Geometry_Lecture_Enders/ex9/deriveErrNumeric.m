function [ Jac, residual ] = deriveErrNumeric( IRef, DRef, I, xi, K )
    % calculate numeric derivative. SLOW
    
    eps = 1e-8;
    Jac = zeros(size(I,1) * size(I,2),6);
    residual = calcErr(IRef,DRef,I,xi,K);
    for j=1:6
        epsVec = zeros(6,1);
        epsVec(j) = eps;
        
        % MULTIPLY epsilon from left onto the current estimate.
        xiPerm =  se3Log(se3Exp(epsVec) * se3Exp(xi));
        Jac(:,j) = (calcErr(IRef,DRef,I,xiPerm,K) - residual) / eps;
    end
end

