

function c = finiteDiffCoeff(x,k)

    n = length(x);
    A = ones(n,n);
    xrow = x';

    for i=2:n
        A(i,:) = (xrow .^(i-1)) ./ factorial(i-1);
    end

    b = zeros(n,1);     % b is right hand side,
    b(k+1) = 1;         % so k'th derivative term remains
    c = A\b;            % solve system for coefficients

    c = c';
    
end
