function [L U] = factor2(A)
    [m, n] = size(A);
    if ( m >= n )    % if skinny
       L = chol( A'*A, 'lower' );
    end

    % force matlab to recognize the upper / lower triangular structure
    L = sparse(L);
    U = sparse(L');
end
