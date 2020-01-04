function 	       A = lsp2pre(L)
%LSP2PRE               A = lsp2pre(L)
%               Converts the line-spectral pairs representation to
%               a set of predictor coefficients. Note the predictor
%		analysis filter is defined as 
%			A(z) = 1 + a1*z^-1 + a2*z^-2 + ...
%		i.e. E(z) = A(z)S(z) where E is the error and S
%				the speech data
%	INPUTS:
%		L	line spectral pairs (must be EVEN length)
%			(In matrix case, each column should be a
%			set of lsp coeffs.)
%	OUTPUTS:
%		A	predictor coefficients (in matrix case, each
%			column is a set of pre coeffs.)
%
i = sqrt(-1);
twpi = 2*pi;

[p,num] = size(L);
A = zeros(p+1,num);

if (rem(p,2) ~= 0),
        error('p not even');
end

for j = 1:num,

l = L(:,j);
rtP(1:2:p) = exp(i*twpi*l(2:2:p));
rtP(2:2:p) = exp(-i*twpi*l(2:2:p));
rtQ(1:2:p) = exp(i*twpi*l(1:2:p));
rtQ(2:2:p) = exp(-i*twpi*l(1:2:p));
rtP(p+1) = 1;
rtQ(p+1) = -1;
Q = poly(rtQ);
P = poly(rtP);
a = real(.5 * (Q + P))';
a = a(1:p+1);
A(:,j) = a;

end
