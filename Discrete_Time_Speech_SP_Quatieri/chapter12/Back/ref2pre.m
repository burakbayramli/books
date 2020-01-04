function a = ref2pre(k);
%REF2PRE        a = REF2PRE(k)  Converts the reflection coefficients to
%               the prediction coefficients. Note, it is assumed that
%		the prediction analysis filter is defined by:
%			A(z) = 1 + a1*z^-1 + a2*z^-2 + ...
%
%	INPUTS:
%               k - reflection coefficients.
%	OUTPUTS:
%		a - prediction coefficients.
%                      a0 = 1;
%               See also PRE2REF.
%            
k = k(:);		% make column
a = zeros(size(k));
p = length(k);
a(1) = -k(1);
for i = 2:p,         
        am = a;
        a(i) = -k(i);
        for j = 1:i-1,
                a(j) = am(j) - k(i)*am(i-j);
        end
end
a = [1;a];
