function k = pre2ref(a);
%PRE2REF        k = PRE2REF(a)   Calculates the reflection coefficients from
%               a valid set of predictor coefficients, i.e. 
%			a = [a0=1, a1,a2, ..., ap]'; 
%		Also, the prediction analysis filter is defined as: 
%			A(z) = 1 + a1*z^-1 + a2*z^-2 + ...
%
%	INPUTS:
%               a - prediction coefficients  (a0 = 1 must be included!)
%	OUTPUTS:
%		k - reflection coefficients.
%
%            
a = a(:);
a = a(2:length(a));
p = length(a);
k = zeros(size(a));
k(p) = -a(p);
for m = p:-1:2,         
        am = a;
        for i = 1:m-1,
                a(i) = (am(i) + k(m)*am(m-i))/(1-k(m)^2);
        end
        k(m-1) = -a(m-1);
end
