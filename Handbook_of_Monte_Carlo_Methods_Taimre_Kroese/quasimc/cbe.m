function a = cbe(k,b) % coefficients of base-b expansion of k
numd = max(0,floor(log(k)/log(b))) + 1; %number of digits
a = zeros(1,numd);
q = b^(numd-1);
for i = 1:numd
    a(i) = floor(k/q);
    k = k - q*a(i);
    q = q/b;
end