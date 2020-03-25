function P = extkorobov(a,d,N)
b = 2;
z(1) = 1;
for i=2:d
    z(i) = mod(z(i-1)*a,N);
end
Z = repmat(z,N,1);
v = vdc(b,N);
B = repmat(v,1,d);
P = mod(B.*Z, 1);