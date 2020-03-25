function P = korobov(a,d,N)
z(1) = 1;
for i=2:d
    z(i) = mod(z(i-1)*a,N);
end
Z = repmat(z,N,1);
B = repmat((1:N)',1,d);
P = mod(B.*Z/N, 1);