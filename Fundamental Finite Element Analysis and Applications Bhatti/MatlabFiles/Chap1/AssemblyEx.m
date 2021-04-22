% Assembly Example
K=zeros(10); R = zeros(10,1);
k = [111,201,301; 201,222,232; 301,232,333]
r = [11; 12; 13]
lm = [1, 2, 5]
R(lm) = R(lm) + r
K(lm, lm) = K(lm, lm) + k

k = [77,80,90; 80,88,100; 90,100,99]
r = [21; 22; 23]
lm = [2, 6, 5]
R(lm) = R(lm) + r
K(lm, lm) = K(lm, lm) + k
