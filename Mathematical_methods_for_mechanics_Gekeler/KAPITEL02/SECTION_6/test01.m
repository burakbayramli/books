function test01
% Test von QR-Zerlegung
A = rand(3,4); B = rand(3,1);
X1 = pinv(A)*B; X2 = mpsolv(A,B);
NORM = norm(X1 - X2)
