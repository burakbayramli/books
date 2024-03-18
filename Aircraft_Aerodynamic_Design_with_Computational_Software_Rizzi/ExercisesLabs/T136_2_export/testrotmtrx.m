% test rotmtrx
omeg = rand(3,1);
p = rand(1,3);
omeg = omeg/norm(omeg);
alpha = pi/2;
Om = [0 -omeg(3) omeg(2); omeg(3) 0 -omeg(1); -omeg(2) omeg(1) 0];
T1 = expm(alpha*Om)
[P,p2]=trot5(omeg,p,alpha);
P
norm(T1(:)-P(:))