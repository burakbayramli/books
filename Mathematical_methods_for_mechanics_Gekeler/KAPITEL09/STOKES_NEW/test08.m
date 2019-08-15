function test08
n = 5;
A = rand(n,n); B = rand(n,n);
C = A*B;
SC = 0;
for i = 1:5
    SC = SC + C(i,i);
end
SC
D = B*A;
SD = 0;
for i = 1:5
   SD = SD + D(i,i);
end
DIFF = SC - SD

