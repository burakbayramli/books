function test01
clc
p = [1 0;
     1 1; 
     1 2;
     2 3;
     1 1;
     1 1
     1 0];
p = p.';
A = [2,3,5,1,6];
[p1,A1] = mesh04_tst(p,A);
p1
A1 
