% Cross generator

n = 1:1500-1;
x1 = cos((3*pi*n/10) + (pi*n.*n/10000));
x2 = cos((6*pi*n/10) - (pi*n.*n/10000));
x = x1 + x2;
x1 = x1(201:1224); 
x2 = x2(201:1224); 
x = x(201:1224); 
 



