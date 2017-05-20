%assuming x1 and x2 and t are defined in the work space
z1=x1+x2;				
subplot(3,1,1), plot(t,z1), grid
title('x1(t)+x2(t)');
%assuming x3 and x4 are defined
z2=x3+x4;
subplot(3,1,2), plot(t,z2), grid  
title('x3(t)+x4(t)');
z3=x1+x4;
subplot(3,1,3), plot(t,z3), grid  
title('x1(t)+x4(t)'), xlabel('t in seconds')
