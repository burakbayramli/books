%Example of DARMA behaviour
% now using filter()

%coeffs. of polynomials A and B
a=[1 0.5 0.7]; b=[1 0.7];

Ni=24; 
u=ones(24,1); %test value
y=filter(b,a,u);

figure(1)
stem(y,'k');
title('evolution of model output');
xlabel('n');


