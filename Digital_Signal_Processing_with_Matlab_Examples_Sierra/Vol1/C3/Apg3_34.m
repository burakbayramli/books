%Example of ARMA behaviour

% ARMA model coeefs
%coeffs. of polynomial A
a1=0.05; a2=0.1;
%variable initial values
y=0.1; y1=0; y2=0;
%coeffs. of polynomial C
c0=0.6; c1=0.4; c2=0.3;
%variable initial values
e1=0; e2=0;

Ni=200; %number of iterations
ry=zeros(1,Ni); %for storage of y values
ee=randn(1,Ni); %vector of random values
 
%iterations
for nn=1:Ni,
    e=ee(nn);
    % ARMA model:
    y=(c0*e+c1*e1+c2*e2)-(a1*y1+a2*y2); 
    ry(nn)=y;
    % memory update:
    y2=y1; y1=y; 
    e2=e1; e1=e; 
end;

figure(1)
plot(ry,'k'),
xlabel('n');
title('evolution of model output');
