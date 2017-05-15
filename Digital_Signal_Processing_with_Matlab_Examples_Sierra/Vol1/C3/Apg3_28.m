%Example of DARMA behaviour

%coeffs. of polynomials A and B
a1=0.5; a2=0.7; b0=1; b1=0.7;
%variable initial values
y=0; y1=0; y2=0; u=0; u1=0;

Ni=24; %number of iterations
ry=zeros(1,Ni); %for storage of y values
 
%iterations
for nn=1:Ni,
    u=1; %test value (edit)
    y=(b0*u+b1*u1)-(a1*y1+a2*y2); %according with the model
    ry(nn)=y;
    y2=y1; y1=y; u1=u; %memory update
end;

figure(1)
stem(ry,'k');
title('evolution of model output');
xlabel('n');


