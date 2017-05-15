% Display of Meyer nu function and 1/2 PHI(w) (right side)
Np=120; %samples per pi
w=0:(pi/Np):(2*pi); %frequencies
L=length(w);
n23=1+((2*Np)/3); %samples for 0..2/3 pi
n43=1+((4*Np)/3); %samples for 0..4/3 pi
wc=w(n23:n43); %central zone

x=((3/(2*pi))*abs(wc))-1;

nu=35-(84*x)+(70*(x.^2))-(20*(x.^3));
nu=(x.^4).*nu;

PHI=zeros(1,L);
PHI(1:(n23-1))=1;
PHI(n23:n43)=cos((pi/2)*nu);

figure(1)
subplot(2,1,1)
plot(x,nu,'k');
xlabel('x'); title('nu');
subplot(2,1,2)
plot(w,PHI,'k');
xlabel('w'); title('PHI(w)');