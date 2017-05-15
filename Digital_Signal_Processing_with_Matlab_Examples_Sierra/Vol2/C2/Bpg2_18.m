% Display of Meyer scaling function phi(t)

Np=120; %samples per pi
Nc=18; %number of data cycles
w=0:(pi/Np):(Nc*pi); %frequencies extended for better time resolution
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

yphi=ifftshift(ifft(PHI)); 
ct=L/Np; %scaling according with sampling rate
phi=ct*yphi; 

figure(1)
tiv=100/(Nc*Np);
tx=(-50):tiv:(50);
t=tx*(Np/60);
Mt=floor(L/2); My=1+(Mt); Mw=4*(Nc+1);
plot(t((Mt-Mw):(Mt+Mw)),real(phi((My-Mw):(My+Mw))),'k');
axis([-7 7 -0.3 1.2]);
xlabel('t'); title('phi(t)');