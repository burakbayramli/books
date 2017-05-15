% Display of Meyer wavelet psi(t)
Np=120; %samples per pi
Nc=19; %number of data cycles
w=0:(pi/Np):(Nc*pi); %frequencies extended for better time resolution
L=length(w);
n23=1+((2*Np)/3); %samples for 0..2/3 pi
n43=1+((4*Np)/3); %samples for 0..4/3 pi
n83=1+((8*Np)/3); %samples for 0..8/3 pi

wc1=w(n23:n43); %zone 1
wc2=w((n43+1):n83); %zone 2

x1=((3/(2*pi))*abs(wc1))-1;
x2=((3/(4*pi))*abs(wc2))-1;

nu1=35-(84*x1)+(70*(x1.^2))-(20*(x1.^3));
nu1=(x1.^4).*nu1;
nu2=35-(84*x2)+(70*(x2.^2))-(20*(x2.^3));
nu2=(x2.^4).*nu2;

PSI=zeros(1,L);
PSI(1:(n23-1))=0;
PSI(n23:n43)=(exp(j*wc1/2).*sin((pi/2)*nu1));
PSI((n43+1):n83)=(exp(j*wc2/2).*cos((pi/2)*nu2));

ypsi=ifftshift(ifft(PSI)); 
ct=L/Np; %scaling according with sampling rate
psi=ct*ypsi; 

figure(1)
tiv=100/(Nc*Np);
tx=(-50):tiv:(50);
t=tx*(Np/60);
Mt=floor(L/2); My=1+(Mt); Mw=4*(Nc+1);
plot(t((Mt-Mw):(Mt+Mw)),real(psi((My-Mw):(My+Mw))),'k');
axis([-7 7 -1 1.2]);
xlabel('t'); title('psi(t)');
