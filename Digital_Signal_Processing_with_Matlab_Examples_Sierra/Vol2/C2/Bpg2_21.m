% Display of Battle-Lemarié phi(t), and psi(t)
Np=120; %samples per pi
Nc=18; %number of data cycles
wiv=pi/Np; %frequency interval
w=wiv:wiv:(Nc*pi)+wiv; %frequencies extended for better time resolution
L=length(w);M=ceil(L/2); 
w(M)=0.000001; %to avoid w=0 and division by cero

V=(1+(2*(cos(w/2)).^2))/3;
PHI=((sin(w/2)).^2)./(((w/2).^2).*sqrt(V));

aux0=(exp(-j*(w/2))).*((sin(w/4)).^4)./(((w/4).^2).*sqrt(V));
aux1=(1-((2/3)*((cos(w/4)).^2)))./(1-((2/3)*((sin(w/4)).^2)));
PSI=aux0.*sqrt(aux1);

yphi=ifftshift(ifft(PHI)); 
ct=L/Np; %scaling according with sampling rate
phi=ct*yphi; 

ypsi=ifftshift(ifft(PSI)); 
psi=ct*ypsi; 

figure(1)

tiv=100/(Nc*Np);
tx=(-50):tiv:(50);
t=tx*(Np/60);
Mt=floor(L/2); My=1+(Mt); Mw=4*(Nc+1);

subplot(1,2,1)
plot(t((Mt-Mw):(Mt+Mw)),real(phi((My-Mw):(My+Mw))),'k');
axis([-5 5 -1 1.5]);
xlabel('t'); title('phi(t)');

subplot(1,2,2)
plot(t((Mt-Mw):(Mt+Mw)),real(psi((My-Mw):(My+Mw))),'k');
axis([-5 5 -1 1.5]);
xlabel('t'); title('psi(t)');

