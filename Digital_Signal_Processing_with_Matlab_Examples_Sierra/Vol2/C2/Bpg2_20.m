% Display of Battle-Lemarié PHI(w), H0(w, H1(w) and PSI(w)
Np=120; %samples per pi
w=(-9*pi):(pi/Np):(9*pi); %frequencies
L=length(w); M=ceil(L/2); 
w(M)=0.000001; %to avoid w=0 and division by cero

aux1=(cos(w/2)).^2;
V1=(1+(2*aux1))/3;
PHI=((sin(w/2)).^2)./(((w/2).^2).*sqrt(V1));
aux2=(sin(w)).^2;
H0=(sqrt(2)*aux1.*sqrt(V1))./sqrt(1-(2*aux2/3));

aux1=(cos((w+pi)/2)).^2;
V2=(1+(2*aux1))/3;
aux2=(sin(w+pi)).^2;
aux=sqrt(2)*aux1.*sqrt(V2)./sqrt(1-(2*aux2/3));
H1=-(exp(-j*w)).*aux;

aux0=(exp(-j*(w/2))).*((sin(w/4)).^4)./(((w/4).^2).*sqrt(V1));
aux1=(1-((2/3)*((cos(w/4)).^2)))./(1-((2/3)*((sin(w/4)).^2)));
PSI=aux0.*sqrt(aux1);

figure(1)
plot(w,V1,'k');
xlabel('w'); title('V');
axis([-30 30 0 1.1]);

figure(2)
subplot(2,2,1)
plot(w,abs(PHI),'k');
xlabel('w'); title('|PHI(w)|');
axis([-30 30 0 1.1]);

subplot(2,2,2)
plot(w,abs(PSI),'k');
xlabel('w'); title('|PSI(w)|');
axis([-30 30 0 1.1]);

subplot(2,2,3)
plot(w(M:(M+Np)),abs(H0(M:(M+Np))),'k');
xlabel('w'); title('|H0(w)|');
axis([0 pi 0 2]);

subplot(2,2,4)
plot(w(M:(M+Np)),abs(H1(M:(M+Np))),'k');
xlabel('w'); title('|H1(w)|');
axis([0 pi 0 2]);


