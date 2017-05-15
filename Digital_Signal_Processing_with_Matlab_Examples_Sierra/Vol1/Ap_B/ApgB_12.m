% Wigner distribution of Gaussian chirplet
t0=5; w0=10; d=6; c=2; %chirplet parameters
t=0:0.05:12; %times vector
g=exp(-(0.5/d)*((t-t0).^2));
v=exp(-j*(w0+((0.5*c)*(t-t0))).*(t-t0));
h=(1/((pi*d)^0.25))*g.*v;

y=h';
Ny=length(y);

%WIGNER-------------------------------------------
zerx=zeros(Ny,1); aux=zerx;
lm=(Ny-1)/2;
zyz=[zerx; y; zerx]; %sandwich zeros-signal-zeros
WD=zeros(Ny,Ny); %space for the Wigner distribution, a matrix
mtau=0:lm; %vector(used for indexes)
for nt=1:Ny, 
    tpos=Ny+nt+mtau; %a vector
    tneg=Ny+nt-mtau; %a vector
    aux(1:lm+1)=(zyz(tpos).*conj(zyz(tneg)));
    aux(1)=0.5*aux(1); %will be added 2 times
    fo=fft(aux,Ny)/(Ny);
    WD(:,nt)=2*real(fo); %a column (harmonics at time nt)
end 

%result display
Ts=0.05; %sampling period
ws=(2*pi)/Ts; wiv=ws/(2*Ny);
w=0:wiv:((ws/2)-wiv);
figure(1)
colmap1; colormap(mapg1); %user colormap
imagesc(t,w,log10(0.01+abs(WD))); axis xy;
title('Wigner distribution of Gaussian chirplet');
ylabel('rad/s'); xlabel('seconds');

