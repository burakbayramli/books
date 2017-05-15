%Wigner distribution of prolate signal

%The prolate signal for our example
[yy,c]=dpss(1001,110);
ys=sum(yy');
pp=(1:1001)/1001; ys=ys.*cos(300*2*pi*pp); %frequency shift to center
ym=ys(300:700); ym=ym-mean(ym);
zey=zeros(1,200);
yr=[zey,ym,zey];
y=hilbert(yr)'; 
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

t=0:(110/800):110; f=0:(110/800):110;
%result display
figure(1)
colmap1; colormap(mapg1); %user colormap
imagesc(t,f,log10(0.0005+abs(WD))); axis xy;
title('Wigner distribution of prolate signal');


