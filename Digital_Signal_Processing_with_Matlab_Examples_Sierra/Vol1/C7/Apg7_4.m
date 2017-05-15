% Plot of 5x6 large logons

%1 basic GMP pulse
fy=100; %central frequency in Hz
fs=1000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;

%signal time intervals set (-0.2 to 0.2 seconds):
tp=-(0.2-tiv):tiv:(0.2); %to get N even
Np=length(tp); 
bw=50/fy; %relative bandwidth (DeltaF=50 Hz)

yr=gauspuls(tp,fy,bw);

t=0:tiv:(Np*tiv)-tiv; %time intervals
f=0:1:((fs/2)-1); %frequency intervals

y=hilbert(yr); %analytical signal

%Wigner:---------------------------------------------------------
N=length(y); 
theta=((0:N-1)-N/2)'*2*pi/N; %normalized frequencies set -pi..pi
tau=(0:N-1)-N/2; %normalized time

att=zeros(N); %intermediate NxN matrix
att=theta*tau/2; %matrix computation (theta(ii)*tau(jj))
ax1=exp(j*att); % NxN matrix
ax2=exp(-j*att); % NxN matrix
disp('step 1');

my1=zeros(N); %intermediate NxN matrix
my2=zeros(N); %intermediate NxN matrix
for ii=1:N, %rows ii
      my1(ii,:)=y(ii)*ax1(ii,:); %along jj
      my2(ii,:)=y(ii)*ax2(ii,:); %along jj
end;
disp('step 2');

Y1=fft(my1);
Y2=fft(my2);
kappa=Y1.*conj(Y2);
WD=fftshift(fft(kappa,[],2));
%---------------------------------------------------------

%GMP=abs(WD(222:260,184:216));
GMP=abs(WD(216:266,180:218));


G1=[GMP GMP GMP GMP GMP GMP]; %adding 6 matrices
GG=[G1;G1;G1;G1;G1]; %adding 5 rows of matrices

mesh(GG);axis([0 240 0 250]); view(30,70); %3D plot with perspective

title('A 3D view of Gabor logons');xlabel('x 0.1 sec'); ylabel('Hz');
