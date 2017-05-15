% Compute Daubechies filters for M=2,4,6...12
w=0:(2*pi/511):pi;

for K=1:6,
a=1; p=1; q=1;
h=[1 1];
M=2*K; %length of the filter

% the h0(n) coefficients
for nn=1:K-1,
   h=conv(h,[1,1]);
   a=-a*0.25*(nn+K-1)/nn;
   p=conv(p,[1,-2,1]);
   q=[0 q 0] + a*p;
end;
q=sort(roots(q));
aux=real(poly(q(1:K-1)));
h=conv(h,aux);    
h0=(h*sqrt(2))/sum(h); %normalization
H0=fft(h0,512); %LP filter frequency response

%the h1(n) coefficients
h1=fliplr(h0); h1(1:2:end)=-h1(1:2:end);
H1=fft(h1,512); %HP filter frequency response

%display
subplot(2,1,1)
plot(w,abs(H0(1:256)),'k'); hold on;
axis([0 pi 0 2]);
title('|H0(w)|'); 

subplot(2,1,2)
plot(w,abs(H1(1:256)),'k'); hold on;
axis([0 pi 0 2]);
title('|H1(w)|'); xlabel('w');


end;

