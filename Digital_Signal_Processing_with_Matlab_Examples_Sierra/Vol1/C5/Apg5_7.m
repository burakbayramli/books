% Truncation and time shift
N=7;
H=[ones(N,1);zeros(128-N,1);zeros(128-N,1);ones(N,1)]'; %points of H(w) of ideal filter
h1=ifft(H,128); %inverse Fourier transform
h=ifftshift(h1); %compose symmetrical plot
h=real(h);

subplot(3,1,1);
n=-64:1:63; %number of plotted h(n)terms
stem(n,h,'k'); %plots h(n)
axis([-65 65 -0.015 0.06]);
ylabel('non-causal h(n)'); title('h(n) truncation and time-shift');

subplot(3,1,2);
n=-25:1:25; %number of truncated ht(n)terms
ht=h((64-25):(64+25)); %truncation of h(n)
stem(n,real(ht),'k'); %plots ht(n)
axis([-65 65 -0.015 0.06]);
ylabel('h(n) truncation');

subplot(3,1,3);
n=0:1:50; %time-shift of 25 samples
hf=ht;
stem(n,real(hf),'k'); %plots hf(n)
axis([-65 65 -0.015 0.06]);
ylabel('h(n) time-shift'); xlabel('n');

