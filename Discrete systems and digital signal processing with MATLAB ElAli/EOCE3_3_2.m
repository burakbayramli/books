n=-10:10;
cn=(1./(2*n.^2*pi)).*((-1).^n-1)+j*(1./(2*n)).*(-1.^n);
cn(11)=pi/4; % cn = pi/4 at n = 0
subplot(2,1,1), stem(n,abs(cn)),
ylabel('Magnitude of cn'),
subplot(2,1,2), stem(n,angle(cn)),
ylabel('Angle of cn'),
xlabel('n');
