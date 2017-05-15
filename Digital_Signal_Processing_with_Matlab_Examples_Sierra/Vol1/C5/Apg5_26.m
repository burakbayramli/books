% FIR filter coeeficients with Saviztky-Golay filter

K=3; %polynomial order
FR=7; %frame size
numd=sgolay(K,FR); %numerator rows
dend=[1]; %denominator
for rr=1:FR,
subplot(FR,1,rr);   
plot(numd(rr,:),'-kx');
axis([0 FR+1 -0.3 1]);
end

