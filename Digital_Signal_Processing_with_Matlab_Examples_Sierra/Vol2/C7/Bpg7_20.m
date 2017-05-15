% two speeches: scatterplot and histograms

%read two sound files
[a,fs1]=wavread('spch1.wav'); %read wav file
[b,fs1]=wavread('spch2.wav'); % "  "  "
a=a-mean(a);
b=b-mean(b);

%display
figure(1)
plot(a,b,'k.'); hold on; %scatterplot
L=1;
plot([-L L],[0 0],'k');
plot([0 0],[-L L],'k');
axis([-L  L -L L]);
title('scatterplot: 2 speeches'); 
xlabel('a'); ylabel('b');

%display of histograms
figure(2)
subplot(1,2,1)
hist(a,50);
axis([-1 1 0 1600]);
title('histogram of signal a');

subplot(1,2,2)
hist(b,50);
axis([-1 1 0 1600]);
title('histogram of signal b');
