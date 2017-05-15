%Scatterplot of two audio signals
%and Histograms of 3 projections: vertical, horizontal and 45º
%you can hear the three projections

%read two audio signals, they have same size
[x,fx]=wavread('wind1.wav'); %read wav file
[y,fy]=wavread('b1.wav'); %read wav file

N=length(x); 

alpha=pi/4; %45º angle in radians

p=zeros(1,N);
%projection of scatterplot on the 45º inclined axis
for i=1:N, 
   p(i)=(x(i)*cos(alpha)) + (y(i)*sin(alpha));
end;   

%display of scatterplot
figure(1)
plot(x,y,'k.'); hold on;
axis([-1.2 1.2 -1.2 1.2]);
%axes
plot([-1.2 1.2],[0 0],'b'); 
plot([0 0],[-1.2 1.2],'b');
%inclined axis
plot([-1.2 1.2],[-1.2 1.2],'r');
title('scatterplot of signals x and y');
xlabel('x'); ylabel('y');

%display of histograms
figure(2)
subplot(3,1,1)
hist(x,50);
axis([-1 1 0 50000]);
title('histogram of signal x');

subplot(3,1,2)
hist(y,50);
axis([-1 1 0 50000]);
title('histogram of signal y');

subplot(3,1,3)
hist(p,50);
axis([-1 1 0 50000]);
title('histogram of projected signal p');

%sounds
soundsc(x,fx); %hear signal x
disp('signal x');
pause %Hit a key!

soundsc(y,fx); %hear signal y
disp('signal y');
pause %Hit a key!

soundsc(p,fx); %hear the projected signal
disp('projected signal p');

