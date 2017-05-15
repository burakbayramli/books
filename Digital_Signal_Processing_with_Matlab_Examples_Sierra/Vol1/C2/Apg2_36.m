% Generation of random data with a desired PDF
% Using analytic inversion

% example of desired distribution function
x=0:(pi/100):(pi/2); 
F=sin(x); %an always growing curve
pf=cos(x); %PDF=derivative of F

% generation of random data
N=2000; %number of data
y=rand(1,N); %uniform distribution
% random data generation:
z=asin(y); %the inverse of F

figure(1)
subplot(1,2,1)
plot(x,F,'k'); 
xlabel('x'); title('desired distribution function');
subplot(1,2,2)
plot(x,pf,'k');
xlabel('x'); title('desired PDF');

figure(2)
hist(z,30); colormap('cool');
xlabel('x');title('histogram of the generated data');

