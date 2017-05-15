% Likelihood example

sig2=1;
%constant
r=1/(2*sig2);

Lh=zeros(4,101); %reserve space

for ni=1:4,
   switch ni
   case 1, N=20;
   case 2, N=60;
   case 3, N=200;
   case 4, N=500;
   end;  
%N is number of data     
%data generation with normal distribution, mean=5, sigma=1
x=5+randn(1,N); 
K=(-N/2)*log(2*pi*sig2);

aux=0;
for nm=1:101,
   mu=(nm-1)/10; %mean
   aux=(x-mu).^2;
   Lh(ni,nm)=K-(r*sum(aux)); %Log-Likelihood
end;

end;

%display
ex=0:0.1:10;
figure(1)
for ni=1:4,
subplot(2,2,ni),   
plot(ex,Lh(ni,:),'k');
axis([0 10 -2000 0]);
end;  
