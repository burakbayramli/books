% Record of Eigenvalues of Water sound

%signal
[y1,fs1]=wavread('Water.wav'); %read wav file

Nf=500; %number of data frames
Nd=100; %number of data per frame

y=y1(1:(Nf*Nd)); %signal segment

%for recording
veig=zeros(Nd,Nf);

%by frames
for nn=0:(Nf-1), 
   aux1=1+(nn*Nd); aux2=(nn+1)*Nd;
   vy=y(aux1:aux2);
   syy=xcorr(vy)/Nd; %symmetrical auto-correlation sequence
   Ry=toeplitz(syy(Nd:(2*Nd)-1)); % auto-correlation matrix   
   E=eig(Ry); %eigenvalues
   veig(:,nn+1)=E;
end

%display
figure(1)
nn=1:Nf;
plot(nn,veig(:,nn),'.k');
title('eigenvalues along data blocks');
xlabel('number of data block');

%print
disp('last data frame info:');
disp('mean eigenvalue')
mean(E)
Ry(1,1)

disp('energy in the filter')
sum(E)
sum(vy.^2)
trace(Ry)

disp('cond(Ry)');
cond(Ry)
