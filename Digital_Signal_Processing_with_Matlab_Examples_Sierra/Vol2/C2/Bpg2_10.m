% Compute a scaling function
% from MAE
% MAE coefficients
hden=4*sqrt(2); %coeff. denominator
hsq=sqrt(3); %factor
h=[(1+hsq)/hden, (3+hsq)/hden, (3-hsq)/hden, (1-hsq)/hden]; %Daubechies 4
hN=(h*2)/sum(h); %normalization
K=length(hN);

Ns=128; %number of fi samples
fi=[ones(1,3*K*Ns),0]/(3*K); %initial iteration

%upsample hN, inserting Ns-1 zeros between samples
hup=[hN;zeros(Ns-1,K)];
hup=hup(1:(Ns*K));

%iteration
for nn=0:12,
   aux=conv(hup,fi);
   fi=aux(1:2:length(aux)); %downsampling by 2  
end

%result
fi=fi(1:(K-1)*Ns); %the supported part
x=(1:length(fi))/Ns;

plot(x,fi,'k'); %plots the scaling function
title('Daubechies 4 scaling function');
