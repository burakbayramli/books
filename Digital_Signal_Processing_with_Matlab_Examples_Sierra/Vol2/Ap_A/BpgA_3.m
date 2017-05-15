% Compute a scaling function with dyadic approach

% Daubechies 4,coeffs.
hden=4*sqrt(2); %coeff. denominator
hsq=sqrt(3); %factor
h=[(1+hsq)/hden, (3+hsq)/hden, (3-hsq)/hden, (1-hsq)/hden]; %Daubechies 4
hN=(h*2)/sum(h); %normalization
K=length(hN);
hrev=hN(K:-1:1); %reverse hN

%M0 matrix
MA=[hrev,zeros(1,(2*K)-2)]; MB=MA;
for nn=1:K-1,
   MA=[0,0,MA(1:(3*K)-4)]; MB=[MB; MA];
end   
M0=MB(:,K:(2*K)-1);

%Solving the first system of equations, for fi(0)..fi(3)
MF=M0-eye(K);
MG=[MF(1:K-1,:);ones(1,K)];
nfi=MG\[zeros(K-1,1);1];

%getting middle & quarter values
fi=nfi(2:length(nfi)-1); fi=conv(hN,fi); 
aux=fi(1:2:length(fi)); %downsampling by 2
y=conv(hN,aux); %quarter values
%merge y and fi
aux=[y;fi,0]; aux=aux(:)'; fi=aux(1:length(aux)-1);

%iteration
hup=hN; Nx=9;
for nn=1:Nx,
   %upsample by 2   
   L=length(hup); aux=[hup;zeros(1,L)]; aux=aux(:)';
   hup=aux(1:2*L-1);  
   y=conv(hup,y); %intermediate terms
   %merge y and fi
   aux=[y;fi,0]; aux=aux(:)'; fi=aux(1:length(aux)-1);    
end

%result
x=(1:length(fi))*(K-1)/length(fi);
plot(x,fi,'k')
title('Daubechies 4 scaling function');
