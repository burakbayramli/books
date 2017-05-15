function wty=B_set(sq,alpha)

%B set for a given direction
% alpha in rads

np=size(sq,1);

[YY,XX]=meshgrid(1:np,1:np);
p=(-sin(alpha)*XX(:))+ (cos(alpha)*YY(:));
[aux,ix]=sort(p); %ascending order
F=sq(ix); %ordered values

%1-D Haar wavelet transform of F
Ns=length(F);
K=floor(log2(Ns)); %number of scales
wty=F;
for n=1:K,
   aux1= wty(1:2:Ns-1) + wty(2:2:Ns);
   aux2= wty(1:2:Ns-1) - wty(2:2:Ns);
   wty(1:Ns)=[aux1,aux2]/sqrt(2);
   Ns=Ns/2;
end;
