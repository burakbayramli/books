function X=gen_decode(P,Nb,l,u)
% decode an array of binary strings(P) into a population(X) of state 
Np=size(P,1); %population size
N=length(Nb); %dimension of the variable(state)
for n=1:Np
   b2=0;
   for m=1:N
      b1=b2+1; b2=b1+Nb(m)-1; %Eq.(7.1-28)
      X(n,m)=bin2dec(P(n,b1:b2))*(u(m)-l(m))/(2^Nb(m)-1)+l(m);
   end
end
