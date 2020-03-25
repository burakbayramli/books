function x=gamrnd_cheng(alpha)
% Gamma(alpha,1) generator using Cheng--Feast method
% Algorithm 4.35
c1=alpha-1; c2=(alpha-1/(6*alpha))/c1; c3=2/c1; c4=1+c3;
c5=1/sqrt(alpha);
flag=0;
while flag==0;
    U1=rand; U2=rand;
    if alpha>2.5
        U1=U2+c5*(1-1.86*U1);
    end
       W=c2*U2/U1;
     flag=(U1<1)&&(U1>0)&&(((c3*U1+W+1/W)<c4)||((c3*log(U1)-log(W)+W)<1));
end
x=c1*W;