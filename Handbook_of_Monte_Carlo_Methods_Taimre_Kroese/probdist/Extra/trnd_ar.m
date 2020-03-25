function X=trnd_ar(nu)
% t_nu generator using Acceptance--Rejection method
% (Algorithm 4.63)
flag=0;

while ~flag
    U1=rand; U2=rand;
    if U1<0.5
        X=1/(4*U1-1);
        V=U2/X^2;
    else
        X=4*U1-3;
        V=U2;
    end
    flag=(V<1-abs(X)/2)||(V<(1+X^2/nu)^(-(nu+1)/2));
end