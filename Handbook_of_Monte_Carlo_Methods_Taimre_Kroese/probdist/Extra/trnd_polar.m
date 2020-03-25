function x=trnd_polar(nu)
% t_nu generator using polar method II
% (algorithm 4.62)
flag=1;

while flag
    U=-1+2*rand;
    V=-1+2*rand;
    W=U^2+V^2;
    flag=W>1;
end

C=U^2/W; R=nu*(W^(-2/nu)-1);

x=sign(U)*sqrt(R*C);
