function x=trnd_ratio(nu)
% t_nu  generator via ratio of uniforms (Algorithm 4.60)

flag=0;

while ~flag
    U=sqrt(nu)*(-1+2*rand);
    V=rand;
    W=V^(1/nu);
    flag=(W^2+U^2/nu)<1;
    x=U/W;
end

