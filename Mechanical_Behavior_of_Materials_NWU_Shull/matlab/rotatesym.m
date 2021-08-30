%rotatesym.m
clear all
syms phi; % declare as symbolic variable
sig=sym(zeros(3)); % create sybolic stress tensor and set to zero
syms sig1p sig2p sig3p % these are the  normal stresses
sig(1,1)=sig1p; % put normal stresses into the tensor
sig(2,2)=sig2p;
sig(3,3)=sig3p;

sigp=sym(zeros(3)); % initalize rotated streses to zero

theta=[phi,pi/2-phi,pi/2;pi/2+phi,phi,pi/2;-pi/2,-pi/2,0];
costheta=cos(theta);
for i=1:3
    for j=1:3
        for k=1:3
            for l=1:3
                sigp(i,j)=sigp(i,j)+costheta(i,k)*costheta(j,l)*sig(k,l);
            end
        end
    end
end
sigp=simplify(sigp);
pretty(sigp);
