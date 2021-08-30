%rotatesym.m
clear all
syms phi; % declare as symbolic variable
sig=sym(zeros(3)); % create sybolic stress tensor and set to zero
syms sig1p sig2p sig3p % these are the  normal stresses
sig(1,1)=sig1p; % put normal stresses into the tensor
sig(2,2)=sig2p;
sig(3,3)=sig3p;
theta=[phi,pi/2+phi,pi/2;pi/2-phi,phi,pi/2;-pi/2,-pi/2,0];
Q=cos(theta);
QT=transpose(Q);
sigp=Q*sig*QT;
sigp=simplify(sigp);
pretty(sigp);
