function Impinv

%Impulse invariance method of anolog-to-digital filter conversion				
%a,b -- s-plane coefficients
%az,bz -- digital filter coefficients

clear all;
b = 1;                     
a = [1.84496 1.920675 1];  
[bz,az]=impinvar(b,a)      %get z-plane coefficients using impulse Inv.
freqz(bz,az,1024);
