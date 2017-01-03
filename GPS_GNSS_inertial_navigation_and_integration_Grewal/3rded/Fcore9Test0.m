% Fcore9Test0.m
%
clear all;
close all;
F   = Fcore9(40,[0;0;0]);
F0  = F;
Phi = expm(100*F0);
w0  = max(max(abs(Phi)));
W   = zeros(9,9);
for k=1:9,
    for n=1:9,
        F   = Fcore9(40,[0;0;0]);
        F(k,n) = 0;
        W(k,n) = max(max(abs(expm(100*F))));
    end;
end;
W,
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
