% Quadtree analysis for larger LH
% several levels

tfg=B_2D_Haar; %function call

th=5; %threshold

sq=tfg(257:512,1:256); %LH square
Kmax=8; %256 = 2^8
%smallest square
Kmin=4; %exponent

disp('please wait');

[Q,A]=B_calc_quadt(sq,th,Kmin,Kmax); %function call

%display
figure(1)
imagesc(A);
title('direction angles matrix');