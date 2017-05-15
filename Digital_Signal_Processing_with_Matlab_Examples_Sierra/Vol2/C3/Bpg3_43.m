% Inverse Radon transform
% phantom data

% The Radon transform of the phantom
P=phantom('Modified Shepp-Logan',256);
theta=0:1:180;
PR=radon(P,theta); %the Radon transform

% The inverse transform

IR=iradon(PR,theta);

figure(1)
imshow(IR);
title('the recovered phantom');

