% Radon transform of a phantom

P=phantom('Modified Shepp-Logan',256);
theta=0:1:180;

PR=radon(P,theta); %the Radon transform

nPR=PR/max(max(PR)); %normalization

figure(1)
imshow(P);
title('the Shepp-Logan phantom');

figure(2)
imshow(nPR);
title('Radon transform (sinogram)');

