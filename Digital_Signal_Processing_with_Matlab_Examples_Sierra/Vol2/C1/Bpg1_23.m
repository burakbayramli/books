% 1D DCT simple example, correlated data
% Recovering from first 3 coeffs
%
RWX=zeros(1,9); %space for recovery

x=[1 2 3 4 5 6 7 8 9]; %original data
WX=dct(x); %the DCT transform

RWX(1:3)=WX(1:3); %select 3 first coeffs

rx=idct(RWX); %recovery of data

%display
figure(1)
stem(x,'k'); hold on;
stem(rx,'rx');
plot([0 10],[0 0],'k-');
axis([0 10 -10 18]);
title('Comparison of original and recovered data');

disp('recovered data');
rx
