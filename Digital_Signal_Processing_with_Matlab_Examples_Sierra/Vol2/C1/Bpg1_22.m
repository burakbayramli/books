% 1D DCT simple example, uncorrelated data
%
x=[9 5 2 4 1 8 7 3 9]; %original data
WX=dct(x); %the DCT transform
E=sum(abs(WX)); %energy of transformation result

%display
figure(1)
stem(WX,'k'); hold on;
plot([0 10],[0 0],'k-');
axis([0 10 -10 18]);
title('Transform of uncorrelated data');

disp('uncorrelated data transform:');
WX
disp('uncorrelated data transform energy:');
E
