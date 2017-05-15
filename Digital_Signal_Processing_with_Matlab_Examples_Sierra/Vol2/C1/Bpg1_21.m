% 1D DCT simple example, correlated data
%
x=[1 2 3 4 5 6 7 8 9]; %original data
WX=dct(x); %the DCT transform
E=sum(abs(WX)); %energy of transformation result

%display
figure(1)
stem(WX,'k'); hold on;
plot([0 10],[0 0],'k-');
axis([0 10 -10 18]);
title('Transform of correlated data');

disp('correlated data transform:');
WX
disp('correlated data transform energy:');
E
