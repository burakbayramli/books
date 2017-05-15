%Scatterplot of IRIS data
% x=sepal width, y=petal length

%read IRIS data
D=dlmread('iris.data')';

%display
scatter(D(2,1:50),D(3,1:50),32,'k'); hold on;
scatter(D(2,51:150),D(3,51:150),32,'r');
axis([0 6 0 8]);
title('IRIS data'); 
xlabel('sepal width'); ylabel('petal length');
