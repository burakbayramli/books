%nm1p03a: to plot a cone
clear, clf
x=-1:0.04:1; y=-1:0.04:1;
[X,Y]=meshgrid(x,y);
Z=1-sqrt(X.^2+Y.^2);
Z=max(Z,zeros(size(Z)));
mesh(X,Y,Z)
