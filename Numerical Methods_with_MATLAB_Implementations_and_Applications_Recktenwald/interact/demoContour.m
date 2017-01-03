% demoContour  Demonstration of a contour plot

xg = linspace(-5,5,20);    %  grid vector
[X,Y] = meshgrid(xg,xg);   %  Create matrices of X and Y values
Z = 2 - X.^2 - Y.^2;       %  Vectorized evaluation of Z = f(X,Y)
contour(X,Y,Z)             %  Create the contour plot
axis('square')
