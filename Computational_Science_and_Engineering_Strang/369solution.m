nodevalues=[1;0;0;0;0;0;0;0];
xy=[1 0  0  0   0   0   0   0;
    1 1  0  1   0   0   0   0;
    1 0  1  0   0   1   0   0;
    1 .5 0  .25 0   0   0   0;
    1 0  .5 0   0   .25 0   0;
    1 .5 1  .25 .5  1   .25 .5;
    1 1 .5  1   .5 .25  .5  .25;
    1 1  1  1   1   1   1   1];
a=inverse(xy)*nodevalues
% a=[1,-3,-3,2,5,2,-2,-2]

% plot the function phi(x,y) in the square, with those nodevalues

[x,y] = meshgrid(0:.1:1, 0:.1:1);
z = a(1) + a(2)*x + a(3)*y + a(4)*(x.^2) + a(5)*(x.*y) + a(6)*(y.^2) +
a(7)*((x.^2).*y) + a(8)*(x.*(y.^2));

surf(x,y,z);