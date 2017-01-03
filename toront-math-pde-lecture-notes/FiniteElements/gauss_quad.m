% I = gauss_quad(@f,x0,x1) uses Gauss quadrature to approximate the
% integral of f from x0 to x1
%
function I = gauss_quad(f,x0,x1);


n = 5;
x(1) = 0;
w(1) = 128/225;
x(2) = sqrt(5-2*sqrt(10/7))/3;
w(2) = (322+13*sqrt(70))/900;
x(3) = - x(2);
w(3) = w(2);
x(4) = sqrt(5+2*sqrt(10/7))/3;
w(4) = (322-13*sqrt(70))/900;
x(5) = - x(4);
w(5) = w(4);

xx = (x0+x1)/2 + x*(x1-x0)/2;
I = dot(w,f(xx));
I = I*(x1-x0)/2;


    