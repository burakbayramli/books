function div_f = bsp05k(X,Y,b)
% Example after Boukir with exact solution
% right side for pressure equation: div f
 f1_x  = -pi^2*b^2*sin(pi*X).^2 + pi^2*b^2*cos(pi*X).^2;
 f2_y = -pi^2*b^2*cos(pi*Y).^2 + pi^2*b^2*sin(pi*Y).^2 ...
        -4*pi^3*cos(pi*X).*sin(pi*Y); 
div_f = f1_x + f2_y;
div_f = div_f(:);
