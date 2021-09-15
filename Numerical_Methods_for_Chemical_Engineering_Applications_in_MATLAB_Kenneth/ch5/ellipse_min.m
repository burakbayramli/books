% ellipse_min.m
% This MATLAB program finds the two nearest points on
% two 2-D ellipses.
% K. Beers. MIT ChE. 8/12/03

function iflag_main = ellipse_min();

iflag_main = 0;

% set the ellipse properties
Ell.rc1 = [0; 0];
Ell.rc2 = [5; 5];
Ell.a1 = 0.5; Ell.b1 = 0.3;
Ell.a2 = 0.2; Ell.b2 = 0.4;

% make figure showing ellipses
theta = linspace(0,2*pi,100);
% ellipse 1
v = Ell.a1.*(cos(theta).^2) + Ell.b1.*(sin(theta).^2);
v = sqrt(1./v);
v_x1 = Ell.rc1(1) + v.*cos(theta);
v_y1 = Ell.rc1(1) + v.*sin(theta);
figure; plot(v_x1,v_y1); 
xlabel('x'); ylabel('y'); hold on;
% ellipse 2
v = Ell.a2.*(cos(theta).^2) + Ell.b2.*(sin(theta).^2);
v = sqrt(1./v);
v_x2 = Ell.rc2(1) + v.*cos(theta);
v_y2 = Ell.rc2(1) + v.*sin(theta);
plot(v_x2,v_y2);

% make initial guess ellipse centers
x0 = zeros(4,1);
x0(1) = Ell.rc1(1); x0(2) = Ell.rc1(2);
x0(3) = Ell.rc2(1); x0(4) = Ell.rc2(2);

% make initial multiplier guesses zero
lambda = zeros(2,1);

% set initial quadratic penalty strength
mu = 1;

% begin multiplier iterations
for iter_mult = 1:100
    
    lambda_old = lambda;
    
    % minimize augmented Lagrangian using
    % unconstrained method
    Options = optimset('Display','off');
    [x,F] = fminunc(@ellipse_min_LA,x0,Options,lambda,mu,Ell);
    x1=x(1); y1=x(2); x2=x(3); y2=x(4);
    
    % plot unconstrained min. on figure
    plot(x1,y1,'o'); plot(x2,y2,'d');
    
    % update Lagrange multipliers
    g = zeros(2,1);
    g(1) = Ell.a1*(x1-Ell.rc1(1))^2 + Ell.b1*(y1-Ell.rc1(2))^2 - 1;
    lambda(1) = lambda(1) - g(1)/mu;
    g(2) = Ell.a2*(x2-Ell.rc2(1))^2 + Ell.b2*(y2-Ell.rc2(2))^2 - 1;
    lambda(2) = lambda(2) - g(2)/mu;
    
    % increase quadratic penalty strength
    mu = mu/2;
    
    % check for convergence
    delta_lambda = lambda_old - lambda;
    if(dot(delta_lambda,delta_lambda) < 1e-6)
        line([x1;x2],[y1;y2]);
        iflag_main = 1;
        break;
    end
end

iflag_main = 0;
return;


% This routine computes the value of the augmented Lagrangian.
function [LA,iflag] = ellipse_min_LA(x,lambda,mu,Ell);
iflag = 0;

x1=x(1); y1=x(2); x2=x(3); y2=x(4);
% compute cost function
F = (x1-x2)^2 + (y1-y2)^2;
% compute equality functions
g = zeros(2,1);
g(1) = Ell.a1*(x1-Ell.rc1(1))^2 + Ell.b1*(y1-Ell.rc1(2))^2 - 1;
g(2) = Ell.a2*(x2-Ell.rc2(1))^2 + Ell.b2*(y2-Ell.rc2(2))^2 - 1;
% compute augmented Lagrangian
LA = F - dot(lambda,g) + (dot(g,g))/(2*mu);

iflag = 1;
return;
