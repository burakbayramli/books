function h=plot2dgauss(mu, Sigma, color)
% plot2dgauss, based on code by Mark Paskin
% function h=plot2dgauss(mu, Sigma, color)
% Plot an ellipse representing the covariance matrix of a Gaussian

if size(Sigma) ~= [2 2], error('Sigma must be a 2 by 2 matrix'); end
%if length(mu) ~= 2, error('mu must be a 2 by 1 vector'); end
if nargin < 3, color = 'r'; end

mu = mu(:);
[U, D] = eig(Sigma);
n = 100;
t = linspace(0, 2*pi, n);
xy = [cos(t); sin(t)];
%k = 1;
k = sqrt(conf2mahal(0.95, 2));
w = (k * U * sqrt(D)) * xy;
z = repmat(mu, [1 n]) + w;
h = plot(z(1, :), z(2, :), color);

