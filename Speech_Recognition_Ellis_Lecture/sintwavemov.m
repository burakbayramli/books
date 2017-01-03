function m = sintwavemov(k,w,z,l,n)
% m = sintwavemov(k,w,z,l,n)  Build a movie of sin travelling wave interference
%    For a pair of opposite-moving sinusoidal travelling waves of 
%    temporal frequency w and spatial frequency k where the ratio 
%    of forward to backward waves at *l* is z (complex), make n 
%    plots of l points of the wave, both forward, backward, sum and 
%    difference.  Record each plot in a movie, returned in m.
% 2001-02-02 dpwe@ee.columbia.edu e6820 acous

fwd = zeros(1,l);
bck = zeros(1,l);

% Since we know travelling waves are just the sinusoids, rewrite them 
% from scratch every time

m = moviein(n);
x = 0:l;
modz = abs(z);
phaz = angle(z) - 2*k*l;

for t = 0:(n-1);
  fwd = cos(k*x - w*t);
  bck = modz*cos(k*x + w*t + phaz);
  plot(x, fwd, 'g--', x, bck, 'c--', x, fwd+bck, 'b', x, fwd-bck, 'r');
  grid
  title(['t = ', num2str(t)]);
  axis([0 l -(1+modz) (1+modz)]);
  m(:,t+1) = getframe;
end
