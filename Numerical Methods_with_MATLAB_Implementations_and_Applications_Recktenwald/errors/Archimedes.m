function Archimedes(d)
% Archimedes  Perimeter of an n-sided polygon inscribed in a circle.
%
% Synopsis:   Archimedes
%             Archimedes(d)
%
% Input:      d = (optional) diameter of the circle;  Default:  d = 1
%
% Output:     Differences between circumference, c, of a circle and perimeter,
%             p, of an n-sided polygon inscribed in the circle.

if nargin<1; d = 1;  end

fprintf('   n     difference    alpha\n');
for n = [4 8 16 32 64]
  p = n*d*sin(pi/n);
  dif = p - pi*d;
  fprintf(' %3d   %12.9f',n,dif);
  h = 1/n;
  if n>4        %  compute alpha only if there is enough data
    fprintf('  %8.5f\n',log(dif/difold)/log(h/hhold));
  else
    fprintf('\n');
  end
  difold = dif;   hhold = h;         %  save for next iteration
end
