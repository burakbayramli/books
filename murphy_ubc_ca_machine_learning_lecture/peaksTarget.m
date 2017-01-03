function  p = peaksTarget(x)
Z = peaks;
%Z = log(Z);
r = round(x(1)); c = round(x(2));
if r >= 1 & r <= size(Z,1) & c >= 1 & c <= size(Z,2)
  p = Z(r,c);
else
  p = inf; %-10000; % invalid
end
