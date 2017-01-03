
x = 0;
h = pi;
for j=1:8
  % f(x) = sin(4x) --> f'(x) = 4*cos(4*x)
  % f(x) = exp(sin(x)) --> f'(x) = cos(x)*exp(sin(x))
  y1(j) = d_mid(@f,x,h);
  H1(j) = h;
  h = h/2;
end

for j=1:8
  N = 2^j;
  h = 2*pi/N;
  x = -pi:h:pi-h;
  F = f(x);
  Fx = fdiff(F);
  y2(j) = Fx(N/2+1);
  H2(j) = h;
  [k,amp] = find_spec(F);  
  A(j) = amp(length(k));
  figure(j)
  plot(k,amp)
end

H1
err1 = abs(y1-1)
err1(1:7)./err1(2:8)
display(' ')
H2
err2 = abs(y2-1)
A

