tspan = linspace(0,2.0,41);
x0 = zeros(length(x),length(tspan));
q0 = x0;

for i=1:length(x)
   [tt,xx] = ode45('xprime', tspan, x(i));
   x0(i,:) = xx';
   end

q0 = exp(-beta*(x0-0.5).^2) .* cos(freq*(x0-0.5));
for j=1:size(x0,2)
   isquare = find(x0(:,j)>1.0 & x0(:,j)<1.5);
   q0(isquare,j) = q0(isquare,j) + 1;
   end

