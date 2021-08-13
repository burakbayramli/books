
% Define the integrand

g=@(s)exp(cos(s));

% Create a grid

x=linspace(0,1,21);
y=zeros(1,21);

% Evaluate the integral int(exp(cos(s)),s,0,x) for
% each value of x on the grid:

for ii=1:length(x)
   y(ii)=quad(g,0,x(ii));
end

% Now plot the result

plot(x,y)
