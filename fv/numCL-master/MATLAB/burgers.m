function u = burgers(b,x,t)

% compute x0 using nonlinear solve
x_next = zeros(length(x),1);

for j=1:length(x)
    x_next(j) = fzero(@(xx)myfun(xx,x(j),t,b),0);
end

u = b*sin(x_next);

return