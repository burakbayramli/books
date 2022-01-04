

function y = intgnd2(x)

    global x0
    global t
    y = exp(-3*x.^2).*cos(t*(x-x0).^2);
