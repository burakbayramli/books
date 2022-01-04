



function y = f1(x)

    ymiddle = 1.0 - .125*x.^2.*(3.0 - x);

  y = (x < 0) + ( (x < 2.001 ) - (x< 0)  ).*ymiddle  + .5*(x > 2);
