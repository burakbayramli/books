

function y = f4(x)


        ymiddle = .05*x.^3 - .15*x.^2 + 1.1;

    y = 1.1*(x < 0) + ( (x < 2.001) - (x < 0) ) .*ymiddle + .9*(x > 2);
