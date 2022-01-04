



function y = f2(x)


      ymiddle  = .25*(x+1).*(x-2).^2;

   y = (x < 0) + ( (x < 2.001 ) - (x < 0) ).*ymiddle ;
