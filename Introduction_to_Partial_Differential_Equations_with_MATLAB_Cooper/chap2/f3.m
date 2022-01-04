


function y = f3(x)

  ymiddle =  .5 - (x+1).^2 .*(x-2)/8 ;

  y =  .5*(x < -1) + ( ( x < 1.001 ) - ( x < -1) ).*ymiddle + (x > 1);
