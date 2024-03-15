% Problem 12 in Chapter 3  
sig = 50; 
      V = 1;
      x = -2:.02:2; 
      y = -2:.02:2; 
      gam = -10;
    for m = 1:length(x)
     for n = 1:length(y)
        xx(m,n) = x(m); yy(m,n) = y(n);
        psiV(m,n) = V * y(n) + gam/4/pi*log(x(m)^2+(y(n)+.05)^2) ...
            - gam/4/pi*log(x(m)^2+(y(n)-.05)^2) ;
        psiS(m,n) =  V*y(n) + (sig/2/pi) * atan2(y(n),x(m)+0.01) ...
            - (sig/2/pi) * atan2(y(n),(x(m)-.01));
     end
    end
    contour(xx,yy,psiV,41,'k'),axis image
    figure(2)
    contour(xx,yy,psiS,41,'k'),hold on, 
    contour(xx,yy,psiS,[0 0],'k'),axis image