% Shape functions in the natural coordinate s
 function N = NmatrixBeam(s,xe)
     L=xe(2)-xe(1);
     N(1)=1/4*(1-s)^2*(2+s);
     N(2)=L/8*(1-s)^2*(1+s);
     N(3)=1/4*(1+s)^2*(2-s);
     N(4)=L/8*(1+s)^2*(s-1);     