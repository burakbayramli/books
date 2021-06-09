% Derivative of the shape functions in the natural coordinate s
function B = BmatrixBeam(s,xe)

     L=xe(2)-xe(1);
     B(1)=3/2*s;
     B(2)=L*(3/4*s-1/4);
     B(3)=-3/2*s;
     B(4)= L*(3/4*s+1/4);
    
     
   