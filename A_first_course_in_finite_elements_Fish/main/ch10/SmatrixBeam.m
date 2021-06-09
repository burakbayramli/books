% evaluates second derivatives of the shape functions 
function S = SmatrixBeam(s,xe)
     L=xe(2)-xe(1);
     S(1)=3/2;
     S(2)=3/4*L;
     S(3)=-3/2;
     S(4)= 3/4*L;
    