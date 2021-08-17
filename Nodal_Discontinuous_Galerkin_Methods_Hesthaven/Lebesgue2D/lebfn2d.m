
function f = lebfn2d(N, lcoeffs, L1, L2, L3) 
 
 X = -1*L2 +  1*L3  + -1*L1;
 Y = -1*L2 + -1*L3  +  1*L1;

 vdm  = Vandermonde2D(N, X, Y);

 fn = vdm*lcoeffs;

 f = sum(abs(fn'));     




