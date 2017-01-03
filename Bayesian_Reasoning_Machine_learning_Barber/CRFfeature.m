function indf=CRFfeature(x,y,t,dimx,dimy)

indxy = reshape(1:dimx*dimy,dimx,dimy);
indyy = reshape(1:dimy*dimy,dimy,dimy);
indf(1,1) = indxy(x(t),y(t)); indf(2,1)=indf(1)+indyy(y(t-1),y(t));