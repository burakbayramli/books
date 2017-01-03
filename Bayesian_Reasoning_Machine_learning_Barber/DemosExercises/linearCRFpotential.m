function pot=linearCRFpotential(x,t,lambda,dimx,dimy)
%LINEARCRFPOTENTIAL a particuarly simple linear chain CRF potential
indxy = reshape(1:dimx*dimy,dimx,dimy);
indyy = reshape(1:dimy*dimy,dimy,dimy);
for ytm=1:dimy
    for yt=1:dimy
        indf(1) = indxy(x(t),yt); indf(2)=indf(1)+indyy(ytm,yt);
        pot.variables=[t-1 t];
        pot.table(ytm,yt)=exp(sum(lambda(indf)));
    end
end