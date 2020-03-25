function X=normrnd_ratio_uniforms
% N(0,1) generator using ratio of uniforms with squeezing  
% (Algorithm 4.49)
ap=sqrt(2/exp(1));am=-ap;flag=1;
while flag
    U=rand;V=am+(ap-am)*rand;X=V/U;
    if X^2<6-8*U+2*U^2
        break
    end
    if X^2>2/U-2*U
        continue
    end
    if X^2<-4*log(U)
        break
    end
end
