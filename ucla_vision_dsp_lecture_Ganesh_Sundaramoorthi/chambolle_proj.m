function p = chambolle_proj(p,I,dt,iters,lambda)
  
  for i=1:iters,
    p1px=TranslateImage(p(:,:,1), -1,  0);
    p1mx=TranslateImage(p(:,:,1),  1,  0);
    p2py=TranslateImage(p(:,:,2),  0, -1);
    p2my=TranslateImage(p(:,:,2),  0,  1);
    
    divp=(p1px-p1mx+p2py-p2my)/2;
    
    pmI=divp-I/lambda;
    pmIpx=TranslateImage(pmI, -1,  0);
    pmImx=TranslateImage(pmI,  1,  0);
    pmIpy=TranslateImage(pmI,  0, -1);
    pmImy=TranslateImage(pmI,  0,  1);
      
    gradpmI(:,:,1) = (pmIpx-pmImx)/2;
    gradpmI(:,:,2) = (pmIpy-pmImy)/2;

    normgradpmI= sqrt( gradpmI(:,:,1).*gradpmI(:,:,1) + ...
		       gradpmI(:,:,2).*gradpmI(:,:,2) );
    p=p+dt*gradpmI;
    p(:,:,1)=p(:,:,1)./(1 + dt*normgradpmI);
    p(:,:,2)=p(:,:,2)./(1 + dt*normgradpmI);
  end