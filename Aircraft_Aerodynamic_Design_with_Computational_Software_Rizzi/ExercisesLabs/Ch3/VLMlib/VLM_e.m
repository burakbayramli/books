function [coeffse,results] = VLM_e(resimn,resimx,geo,state,torfoil,lattictype,torfoilparam)
nwing  = geo.nwing;
coeffs = cell(resimx-resimn+1,1);
nx0 = geo.nx;
ny0 = geo.ny;
for resi = resimn:resimx
  nnn = 2^(resi-1)
  for kk = 1:nwing
    for jk = 1:geo.nelem(kk)
      geo.nx(kk,jk) = nnn*nx0(kk,jk);
      geo.ny(kk,jk) = nnn*ny0(kk,jk);
%      if jk >= 12 || jk == 8 % very thin segments!
%        geo.ny(kk,jk) = 1;
      end
  end
 % disp(['resi: ' num2str(resi)])
  [coeff2,results] = VLMfoil(geo,state,torfoil,lattictype,torfoilparam);
  coeffs{resi-resimn+1} = coeff2 ;          
 % fprintf('%f ',coeff2);
 % fprintf('\n');          
end  % resi loop
coeffse = extrap(coeffs,1,2);