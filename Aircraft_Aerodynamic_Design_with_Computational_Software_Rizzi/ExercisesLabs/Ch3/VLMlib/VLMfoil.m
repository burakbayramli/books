function  [coeff2,results] = VLMfoil(geo,state,torfoil,lattictype,torfoilparam);
% VLM
trfitmax = 1;
if torfoil
  polarrelist  = torfoilparam.polarrelist;
  polarre0list = torfoilparam.polarre0list;
  clmax        = torfoilparam.clmax;
  alfmax       = torfoilparam.alfmax;
  clialpha     = torfoilparam.clialpha;
  tol          = torfoilparam.tol;
  debplot      = 0;
  trfitmax     = torfoilparam.trfitmax;
  liftslopecorr = torfoilparam.liftslopecorr;
  nwing        = geo.nwing;
  TW00         = geo.TW;
  nsecmax      = max((1+geo.symetric).*geo.nelem+1);
  nw           = sum((1+geo.symetric).*geo.nelem+1);
  dangold      = zeros(nwing,nsecmax);
  dang         = zeros(nwing,nsecmax);
end

% start iteration
for trfit = 1:trfitmax
  [lattice,ref]=fLattice_setup2(geo,state,lattictype); 
  Sref    = ref.S_ref;
  results = solver9([],state,geo,lattice,ref);
  results = coeff_create3(results,lattice,state,ref,geo);
  if ~torfoil
    coeff2 = [results.CL,results.CD,results.CY,results.Cl,results.Cm,results.Cn];
    return
  end
  yst   = results.ystation;
  locch = results.localchord;
  paspa = results.panelspan;
  cl    = results.CL_local;
  cd    = results.CD_local;
  cy    = results.CY_local;
  fprintf('%s',['trfi ' num2str(trfit)])
  for wingno=1:nwing
    fprintf('%s',[' wingno ' num2str(wingno)])
    nele = geo.nelem(wingno);
    polarre  = polarrelist{wingno};
    polarre0 = polarre0list{wingno};
    iend     = find(locch(:,wingno)==0);
    if isempty(iend)
      iend = size(locch,1);
      ii   = 1:iend;
    else
      ii   = 1:iend-1;
    end
    ystii = yst(ii,wingno);
    clii  = cl(ii,wingno);
    cdii  = cd(ii,wingno);
    cyii  = cy(ii,wingno);
    paspaii = paspa(ii,wingno);
    locchii = locch(ii,wingno);
    nii1  = size(ystii,1);
% find segment break distance from root projected on xzplane
    sppy   = [geo.starty(wingno),cumsum(geo.b(wingno,1:nele).*cos(geo.dihed(wingno,1:nele)))]; 
    sppz   = [geo.startz(wingno),cumsum(geo.b(wingno,1:nele).*sin(geo.dihed(wingno,1:nele)))]; 
    spp    = sqrt(sppy.^2+sppz.^2);
    [tw,dh] = seg2sec(squeeze(geo.TW(wingno,1:nele,:)),geo.dihed(wingno,1:nele));    
    if geo.symetric(wingno)
      %spp = [-fliplr(spp),spp(2:end)];
      %tw  = [flipud(tw);tw(2:end)];
      %dh  = [-flipud(dh);dh(2:end)];
      % instead, take only half of tyhe symmetric
      ystii = ystii(nii1/2+1:end);
      clii  = clii(nii1/2+1:end);
      cdii  = cdii(nii1/2+1:end);
      cyii  = cyii(nii1/2+1:end);
      paspaii = paspaii(nii1/2+1:end);
      locchii = locchii(nii1/2+1:end);
    end
    nii = length(ystii);
    nbrk = size(tw,1);
% interpolate dh, tw 
    tws  = interp1(spp,tw,ystii,'extrap');
    dhs  = interp1(spp,dh,ystii,'extrap');
    cdh  = cos(dhs); sdh = sin(dhs);
% rotate coefficients from wind to local wind by dihedral
    clloc  = sdh.*cyii + cdh.*clii;
    
% find cd from drag polar
    cdii  = interp1(polarre(:,2),polarre(:,3),clloc,'extrap');
    cdpii = interp1(polarre(:,2),polarre(:,4),clloc,'extrap');
% record drag results
    CL1(wingno) = results.CLwing(wingno);
    CDi(wingno) = results.CDwing(wingno);
    CD1(wingno) = sum(cdii.* paspaii.*locchii)/Sref;
    CDp1(wingno)= sum(cdpii.*paspaii.*locchii)/Sref;
    CDv1(wingno)= CD1(wingno)-CDp1(wingno);
% drag summed only over right half, if symmetric
    if geo.symetric(wingno)
      CD1(wingno) = 2*CD1(wingno);
      CDp1(wingno)= 2*CDp1(wingno);
      CDv1(wingno)= CD1(wingno)-CDp1(wingno);
    end
      
% find angle changes at sections
    clbrk = interp1(ystii,clloc,spp,'extrap');
    cdh1  = interp1(ystii,cdh,spp,'extrap');
    sdh1  = interp1(ystii,sdh,spp,'extrap');
% local alpha (dihed)
    ang   = atan(cdh1*tan(state.alpha) + sdh1*tan(state.betha)/cos(state.alpha)); % row      
% change twist
    ang0  = interp1(polarre0(:,2),polarre0(:,1),clbrk,'extrap');
    clv   = interp1(polarre(:,1),polarre(:,2),ang0,'extrap');
% alfaloc = state.alpha + tw + dalfa < alfmax(wingno)
    dangmax = alfmax(wingno)-tw'*180/pi-ang*180/pi;
    tmp = (clv - clbrk)/clialpha(wingno);
      if debplot && wingno==1
      plot(ystii,clii,'-k','linewidth',2) 
      plot(spp,clbrk,'x-k');
      ystii
      clii
      spp
      clbrk
      clv
      tmp
      drawnow
    end
    
    dang(wingno,1:nbrk) = min((clv - clbrk)/clialpha(wingno),dangmax);
% add alpha correction here for foil thickness
    clfact = clialpha(wingno)/(2*pi*pi/180) - 1;
    dtw    = (tw + state.alpha)*clfact;
    for iseg = 1:nele
      geo.TW(wingno,iseg,1) = TW00(wingno,iseg,1)+dang(wingno,iseg)*pi/180   + dtw(iseg);
      geo.TW(wingno,iseg,2) = TW00(wingno,iseg,2)+dang(wingno,iseg+1)*pi/180 + dtw(iseg+1);
    end
  end   % end wing loop
  fprintf('%s\n',';')
 % for k = 1:nwing
 %   norm(dang(k,:)-dangold(k,:))
 % end
  tst = norm(dang(:)-dangold(:))/sqrt(nw)
  if tst < tol || trfit == trfitmax
    break
  end
  dangold = dang;
end % end trfit loop

if norm(tst) > tol
  disp('Tfoil max it w/o conv.')
end
coeff2 =[sum(CL1),sum(CD1)+sum(CDi),sum(CDp1),sum(CDv1),results.CY,...
  results.Cl,results.Cm,results.Cn];     
end
