% extend so no symm
geo2 = geo;
geo2.fnx = [geo2.fnx;geo2.fnx]; 
geo2.ny  = [geo2.ny;geo2.ny];
geo2.nx  = [geo2.nx;geo2.nx];
geo2.fsym    = zeros(2,5);
geo2.fc      = ones(2,1)*geo2.fc;
geo2.flapped = ones(2,1)*geo2.flapped;
geo2.TW(2,:,:) = geo2.TW(1,:,:);
geo2.foil(2,:,:) = geo2.foil(1,:,:);
geo2.T(2,:) = geo2.T;
geo2.SW(2,:) = geo2.SW;
geo2.c(2) = geo2.c;
geo2.dihed(2,:) = pi-geo2.dihed;
geo2.b(2,:) = geo2.b;
geo2.symetric = zeros(1,2);
geo2.startx(2) = geo2.startx;
geo2.starty(2) = geo2.starty;
geo2.startz(2) = geo2.startz;
geo2.nwing = 2
geo2.nelem(2) =geo2.nelem;
geo2.flap_vector(2,:) =[1 -1 -1 1 1].*geo2.flap_vector;
fn = fieldnames(structure)
structure2 = structure;
ns = length(fn)
for k = 1:ns
    structure2.(fn{k})=ones(2,1)*structure2.(fn{k});
end
[lattice2,ref]=fLattice_setup2(geo2,state,0);
subplot(212)
g=fill3(lattice2.XYZ(:,:,1)',lattice2.XYZ(:,:,2)',lattice2.XYZ(:,:,3)','w');
set(g,'LineWidth',2);
pause
%geometryplot(lattice,geo,ref);
results = [];
[results]=solver9(results,state,geo2,lattice2,ref);
[results]=coeff_create3(results,lattice2,state,ref,geo2);  
for wingno = 1:geo2.nwing
mesh = generate_struct_mesh(geo2,lattice2,structure2,wingno);
FEMresult=FEMsolver(structure2.nx,geo2,lattice2,state,ref,structure2,mesh,results,wingno);
FEMresultplot2(FEMresult,mesh);
[latticenew,meshnew] = ftlatticedeform...
    (geo2,lattice2,mesh,FEMresult,wingno);
pause
end