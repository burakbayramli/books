cd output
    load q1-Cx
cd ..

cd state
    load 078.mat
cd ..

cd aircraft
    load A320w2
cd structure
    load test2
cd ..
cd ..

wingno=1;

   [lattice,ref]=fLattice_setup2(geo,state,1);    %Setting up the lattice0
   
   
mesh=generate_struct_mesh( geo,lattice,structure,wingno);

FEMresult=FEMsolver(100,geo,lattice,state,ref,structure,mesh,results,wingno);

FEMresultplot2(FEMresult,mesh)












return
figure(1)

   [lattice,ref]=fLattice_setup2(geo,state,1);    %Setting up the lattice
   plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k');
   axis equal



hold on
plot3(mesh.GP_SB(:,1),mesh.GP_SB(:,2),mesh.GP_SB(:,3),'o');

if geo.symetric
    plot3(mesh.GP_P(:,1),mesh.GP_P(:,2),mesh.GP_P(:,3),'o');
end

figure(2)
plot(mesh.GP_SB(2:end,1),mesh.profile.w);hold on
plot(mesh.GP_SB(2:end,1),mesh.profile.h,'r')

xlabel('spanstation, l ,[m]');
ylabel('Spar dimension, l ,[m]');
legend('width','height')

return
    
    
    
    

fplotspar(geo,results,structure,wingno)