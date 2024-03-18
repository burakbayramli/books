function [converge]=zgridconverge(geo,state,wing,direction,criterion)

results=[];
solvertype=1;
xscalefactor=2;
iterationlimit=10;
panellimit=1000;
g0=geo;
%g0.nx=double(g0.nx>0);
%g0.ny=double(g0.ny>0); 
g1=geo;
%g1.nx=2*double(g0.nx>0);
%g1.ny=2*double(g0.ny>0); 
%g1.nx=ceil(1.3*g0.nx);
%g1.ny=ceil(1.3*g0.ny); 


%% Computing baseline results
[lattice,ref]=fLattice_setup2(g0,state,solvertype);  
[results]=solver9(results,state,g0,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,g0);

CL0=results.CL;
CD0=results.CD;
Cm0=results.Cm;

CL(1)=results.CL;
CD(1)=results.CD;
Cm(1)=results.Cm;
A(1)=0;


%% Iterating
%criterion=0.01;
converged=0;
k=0;

xscalefactor=1.5;


while converged==0   
    k=k+1;  
    
    if k>3
        xscalefactor=1.3;
    end
    if direction ==1
            g1.nx(wing,:) =ceil(g0.nx(wing,:)*xscalefactor);
    else
            g1.ny(wing,:) =ceil(g0.ny(wing,:)*xscalefactor);
    end

    
    
    [lattice,ref]=fLattice_setup2(g1,state,solvertype);  
    [results]=solver9(results,state,g1,lattice,ref);
    [results]=coeff_create3(results,lattice,state,ref,g1);
    
%     figure(4)% 	Delta cp plot
% rotate3d on 
% colormap(hot);
% fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.cp')
% title('Delta cp distribution')
% colorbar('vert')
% axis equal

     %figure(k+200)
     %plot3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','k');
    
    CL(k)=results.CL;
    CD(k)=results.CD;
    Cm(k)=results.Cm;
    cond(k)=results.dwcond;

    A(k)=sum(sum(g1.nx.*g1.ny));
    
    %convL=abs(diff(CL./CL(1)));
    %convD=abs(diff(CD./CD(1)));
    convL=abs(1-CL(2:end)./CL(1:(end-1)));
    convD=abs(1-CD(2:end)./CD(1:(end-1)));



    try
    if convL(k-1)<=criterion
        if convD(k-1)<=criterion
           display('CONVERGED! - Grid convergence OK.')
           converged=1;
           converge.nx=g1.nx;
           converge.ny=g1.ny;
           converge.CL=CL;
           converge.CD=CD;
           converge.Cm=Cm;
           converge.cond=cond;
           converge.panels=A;
           converge.converged=1;
           
        end
    end
    end
    
    if k>iterationlimit
        tdisp('NOT CONVERGED!')
        converge.converged=0;
        return
    end
    
    if A(k)>panellimit
        tdisp('To many panels for convergence study!')
        converge.converged=0;
        return
    end
    
    g0=g1;
      
    

end
