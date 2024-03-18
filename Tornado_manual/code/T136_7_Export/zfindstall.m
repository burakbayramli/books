function [results,state]=zfindstall(geo,state,ref,results,stallCL)


solvertype=1;
dalpha=0.01;

[lattice,ref]=fLattice_setup2(geo,state,solvertype);  
[results]=solver9(results,state,geo,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,geo);

[I(1) J(1)]=max(results.CL_local(:,1));
state.alpha=state.alpha+dalpha;
converged=0;

k=1;
while converged==0
    k=k+1;
    [lattice,ref]=fLattice_setup2(geo,state,solvertype);  
    [results]=solver9(results,state,geo,lattice,ref);
    [results]=coeff_create3(results,lattice,state,ref,geo);
    [I(k) J(k)]=max(results.CL_local(:,1));  

      %plot(results.ystation(:,1),results.CL_local(:,1));
      %hold on
      
    dI=diff(I);
    a(k)=dI(k-1)/dalpha(k-1);
    
    dalpha(k)=(stallCL-I(k))/a(k);
    
        
    if abs(dalpha(k))<=0.0001
        tdisp('Converged')   
        return
    end
    
    if k>9
        tdisp('Not converged.')
        return
    end
    state.alpha=state.alpha+dalpha(k);
end