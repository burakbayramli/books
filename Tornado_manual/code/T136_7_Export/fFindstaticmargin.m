function [output]=fFindstaticmargin(geo,state)

%This is only a beta testfunction so far
%It computes the aerodynamic center output.ac
%and the stability margin, output.h


cConvergence_criteria=0.00001;
solvertype=1;

results.matrix=ones(9,6,1);        
%% Computing baseline results
[lattice,ref]=fLattice_setup2(geo,state,solvertype);  
[results]=solver9(results,state,geo,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,geo);

CLa0=results.CL_a;
Cma0=results.Cm_a;

var0=CLa0/Cma0;

%% Find Aerodynamic Center
converged=0;
step=0.5;
i=0;
while converged==0
    i=i+1;
    geo.ref_point(1)=geo.ref_point(1)+step;    
    [lattice,ref]=fLattice_setup2(geo,state,solvertype);  
    [results]=solver9(results,state,geo,lattice,ref);
    [results]=coeff_create3(results,lattice,state,ref,geo);

    CLa1=results.CL_a;
    Cma1=results.Cm_a;
    
    var1=Cma1/CLa1;
   
    DvarDstep=(var1-var0)/step;
    
    step=-var1/(DvarDstep);
 
    if abs(var1)<cConvergence_criteria
        disp('converged')
        output.ac=geo.ref_point;
        output.h=-(output.ac-geo.CG)./ref.C_mac;
        return
    end
    
    if i==10
        disp('Max iterations in fFindstaticmargin')
        output=[];
        return
    end
    var0=var1;
end
    
    
    
    