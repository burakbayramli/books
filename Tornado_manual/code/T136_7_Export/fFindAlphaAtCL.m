function [results,alpha]=fFindAlphaAtCL(geo,state,solvertype,CL_target)
%Add on function for Tornado.
%This function computes at which angle of attack a the [CL_target] is obtained
%Input is standard Tornado geometry and state data, which solver to use and
%what Target CL to go for.
%
%This is lolcode: Can I has cookie now pleez?

results.matrix=ones(9,6,1);        
 
%% Computing baseline results
[lattice,ref]=fLattice_setup2(geo,state,solvertype);  
[results]=solver9(results,state,geo,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,geo);

converged=0;
k=0;
%% Iterating
while converged==0; %Looping until converged condition
    k=k+1;
    
    Delta_CL=CL_target-results.CL;
    Delta_alpha=Delta_CL/results.CL_a; 
    state.alpha=state.alpha+Delta_alpha;
    
    if abs(Delta_alpha)<0.0001
       converged=1;
       %tdisp('C O N V E R G E D ! ! !')
       %tdisp(' ')
       %tdisp(strcat('Required CL at: ', num2str( state.alpha ) , ' radians body alpha'))
       alpha=state.alpha;
       return
    end
    
    if k>9
       tdisp('NOT CONVERGED!!!')
       results=[];
       return
    end
        
    [lattice,ref]=fLattice_setup2(geo,state,solvertype);  
    [results]=solver9(results,state,geo,lattice,ref);
    [results]=coeff_create3(results,lattice,state,ref,geo);          
end   
   results.matrix(:,:)=[results.CL results.CL_a results.CL_b results.CL_P results.CL_Q results.CL_R
           results.CD results.CD_a results.CD_b results.CD_P results.CD_Q results.CD_R
           results.CC results.CC_a results.CC_b results.CC_P results.CC_Q results.CC_R
           results.Cl results.Cl_a results.Cl_b results.Cl_P results.Cl_Q results.Cl_R
           results.Cm results.Cm_a results.Cm_b results.Cm_P results.Cm_Q results.Cm_R
           results.Cn results.Cn_a results.Cn_b results.Cn_P results.Cn_Q results.Cn_R
           results.CX results.CX_a results.CX_b results.CX_P results.CX_Q results.CX_R
           results.CY results.CY_a results.CY_b results.CY_P results.CY_Q results.CY_R
           results.CZ results.CZ_a results.CZ_b results.CZ_P results.CZ_Q results.CZ_R]; 
   end     
   

