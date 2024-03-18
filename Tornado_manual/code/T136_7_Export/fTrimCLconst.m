function [results,rudderangle,geo,state]=trim(geo,state,trimaxis,trimwing,trimrudder,trimallmove,solvertype)
%Trimfunction for TORNADO
% This trimfunction keeps the lift coefficient constant by changing the
% state angle of attack.
%
% Trimaxis is the body axis of momentum to trim around:
%   1=l (roll);
%   2=m (pitch);
%   3=n (yaw);
%
% Trimwing is the wing number to change incidence on to acieve trim
%
% Trimrudder is the rudder (control effector) to change setting of in order
% to acieve trim.
%
%  Output:
%   rudderangle is either the needed change in incedence of a wing, Or the 
%   rudder setting needed to acieve trim.
%
%   Spånga, 2021-09-19:   Updated to MATLAB R2020 R2020  
%%%%%%%%
max_iterations=50;
results.matrix=ones(9,6,1);        
twistdelta=0.02;
converged=0;
rudderangle=0;  


%% Checking input
if trimaxis==1  
elseif trimaxis==2
elseif trimaxis==3
else
    terror(18)
    results=[];
    return
end

if trimwing*trimrudder>0
    terror(19)
    results=[];
    return
end
if trimrudder>0;
    if sum(sum(geo.flapped'))==0
         terror(2)
         results=[];
    return
    end
end
if sum(sum(geo.flapped'))<trimrudder
    terror(2)
    results=[];
    return
end

if trimwing<1
    if trimrudder<1
        if trimallmove<1
            terror(20)
            results=[];
            return
        end
    end
end




%% Computing baseline results
[lattice,ref]=fLattice_setup2(geo,state,solvertype);  
[results]=solver9(results,state,geo,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,geo);

    if trimaxis==1
         m0=results.Cl;
    elseif trimaxis==2
         m0=results.Cm;
    elseif trimaxis==3
         m0=results.Cn;
    end

    
    
disp('--------------')
disp(' ')
disp(' Which type of CL do you wish to trim in heave for?')
disp(' [1]  Current CL')
disp(' [2]  Specified CL, (entry follows)')
disp(' [3]  CL corresponding to specific mass. (entry follows) ') 
disp(' ')
disp(' [0]  Exit/upmenu')
disp(' ')

quest=input('Please enter option from above please:');
disp(' ')


if quest==1
    CL0=results.CL;
elseif quest==2
    CL0=input('Heavetrim CL:');
    TargetCL=CL0;
elseif quest==3
    m0=input('Heavetim mass [kg]:');
    q=state.rho*state.AS^2/2;
    CL0=m0*9.82/(q*ref.S_ref);
    TargetCL=CL0;
else
    return
end
    


    
    
    
        %CL0=results.CL;

k=0;
rudderangle=0;
%% Iterating

    while converged==0 %Looping until converged condition
        k=k+1;
        %rudderangle=rudderangle+twistdelta;
        
        converged2=0;
        
%%Trim for CL
            while converged2==0
                [results]=solver9(results,state,geo,lattice,ref);
                [results]=coeff_create3(results,lattice,state,ref,geo);
                
                CL1=results.CL;
                Delta_CL=CL1-CL0;
                
                    if abs(Delta_CL)<0.001
                        converged2=1;
                        %tdisp('CL CONVERGED')
                        CL0=CL1;
                    else
                        state.alpha=state.alpha-Delta_CL/results.CL_a;
                
                        
                    end   
            end
 %CL Trimmed                       
 %Trimming CM           
            
            converged3=0;
            deflection=0.01;
          
            while converged3==0
                  k=k+1;
                [lattice,ref]=fLattice_setup2(geo,state,solvertype);  
                [results]=solver9(results,state,geo,lattice,ref);
                [results]=coeff_create3(results,lattice,state,ref,geo);
                if trimaxis==1
                     m0=results.Cl;
                elseif trimaxis==2
                     m0=results.Cm;
                elseif trimaxis==3
                     m0=results.Cn;
                end

                
                if trimwing>0
                    rudderangle=deflection;
                     Raxle=[0 cos(geo.dihed(trimwing,1)) sin(geo.dihed(trimwing,1))];
                     hinge_pos=[geo.startx(trimwing)+0.25*geo.c(trimwing) geo.starty(trimwing) geo.startz(trimwing) ];
                     lattice=wingrotation2(trimwing,geo,lattice,Raxle,hinge_pos,rudderangle);

                end
                if trimrudder>0
                      [n,m]=find(geo.flapped');
                       d0=geo.flap_vector(m(trimrudder),n(trimrudder));  
                        
                      %rudderangle=rudderangle+twistdelta;
                      rudderangle=deflection;
                      %change ruddersetting          
                      geo.flap_vector(m(trimrudder),n(trimrudder))=rudderangle;
                      [lattice,ref]=fLattice_setup2(geo,state,solvertype);  
                end
                
                if trimallmove>0
                    d0=geo.allmove_def;
                    geo.allmove_def=deflection;

                    [lattice,ref]=fLattice_setup2(geo,state,solvertype); 

                end
                
                
                
                %plot(k,CL1,'*')



                [results]=solver9(results,state,geo,lattice,ref);
                [results]=coeff_create3(results,lattice,state,ref,geo);

                if trimaxis==1
                     m1=results.Cl;
                elseif trimaxis==2
                     m1=results.Cm;
                elseif trimaxis==3
                     m1=results.Cn;
                end


               Cm=m1;
               CL1=results.CL;
                if abs(m1)<0.001
                    converged3=1;
                    %tdisp('Cm CONVERGED')
                  
                    
                else
                    dm_dD=(m1-m0)/deflection;   
                    
                    deflection=d0-m0/dm_dD;
                    m0=m1;
                end
                
                

       
%         plot(k,m1,'o')
%         hold on
%         plot(k,CL1,'*')
%         hold on
                if k>max_iterations
                    tdisp('NOT CONVERGED!!!')
                    results=[];
                    return
                end
            end
            
            if abs(results.CL-TargetCL)<0.001
                   tdisp('CL CONVERGED')
               if abs(m1)<0.001
                   tdisp('Cm CONVERGED')
                   return
               end
            else converged2=0;   
            end


        
        
        
        %plot(k,CL1,'*')

        
            

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
   

