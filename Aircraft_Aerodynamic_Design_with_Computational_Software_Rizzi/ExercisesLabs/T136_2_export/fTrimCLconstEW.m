function [results,rudderangle,state,engine,converged]=trim(geo,state,trimaxis,trimwing,trimrudder,solvertype,engine,weight,results)
%Trimfunction for TORNADO
% This trimfunction keeps the lift coefficient constant by changing the
% state angle of attack.
%
% Trimaxis is the body axis of momentum to trim around:
%   1=l (roll);
%   2=m (pitch);
%   3=n (yaw);
%
% Trimwing is the wing number to change incidence on to achieve trim
%
% Trimrudder is the rudder (control effector) to change setting of in order
% to achieve trim.
%
%  Output:
%   rudderangle is either the needed change in incidence of a wing, Or the 
%   rudder setting needed to achieve trim.
%%%%%%%%
max_iterations=30;
twistdelta=0.02;
converged=0;
rudderangle=0;
state.theta=0;

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
            terror(20)
            results=[];
            return
    end
end

%% Computing baseline results
[lattice,ref]=fLattice_setup2(geo,state,solvertype);  
[results]=solver9(results,state,geo,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,geo);

qS=0.5*state.rho*state.AS^2*ref.S_ref;

thrustlapse=1;%fThrustLapse(state);

engine.TXtot=sum(thrustlapse*engine.thrust.*engine.unitvector(:,1));
engine.TZtot=sum(thrustlapse*engine.thrust.*engine.unitvector(:,3));
engine.throttle=-qS*(-results.CX+results.CD0)/engine.TXtot;

if trimaxis==1
	m0=results.Cl+sum(engine.moments(:,1))*engine.throttle;
elseif trimaxis==2
	mW=weight*cos(state.theta)*(geo.CG(1)-geo.ref_point(1))/(qS*ref.C_mac);
    m0=results.Cm+mW+sum(engine.moments(:,2))*engine.throttle/(qS*ref.C_mac);
elseif trimaxis==3
	m0=results.Cn+sum(engine.moments(:,3))*engine.throttle;
end

k=0;
rudderangle=0;
%% Iterating

while ~converged; %Looping until converged condition
    k=k+1;
    rudderangle=rudderangle+twistdelta;
    
    if trimwing
        %change twist
        geo.TW(trimwing,:,:)=geo.TW(trimwing,:,:)+twistdelta;
    end
    if trimrudder
        %change ruddersetting          
        [n,m]=find(geo.flapped');
        geo.flap_vector(m(trimrudder),n(trimrudder))=rudderangle; 
    end
        
    [lattice,ref]=fLattice_setup2(geo,state,solvertype);  
    [results]=solver9(results,state,geo,lattice,ref);
    [results]=coeff_create3(results,lattice,state,ref,geo);
        
    if trimaxis==1
        m1=results.Cl+sum(engine.moments(:,1))*engine.throttle;
    elseif trimaxis==2
        m1=results.Cm+mW+sum(engine.moments(:,2))*engine.throttle/(qS*ref.C_mac);
    elseif trimaxis==3
        m1=results.Cn+sum(engine.moments(:,3))*engine.throttle;
    end

	if abs(m1)<0.001
        if abs(engine.throttle*engine.TZtot/qS-weight*cos(state.theta)/qS+results.CZ)<0.001
            if abs(engine.throttle*engine.TXtot/qS-results.CX+results.CD0)<0.001
            	converged=1;
            	tdisp('C O N V E R G E D ! ! !')
            	return
            else
                engine.throttle=-qS*(-results.CX+results.CD0)/engine.TXtot;
            end
        else
            [state,results,lattice,ref]=iterateAlpha(engine,state,weight,qS,results,geo,lattice,ref,solvertype,max_iterations);
            engine.throttle=-qS*(-results.CX+results.CD0)/engine.TXtot;
        end
    end
            
	if k>max_iterations
        tdisp('NOT CONVERGED!!!')
        results=[];
        return
    end
	dm_dTW=(m1-m0)/twistdelta;
	m0=m1;       
	twistdelta=-m0/dm_dTW;
        
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
   

