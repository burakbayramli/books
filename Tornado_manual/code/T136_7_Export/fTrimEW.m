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
max_iterations=100;
twistdelta=0.02;
converged=0;
toggle=2;
rudderangle=0;
state.theta=0; %in rad.
state.phi=0; %in rad.

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
CDvector=[cos(state.alpha)*cos(state.betha) sin(state.betha) sin(state.alpha)];
CD0=sum(sum(results.CD0))*CDvector;
CDX=results.CX+CD0(1);
CDZ=results.CZ+CD0(3);

for i=1:1:engine.number
    engine.moments(i,:)=cross((engine.cgpos(i,:)-geo.ref_point),engine.thrust(i)*engine.thrustv(i,:));
end

weight.unitvector=[cos(state.phi)*sin(state.theta) sin(state.phi)*cos(state.alpha) -cos(state.phi)*cos(state.theta)];
weight.moments=cross((geo.CG-geo.ref_point),weight.gravity*weight.unitvector);

TXtot=sum(engine.thrust.*engine.thrustv(:,1));
TZtot=sum(engine.thrust.*engine.thrustv(:,3));

engine.throttle=-(qS*CDX+weight.gravity*weight.unitvector(1))/TXtot;
m0=results.MOMENTS(trimaxis)+weight.moments(trimaxis)+sum(engine.moments(:,trimaxis))*engine.throttle;
CZ0=results.CZ;
CX0=results.CX;
CL0=results.CL;

k=0;
rudderangle=0;
%% Iterating
QQ=0;
while ~converged; %Looping until converged condition
    k=k+1;
    rudderangle=rudderangle+twistdelta;
    dCL=results.CL-CL0;
    
    %[CDtot]=dragcorrection(geo,ref,body,results,state,dCL,qS);
    CDtot=results.CD0;
    
    CDvector=[cos(state.alpha)*cos(state.betha) sin(state.betha)*cos(state.phi) sin(state.alpha)*-sin(state.phi)];
    CD0=CDtot*CDvector;
    CDX=results.CX+CD0(1);
    CDZ=results.CZ+CD0(3);
    
    if trimwing
        rudderangle=twistdelta;
        Raxle=[0 cos(geo.dihed(trimwing,1)) sin(geo.dihed(trimwing,1))];
        %Setting the rotation axle to the dihedral of the trimming wings first partition.
        hinge_pos=[geo.startx(trimwing)+0.25*geo.c(trimwing) geo.starty(trimwing) geo.startz(trimwing) ];
        %Rotating wing about c/4 root chord point.
        lattice=wingrotation2(trimwing,geo,lattice,Raxle,hinge_pos,rudderangle);
    end
    if trimrudder
        %change ruddersetting          
        [n,m]=find(geo.flapped');
        geo.flap_vector(m(trimrudder),n(trimrudder))=rudderangle;
        [lattice,ref]=fLattice_setup2(geo,state,solvertype);
    end
    
    weight.unitvector=[cos(state.phi)*sin(state.theta) sin(state.phi)*cos(state.alpha) -cos(state.phi)*cos(state.theta)];
    weight.moments=cross((geo.CG-geo.ref_point),weight.gravity*weight.unitvector);
    
    [results]=solver9(results,state,geo,lattice,ref);
    [results]=coeff_create3(results,lattice,state,ref,geo);

    m1=(results.MOMENTS(trimaxis)+weight.moments(trimaxis)+sum(engine.moments(:,trimaxis))*engine.throttle)/(qS*ref.C_mac);
    dm_dTW=(m1-m0)/twistdelta;
    
    if abs(m1)<0.001
        net_surgeforce=(engine.throttle*TXtot+weight.gravity*weight.unitvector(1))/qS + CDX;
        if abs(net_surgeforce)<0.001
            net_heaveforce=(engine.throttle*TZtot+weight.gravity*weight.unitvector(3))/qS+CDZ;
        	if abs(net_heaveforce)<0.001
            	converged=1;
                disp(' ')
            	tdisp('C O N V E R G E D ! ! !')
            	return
            else
            	[state,results,lattice,ref]=iterateAlpha(engine,state,weight,qS,results,geo,lattice,ref,solvertype,TZtot,CD0,toggle,trimwing,Raxle,hinge_pos,rudderangle);
                % iterates alpha to balance the heave forces due to the lift, aircraft weight and engine thrust.
                engine.throttle=-(qS*CDX+weight.gravity*weight.unitvector(1))/TXtot;
                % balances surge forces for next trim iteration.
                toggle=toggle-1;
                QQ=QQ+1;
                plot(QQ,engine.throttle,'o')
                hold on
                plot(QQ,engine.throttle,'o')
            end
        else
            engine.throttle=-(qS*CDX+weight.gravity*weight.unitvector(1))/TXtot;
            % balances surge forces for next trim iteration.
        end
    end
            
	if k>max_iterations
        tdisp('NOT CONVERGED!!!')
        results=[];
        return
    end
    
	m0=m1;
  	twistdelta=-0.5*m0/dm_dTW;

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
   

