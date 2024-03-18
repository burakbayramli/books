function [results2,rudderangle,Treq,iter]=trim(geo,state,mg)
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
%%%%%%%%
max_iterations=20;
results.matrix=ones(9,6,1);        
converged=0;
rudderangle=0; 
solvertype=1;
results2=[];


[rho a p mu]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
Mach=state.AS/a;    

%% Computing baseline results


geo.ref_point=geo.CG; %Computing moments around CG

[lattice,ref]=fLattice_setup2(geo,state,solvertype); 
qS=0.5*state.rho*state.AS^2*ref.S_ref;
c=ref.C_mac;

[CD0_wing results.Re results.Swet results.Vol]=zeroliftdragpred(Mach,state.ALT,geo,ref);
 
%CD0_blunt=zldpblunt(Mach,state.ALT,body,ref); %Blunt bodies: Fuse, nacelle...etc..
% CD0f=sum(sum(CD0_wing))+sum(CD0_blunt);
CD0f=0.0050;


[results]=solver9(results,state,geo,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,geo);


trimwing=2; %MUST TRIM WITH SECOND WING.
rudderangle=0.1*pi/180;
Raxle=[0 cos(geo.dihed(trimwing,1)) sin(geo.dihed(trimwing,1))];
hinge_pos=[geo.startx(trimwing)+0.25*geo.c(trimwing) geo.starty(trimwing) geo.startz(trimwing) ];

I=0;
while converged==0
I=I+1;
        lattice=wingrotation2(trimwing,geo,lattice,Raxle,hinge_pos,rudderangle);

%% Second run    
    [results2]=solver9(results2,state,geo,lattice,ref);
    [results2]=coeff_create3(results2,lattice,state,ref,geo);    

%% Differentiating
    a=state.alpha;
    dD=qS*(results2.CD-results.CD);
    dL=qS*(results2.CL-results.CL);
    dm=c*qS*(results2.Cm-results.Cm);
      
    Ddt=(dD)/rudderangle;
    Ldt=(dL)/rudderangle;
    mdt=(dm)/rudderangle;

    if abs(dD/qS)<0.001;
        if abs(dL/qS)<0.001;
            if abs(dm/(qS*c))<0.001
                Treq=R(2);
                iter=I;
                return
            end
        end
    end
    
    
    
    D0=qS*(results.CD+CD0f);
    L0=qS*(results.CL);
    m0=c*qS*(results.Cm);

    Da=qS*(results.CD_a);
    La=qS*(results.CL_a);
    ma=c*qS*(results.Cm_a);
    

    V0=[D0 L0-mg m0]';
    V0(2)

    A=[Da      -1                          Ddt
       La      (g+a)                       Ldt
       ma      -cos(g)*dz-sin(g)*dx        mdt];

    R=A\(-V0);

    state.alpha=state.alpha+R(1)*0.4;
    rudderangle=R(3)*0.4;    
    results=results2;
    
    if I==max_iterations
        Treq=0;
        disp('NOT CONVERGED')
        return
        
    end
    


end

toc
return
end
   












   

