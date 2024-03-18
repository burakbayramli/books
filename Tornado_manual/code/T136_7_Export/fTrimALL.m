function [results,rudderangle]=trim(geo,state,mg,engine)
%Trimfunction for TORNADO
% This trimfunction keeps the lift coefficient constant bay changing the
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
max_iterations=10;
results.matrix=ones(9,6,1);        
twistdelta=0.02;
converged=0;
rudderangle=0; 
solvertype=1;
results2=[];
R(2)=0;


dx=engine.cgpos(1)-geo.CG(1);
dz=engine.cgpos(3)-geo.CG(3);
g=-atan(engine.thrustv(3)/engine.thrustv(1))

[rho a p mu]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
Mach=state.AS/a;    

%% Computing baseline results





geo.ref_point=geo.CG; %Computing moments around CG

[lattice,ref]=fLattice_setup2(geo,state,solvertype); 
qS=state.AS^2*state.rho/2*ref.S_ref;

[CD0_wing results.Re results.Swet results.Vol]=zeroliftdragpred(Mach,state.ALT,geo,ref);
 CD0_blunt=0;% zldpblunt(Mach,state.ALT,body,ref); %Blunt bodies: Fuse, nacelle...etc..
 CD0=sum(sum(CD0_wing))+sum(CD0_blunt);
 results.CD0=CD0;

CXD=CD0*cos(state.alpha);
CZD=CD0*sin(state.alpha);


[results]=solver9(results,state,geo,lattice,ref);
[results]=coeff_create3(results,lattice,state,ref,geo);


trimwing=2; %MUST TRIM WITH SECOND WING.
rudderangle=1*pi/180;
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
    
    dX=results2.CX-results.CX;
    dZ=results2.CZ-results.CZ;
    dm=results2.Cm-results.Cm;
    
    figure(1)
    plot(I,dX,'x')
    hold on
    plot(I,dZ,'^')
    plot(I,dm,'o')
    drawnow
    
    figure(2);hold on
    plot(I,state.alpha*180/pi,'s')
    plot(I,rudderangle*180/pi,'v')
    
    
    figure(3)
    hold on
    plot(I,R(2),'*')

    CXdt=(dX)/rudderangle;
    CZdt=(dZ)/rudderangle;
    Cmdt=(dm)/rudderangle;

    if abs(dX)<0.01;
        if abs(dZ)<0.01;
            if abs(dm)<0.01
                disp('Walrus!')
                return
            end
        end
    end
    
    CXD=CD0*cos(state.alpha);
    CZD=CD0*sin(state.alpha);
    
    CX0=results.CX+CXD;
    CZ0=results.CZ+CZD;
    Cm0=results.Cm;

    CXa=results.CX_a;
    CZa=results.CZ_a;
    Cma=results.Cm_a;
    c=ref.C_mac;

    V0=qS*[CX0 CZ0-mg/qS c*Cm0]';

    A=[qS*CXa+mg      -cos(g)                 qS*CXdt
       qS*CZa          sin(g)                 qS*CZdt
       c*qS*Cma       -cos(g)*dz-sin(g)*dx  c*qS*Cmdt]

    R=A\V0;

    state.alpha=state.alpha+R(1);
    rudderangle=R(3);    
    results=results2;
  
    

end





    return
    
    
    

        
        
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
   












   

