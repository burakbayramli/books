function []=solverloop5(results,quest,JID,lattice,state,geo,ref);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1999, 2007 Tomas Melin
%
% This file is part of Tornado
%
% Tornado is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public
% License as published by the Free Software Foundation;
% either version 2, or (at your option) any later version.
%
% Tornado is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied
% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE.  See the GNU General Public License for more
% details.
%
% You should have received a copy of the GNU General Public
% License along with Tornado; see the file GNU GENERAL 
% PUBLIC LICENSE.TXT.  If not, write to the Free Software 
% Foundation, 59 Temple Place -Suite 330, Boston, MA
% 02111-1307, USA.
%
% usage: []=solverloop(results,quest,JID,lattice,state,geo,ref);
%
% This is the main computational function of Tornado. It computes allf of
% the results in the RESULTS struct. QUEST is a switch for which type of
% computation is to be performed. JID is the Job IDentifier, which is used
% to name the resultfiles. LATTICE, STATE, GEO and REF are all input
% structures defined in inpt and statesetup.
%
% Example:
%
%   []=solverloop(results,quest,JID,lattice,state,geo,ref);
%
% Calls:
%           solver8
%           coeff_create
%           spanload6
%           terror
%           solver_svl
%           solver_sym
%           fLattice_setup
%           
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado core function
%
% Revision History:
%   Bristol,  2007 06 27:  Addition of new header. TM.
%   Bristol,  2007 06 25:  Subsidary functions moved inline, TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
settings=config('startup');
resetstate=state;


%%%%%%%%%%%%%%%%%%
%determine flap settings on flapped partitions

rudder_index=find(geo.flapped);
flap_setting=geo.flap_vector(rudder_index);


tdisp(' ')
tdisp('Solution started, please wait. ') 
tdisp(' ')
switch(quest)
   
case 1 %Simple Solver 
  [results]=solver9(results,state,geo,lattice,ref);
  [results]=coeff_create3(results,lattice,state,ref,geo);   

  fname=strcat(JID,'-Cx');
  cd(settings.odir)
  		save(fname,'results','geo','lattice','state','ref')
  cd(settings.hdir)
  
  tdisp(' ')
  tdisp(strcat(' Solution available in output/',fname))
  tdisp(' ')
    
  
case 2
   lock(4)=0;
   
   a1=input('	From alfa [deg]: ')*pi/180;
   b1=input('	Increment [deg]: ')*pi/180;
   c1=input('	To alpha [deg]: ')*pi/180;
   j=0;

   results.matrix=ones(9,6,1);
   for alpha=a1:b1:c1
        
        state.alpha=alpha;
        j=j+1;
        
        %The lattice type was hardcoded to type 0 in version 135...
        
        [lattice,ref]=fLattice_setup2(geo,state,1);  
        [results]=solver9(results,state,geo,lattice,ref);
        [results]=coeff_create3(results,lattice,state,ref,geo);

 
      	results.alpha_sweep(j)=state.alpha;	
        
        
results.matrix(:,:,j)=[results.CL results.CL_a results.CL_b results.CL_P results.CL_Q results.CL_R
           results.CD results.CD_a results.CD_b results.CD_P results.CD_Q results.CD_R
           results.CC results.CC_a results.CC_b results.CC_P results.CC_Q results.CC_R
           results.Cl results.Cl_a results.Cl_b results.Cl_P results.Cl_Q results.Cl_R
           results.Cm results.Cm_a results.Cm_b results.Cm_P results.Cm_Q results.Cm_R
           results.Cn results.Cn_a results.Cn_b results.Cn_P results.Cn_Q results.Cn_R
           results.CX results.CX_a results.CX_b results.CX_P results.CX_Q results.CX_R
           results.CY results.CY_a results.CY_b results.CY_P results.CY_Q results.CY_R
           results.CZ results.CZ_a results.CZ_b results.CZ_P results.CZ_Q results.CZ_R]; 
        
        

end
   state=resetstate;
   fname=strcat(JID,'-Cx_alpha');
   cd(settings.odir)
   	save(fname,'results','geo','state','lattice','ref')
   cd(settings.hdir)
   
   disp(' ')
   disp(strcat(' Solution available in output/',fname))
   disp(' ')
   
   
   
   
   

case 3
   lock(5)=0;
   a=input('	From beta [deg]: ')*pi/180;
   b=input('	Increment [deg]: ')*pi/180;
   c=input('	To beta [deg]: ')*pi/180;
   i=0;
   results.matrix=ones(9,6,1);
   for angle=a:b:c
        i=i+1;
        state.betha=angle;
        
        [lattice,ref]=fLattice_setup2(geo,state,0); 
        [results]=solver9(results,state,geo,lattice,ref);
        [results]=coeff_create3(results,lattice,state,ref,geo);

      	results.betha_sweep(i)=angle;
      
        results.matrix(:,:,i)=[results.CL results.CL_a results.CL_b results.CL_P results.CL_Q results.CL_R
           results.CD results.CD_a results.CD_b results.CD_P results.CD_Q results.CD_R
           results.CC results.CC_a results.CC_b results.CC_P results.CC_Q results.CC_R
           results.Cl results.Cl_a results.Cl_b results.Cl_P results.Cl_Q results.Cl_R
           results.Cm results.Cm_a results.Cm_b results.Cm_P results.Cm_Q results.Cm_R
           results.Cn results.Cn_a results.Cn_b results.Cn_P results.Cn_Q results.Cn_R
           results.CX results.CX_a results.CX_b results.CX_P results.CX_Q results.CX_R
           results.CY results.CY_a results.CY_b results.CY_P results.CY_Q results.CY_R
           results.CZ results.CZ_a results.CZ_b results.CZ_P results.CZ_Q results.CZ_R];

	end
       state=resetstate;

	fname=strcat(JID,'-Cx_beta');

   cd(settings.odir)
      save(fname,'results','geo','state','lattice','ref')
   cd(settings.hdir)
      
   disp(' ')
   disp(strcat(' Solution available in output/',fname))
   disp(' ')
   
   
   
   
case 5
   lock(7)=0;
   a=input('	From P [deg/s]: ')*pi/180;
   b=input('	Increment [deg/s]: ')*pi/180;
   c=input('	To P [deg/s]: ')*pi/180;
   i=0;
   results.matrix=ones(9,6,1);
   for angle=a:b:c
        i=i+1;
        state.P=angle;
        
        [lattice,ref]=fLattice_setup2(geo,state,0); 
        [results]=solver9(results,state,geo,lattice,ref);
        [results]=coeff_create3(results,lattice,state,ref,geo);

      	results.P_sweep(i)=angle;
       
                results.matrix(:,:,i)=[results.CL results.CL_a results.CL_b results.CL_P results.CL_Q results.CL_R
           results.CD results.CD_a results.CD_b results.CD_P results.CD_Q results.CD_R
           results.CC results.CC_a results.CC_b results.CC_P results.CC_Q results.CC_R
           results.Cl results.Cl_a results.Cl_b results.Cl_P results.Cl_Q results.Cl_R
           results.Cm results.Cm_a results.Cm_b results.Cm_P results.Cm_Q results.Cm_R
           results.Cn results.Cn_a results.Cn_b results.Cn_P results.Cn_Q results.Cn_R
           results.CX results.CX_a results.CX_b results.CX_P results.CX_Q results.CX_R
           results.CY results.CY_a results.CY_b results.CY_P results.CY_Q results.CY_R
           results.CZ results.CZ_a results.CZ_b results.CZ_P results.CZ_Q results.CZ_R];

         
   end
      state=resetstate;

   fname=strcat(JID,'-Cx_P');
 
   cd(settings.odir)
     save(fname,'results','geo','state','lattice','ref')
   cd(settings.hdir)
   
   disp(' ')
   disp(strcat(' Solution available in output/',fname))
   disp(' ')
   
case 6
   lock(8)=0;
   a=input('	From Q [deg/s]: ')*pi/180;
   b=input('	Increment [deg/s]: ')*pi/180;
   c=input('	To Q [deg/s]: ')*pi/180;
   i=0;
   results.matrix=ones(9,6,1);
   for angle=a:b:c
      i=i+1;
         
        state.Q=angle;
        
        [lattice,ref]=fLattice_setup2(geo,state,0); 
        [results]=solver9(results,state,geo,lattice,ref);
        [results]=coeff_create3(results,lattice,state,ref,geo);

      	results.Q_sweep(i)=angle;
      
        results.matrix(:,:,i)=[results.CL results.CL_a results.CL_b results.CL_P results.CL_Q results.CL_R
           results.CD results.CD_a results.CD_b results.CD_P results.CD_Q results.CD_R
           results.CC results.CC_a results.CC_b results.CC_P results.CC_Q results.CC_R
           results.Cl results.Cl_a results.Cl_b results.Cl_P results.Cl_Q results.Cl_R
           results.Cm results.Cm_a results.Cm_b results.Cm_P results.Cm_Q results.Cm_R
           results.Cn results.Cn_a results.Cn_b results.Cn_P results.Cn_Q results.Cn_R
           results.CX results.CX_a results.CX_b results.CX_P results.CX_Q results.CX_R
           results.CY results.CY_a results.CY_b results.CY_P results.CY_Q results.CY_R
           results.CZ results.CZ_a results.CZ_b results.CZ_P results.CZ_Q results.CZ_R];
         
    
      end
         state=resetstate;

      fname=strcat(JID,'-Cx_Q');

   cd(settings.odir)
      save(fname,'results','geo','state','lattice','ref')
   cd(settings.hdir)
      
   disp(' ')
   disp(strcat(' Solution available in output/',fname))
   disp(' ')
   
case 7
   lock(9)=0;
   a=input('	From R [deg/s]: ')*pi/180;
   b=input('	Increment [deg/s]: ')*pi/180;
   c=input('	To R [deg/s]: ')*pi/180;
   i=0;
   results.matrix=ones(9,6,1);
   for angle=a:b:c
      i=i+1;
         disp('****************')
        state.R=angle
        
        [lattice,ref]=fLattice_setup2(geo,state,0); 
        [results]=solver9(results,state,geo,lattice,ref);
        [results]=coeff_create3(results,lattice,state,ref,geo);

      	results.R_sweep(i)=angle;
        roll  =results.Cl
        rollq =results.Cl_R
        
        figure(1)
        subplot(1,2,1)
        plot(state.R,roll,'*')
        hold on
        
        subplot(1,2,2)
        plot(state.R,rollq,'o')
        hold on
        pause

      
        results.matrix(:,:,i)=[results.CL results.CL_a results.CL_b results.CL_P results.CL_Q results.CL_R
           results.CD results.CD_a results.CD_b results.CD_P results.CD_Q results.CD_R
           results.CC results.CC_a results.CC_b results.CC_P results.CC_Q results.CC_R
           results.Cl results.Cl_a results.Cl_b results.Cl_P results.Cl_Q results.Cl_R
           results.Cm results.Cm_a results.Cm_b results.Cm_P results.Cm_Q results.Cm_R
           results.Cn results.Cn_a results.Cn_b results.Cn_P results.Cn_Q results.Cn_R
           results.CX results.CX_a results.CX_b results.CX_P results.CX_Q results.CX_R
           results.CY results.CY_a results.CY_b results.CY_P results.CY_Q results.CY_R
           results.CZ results.CZ_a results.CZ_b results.CZ_P results.CZ_Q results.CZ_R];

	end
       state=resetstate;

	fname=strcat(JID,'-Cx_R');
   cd(settings.odir)
        save(fname,'results','geo','state','lattice','ref')
   cd(settings.hdir)
   
   disp(' ')
   disp(strcat(' Solution available in output/',fname))
   disp(' ')
   
case 4
   lock(6)=0;
  
   
    if sum(sum(geo.flapped))>0
              rudder=input('Sweep rudder number: [1..]: ');         
              [n,m]=find(geo.flapped');
    else
             disp(' No trailing edge control surfaces in the current geometry.')
             return
    end
    
    resetdelta=geo.flap_vector;
    a=input('	From delta [deg]: ')*pi/180;
    b=input('	Increment [deg]: ')*pi/180;
    c=input('	To  delta [deg]: ')*pi/180;
    i=0;
    def=a;
    results.matrix=ones(9,6,1);

   for angle=a:b:c
      i=i+1;

      geo.flap_vector(m(rudder),n(rudder))=angle;
        
      [lattice,ref]=fLattice_setup2(geo,state,0); 
      [results]=solver9(results,state,geo,lattice,ref);
      [results]=coeff_create3(results,lattice,state,ref,geo);

      results.delta_sweep(i)=angle;
      
        results.matrix(:,:,i)=[results.CL results.CL_a results.CL_b results.CL_P results.CL_Q results.CL_R
           results.CD results.CD_a results.CD_b results.CD_P results.CD_Q results.CD_R
           results.CC results.CC_a results.CC_b results.CC_P results.CC_Q results.CC_R
           results.Cl results.Cl_a results.Cl_b results.Cl_P results.Cl_Q results.Cl_R
           results.Cm results.Cm_a results.Cm_b results.Cm_P results.Cm_Q results.Cm_R
           results.Cn results.Cn_a results.Cn_b results.Cn_P results.Cn_Q results.Cn_R
           results.CX results.CX_a results.CX_b results.CX_P results.CX_Q results.CX_R
           results.CY results.CY_a results.CY_b results.CY_P results.CY_Q results.CY_R
           results.CZ results.CZ_a results.CZ_b results.CZ_P results.CZ_Q results.CZ_R];;
      

	end
    
    geo.flap_vector=resetdelta;

	fname=strcat(JID,'-Cx_d');

   cd(settings.odir)
        save(fname,'results','geo','state','lattice','ref')
   cd(settings.hdir)  
   
   disp(' ')
   disp(strcat(' Solution available in output/',fname))
   disp(' ')
   
   %Resetting control surfaces to zero deflection
   geo.flap_vector(n(rudder),m(rudder))=0;
   [lattice,ref]=fLattice_setup2(geo,state,0);  
   
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
case 8
   disp(' Option removed')
    
case 10
    %Estimated Zero Lift Drag, Eckerts equation.
    tdisp('Friction drag estimation.')
    [rho a p mu]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
    Mach=state.AS/a;                           %Computing local TAS 
    
    [results.CD0 results.Re results.Swet results.Vol]=zeroliftdragpred(Mach,state.ALT,geo,ref);

    fname=strcat(JID,'-Cnull');

    cd(settings.odir)
            save(fname,'results','geo','state','lattice','ref')
    cd(settings.hdir)  
    
    
    case 11  
    disp(' Option removed')
case 12
    disp(' Option removed')
  
    
case 13
disp('****************** ')
   disp(' Trim aircraft around:')
   disp('   [1]  Roll axis.')
   disp('   [2]  Pitch axis.')
   disp('   [3]  Yaw axis.')
   trimaxis=input('   Axis [1 2 3]:  ');
   disp(' ') 
   disp(' Trim aircraft by:')
   disp('   [1]  Changing wing incidence.')
   disp('   [2]  Changing control effector setting.')
   choice=input('   Please enter option frome above please: ');
   disp(' ') 
   if choice==1;
        trimwing=input('   Which number is the wing to trim with: ');
        trimrudder=0;
        
   elseif choice==2
        trimrudder=input('   Which number is the control effector to trim with: ');
        trimwing=0;
   else
       disp('Bad input.')
       return
   end
   
   disp(' ') 
   disp(' State:')
   disp('   [1]  Keep angle of attack constant.')
   disp('   [2]  Keep lift coefficient constant.')
   choice2=input('   Please enter option from above please: ');
   
   
   
   disp(' ') 
   solvertype=1;
   if choice2==1;   
        [results,rudderangle,geo,state]=fTrimAconst(geo,state,trimaxis,trimwing,trimrudder,solvertype);
        disp(' ') 
   elseif choice2==2;
        [results,rudderangle,geo,state]=fTrimCLconst(geo,state,trimaxis,trimwing,trimrudder,solvertype);
        disp(' ')
   else
       disp(' Bad input, no computation made.')
   end
        
   if choice==1;
      disp(('Aircraft will be in trim with an additional'));
      disp(strcat(('wing incidence setting of degrees: '),num2str(rudderangle*180/pi)));
      disp(strcat(('At a body reference angle of attack of degrees: '),num2str(state.alpha*180/pi)));
   end
    
   if choice==2;
      disp(strcat(('Aircraft will be in trim with a control effector setting of degrees: '),num2str(rudderangle*180/pi)));
      disp(strcat(('At an angle of attack of: '),num2str(state.alpha*180/pi))); 
   end
   disp(' ');
    
   
   
  fname=strcat(JID,'-Cx');
  cd(settings.odir)
  		save(fname,'results','geo','lattice','state','ref')
  cd(settings.hdir)
  
  tdisp(' ')
  tdisp(strcat(' Trimmed solution available in output/',fname))
  tdisp(' ')
   
    
    return

   
   case 14 
    disp(' ')
    disp('**********')
    disp(' ')
    wing=input('Check grid convergence on wing number: ');
    disp(' ')
    disp('Check convergence in direction: ')
    disp(' [1].   Chord wise.')
    disp(' [2].   Span wise.')
    disp(' ')
    direction=input('Direction [1 2]: ');
    disp(' ')
    criterion=input('Convergence criterion (recommend 0.01): ');
    
    converge=zgridconverge2(geo,state,wing,direction,criterion);
    
    if converge.converged
    %    update=input('Do you wish to update current geometry, [0 1]:');
    %    if update
            geo.nx=converge.nx
            geo.ny=converge.ny
            lattice=[];
            
            figure(200)
            subplot(3,1,1)
            title('Convergence history')
            plot(converge.panels,converge.CL,'-o')
            xlabel('Number of Panels')
            ylabel('Lift coefficient, C_L,   [-]')
            subplot(3,1,2)
            plot(converge.panels,converge.CD,'-o')
            xlabel('Number of Panels')
            ylabel('Drag coefficient, C_{Di},   [-]')
            subplot(3,1,3)
            plot(converge.panels,converge.Cm,'-o')
            xlabel('Number of Panels')
            ylabel('Pitching moment coefficient, C_m,   [-]')
            
            
            
     %   end
    end
   
    case 15
    disp(' ')
    disp('**********')
    disp(' ')
    CLmax=input('Main wing profile maximum CL: ');
    
    [results state]=zfindstall(geo,state,ref,results,CLmax);
    disp(' ')
    disp(strcat('Stall angle of attack [deg]: ',num2str(state.alpha*180/pi)));
    disp(' ')   
    
   
    fname=strcat(JID,'-Cx');
    cd(settings.odir)
       	save(fname,'results','geo','lattice','state','ref')
    cd(settings.hdir)
  
    tdisp(' ')
    tdisp(strcat(' Solution available in output/',fname))
    tdisp(' ')
    
    
    
    case 16
    
    tdisp(' ')
    tdisp('**********')
    tdisp(' ')  
    
    tdisp('CAUTION: Function not fully validated yet.')
    tdisp(' ')
    
    [rho a p mu]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
    Mach=state.AS/a;                           %Computing local TAS 
    
    A=funsteady(lattice,geo,ref,Mach);
    
    results.CZ_a_dot=A(1);
    results.Cm_a_dot=A(2);
      
    lattice.COLLOC=[lattice.COLLOC(:,1) lattice.COLLOC(:,3) lattice.COLLOC(:,2)];
    lattice.N=[lattice.N(:,1) lattice.N(:,3) lattice.N(:,2)];
    
    lattice.VORTEX2(:,:,1)=lattice.VORTEX(:,:,1);
    lattice.VORTEX2(:,:,3)=lattice.VORTEX(:,:,2);
    lattice.VORTEX2(:,:,2)=lattice.VORTEX(:,:,3);
    lattice.VORTEX=lattice.VORTEX2;
    
    lattice.XYZ2(:,:,1)=lattice.XYZ(:,:,1);
    lattice.XYZ2(:,:,3)=lattice.XYZ(:,:,2);
    lattice.XYZ2(:,:,2)=lattice.XYZ(:,:,3);
    lattice.XYZ=lattice.XYZ2;
        
    A=funsteady(lattice,geo,ref,Mach);
    
    results.CY_b_dot=A(1);
    results.Cn_b_dot=A(2);
      
    fname=strcat(JID,'-Cx_dot');
    cd(settings.odir)
       	save(fname,'results','geo','lattice','state','ref')
    cd(settings.hdir)
     tdisp(' ')
        tdisp(strcat(' Solution available in output/',fname))
        tdisp(' ')
    
    
case 17
    %%%%%%%%%
    % Unsteady, acceleration free, all coefficients.
    %%%%%%%%%
    % Part one, STATIC COEFFICIENTS
    %
    %%%%%%%%%
    
    [results]=solver9(results,state,geo,lattice,ref);
    [results]=coeff_create3(results,lattice,state,ref,geo);   
    
    %%%%%%%%%
    % Part two, UNSTEADY COEFFICIENTS
    %
    %%%%%%%%%
    
    tdisp(' ')
    tdisp('**********')
    tdisp(' ')  
    
    tdisp('All inviscous coefficients.')
    tdisp('CAUTION: Function not fully validated yet.')
    tdisp(' ')
    
    [rho a p mu]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
    Mach=state.AS/a;                           %Computing local TAS 
    
    A=funsteady(lattice,geo,ref,Mach);
    
    results.CZ_a_dot=-A(1);
    results.Cm_a_dot=-A(2);
    
    %Flipping the lattice to get the sideslipe solved.
        lattice.COLLOC=[lattice.COLLOC(:,1) lattice.COLLOC(:,3) lattice.COLLOC(:,2)];
        lattice.N=[lattice.N(:,1) lattice.N(:,3) lattice.N(:,2)];
    
        lattice.VORTEX2(:,:,1)=lattice.VORTEX(:,:,1);
        lattice.VORTEX2(:,:,3)=lattice.VORTEX(:,:,2);
        lattice.VORTEX2(:,:,2)=lattice.VORTEX(:,:,3);
        lattice.VORTEX=lattice.VORTEX2;
    
        lattice.XYZ2(:,:,1)=lattice.XYZ(:,:,1);
        lattice.XYZ2(:,:,3)=lattice.XYZ(:,:,2);
        lattice.XYZ2(:,:,2)=lattice.XYZ(:,:,3);
        lattice.XYZ=lattice.XYZ2;
    
    A=funsteady(lattice,geo,ref,Mach);
    
    results.CY_b_dot=-A(1);
    results.Cn_b_dot=-A(2);
    
    warning off
    factor1=results.CL/results.CZ;  %L scaling factor  
    if isnan(factor1)
        factor1=1;
    end
    
    factor2=results.CD/results.CZ;  %D scaling factor  
    if isnan(factor2)
        factor2=0;
    end
 
    results.CL_a_dot=results.CZ_a_dot*factor1;
    results.CD_a_dot=results.CZ_a_dot*factor2;
    results.CC_a_dot=results.CY_b_dot;
    
    %Fixing all other coefficients
    
    CZchange=(state.adot*results.CZ_a_dot+results.CZ)./results.CZ;
    if isnan(CZchange)
        CZchange=1;
    end
    CYchange=(state.bdot*results.CY_b_dot+results.CY)./results.CY;
    if isnan(CYchange)
        CYchange=1;
    end
    Cmchange=(state.adot*results.Cm_a_dot+results.Cm)./results.Cm;
    if isnan(Cmchange)
        Cmchange=1;
    end
    Cnchange=(state.bdot*results.Cn_b_dot+results.Cn)./results.Cn;
    if isnan(Cnchange)
        Cnchange=1;
    end
    warning on
    
    results.F(:,1)=results.F(:,1).*CZchange;
    results.F(:,2)=results.F(:,2).*CYchange;
    results.F(:,3)=results.F(:,3).*CZchange;   
    results.FORCE(1)=results.FORCE(1).*CZchange;
    results.FORCE(2)=results.FORCE(2).*CYchange;
    results.FORCE(3)=results.FORCE(3).*CZchange;
    
    results.M(:,1)=results.M(:,1);
    results.M(:,2)=results.M(:,2).*Cmchange;
    results.M(:,3)=results.M(:,3).*Cnchange;
    results.MOMENTS(1)=results.MOMENTS(1);
    results.MOMENTS(2)=results.MOMENTS(2).*Cmchange;
    results.MOMENTS(3)=results.MOMENTS(3).*Cnchange;

    results.CX=results.CX*CZchange;
    results.CY=results.CY*CYchange;
    results.CZ=results.CZ*CZchange;
    results.CL=results.CL*CZchange;
    results.CD=results.CD*CZchange;
    results.CC=results.CC*CYchange;
    results.Cl=results.Cl;
    results.Cm=results.Cm+state.adot*results.Cm_a_dot;
    results.Cn=results.Cn+state.bdot*results.Cn_b_dot;
    
             results.CL_a=results.CL_a*CZchange;
             results.CD_a=results.CD_a*CZchange;
             results.CC_a=results.CC_a*CYchange;
             results.CX_a=results.CX_a*CZchange;
             results.CY_a=results.CY_a*CYchange;
             results.CZ_a=results.CZ_a*CZchange;
             results.Cl_a=results.Cl_a         ;
             results.Cm_a=results.Cm_a*Cmchange;
             results.Cn_a=results.Cn_a*Cnchange;
             
             results.CL_b=results.CL_b*CZchange;
             results.CD_b=results.CD_b*CZchange;
             results.CC_b=results.CC_b*CYchange;
             results.CX_b=results.CX_b*CZchange;
             results.CY_b=results.CY_b*CYchange;
             results.CZ_b=results.CZ_b*CZchange;
             results.Cl_b=results.Cl_b         ;
             results.Cm_b=results.Cm_b*Cmchange;
             results.Cn_b=results.Cn_b*Cnchange;
             
             results.CL_P=results.CL_P*CZchange;
             results.CD_P=results.CD_P*CZchange;
             results.CC_P=results.CC_P*CYchange;
             results.CX_P=results.CX_P*CZchange;
             results.CY_P=results.CY_P*CYchange;
             results.CZ_P=results.CZ_P*CZchange;
             results.Cl_P=results.Cl_P         ;
             results.Cm_P=results.Cm_P*Cmchange;
             results.Cn_P=results.Cn_P*Cnchange;
             
             results.CL_Q=results.CL_Q*CZchange;
             results.CD_Q=results.CD_Q*CZchange;
             results.CC_Q=results.CC_Q*CYchange;
             results.CX_Q=results.CX_Q*CZchange;
             results.CY_Q=results.CY_Q*CYchange;
             results.CZ_Q=results.CZ_Q*CZchange;
             results.Cl_Q=results.Cl_Q         ;
             results.Cm_Q=results.Cm_Q*Cmchange;
             results.Cn_Q=results.Cn_Q*Cnchange;
             
             results.CL_R=results.CL_R*CZchange;
             results.CD_R=results.CD_R*CZchange;
             results.CC_R=results.CC_R*CYchange;
             results.CX_R=results.CX_R*CZchange;
             results.CY_R=results.CY_R*CYchange;
             results.CZ_R=results.CZ_R*CZchange;
             results.Cl_R=results.Cl_R         ;
             results.Cm_R=results.Cm_R*Cmchange;
             results.Cn_R=results.Cn_R*Cnchange;
             
             results.CL_d=results.CL_d*CZchange;
             results.CD_d=results.CD_d*CZchange;
             results.CC_d=results.CC_d*CYchange;
             results.CX_d=results.CX_d*CZchange;
             results.CY_d=results.CY_d*CYchange;
             results.CZ_d=results.CZ_d*CZchange;
             results.Cl_d=results.Cl_d         ;
             results.Cm_d=results.Cm_d*Cmchange;
             results.Cn_d=results.Cn_d*Cnchange;
    
             results.ForcePerMeter=results.ForcePerMeter*CZchange;
             results.CL_local=results.CL_local*CZchange;
    
    
    fname=strcat(JID,'-Cxunst');
    cd(settings.odir)
       	save(fname,'results','geo','lattice','state','ref')
    cd(settings.hdir)
        
    tdisp(' ')
        tdisp(strcat(' Solution available in output/',fname))
        tdisp(' ')
    
    
  %fname=strcat(JID,'-Cx');
  %cd(settings.odir)
 % 		save(fname,'results','geo','lattice','state','ref')
 % cd(settings.hdir)
 % 
 % tdisp(' ')
 % tdisp(strcat(' Solution available in output/',fname))
 % tdisp(' ')
    

 
    case 18
        tdisp('Strip theory drag estimation')
        tdisp('Function not fully validated yet. Proceed with caution.')
        latticetype=1; %Standard VLM to start with.
        
        test=fCheckThickness(geo);
        
        if test==0;
            terror(22)
            return
        end
        
        
        [lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice
        [results]=solver9(results,state,geo,lattice,ref);
        [results]=coeff_create3(results,lattice,state,ref,geo);   
        
        A=fViscCorr2(geo,state,lattice,results,ref);    %Strip theory viscous drag.
        results.CDv=A.totalvdragcoeff;                  %
        results.BLparam=A;                              %just send everything to file.

        fname=strcat(JID,'-viscous');
        cd(settings.odir)
             save(fname,'results','geo','lattice','state','ref')
         cd(settings.hdir)
    
        tdisp(' ')
             tdisp(strcat(' Solution available in output/',fname))
        tdisp(' ')
        
        tdisp(strcat('**********',' Alpha = ',num2str(state.alpha*57.2958),'  *****************'))
        tdisp(strcat('CL = ',num2str(results.CL),' CDv = ',num2str(results.CDv),' CDi = ',num2str(results.CD)))
        tdisp(strcat('Cl = ',num2str(results.Cl),' Cn = ',num2str(results.Cn),' Cm = ',num2str(results.Cm)))
        tdisp(strcat('Cl_p = ',num2str(results.Cl_P),' Cn_p = ',num2str(results.Cn_P)))
        tdisp(strcat('Cl_r = ',num2str(results.Cl_R),' Cn_r = ',num2str(results.Cn_R)))
        tdisp('**************************************************************************')
        
        
    
    case 19
        
            tdisp(' ')
                tdisp('Tsagi strip theory implementation')
            tdisp(' ')  
    
   
            tdisp('CAUTION: Function not implemented.')
            tdisp(' ')
    return
    %result=visc_corr(state,geo,lattice,ref);
   % 
   %     fname=strcat(JID,'-viscous');
   %     cd(settings.odir)
   %          save(fname,'results','geo','lattice','state','ref')
   %     cd(settings.hdir)
    
   %     tdisp(' ')
   %          tdisp(strcat(' Solution available in output/',fname))
   %     tdisp(' ')
    
    
    %tdisp(strcat('**********',' Alpha = ',num2str(state.alpha*57.2958),'  *****************'))
    %tdisp(strcat('CN = ',num2str(result.CN),' CA = ',num2str(result.CA),' CS = ',num2str(result.CS)))
    %tdisp(strcat('Cl = ',num2str(result.Cl),' Cn = ',num2str(result.Cn),' Cm = ',num2str(result.Cm)))
    %tdisp(strcat('Cl_p = ',num2str(result.Cl_p),' Cn_p = ',num2str(result.Cn_p)))
    %tdisp(strcat('Cl_r = ',num2str(result.Cl_r),' Cn_r = ',num2str(result.Cn_r)))
    %tdisp('*********************************************************************************')

    
    
    
    
    
    
        
    case 20
        disp(' ')
        solvertype=1;  %Hardcoded fixed wake so far
        CL_target=input('  What is the target CL:');
        [results,alpha]=fFindAlphaAtCL(geo,state,solvertype,CL_target);
        disp(' ')
        disp(strcat('Target CL of   :',num2str(CL_target)));
        disp(strcat(strcat('at an alpha of :',num2str(alpha)),' [rad]'));
        disp(strcat(strcat('or             : ',num2str(alpha*180/pi)),' [deg]'));
        
        
    case 21
        disp(' ')
        out=fFindstaticmargin(geo,state);
        disp('****************************************************')
        disp(strcat('Aerodynamic center located at :',num2str(out.ac)));
        disp(strcat('Static margin                 :',num2str(out.h(1))));
        disp('****************************************************')
        
%% =====================Trimmed Alpha Sweep Polar============================
   case 22    
    
   lock(4)=0;
   
   a1=input('	From alpha [deg]: ')*pi/180;
   b1=input('	Increment [deg]: ')*pi/180;
   c1=input('	To alpha [deg]: ')*pi/180;
   j=0;
   
   trimaxis=2; % trim arount pitch axis
   disp(' ') 
   disp(' Trim aircraft by:')
   disp('   [1]  Changing wing incidence.')
   disp('   [2]  Changing control effector setting.')
   choice=input('   Please enter option from above please: ');
   disp(' ') 
   
   if choice==1;
        trimwing=input('   Which number is the wing to trim with: ');
        trimrudder=0;
        
   elseif choice==2
        trimrudder=input('   Which number is the control effector to trim with: ');
        trimwing=0;
   else
       disp('Bad input.')
       return
   end
   
   %disp(' ') 
   %disp(' State:')
   %disp('   [1]  Keep angle of attack constant.')
   %disp('   [2]  Keep lift coefficient constant.')
   %choice2=input('   Please enter option from above please: ');
   
   choice2=1; %Keep angle of attack constant.
   
   
   % d1 = Number of Alphas
   d1=(c1-a1)/b1+1;
   results.matrix=ones(9,6,d1);  %initializing the results matrix;
   
   solvertype=1; %For now, always use fixed wake.
    
for alpha=a1:b1:c1
        
        state.alpha=alpha;
        disp(' ')
        disp('Alpha = ')
        disp(alpha*180/pi());
        j=j+1;
        
   if choice2==1;   
        [lemma_results,rudderangle]=fTrimAconst(geo,state,trimaxis,trimwing,trimrudder,solvertype);
   elseif choice2==2;
        [lemma_results,rudderangle]=fTrimCLconst(geo,state,trimaxis,trimwing,trimrudder,solvertype);
   else
       disp(' Bad input, no computation made.')
   end

   %if choice==1;
   %   disp(('Aircraft will be in trim with an additional'));
   %   disp(strcat(('wing incidence setting of degrees: '),num2str(rudderangle*180/pi)));    
   %end
    
   %if choice==2;
   %   disp(strcat(('Aircraft will be in trim with a control effector setting of degrees: '),num2str(rudderangle*pi/180)));
   %end
   
   results.alpha_sweep(j)=state.alpha;	
   results.rudderangle(j)=rudderangle;
try   
    results.matrix(:,:,j)=lemma_results.matrix;
catch
end

end
   state=resetstate;
   fname=strcat(JID,'-Cx_alpha');
   cd(settings.odir)
   	save(fname,'results','geo','lattice','state','ref')
   cd(settings.hdir)
   
   disp(' ')
   disp(strcat(' Trimmed solution available in output/',fname))
   disp(' ')   
        
case 23
    
    [rho a p mu]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
    Mach=state.AS/a;                           %Computing local TAS 
   
    results.CD0_blunt=zldpblunt(Mach,state.ALT,geo.body,ref)
    
    
   fname=strcat(JID,'-blunt');
   cd(settings.odir)
   	save(fname,'results','geo','lattice','state','ref')
   cd(settings.hdir)
   
   disp(' ')
   disp(strcat(' Blunt body solution available in output/',fname))
   disp(' ')   
    
    
case 0
    
    
    case 24
        %Test of strip theory AoA sweep
   j=0;     
   a1=input('	From alpha [deg]: ')*pi/180;
   b1=input('	Increment [deg]: ')*pi/180;
   c1=input('	To alpha [deg]: ')*pi/180;
   
   tdisp('Strip theory drag estimation')
        tdisp('Function not fully validated yet. Proceed with caution.')
        latticetype=1; %Standard VLM to start with.
        
        test=fCheckThickness(geo);
        
        if test==0;
            terror(22)
            return
        end
        g=a1:b1:c1;
        J=size(g);
        
   for alpha=a1:b1:c1
        disp(strcat('Percent done: ',num2str(round(100*j/J(2)))))
        state.alpha=alpha;
        j=j+1;
        
        [lattice,ref]=fLattice_setup2(geo,state,latticetype);    %Setting up the lattice
        [results]=solver9(results,state,geo,lattice,ref);
        [results]=coeff_create3(results,lattice,state,ref,geo);   
        
        A(j)=fViscCorr2(geo,state,lattice,results,ref);    %Strip theory viscous drag.
        results.CDv(j)=A(j).totalvdragcoeff;                  %
        results.BLparam(j)=A(j);                              %just send everything to file.
        results.CL2(j)=results.CL;
        results.CD2(j)=results.CD;
        
        
        
        
        
   end
    
    fname=strcat(JID,'-viscous');
        cd(settings.odir)
             save(fname,'results','geo','lattice','state','ref')
         cd(settings.hdir)
    
        tdisp(' ')
             tdisp(strcat(' Solution available in output/',fname))
        tdisp(' ')
    
  
otherwise
   terror(9)
end
   lock(3)=0;
end %function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


