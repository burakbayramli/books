function []=resultplot(var);
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
%%% Result plot function
%
% This function plots the results of a tornado run using standard Matlab
% plots. The function reads the output files and displays the data.
%
% usage:   []=resultplot(VAR)
%
% VAR is the selector for which type of results should be plotted. The
% selector is choosen in postproc, and the list of choises are listed in
% questions(8).
%
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado interface function.
%
% Revision History:
%   Bristol,  2007 06 27:  Addition of new header. TM.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
settings=config('startup');

%disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
%disp('Available static solution result files are: ')
%cd(settings.odir)%
%	dir *-Cx.mat
%cd(settings.hdir)
%data=questions(10);


%if isempty(data)
%   data=('trial');
%end



switch var
    case 1
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available static solution result files are: ')
        cd(settings.odir)
            dir *-Cx.mat
        cd(settings.hdir)
        data=questions(10);
        
        
    try
    cd(settings.odir)
	    fname=strcat(data,'-Cx');
	    load(fname)
        cd(settings.hdir)   
    catch
        cd(settings.hdir)
        terror(12)
   return
end

[x y z]=midpoint(lattice.XYZ);
d=size(z,2);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure(4)% 	Delta cp plot
rotate3d on 
colormap(hot);
fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.cp')
title('Delta cp distribution')
colorbar('vert')
axis equal

% Sonic warning removed, error in underlaying theory.
%try
%if results.sonicWarning==1
%    figure(104)% 	Supersonic warning plot
%    rotate3d on
%    MAP=[0.7 1 0.7;1 0 0];
%    colormap(MAP);
%    h=fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.sonicpanels');
%    title('Supersonic Flow Warning. Red panels supersonic.')
%    axis equal
%end
%end



%*************************************************
% Spanload plot, first attempt.

try
   figure(10)
   hold on, grid on
   plot(results.ystation(:,1),results.ForcePerMeter(:,1));
  title('Spanload on main wing');
  ylabel('Force per meter');
  xlabel('Spanstation')
catch
end
%Local CL plot
try
    
        figure(11)
        hold on, grid on
        plot(results.ystation(:,1),results.CL_local(:,1));
        title('Local CL on main wing');
        ylabel('CL');
        xlabel('Spanstation')
        text(0,0,strcat('Total CL ',num2str(results.CL)));
        
        
        figure(12)
        hold on, grid on
        plot(results.ystation(:,1),results.CD_local(:,1));
        title('Local CD on main wing');
        ylabel('CD');
        xlabel('Spanstation')
        text(0,0,strcat('Total CD ',num2str(results.CD)));
        
        
        
        
        
        
        
        
        
        
        
catch
end

try
   figure(13)
   hold on, grid on
   
   
   plot(results.ystation(:,1)./(ref.b_ref/2),results.CL_local(:,1)./results.CL);
   
   
   title('Normalized Local CL on main wing');
   
   ylabel('Normalized CL');
   xlabel('Normalized spanstation, \eta, [-]')
   
catch
end

try
   %% Bending moment diagram
   %Bending moment computation disabeled in version 136, moved to
   %structural comoutation.
    
   figure(14)
   hold on, grid on
   title('Bending moment on main wing');   
   ylabel('Bending moment, M_b, [Nm]');
   xlabel('Spanstation, y, [m]')
   plot(results.ystation(:,1),results.bend(:,1))
   


   figure(15)
    plot(results.ystation(:,1),results.shear(:,1)) ;
    hold on, grid on
   title('Shear force on main wing');   
   ylabel('Shear force, F_s, [N]');
   xlabel('Spanstation, y, [m]')
    
catch
    
  
end
%*************************************************
figure(7)
axis off
text(0,1,'Tornado Computation Results ')
text(0,.95,'JID: '); text(0.25,0.95,data)
text(0.4,.95,'Downwash matrix condition: '); 
                                        text(0.8,0.95,num2str(results.dwcond))

text(0.4,.9,'Supersonic flow warning: ');
                                        try
                                        text(0.8,0.9,num2str(results.sonicWarning))
                                        catch
                                        text(0.8,0.9,'N/A')  
                                        end
                                        
text(0,.90,'Reference area: ');         text(0.25,0.90,num2str(ref.S_ref));	
text(0,.85,'Reference chord: ');        text(0.25,.85,num2str(ref.C_mac));
text(0,.8,'Reference span: ');          text(0.25,.8,num2str(ref.b_ref));  

text(0.4,.85,'Reference point pos: ');  text(0.8,.85,num2str(geo.ref_point));
try
text(0.4,.80,'Center of gravity  : ');  text(0.8,.80,num2str(geo.CG));
end
text(0,.7,'Net Wind Forces: (N)');			
   text(0.0,.65,'Drag: ');              text(0.1,.65,num2str(results.D));
   text(0.0,.6,'Side: ');           	text(0.1,.6,num2str(results.C));
   text(0.0,.55,'Lift: ');              text(0.1,.55,num2str(results.L));
   
text(0.35,.7,'Net Body Forces: (N)');			
   text(0.35,.65,'X: ');            	text(0.4,.65,num2str(results.FORCE(1)));
   text(0.35,.6,'Y: ');                 text(0.4,.6,num2str(results.FORCE(2)));
   text(0.35,.55,'Z: ');                text(0.4,.55,num2str(results.FORCE(3)));
    
text(0.7,.7,'Net Body Moments: (Nm)');
   text(0.7,.65,'Roll: ');              text(0.8,.65,num2str(results.MOMENTS(1)));
   text(0.7,.6,'Pitch: ');              text(0.8,.6,num2str(results.MOMENTS(2)));   
   text(0.7,.55,'Yaw: ');               text(0.8,.55,num2str(results.MOMENTS(3)));
   
text(0,.45,'CL ');                      text(0.1,.45,num2str(results.CL))   
text(0,.4, 'CD ');                      text(0.1,.4,num2str(results.CD)) 
text(0,.35,'CY ');                      text(0.1,.35,num2str(results.CY))
text(0,.30,'CD_t_r_e_f_f_t_z ');

try
    text(0.15,.30,num2str(results.Trefftz_drag_Coeff))
catch
    text(0.15,.30,'N/A')
end


text(0.35,.45,'CZ ');           text(0.45,.45,num2str(results.CZ))
text(0.35,.4,'CX ');            text(0.45,.4,num2str(results.CX))
text(0.35,.35,'CC ');           text(0.45,.35,num2str(results.CC))

text(0.7,.45,'Cm ');            text(0.8,.45,num2str(results.Cm))
text(0.7,.4,'Cn ');             text(0.8,.4,num2str(results.Cn))
text(0.7,.35,'Cl ');        	text(0.8,.35,num2str(results.Cl))

text(0,.2,'STATE: ');
text(0,.15,'\alpha [deg]: ');   text(.15,.15,num2str(state.alpha*180/pi));
text(0,.1,'\beta [deg]: ');     text(.15,.1,num2str(state.betha*180/pi));
text(0,.05,'Airspeed: ');       text(.15,.05,num2str(state.AS));
try
 text(0,.0,'Altitude: ');       text(.15,.0,num2str(state.ALT));
end
 text(0,-.05,'Density: ');      text(.15,-.05,num2str(state.rho));

text(0.3,.15,'P [rad/s]: ');    text(.45,.15,num2str(state.P));
text(0.3,.10,'Q [rad/s]: ');    text(.45,.10,num2str(state.Q));
text(0.3,.05,'R [rad/s]: ');    text(.45,.05,num2str(state.R));


    text(0.3,0.0,'PG Correction: ');
try
    text(0.55,0.0,num2str(state.pgcorr))
catch
    text(0.55,0,'N/A')
end
text(0.6,.1,'Rudder setting [deg]:'); text(.9,.1,num2str(geo.flap_vector*180/pi));


[void sos void]=ISAtmosphere(state.ALT);
Mach=state.AS/sos;
text(0.3,-.05,'Mach: ');    text(.45,-.05,num2str(Mach));

%%%%%
%
%%%%%
figure(8)
axis off
grid on
text(0,1,'TORNADO CALCULATION RESULTS, Derivatives')
text(0,.95,'JID: ');                text(0.25,0.95,data)
text(0,.90,'Reference area: ');	    text(0.25,0.90,num2str( ref.S_ref ));	
text(0,.85,'Reference chord: ');    text(0.25,.85,num2str(ref.C_mac));
text(0,.8,'Reference span: ');      text(0.25,.8,num2str(ref.b_ref));
  
   
text(0.4,.90,'\alpha [deg]: '); 	text(.55,.9,num2str(state.alpha*180/pi));
text(0.4,.85, '\beta [deg]: ');     text(.55,.85,num2str(state.betha*180/pi));
text(0.4,.8,'Airspeed: ');          text(.55,.8,num2str(state.AS));

text(0.65,.9, 'P [rad/s]: ');  		text(.8,.9,num2str(state.P));
text(0.65,.85,'Q[rad/s]: '); 		text(.8,.85,num2str(state.Q));
text(0.65,.8,' R[rad/s]: ');  		text(.8,.8,num2str(state.R));

figure(8)
text(0,.7,'CL derivatives : ');			

text(0,.65,'CL_{\alpha}');		    text(0.15,.65,num2str(results.CL_a));
text(0,.6,'CL_{\beta}');		    text(0.15,.6,num2str(results.CL_b));
text(0,.55,'CL_P');			        text(0.15,.55,num2str(results.CL_P));
text(0,.5,'CL_Q');			        text(0.15,.5,num2str(results.CL_Q));
text(0,.45,'CL_R');			        text(0.15,.45,num2str(results.CL_R));
   
text(0,.35,'Roll derivatives : ');			
   
text(0,.3,'Cl_{\alpha}');		    text(0.15,.3,num2str(results.Cl_a));
text(0,.25,'Cl_{\beta}');		    text(0.15,.25,num2str(results.Cl_b));
text(0,.2,'Cl_P');			        text(0.15,.2,num2str(results.Cl_P));
text(0,.15,'Cl_Q');			        text(0.15,.15,num2str(results.Cl_Q));
text(0,.1,'Cl_R');			        text(0.15,.1,num2str(results.Cl_R));
   
text(0.35,.7,'CD derivatives : ');			

text(0.35,.65,'CD_{\alpha}');	text(0.5,.65,num2str(results.CD_a));
text(0.35,.6,'CD_{\beta}');	text(0.5,.6,num2str(results.CD_b));
text(0.35,.55,'CD_P');		text(0.5,.55,num2str(results.CD_P));
text(0.35,.5,'CD_Q');		text(0.5,.5,num2str(results.CD_Q));
text(0.35,.45,'CD_R');		text(0.5,.45,num2str(results.CD_R));
   
text(0.35,.35,'Pitch derivatives : ');			

text(0.35,.3,'Cm_{\alpha}');	text(.5,.3,num2str(results.Cm_a));
text(0.35,.25,'Cm_{\beta}');	text(0.5,.25,num2str(results.Cm_b));
text(0.35,.2,'Cm_P');		text(0.5,.2,num2str(results.Cm_P));
text(0.35,.15,'Cm_Q');		text(0.5,.15,num2str(results.Cm_Q));
text(0.35,.1,'Cm_R');		text(0.5,.1,num2str(results.Cm_R));
   
text(0.7,.7,'CY derivatives : ');			

text(0.7,.65,'CY_{\alpha}');	text(0.85,.65,num2str(results.CY_a));
text(0.7,.6,'CY_{\beta}');		text(0.85,.6,num2str(results.CY_b));
text(0.7,.55,'CY_P');		text(0.85,.55,num2str(results.CY_P));
text(0.7,.5,'CY_Q');			text(0.85,.5,num2str(results.CY_Q));
text(0.7,.45,'CY_R');		text(0.85,.45,num2str(results.CY_R));
   
text(0.7,.35,'Yaw derivatives : ');			

text(0.7,.3,'Cn_{\alpha}');	text(.85,.3,num2str(results.Cn_a));
text(0.7,.25,'Cn_{\beta}');	text(0.85,.25,num2str(results.Cn_b));
text(0.7,.2,'Cn_P');			text(0.85,.2,num2str(results.Cn_P));
text(0.7,.15,'Cn_Q');		text(0.85,.15,num2str(results.Cn_Q));
text(0.7,.1,'Cn_R');			text(0.85,.1,num2str(results.Cn_R));
   
   
figure(9)

axis off
%grid on
text(0,1,'TORNADO CALCULATION RESULTS, Central difference, RUDDER DERIVs')
text(0,.95,'JID: '); text(0.25,0.95,data)

text(0,.90,'Reference area: ');	text(0.25,0.90,num2str(ref.S_ref));	
text(0,.85,'Reference chord: ');text(0.25,.85,num2str(ref.C_mac));
text(0,.8,'Reference span: ');text(0.25,.8,num2str(ref.b_ref));
   
text(0.4,.90,'\alpha: '); 	text(.55,.9,num2str(state.alpha*180/pi));
text(0.4,.85,'\beta: ');     text(.55,.85,num2str(state.betha*180/pi));
text(0.4,.8,'Airspeed: ');  text(.55,.8,num2str(state.AS));

text(0.65,.9,'P: ');  		text(.8,.9,num2str(state.P));
text(0.65,.85,'Q: '); 		text(.8,.85,num2str(state.Q));
text(0.65,.8,'R: ');  		text(.8,.8,num2str(state.R));

text(0,.45,'CL_{\delta}');			text(0.15,.45,num2str(results.CL_d'));
text(0,.05,'Cl_{\delta}');			text(0.15,.05,num2str(results.Cl_d'));
text(0.35,.45,'CD_{\delta}');		text(0.5,.45,num2str(results.CD_d'));
text(0.35,.05,'Cm_{\delta}');		text(0.5,.05,num2str(results.Cm_d'));
text(0.7,.45,'CY_{\delta}');		text(0.85,.45,num2str(results.CY_d'));
text(0.7,.05,'Cn_{\delta}');		text(0.85,.05,num2str(results.Cn_d'));



%Bend and shear plot
    %%%%%%%%%%%%%%%%5
    %Stuff below is experimental
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %Hardcoded to plot the first wing data.

H=figure(1);
hold on
set(H,'Position',[10 10 0.8*3*210 0.8*3*297])
%Changing variables to plot only partition outline
g2=geo;
g2.nwing=1;
g2.nx=double(g2.nx>0);
g2.ny=double(g2.ny>0); 
g2.fnx=double(g2.fnx>0);
g2.symetric=geo.symetric(1);


g2.nx=g2.nx(1,:);
g2.ny=g2.ny(1,:);
g2.fnx=g2.fnx(1,:);
g2.nelem=g2.nelem(1);
g2.flapped=geo.flapped(1,:);
g2.fsym=geo.fsym(1,:);
g2.flap_vector=geo.flap_vector(1,:);


s2.AS=1;
s2.alpha=0;
s2.betha=0;
s2.P=0;
s2.Q=0;
s2.R=0;
s2.ALT=0;
s2.rho=1;
s2.pgcorr=0;


        
[l2,ref]=fLattice_setup2(g2,s2,1);

subplot(4,1,1,'position',[0.1 0.8 0.8 0.17]); hold on 
%axes('position',[0.1 0.8 0.8 0.17])
g=fill3(l2.XYZ(:,:,1)',l2.XYZ(:,:,2)',l2.XYZ(:,:,3)','w');
set(g,'LineWidth',2);
view([90,90]);
%axis equal
hold on
%xlabel('Aircraft body x-coordinate')
%ylabel('Aircraft body y-coordinate')
%zlabel('Aircraft body z-coordinate')
title('Wing aerodynamic loading')
axis off


subplot(4,1,2)
%axes()
h1=plot(results.ystation(:,1),results.shear(:,1));
hold on
set(h1,'LineWidth',2)
h2=gca;
%set(h2,'XTickLabel',[],'position',[0.1 0.6 0.8 0.17])
grid on
ylabel('Shear force, F_z, [N]')

subplot(4,1,3)
%axes();
h1=plot(results.ystation(:,1),results.bend(:,1));
hold on
set(h1,'LineWidth',2)
h2=gca;
%set(h2,'XTickLabel',[],'position',[0.1 0.4 0.8 0.17])


grid on
ylabel('Bend moment, M_x, [Nm]')

subplot(4,1,4)
%axes('position',[0.1 0.2 0.8 0.17]);
h1=plot(results.ystation(:,1),results.twist(:,1));
hold on
set(h1,'LineWidth',2)
%h2=gca;
%set(h5,'OuterPosition',[0 0.0 1 0.2])
grid on
ylabel('Twist moment, M_y, [Nm]')
xlabel('Span station, y, [m]')
h2=gca;
%set(h2,'position',[0.1 0.2 0.8 0.17])























  case 2

        %Case removed




        
	case 3
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available alpha sweep solution result files are: ')
        cd(settings.odir)
            dir *-Cx_alpha.mat
        cd(settings.hdir)
        data=questions(10);
        
        
   	try
			cd(settings.odir)
				fname=strcat(data,'-Cx_alpha');
				load(fname)
			cd(settings.hdir)   
		catch
   		cd(settings.hdir)
   		terror(12)
   	return
		end

   
   figure(15) 
   subplot(3,2,3), plot(results.alpha_sweep,squeeze(results.matrix(2,1,:)));
   xlabel('Alpha [rad]')
   ylabel('CD')
   
   subplot(3,2,5), plot(results.alpha_sweep,squeeze(results.matrix(3,1,:)));
   xlabel('Alpha [rad]')
   ylabel('CY')
   
   subplot(3,2,2), plot(results.alpha_sweep,squeeze(results.matrix(4,1,:)));
   xlabel('Alpha [rad]')
   ylabel('Cl')
   
   subplot(3,2,4), plot(results.alpha_sweep,squeeze(results.matrix(5,1,:)));
   xlabel('Alpha [rad]')
   ylabel('Cm')
   
   subplot(3,2,6), plot(results.alpha_sweep,squeeze(results.matrix(6,1,:)));
   xlabel('Alpha [rad]')
   ylabel('Cn')
   
   subplot(3,2,1), plot(results.alpha_sweep,squeeze(results.matrix(1,1,:)));
   xlabel('Alpha [rad]')
   ylabel('CL')
  
  axes
  text(0.0,1.05,'Coefficient dependency on alpha');
  axis off  
  
  
  
  figure(16)
        plot(squeeze(results.matrix(1,1,:)),squeeze(results.matrix(2,1,:)));
        xlabel('CL,[-].');
        ylabel('CD,[-].');
        title('Induced Drag Polar');
        grid
        hold on
  
  figure(17)
        
        alpha1=results.alpha_sweep.*180/pi;
        CL=squeeze(results.matrix(1,1,:));
        CD=squeeze(results.matrix(2,1,:));
        Cm=squeeze(results.matrix(5,1,:));
        faeropolar(alpha1,CL,CD,Cm,'-o');
  
    try 
        alpha1=results.alpha_sweep.*180/pi;
        delta1=results.rudderangle.*180/pi;
        
        figure(18)
        plot(alpha1,delta1)
        xlabel('Angle of attack, \alpha, [deg].')
        ylabel('Control effector deflection, \delta, [deg].')
        title('Effector angle to trim.')
        %hold on
    end
    
 
case 4
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available beta sweep solution result files are: ')
        cd(settings.odir)
            dir *-Cx_beta.mat
        cd(settings.hdir)
        data=questions(10);
      try
			cd(settings.odir)
				fname=strcat(data,'-Cx_beta')
				load(fname)
			cd(settings.hdir)   
		catch
   		cd(settings.hdir)
   		terror(12)
   	return
		end

   
   figure(10)

   subplot(3,2,1), plot(results.betha_sweep,squeeze(results.matrix(1,1,:)));
   xlabel('Beta [rad]')
   ylabel('CL')
   
   subplot(3,2,3), plot(results.betha_sweep,squeeze(results.matrix(2,1,:)));
   xlabel('Beta [rad]')
   ylabel('CD')
   
   subplot(3,2,5), plot(results.betha_sweep,squeeze(results.matrix(3,1,:)));
   xlabel('Beta [rad]')
   ylabel('CY')
   
   subplot(3,2,2), plot(results.betha_sweep,squeeze(results.matrix(4,1,:)));
   xlabel('Beta [rad]')
   ylabel('Cl')
   
   subplot(3,2,4), plot(results.betha_sweep,squeeze(results.matrix(5,1,:)));
   xlabel('Beta [rad]')
   ylabel('Cm')
   
   subplot(3,2,6), plot(results.betha_sweep,squeeze(results.matrix(6,1,:)));
   xlabel('Beta [rad]')
   ylabel('Cn')
   
  axes
  text(0.0,1.05,'Coefficient dependency on beta (sideslip)')
  axis off
  
  
case 5 
    disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available rudder sweep solution result files are: ')
        cd(settings.odir)
            dir *-Cx_d.mat
        cd(settings.hdir)
        data=questions(10);
   	try
			cd(settings.odir)
				fname=strcat(data,'-Cx_d')
				load(fname)
			cd(settings.hdir)   
		catch
   		cd(settings.hdir)
   		terror(12)
   	return
		end
   
   figure(11)

   subplot(3,2,1), plot(results.delta_sweep,squeeze(results.matrix(1,1,:)));
   xlabel('Delta [rad]')
   ylabel('CL')
   
   subplot(3,2,3), plot(results.delta_sweep,squeeze(results.matrix(2,1,:)));
   xlabel('Delta [rad]')
   ylabel('CD')
   
   subplot(3,2,5), plot(results.delta_sweep,squeeze(results.matrix(3,1,:)));
   xlabel('Delta [rad]')
   ylabel('CY')
   
   subplot(3,2,2), plot(results.delta_sweep,squeeze(results.matrix(4,1,:)));
   xlabel('Delta [rad]')
   ylabel('Cl')
   
   subplot(3,2,4), plot(results.delta_sweep,squeeze(results.matrix(5,1,:)));
   xlabel('Delta [rad]')
   ylabel('Cm')
   
   subplot(3,2,6), plot(results.delta_sweep,squeeze(results.matrix(6,1,:)));
   xlabel('Delta [rad]')
   ylabel('Cn')   
   
   axes
  	text(0.0,1.05,'Coefficient dependency on delta (rudder deflection)')
  	axis off
    
    
case 6
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available roll speed sweep solution result files are: ')
        cd(settings.odir)
            dir *-Cx_P.mat
        cd(settings.hdir)
        data=questions(10);
    
   	try
			cd(settings.odir)
				fname=strcat(data,'-Cx_P')
				load(fname)
			cd(settings.hdir)   
		catch
   		cd(settings.hdir)
   		terror(12)
   	return
		end
   figure(12)

   subplot(3,2,1), plot(results.P_sweep,squeeze(results.matrix(1,1,:)));
   xlabel('P [-]')
   ylabel('CL [-]')
   
   subplot(3,2,3), plot(results.P_sweep,squeeze(results.matrix(2,1,:)));
   xlabel('P [-]')
   ylabel('CD [-]')
   
   subplot(3,2,5), plot(results.P_sweep,squeeze(results.matrix(3,1,:)));
   xlabel('P [-]')
   ylabel('CY [-]')
   
   subplot(3,2,2), plot(results.P_sweep,squeeze(results.matrix(4,1,:)));
   xlabel('P [-]')
   ylabel('Cl [-]')
   
   subplot(3,2,4), plot(results.P_sweep,squeeze(results.matrix(5,1,:)))
   xlabel('P [-]')
   ylabel('Cm [-]')
   
   subplot(3,2,6), plot(results.P_sweep,squeeze(results.matrix(6,1,:)));
   xlabel('P [-]')
   ylabel('Cn [-]')  
   
   axes
  	text(0.0,1.05,'Coefficient dependency on P (Roll speed)')
  	axis off
    
    
case 7
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available pitch speed sweep solution result files are: ')
        cd(settings.odir)
            dir *-Cx_Q.mat
        cd(settings.hdir)
        data=questions(10);
   	try
			cd(settings.odir)
				fname=strcat(data,'-Cx_Q')
				load(fname)
			cd(settings.hdir)   
		catch
   		cd(settings.hdir)
   		terror(12)
   	return
		end
   figure(14)

   subplot(3,2,1), plot(results.Q_sweep,squeeze(results.matrix(1,1,:)));
   xlabel('Q [-]')
   ylabel('CL [-]')
   
   subplot(3,2,3), plot(results.Q_sweep,squeeze(results.matrix(2,1,:)));
   xlabel('Q [-]')
   ylabel('CD [-]')
   
   subplot(3,2,5), plot(results.Q_sweep,squeeze(results.matrix(3,1,:)));
   xlabel('Q [-]')
   ylabel('CY [-]')
   
   subplot(3,2,2), plot(results.Q_sweep,squeeze(results.matrix(4,1,:)));
   xlabel('Q [-]')
   ylabel('Cl [-]')
   
   subplot(3,2,4), plot(results.Q_sweep,squeeze(results.matrix(5,1,:)));
   xlabel('Q [-]')
   ylabel('Cm [-]')
   
   subplot(3,2,6), plot(results.Q_sweep,squeeze(results.matrix(6,1,:)));
   xlabel('Q [-]')
   ylabel('Cn [-]')   
   
   axes
  	text(0.0,1.05,'Coefficient dependency on Q (Pitch speed)')
  	axis off
    
    
case 8  
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available yaw speed sweep solution result files are: ')
        cd(settings.odir)
            dir *-Cx_R.mat
        cd(settings.hdir)
        data=questions(10);
   	try
			cd(settings.odir)
				fname=strcat(data,'-Cx_R')
				load(fname)
			cd(settings.hdir)   
		catch
   		cd(settings.hdir)
   		terror(12)
   	return
		end
   figure(13)

   subplot(3,2,1), plot(results.R_sweep,squeeze(results.matrix(1,1,:)));
   xlabel('R [-]')
   ylabel('CL [-]')
   
   subplot(3,2,3), plot(results.R_sweep,squeeze(results.matrix(2,1,:)));
   xlabel('R [-]')
   ylabel('CD [-]')
   
   subplot(3,2,5), plot(results.R_sweep,squeeze(results.matrix(3,1,:)));
   xlabel('R [-]')
   ylabel('CY [-]')
   
   subplot(3,2,2), plot(results.R_sweep,squeeze(results.matrix(4,1,:)));
   xlabel('R [-]')
   ylabel('Cl [-]')
   
   subplot(3,2,4), plot(results.R_sweep,squeeze(results.matrix(5,1,:)));
   xlabel('R [-]')
   ylabel('Cm [-]')
   
   subplot(3,2,6), plot(results.R_sweep,squeeze(results.matrix(6,1,:)));
   xlabel('R [-]')
   ylabel('Cn [-]')  
   
   axes
  	text(0.0,1.05,'Coefficient dependency on R (Yaw speed)')
  	axis off     
%end

case 9
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available static solution result files are: ')
        cd(settings.odir)
            dir *-Cx.mat
        cd(settings.hdir)
        data=questions(10);
    try
    cd(settings.odir)
	    fname=strcat(data,'-Cx');
	    load(fname)
        cd(settings.hdir)   
    catch
        cd(settings.hdir)
        terror(12)
    return
    end
    
    results=trefftz5(results,state,geo,lattice,ref);
    
    x=results.treffts.x;
    y=results.treffts.y;
    D=results.treffts.D;
    VTOT=results.treffts.VTOT;
    VVEC=results.treffts.VVEC;
    
    
    figure(23)
    surf(x,y,D)
    xlabel('y coordinate, [m]')
    ylabel('z coordinate, [m]')
    zlabel('Distributed drag contrubution, [N]')
    title('Trefftz plane drag contribution field.') %*** Treffz changed to Trefftz (AT)


    figure(22)
    contour(x,y,VTOT,20), hold on
    quiver(x,y,VVEC(:,:,1),VVEC(:,:,2),5)
    xlabel('y coordinate, [m]')
    ylabel('z coordinate, [m]')
    title('Trefftz plane velocity vector field.') %*** Treffz changed to Trefftz (AT)
    
    cd(settings.odir)
	    fname=strcat(data,'-Cx');
	    save(fname,'results','state','geo','lattice','ref')
    cd(settings.hdir)   
   
case 10
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available flap plate drag result files are: ')
        cd(settings.odir)
            dir *-Cnull.mat
        cd(settings.hdir)
        data=questions(10);
   try
    cd(settings.odir)
	    fname=strcat(data,'-Cnull');
	    load(fname)
        cd(settings.hdir)  
    catch
        cd(settings.hdir)
        terror(12)
    return
    end
    figure(31)
    axis off
    grid on
    text(0,1,'TORNADO Zero lift drag estimation.')
    text(0,.95,'JID: '); text(0.25,0.95,data)
    text(0,.90,'Reference area: ');	text(0.25,0.90,num2str( ref.S_ref ));	
    text(0,.85,'Reference chord: ');text(0.25,.85,num2str(ref.C_mac));
    text(0,.8,'Reference span: ');text(0.25,.8,num2str(ref.b_ref));
    
    text(0.65,.95,'Re: ');  		text(.8,.95,num2str(round(results.Re)));
    text(0.65,.9,'CD_0: ');  		text(.8,.9,num2str(sum(sum(results.CD0))));
    text(0.65,.85,'Swet: '); 		text(.8,.85,num2str(sum(sum(results.Swet))));
    text(0.65,.8,'Volume: ');  		text(.8,.8,num2str(sum(sum(results.Vol))));
    
    		
	text(0.0,.60,'Zero lift drag per part: ');		text(0.35,.6,num2str(results.CD0));
    text(0.0,.35,'Wetted area per part: ');		    text(0.35,.35,num2str(results.Swet));
    text(0.0,.1,'Internal volume per part: ');		text(0.35,.1,num2str(results.Vol));
    
    
    %text(0.0,.6,'Side: ');			text(0.1,.6,num2str(results.C));
    %text(0.0,.55,'Lift: ');		    text(0.1,.55,num2str(results.L));
    
    
 case 11 %***
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available polar breakdown result files are: ')
        cd(settings.odir)
            dir *-Cx_drginf.mat
        cd(settings.hdir)
        data=questions(10);
     
	try %***
        cd(settings.odir) %***
	    fname=strcat(data,'-Cx_drginf'); %***
        %fname=strcat(data,'-Cx_drgout'); %*** alternative
	    load(fname) %***
        cd(settings.hdir) %***
    catch %***
        cd(settings.hdir) %***
        terror(12) %***
    return %***
    end %***
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %***
    %function drgplot() %***

    drgdata=zeros(numbmhs+1,numbcls+1,5); %*** contains drag components to plot in stacked columns
    %*** if more drag components are to be added change '5' to required
    %*** number of components and add/remove components below
    
    for i=1:1:numbmhs+1 %***
        drgdata(i,:,1)=vorinrd; %*** auto-vortex induced drag of wing
        drgdata(i,:,3)=PROFCOR; %*** form drag as a function of op. CL
    end %***

    drgdata(:,:,2)=EZELDRG; %*** zero-lift drag
    drgdata(:,:,4)=TTLTRMD; %*** total trim drag
    drgdata(:,:,5)=TCDCINC; %*** compressibility drag

    drgdata(1,:,:)=[]; %*** 
    drgdata(:,1,:)=[]; %***  

    drgdata=permute(drgdata,[3 2 1]);    %rotating drgdata dims %***
    
    % Manipulating CL values for x-axis
    xdatacl=zeros(numbcls+1); %***
    xdatacl=(MASTDRG(1,:)/10); %***
    xdatacl(1)=[]; %***
    
    % Plot drag breakdown for each mach number %***
    for j=1:1:numbmhs %***
        figure(30+j) %***
        grid on;
        drgdats=(squeeze(drgdata(:,:,j)))'; %*** taking 2D Mach number slices of 3D drgdata matrix
        %colormap(bone)
        bar(xdatacl,drgdats,1.5) %*** producing grouped column plot of CL vs. CD (counts)
        title(['Mach ',num2str(MASTDRG(j+1,1)/10)]) %***
        xlabel('C_L') %***
        ylabel('Drag (Counts)') %***
        legend('Vor-Ind_w','Zero-Lift','Profile','Trim','Comp','Location','NorthWest','Orientation','horizontal') %***
    end %***

    %end %drgplot function %***
    
    
 case 12 %***
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available polar breakdown result files are: ')
        cd(settings.odir)
            dir *-Cx_drgout.mat
        cd(settings.hdir)
        data=questions(10);
	try %***
        cd(settings.odir) %***
	    %fname=strcat(data,'-Cx_drginf'); %***
        fname=strcat(data,'-Cx_drgout'); %*** alternative
	    load(fname) %***
        cd(settings.hdir) %***
    catch %***
        cd(settings.hdir) %***
        terror(12) %***
    return %***
    end %***
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %***
    %function drgplot() %***

    drgdata=zeros(numbmhs+1,numbcls+1,5); %*** contains drag components to plot in stacked columns
    %*** if more drag components are to be added change '5' to required
    %*** number of components and add/remove components below
    
    for i=1:1:numbmhs+1 %***
        drgdata(i,:,1)=PROFCOR; %*** form drag as a function of op. CL
    end %***

    drgdata(:,:,2)=TCDTINC; %*** trim drag due to h-tail VI
    drgdata(:,:,3)=TCDWINC; %*** trim drag due to wing increment VI
    drgdata(:,:,4)=TTTHRST; %*** tail thrust due to wing downwash
    drgdata(:,:,5)=TCDCINC; %*** compressibility drag

    drgdata(1,:,:)=[]; %*** 
    drgdata(:,1,:)=[]; %***  

    drgdata=permute(drgdata,[3 2 1]);    %rotating drgdata dims %***
    
    % Manipulating CL values for x-axis
    xdatacl=zeros(numbcls+1); %***
    xdatacl=(MASTDRG(1,:)/10); %***
    xdatacl(1)=[]; %***
    
    % Plot drag breakdown for each mach number %***
    for j=1:1:numbmhs %***
        figure(30+j) %***
        grid on;
        drgdats=(squeeze(drgdata(:,:,j)))'; %*** taking 2D Mach number slices of 3D drgdata matrix
        %colormap(bone)
        bar(xdatacl,drgdats,'group') %*** producing grouped column plot of CL vs. CD (counts)
        title(['Mach ',num2str(MASTDRG(j+1,1)/10)]) %***
        xlabel('C_L') %***
        ylabel('Drag (Counts)') %***
        legend('Profile','Trim H-Tail','Trim Wing','Tail Thrust','Comp','Location','NorthWest','Orientation','horizontal') %***
    end %***

    %end %drgplot function %***
    
    
    
    
    
    
    
    
    
    
    
    case 15
        %plotting unsteady derivatives
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available polar breakdown result files are: ')
        cd(settings.odir)
            dir *-Cx_dot.mat
        cd(settings.hdir)
        data=questions(10);
        try
            cd(settings.odir)
            fname=strcat(data,'-Cx_dot');
            load(fname)
            cd(settings.hdir)  
        catch
            cd(settings.hdir)
            terror(12)
            return
        end
        figure(32)
        axis off
        grid on
        text(0,1,'TORNADO unsteady derivatives estimation.')
        text(0,.95,'JID: '); text(0.25,0.95,data)
        text(0,.90,'Reference area: ');	text(0.25,0.90,num2str( ref.S_ref ));	
        text(0,.85,'Reference chord: ');text(0.25,.85,num2str(ref.C_mac));
        text(0,.8,'Reference span: ');text(0.25,.8,num2str(ref.b_ref));
    
        text(0,.7,'CZ_{\alpha_{dot}}: ');  		text(.2,.7,num2str(results.CZ_a_dot));
        text(0,.65,'Cm_{\alpha_{dot}}: ');  		text(.2,.65,num2str(results.Cm_a_dot));
    
        text(0,.6,'CY_{\beta_{dot}}: ');  		text(.2,.6,num2str(results.CY_b_dot));
        text(0,.55,'Cn_{\beta_{dot}}: ');  		text(.2,.55,num2str(results.Cn_b_dot));


case 16
%Unsteady data
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available polar breakdown result files are: ')
        cd(settings.odir)
            dir *-Cxunst.mat
        cd(settings.hdir)
        data=questions(10);
    try
        cd(settings.odir)
	    fname=strcat(data,'-Cxunst');
	    load(fname)
        cd(settings.hdir)   
    catch
        cd(settings.hdir)
        terror(12)
   return
    end

    [x y z]=midpoint(lattice.XYZ);
    d=size(z,2);

%*************************************************
figure(7)
axis off
text(0,1,'Tornado Computation Results: Unsteady, acceleration free, solution ')
text(0,.95,'JID: '); text(0.25,0.95,data)
text(0.4,.95,'Downwash matrix condition: '); 
                                        text(0.8,0.95,num2str(results.dwcond))

text(0,.90,'Reference area: ');         text(0.25,0.90,num2str(ref.S_ref));	
text(0,.85,'Reference chord: ');        text(0.25,.85,num2str(ref.C_mac));
text(0,.8, 'Reference span: ');         text(0.25,.8,num2str(ref.b_ref));  

text(0.4,.85,'Reference point pos: ');  text(0.8,.85,num2str(geo.ref_point));
try
text(0.4,.80,'Center of gravity  : ');  text(0.8,.80,num2str(geo.CG));
end
text(0,.7,'Net Wind Forces: (N)');			
   text(0.0,.65,'Drag: ');              text(0.1,.65,num2str(results.D));
   text(0.0,.6,'Side: ');           	text(0.1,.6,num2str(results.C));
   text(0.0,.55,'Lift: ');              text(0.1,.55,num2str(results.L));
   
text(0.35,.7,'Net Body Forces: (N)');			
   text(0.35,.65,'X: ');            	text(0.4,.65,num2str(results.FORCE(1)));
   text(0.35,.6,'Y: ');                 text(0.4,.6,num2str(results.FORCE(2)));
   text(0.35,.55,'Z: ');                text(0.4,.55,num2str(results.FORCE(3)));
    
text(0.7,.7,'Net Body Moments: (Nm)');
   text(0.7,.65,'Roll: ');              text(0.8,.65,num2str(results.MOMENTS(1)));
   text(0.7,.6,'Pitch: ');              text(0.8,.6,num2str(results.MOMENTS(2)));   
   text(0.7,.55,'Yaw: ');               text(0.8,.55,num2str(results.MOMENTS(3)));
   
text(0,.45,'CL ');                      text(0.1,.45,num2str(results.CL))   
text(0,.4, 'CD ');                      text(0.1,.4,num2str(results.CD)) 
text(0,.35,'CY ');                      text(0.1,.35,num2str(results.CY))
text(0,.30,'CD_t_r_e_f_f_t_z ');

try
    text(0.15,.30,num2str(results.Trefftz_drag_Coeff))
catch
    text(0.15,.30,'N/A')
end


text(0.35,.45,'CZ ');           text(0.45,.45,num2str(results.CZ))
text(0.35,.4, 'CX ');           text(0.45,.4,num2str(results.CX))
text(0.35,.35,'CC ');           text(0.45,.35,num2str(results.CC))

text(0.7,.45,'Cm ');            text(0.8,.45,num2str(results.Cm))
text(0.7,.4, 'Cn ');            text(0.8,.4,num2str(results.Cn))
text(0.7,.35,'Cl ');        	text(0.8,.35,num2str(results.Cl))

text(0,.2,'STATE: ');
text(0,.15,'\alpha [deg]: ');   text(.15,.15,num2str(state.alpha*180/pi));
text(0,.1, '\beta [deg]: ');    text(.15,.1,num2str(state.betha*180/pi));
text(0,.05,'Airspeed: ');       text(.15,.05,num2str(state.AS));
try
 text(0,.0,'Altitude: ');       text(.15,.0,num2str(state.ALT));
end
 text(0,-.05,'Density: ');      text(.15,-.05,num2str(state.rho));

text(0.3,.15,'P     [deg/s]: ');    text(.5,.15,num2str(state.P*180/pi));
text(0.3,.10,'Q     [deg/s]: ');    text(.5,.10,num2str(state.Q*180/pi));
text(0.3,.05,'R     [deg/s]: ');    text(.5,.05,num2str(state.R*180/pi));
text(0.3,.0,'\alpha_{dot} [deg/s]: ');  text(.5,.0,num2str(state.adot*180/pi));
text(0.3,-.05,'\beta_{dot}  [deg/s]: '); text(.5,-.05,num2str(state.bdot*180/pi));


    text(0,-0.1,'PG Correction: ');
try
    text(0.25,-0.1,num2str(state.pgcorr))
catch
    text(0.25,-0.1,'N/A')
end
text(0.6,.1,'Rudder setting [deg]:'); text(.9,.1,num2str(geo.flap_vector*180/pi));


%%%%%
%
%%%%%
figure(8)
axis off
grid on
text(0,1,'TORNADO CALCULATION RESULTS: Derivatives.')
text(0,.95,'JID: ');                text(0.25,0.95,data)
text(0,.90,'Reference area: ');	    text(0.25,0.90,num2str( ref.S_ref ));	
text(0,.85,'Reference chord: ');    text(0.25,.85,num2str(ref.C_mac));
text(0,.8,'Reference span: ');      text(0.25,.8,num2str(ref.b_ref));
  
   
text(0.4,.90,'\alpha [deg]: '); 	text(.55,.9,num2str(state.alpha*180/pi));
text(0.4,.85, '\beta [deg]: ');     text(.55,.85,num2str(state.betha*180/pi));
text(0.4,.80,'Airspeed: ');         text(.55,.8,num2str(state.AS));

text(0.65,.90, 'P     [deg/s]: ');  	text(.85,.9,num2str(state.P*180/pi));
text(0.65,.85, 'Q     [deg/s]: '); 		text(.85,.85,num2str(state.Q*180/pi));
text(0.65,.80, 'R     [deg/s]: ');      text(.85,.8,num2str(state.R*180/pi));

text(0.65,.75,'\alpha_{dot} [deg/s]: ');  text(.85,.75,num2str(state.adot*180/pi));
text(0.65,.70,'\beta_{dot}  [deg/s]: ');  text(.85,.70,num2str(state.bdot*180/pi));

text(0,.6,'CL derivatives : ');			

text(0,.55,'CL_{\alpha}');		    text(0.15,.55,num2str(results.CL_a));
text(0,.5,'CL_{\beta}');		    text(0.15,.5,num2str(results.CL_b));
text(0,.45,'CL_P');			        text(0.15,.45,num2str(results.CL_P));
text(0,.4,'CL_Q');			        text(0.15,.4,num2str(results.CL_Q));
text(0,.35,'CL_R');			        text(0.15,.35,num2str(results.CL_R));
   
text(0,.25,'Roll derivatives : ');			
   
text(0,.2,'Cl_{\alpha}');		    text(0.15,.2,num2str(results.Cl_a));
text(0,.15,'Cl_{\beta}');		    text(0.15,.15,num2str(results.Cl_b));
text(0,.1,'Cl_P');			        text(0.15,.1,num2str(results.Cl_P));
text(0,.05,'Cl_Q');			        text(0.15,.05,num2str(results.Cl_Q));
text(0,.0,'Cl_R');			        text(0.15,.0,num2str(results.Cl_R));
   
text(0.35,.6,'CD derivatives : ');			

text(0.35,.55,'CD_{\alpha}');	text(0.5,.55,num2str(results.CD_a));
text(0.35,.5,'CD_{\beta}');	text(0.5,.5,num2str(results.CD_b));
text(0.35,.45,'CD_P');		text(0.5,.45,num2str(results.CD_P));
text(0.35,.4,'CD_Q');		text(0.5,.4,num2str(results.CD_Q));
text(0.35,.35,'CD_R');		text(0.5,.35,num2str(results.CD_R));
   
text(0.35,.25,'Pitch derivatives : ');			

text(0.35,.2,'Cm_{\alpha}');	text(.5,.2,num2str(results.Cm_a));
text(0.35,.15,'Cm_{\beta}');	text(0.5,.15,num2str(results.Cm_b));
text(0.35,.1,'Cm_P');           text(0.5,.1,num2str(results.Cm_P));
text(0.35,.05,'Cm_Q');          text(0.5,.05,num2str(results.Cm_Q));
text(0.35,.0,'Cm_R');           text(0.5,.0,num2str(results.Cm_R));
   
text(0.7,.6,'CY derivatives : ');			

text(0.7,.55,'CY_{\alpha}');	text(0.85,.55,num2str(results.CY_a));
text(0.7,.5,'CY_{\beta}');		text(0.85,.5,num2str(results.CY_b));
text(0.7,.45,'CY_P');           text(0.85,.45,num2str(results.CY_P));
text(0.7,.4,'CY_Q');			text(0.85,.4,num2str(results.CY_Q));
text(0.7,.35,'CY_R');           text(0.85,.35,num2str(results.CY_R));
   
text(0.7,.25,'Yaw derivatives : ');			

text(0.7,.2,'Cn_{\alpha}');     text(.85,.2,num2str(results.Cn_a));
text(0.7,.15,'Cn_{\beta}');     text(0.85,.15,num2str(results.Cn_b));
text(0.7,.1,'Cn_P');			text(0.85,.1,num2str(results.Cn_P));
text(0.7,.05,'Cn_Q');           text(0.85,.05,num2str(results.Cn_Q));
text(0.7,.0,'Cn_R');			text(0.85,.0,num2str(results.Cn_R));
   
   
figure(9)

axis off
%grid on
text(0,1,'TORNADO CALCULATION RESULTS, Central difference, RUDDER DERIVs')
text(0,.95,'JID: '); text(0.25,0.95,data)

text(0,.90,'Reference area: ');	text(0.25,0.90,num2str(ref.S_ref));	
text(0,.85,'Reference chord: ');text(0.25,.85,num2str(ref.C_mac));
text(0,.8,'Reference span: ');text(0.25,.8,num2str(ref.b_ref));
   
text(0.4,.90,'\alpha: '); 	text(.55,.9,num2str(state.alpha*180/pi));
text(0.4,.85,'\beta: ');     text(.55,.85,num2str(state.betha*180/pi));
text(0.4,.8,'Airspeed: ');  text(.55,.8,num2str(state.AS));

text(0.65,.9,'P: ');  		text(.85,.9,num2str(state.P));
text(0.65,.85,'Q: '); 		text(.85,.85,num2str(state.Q));
text(0.65,.8,'R: ');  		text(.85,.8,num2str(state.R));
text(0.65,.75,'\alpha_{dot}: ');  text(.85,.75,num2str(state.adot*180/pi));
text(0.65,.70,'\beta_{dot}: ');  text(.85,.70,num2str(state.bdot*180/pi));

text(0,.45,'CL_{\delta}');			text(0.15,.45,num2str(results.CL_d'));
text(0,.05,'Cl_{\delta}');			text(0.15,.05,num2str(results.Cl_d'));
text(0.35,.45,'CD_{\delta}');		text(0.5,.45,num2str(results.CD_d'));
text(0.35,.05,'Cm_{\delta}');		text(0.5,.05,num2str(results.Cm_d'));
text(0.7,.45,'CY_{\delta}');		text(0.85,.45,num2str(results.CY_d'));
text(0.7,.05,'Cn_{\delta}');		text(0.85,.05,num2str(results.Cn_d'));




case 17
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available blunt body drag result files are: ')
        cd(settings.odir)
            dir *-blunt.mat
        cd(settings.hdir)
        data=questions(10);
   try
    cd(settings.odir)
	    fname=strcat(data,'-blunt');
	    load(fname)
        cd(settings.hdir)  
    catch
        cd(settings.hdir)
        terror(12)
    return
    end
    figure(31)
    axis off
    grid on
    text(0,1,'TORNADO Blunt body friction drag estimation.')
    text(0,.95,'JID: '); text(0.25,0.95,data)
    text(0,.90,'Reference area: ');	text(0.25,0.90,num2str( ref.S_ref ));	
    text(0,.85,'Reference chord: ');text(0.25,.85,num2str(ref.C_mac));
    text(0,.8,'Reference span: ');text(0.25,.8,num2str(ref.b_ref));
    
    %text(0.65,.95,'Re: ');  		text(.8,.95,num2str(round(results.Re)));
    text(0.65,.9,'CD_{0Body}: ');  		text(.8,.9,num2str(sum(sum(results.CD0_blunt))));
    %text(0.65,.85,'Swet: '); 		text(.8,.85,num2str(sum(sum(results.Swet))));
    %text(0.65,.8,'Volume: ');  		text(.8,.8,num2str(sum(sum(results.Vol))));
    
    		
	text(0.0,.60,'Body friction drag per part: ');		text(0.40,.6,sprintf('%0.4f\n',results.CD0_blunt));
    %text(0.0,.35,'Wetted area per part: ');		    text(0.35,.35,num2str(results.Swet));
    %text(0.0,.1,'Internal volume per part: ');		text(0.35,.1,num2str(results.Vol));

 
   case 18
        disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""');
        disp('Available static solution result files are: ')
        cd(settings.odir)
            dir *-Cx.mat
        cd(settings.hdir)
        data=questions(10);
        
        
    try
    cd(settings.odir)
	    fname=strcat(data,'-Cx');
	    load(fname)
        cd(settings.hdir)   
    catch
        cd(settings.hdir)
        terror(12)
        return
    end   
    
    Rudrs=sum(sum(geo.flapped));
    disp(strcat({'This geometry has '}, num2str(Rudrs), {' control effectors'}))
    Rnr=input('Hinge moment about which control effector?: ');
    data=fhingemoments((Rnr),results,lattice,geo,state);
    disp(' ')
    disp(' *********** ')
    disp(' ')
    disp(strcat({'Hinge moments are:    '},num2str(data.M),{'    [Nm]'}));
    disp(strcat({'Moment Coefficients:    '},num2str(data.coeff),{'    [-]'}));
   
    disp(' *********** ')

end%function
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [x,y,z]=midpoint(XYZ);

[a b c]=size(XYZ);

for i=1:a
   x(i)=((XYZ(i,1,1)+XYZ(i,2,1))*2+XYZ(i,3,1)+XYZ(i,4,1))/6;
   y(i)=((XYZ(i,1,2)+XYZ(i,2,2))+XYZ(i,3,2)+XYZ(i,4,2))/4;
   z(i)=((XYZ(i,1,3)+XYZ(i,2,3))+XYZ(i,3,3)+XYZ(i,4,3))/4;
end
end%function
