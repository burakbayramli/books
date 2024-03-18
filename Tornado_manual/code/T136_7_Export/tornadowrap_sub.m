function [results,ref,out]=tornadowrap_sub(geo,state);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 2007 Tomas Melin
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
% usage: [results,ref] =  tornadowrap(geo,state)
%
% Calls Tornado main functions to compute a result file.
% First call is to the vortex lattice function, and then to a zero lift
% drag estimator function (Eckerts equation), which also computes wetted
% area and internal volume.
%
%%
% RESULTS: A struct containing a lot of variables :            												
%
% Solver matix condition: dwcond
% Forces & moments:       F, FORCE, M, MOMENTS, L, D, C 
% Vortex strengths:       gamma 
% Pressure Distribution:  cp 
% Coefficients:           CL    CD    CC    CX    CY    CZ    Cl    Cm    Cn 
%
% Alpha Derivatives:      CL_a  CD_a  CC_a  CX_a  CY_a  CZ_a  Cl_a  Cm_a  Cn_a
% Beta Derivatives:       CL_b  CD_b  CC_b  CX_b  CY_b  CZ_b  Cl_b  Cm_b  Cn_b
% Roll Rate Derivs:       CL_P  CD_P  CC_P  CX_P  CY_P  CZ_P  Cl_P  Cm_P  Cn_P
% Pitch Rate Derivs:      CL_Q  CD_Q  CC_Q  CX_Q  CY_Q  CZ_Q  Cl_Q  Cm_Q  Cn_Q 
% Yaw  rate Derivs;       CL_R  CD_R  CC_R  CX_R  CY_R  CZ_R  Cl_R  Cm_R  Cn_R 
% Rudder derivs:          CL_d  CD_d  CC_d  CX_d  CY_d  CZ_d  Cl_d  Cm_d  Cn_d 
% 
% Load distribution:      ystation  ForcePerMeter CL_local
%
% Zero lift drag:         CD0  Re  Swet  Vol
%
%%
% GEO: A struct describing the aircraft geometry, Tornado standard
%
% flapped, nwing, nelem, CG , ref_point , symetric , startx , starty , 
% startz , c , foil, nx , TW , dihed , ny , b , T , SW , meshtype, fc, 
% fnx , fsym , flap_vector %
%
%%
% STATE: A Struct describing the aircraft state
% AS, alpha, betha, P, Q, R, alpha_dot, beta_dot, ALT, rho, pgcorr										
%
%%
% REF: A struct describing which reference values was used in the
% computation.
%   b_ref, S_ref, C_mgc, C_mac, mac_pos
%
%%
% Example:
%   
%     geo=geometry_translation_funtion_qcard_2_tornado(geo);
%     state=state_translation_function_qcard_2_tornado(state);
%     
%     [results]=tornadowrap(geo,state);
%
%     results=result_translation_function_tornado_2_qcard(results)
%
%%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Vortex Lattice Module
%
% Revision History:
%   Bristol, 2008-01-17:  File Created   TM
%              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISCUSSION:
% THis is the place to discuss the file while we work on it. Please sign your
% entries with time and date stamp.
%
% TM, 2008-01-17: Should I put in some more descriptors on the ingoing
% variables?
%   
% TM, 2008-03-18: Added unsteady data to the state vector.
%
%
%
%
%
%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INITIALIZE

settings=config('startup');
results=[];     %Initializing results struct.
lattictype=1;   %Standard VLM, the faster version.
%lattictype=0;  %Tornado freestream following wake VLM

%% Check input
if ischar(geo)
    cd(settings.acdir)                                     
        load(geo)                                         %load aircraft file
    cd(settings.hdir)  
end

if ischar(state)
    cd(settings.sdir)                                     %sure we get the unsteady file    
        load(state)                                         %load aircraft file
    cd(settings.hdir)  
end


%
geo.starty(1)=0;
geo.TW(1,1,1)=0.01;
geo.TW(1,1,2)=0.01;
geo.meshtype(2,:)=1;
geo.meshtype(1,1)=1;
geo.ny(2,6)=1;
geo.ny(2,7)=1;


geo.dihed(2,:)=-geo.dihed(2,:);
geo.dihed(3,:)=-geo.dihed(3,:);
geo.dihed(4,:)=-geo.dihed(4,:);

geo.b(2:4,:)=0.261052384583333;   %Original has arclength....*sigh*

geo.nx(1,1)=10;
geo.nx(2,:)=10;


% %Computation start
% [lattice,ref]=fLattice_setup2(geo,state,lattictype);    %Setting up the lattice
% [results]=solver9(results,state,geo,lattice,ref);       %Solving for forces
% [results]=coeff_create3(results,lattice,state,ref,geo); %computing coefficients
% 
% [void a void void]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
% Mach=state.AS/a;                                %Computing local Mach no
% [results.CD0 results.Re results.Swet results.Vol]...
%     =zeroliftdragpred(Mach,state.ALT,geo,ref);  %Computing zero lift drag with Ekerts equation. 
% 
% 
% 
% figure(4)% 	Delta cp plot
% rotate3d on 
% colormap(hot);
% fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.cp')
% title('Delta cp distribution')
% colorbar('vert')
% axis equal



for i=1:10
    
    
   
    r=0.5*i^1.5;
    %r=9;
    
    
    panelspan=r*pi/12;
    
    panels=ceil(5*panelspan);

 
 geo.b(2:4,:)=0.261052384583333*r;   %Original has arclength....*sigh*
 geo.ny(2:4,6:7)=panels;
 
 geo.ny(2,7)=panels;
 geo.starty(2)=-r;
 geo.startz(2)=r;


 geo.starty(3)=-r;
 geo.startz(3)=r;
 

 geo.starty(4)=-r;
 geo.startz(4)=r;
    

[lattice,ref]=fLattice_setup2(geo,state,lattictype);    %Setting up the lattice
[results]=solver9(results,state,geo,lattice,ref);       %Solving for forces
[results]=coeff_create3(results,lattice,state,ref,geo); %computing coefficients
CL(i)=results.CL;
Cm(i)=results.Cm;
CD(i)=results.CD;

out.CLa(i)=CL(i).*100;
out.R(i)=r;


figure(1)
hold on
plot(r,CL(i)*100,'*')
drawnow


 figure(4)% 	Delta cp plot
 

 rotate3d on 
 colormap(hot);
 h=fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.cp');
 set(h(:),'Linestyle',':')
 view([-1 1 1])
 title('Delta cp distribution')
 colorbar('vert')
 axis equal

pause(1)


end

%keyboard
%pause


end
