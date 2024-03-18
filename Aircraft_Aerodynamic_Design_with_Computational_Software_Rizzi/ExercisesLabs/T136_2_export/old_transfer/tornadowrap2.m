function [results,ref]=tornadowrap2(geo,state);
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
%% INITIALIZING
%Computation start
settings=config('startup');
results=[];             % Initializing results struct.
lattictype=1;           % Standard VLM, the faster version.
%lattictype=0;          % Tornado freestream following wake VLM.
JID=('batchjob');       % Filename for the saved data
jobtype=17;             % Run the unsteady calcs   

try state.adot;         % Check if the state struct has the unsteady data 
catch
    state.adot=0;       % if not, ad them as zeros.
    state.bdot=0;
end

%% Computing
[lattice,ref]=fLattice_setup2(geo,state,lattictype);    %Setting up the lattice
solverloop5(results,jobtype,JID,lattice,state,geo,ref);      %Solving

fname=strcat(JID,'-Cxunst');                            %catenating filename ending to make 
cd(settings.odir)                                       %sure we get the unsteady file    
   	load(fname)                                         %load unsteady data
cd(settings.hdir)

[void a void void]=ISAtmosphere(state.ALT);     %Calling International Standard atmosphere.
Mach=state.AS/a;                                %Computing local Mach no
[results.CD0 results.Re results.Swet results.Vol]...
    =zeroliftdragpred(Mach,state.ALT,geo,ref);  %Computing zero lift drag with Ekerts equation.

A=fViscCorr2(geo,state,lattice,results,ref);    %Strip theory viscous drag.
results.CDv=A.totalvdragcoeff;                  %

end



