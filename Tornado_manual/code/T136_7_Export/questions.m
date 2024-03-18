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
% usage: [output] = questions (A)
%
% This function displays the user interface texts and collects
% input data, which is a selection from a list of tasks. A is the number
% of the menu to be displayed.
%
% Example:
%
%  answ=questions(1);       %question string generator function
%   switch (answ)
%       case 1
%           %enter new geometry variables 
%           [geo]=inpt18(geo);    
%           stat=0;
%       case 2
%           %Enter new state variables   
%           [state]=statesetup2(state);
%           stat=0;   
%       Otherwise
%           return
%       end
%
% Calls:
%           none
%
% Author:   Tomas Melin <melin@kth.se>
% Keywords: Tornado text based user interface
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%   Spånga, 2021-09-19:   Updated to MATLAB R2020, TM  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [output]=questions(no)


switch no
case 1
  	disp('______________________________________________________')
	disp('  TORNADO  Version 136.007 Beta version               ')
    disp('  build 2021 12 10                                    ')
	disp('  Main Menu                                           ')
	disp('______________________________________________________')
   disp(' ')
   disp(' Input operations. ');
   disp('	[1]. Aircraft geometry setup')
   disp('	[2]. Flight condition setup')
   disp('	[3]. Change rudder setting')
   disp('	[4]. Move reference point ')
   disp(' ')
   disp(' Lattice operations. ');
   disp('	[5]. Generate lattice. ')
   disp('	     ')
   disp(' Computation operations.')
   disp(' 	[6]. Processor access')
   disp(' ')
   disp(' Post processing and interactive operations.')
   disp('	[7]. Post processing, Result/Plot functions')
   disp('	[8]. Keyboard access')
   disp(' ')
   disp(' Auxiliary operations.')
   disp('   [10]. About / Release Info')
   disp('   [100]. Help files')
   disp('	[0]. Exit Tornado')
	disp(' ')
	output=input('	Please enter choice from above: ');
case 2
   disp(' ')
	disp('______________________________________________________')
	disp('  Main Menu                                         ')
   disp('      |---->Geometry setup menu                     ')
   disp('                                                    ')
	disp('______________________________________________________')

   disp(' ')
   disp('	[1]. Define new geometry ')   
   disp('	[2]. Load geometry')
   disp('	[3]. Edit current geometry')
   disp('	[4]. Save current geometry')
   disp(' ')
   if config('expert')
        disp('    [5]. Define blunt body data (for friction drag est).')
        disp('    [6]. Define wing internal structure (for aeroelasticity).')
        disp('    [7]. Define propeller geometry (for slipstream estimation ).')
   end
   disp(' ')
   disp('	[0]. Back / up menu')
   disp(' ')
   output=input(' 	Please enter choice from above: ');
case 3
   %Orphaned case
   disp(' ')
	disp('______________________________________________________')
	disp(' ')
	disp(' ')
   disp('	[1]. Save data to disk.')   
   disp('	[2]. Continue / up menu. ')
   disp(' ')
	output=input('	Please enter choice from above: ');
case 4
   disp(' ')
	disp('______________________________________________________')
   disp('                                                    ')
   disp('  Main Menu                                         ')
   disp('      |---->State setup menu                        ')
   disp('                                                    ')
	disp('______________________________________________________')
	disp(' ')
	disp('	[1]. Define new state ')   
    disp('	[2]. Load state')
    disp('    [3]. Save current state')
    disp(' ')
    disp('    [4]. Change angle of attack')
	disp(' ')
	disp('	[0]. Back / up menu')
	disp(' ')
	output=input(' Please enter choice from above: ');

case 5
   disp(' ')
	disp('______________________________________________________')
   disp('                                                    ')
   disp('  Main Menu                                         ')
   disp('      |----->Move reference point                   ')
   disp('                                                    ')
	disp('______________________________________________________')
	disp(' ')
	disp('	[1]. Move reference point in x')   
	disp('	[2]. Move reference point in y')
	disp('	[3]. Move reference point in z')
   disp(' ')
   disp('	[4]. Move reference point to position on MAC')
   disp(' ')
   disp('	[5]. Move center of gravity to position on MAC')

   disp(' ')
	disp('	[0]. Cancel')
	disp(' ')
	output=input(' Please enter choice from above: ');
case 6
   disp(' ')
	disp('______________________________________________________')
   disp('                                                    ')
   disp('  Main Menu                                         ')
   disp('      |---->Geometry setup menu                     ')
   disp('                    |---------->Geometry editor menu')
   disp('                                                    ')
	disp('______________________________________________________')
   disp(' ')
   disp('[1] Add Wing')
   disp('[2] Remove Wing')
   disp(' ')
   disp('[3] Add partition to a wing')
   disp('[4] Remove partition from a wing')
   disp(' ')
   disp('[5] View wing data')
   disp(' ')
   disp('[6] Edit wing/partition data')   
   disp(' ')
   disp('[7] Plot Geometry')
   disp(' ')
   disp('[0] Back / up menu')
   disp(' ')
   output=input(' Please enter choice from above: ');
case 7

   
case(9)
   disp(' ')
	disp('______________________________________________________')
	disp('                                                     ')
   disp('  Release info / Copyright info                       ')
   disp('                                                      ')
	disp('______________________________________________________')
   disp(' ')
   disp(' [1]. Release info ')
	disp(' [2]. Copyright info ')
	disp(' ')
	disp(' [0]. Back / up menu')
   disp(' ')
   
case(10)
   disp(' ')
	disp('______________________________________________________')
   disp(' ')
   output=input(' Enter JID to plot: ','s');
   disp(' ')
 
    
    
 case(11)    
             disp(' ')
             disp('******************')        
             disp('Enter which type of speed you whish to enter: ')
             disp(' ')
             disp(' International units:')
             disp(' ')
             disp(' [1]. True airspeed (TAS) at SSL             [m/s]')
             disp(' [2]. True airspeed (TAS) at altitude        [m/s, m]')
             disp(' [3]. Equivalent airspeed (EAS) at altitude  [m, m/s]')
             disp(' [4]. Calibrated air speed (CAS) at altitude [m, m/s] ')
             disp(' [5]. Mach number at altitude                [-, m] ')
             disp(' ')
             disp(' Imperial Units:')
             disp(' ')
             disp(' [6]. True airspeed (TAS) at altitude        [kts, ft]')
             disp(' [7]. Equivalent airspeed (EAS) at altitude  [kts, ft]')
             disp(' [8]. Calibrated air speed (CAS) at altitude [kts, ft] ')
             disp(' [9]. Mach number at altitude                [-, ft] ')
             disp(' ')
             disp(' ')
             disp('         If unsure, select option number one.')
             disp(' ')
             output=input('       Type of speed selection: '); 
    
    
    case(12)
        disp('******************')        
        disp('Which type of method to use: ')
        disp(' ')
        disp('      [0] = Freestream following wake, Tornado method')
        disp('      [1] = Fixed wake, standard VLM method')
        disp('  ')
        output=input('       Type of lattice selection: ');
    
    case(13) 
        output=1;
        
    case(14)
        disp(' ')
        disp('Available sequenses: ')
        disp('[2]. Alpha sweep')
        disp('[3]. Beta sweep')
        disp('[4]. Delta sweep (control surface deflection.) ')
        disp('[5]. Roll rate sweep  ')
        disp('[6]. Pitch rate sweep  ')
        disp('[7]. Yaw rate sweep ')
        output=input('       Sweep parameter: ');
        
        
    case(15)
        disp('___________________________________')
        disp(' ')
        disp(' [1]. Aero derivative definitions.')
        disp(' ')
        disp(' [0].  Back / up menu')
    
    case(16)
        
      
    
    case 17
   disp(' ')
	disp('______________________________________________________')
   disp('                                                    ')
   disp(' Main Menu                                          ')
   disp('     |---->Main processor menu                      ')
   disp('                                                    ')
	disp('______________________________________________________')
   disp('Low order solutions: ')
   disp('   [1]. Static computation at selected state. ') 
   disp('   [2]. Sequential state parameter sweep menu:      ')
   disp(' ')
   disp('High order methods:')
   disp('   [3]. Trimmed aircraft polar point. ')
   disp('   [4]. Trimmed pitch sweep, polar.')
   
   disp('   [5]. [Unused]')
   disp('   [6]. [Unused]')
   
   %disp('   [5]. Unsteady, acceleration free, time coefficients only.')
   %disp('   [6]. Unsteady, acceleration free, all inviscous coefficients.')
   
   disp(' ')
   disp('Auxillary operations:')
   disp('   [7]. Viscous Drag Estimation Methods Menu:')
   disp('   [8]. Grid convergence study.')
   disp('   [9]. Find stall angle of attack.')
   disp('   [10]. Find alpha at prescribed CL.')
   disp('   [11]. Compute static margin.')
   %disp('   [12]. Find flow field direction at engine inlet.')
   disp(' ')
   disp('Aeroelastic operations ')
   disp('   [13]. Aeroelastic submenu.')
   disp(' ')
   disp(' ') 

   disp('   [0]. Cancel / up menu. ')
   disp(' ')
    
    case(18)
        disp(' ')
        disp('Available sequenses: ')
        disp('  [1]. Alpha sweep')
        disp('  [2]. Beta sweep')
        disp('  [3]. Delta sweep (control surface deflection.) ')
        disp('  [4]. Roll rate sweep  ')
        disp('  [5]. Pitch rate sweep  ')
        disp('  [6]. Yaw rate sweep ')
        disp(' ')
        disp('   [0]. Cancel / up menu. ')
 
        output=input('       Sweep parameter: ');

   case(19)
   disp('   [1]. Zero lift drag prediction, Flat plate approximation. ') 
   disp('   [2]. Strip theory viscous correction, inline computed.')
   disp('   [3]. Strip theory viscous correction, database data.')
   disp('   [4]. Blunt body drag, Eckerts equation.')
   disp(' ')
   disp('   [0]. Cancel / up menu. ')
   disp(' ')
   output=input('       Select function: ');
   
   
   case(20)
    disp(' ')
	disp('______________________________________________________')
   disp('                                                    ')
   disp('  Main Menu                                         ')
   disp('      |---->Tornado post processing functions       ')
	disp('______________________________________________________')

	disp(' ')
	disp('  [1].  Clear plots')
	disp('  [2].  Geometry plot')
    disp(' ')
    disp('Solution plots') 
	disp('  [3].  Static state')     
    disp('  [4].  Parameter sweep sub menu')
    disp('  [5].  [Unused]')
    disp('  [6].  [Unused]')
    
  
    %disp('  [5].  Unsteady state, time coefficients only')
    %disp('  [6].  Unsteady state, all coefficients')
    
    disp(' ')
    disp('Viscous drag estimation plots')   
    disp('  [7].  Plot wing system zero lift drag estimation')
    disp('  [8].  Plot body friction drag estimation')
    disp(' ')
    disp('Post processing computations')
    disp('  [9].  Perform a trefftz plane analysis, (experimental, only for freestream wake)') 
    disp('  [10]. Export simple state results to textfile')
    disp('  [11]. Compute hinge moment on a TE control effector. (Experimental)')
    disp(' ')
	disp(' [0].   Back / up menu')
    disp(' ')
    output=input(' Please enter choice from above: ');
   
    case 21
    disp(' ')
    disp('Solution plots, parameter sweep.')
    disp('      [1].  Alpha')
	disp('      [2].  Beta ')
	disp('      [3].  Roll rate, P ')
	disp('      [4].  Pitch rate, Q')
    disp('      [5].  Yaw rate, R')
    disp(' ')
    disp('      [6].  Delta, control surface deflection')
    disp(' ')
    output=input(' Please enter choice from above: ');
   
    case(22)
    disp(' ')
	disp('______________________________________________________')
    disp('                                                      ')
    disp(' Main Menu                                            ')
    disp('     |---->Main processor menu                        ')
    disp('                |------> Aeroelastic functions        ')
	disp('______________________________________________________')
     
    disp(' ')
    disp(' Caution, under development. Only main wing elastics so far. ')
    disp(' ')
    
    disp('      [1].  Define new wing structure')
    disp('      [2].  LOAD wing structure')
    disp('      [3].  SAVE wing structure')
    disp(' ')
    disp('      [5].  Plot wing structure (also generates mesh.) ')
    disp(' ')
	
	disp('  Available Loadcases   ')
    disp('      [6].  Quickshot deflections. Loadcase from state.  ')
    disp('      [7].  2.5 g pullup non iterative          [experimental] ')
    disp(' ')
    disp('      [8].  Variable load, pullup ITERATIVE   [experimental]  ')
	disp('      [9].  Not available in this version  ')
    disp('  ')    
    disp(' ')
    disp('      [88].  Keyboard access')
    disp(' ')
    disp('      [0].  Exit, upmenu')
    disp(' ')
    output=input(' Please enter choice from above: ');
        
     
        
    otherwise
      
end
