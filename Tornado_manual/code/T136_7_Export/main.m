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
% usage: [-] = MAIN (-)
%
% Invokes the User interface script of Tornado to allow 
% two-way communication with the core computational functions.
% The user interface itself if text based and allows the user
% to load and create a new aircraft geometry, flight condition 
% and generate the computational lattice (mesh). Additionally 
% the user may launch the solver in different modes and to rewiev
% the results.
%
% Example:
%
%  [void] = main (void);
%
% Calls:
%       splash          To display splash screen at startup.
%       questions       Contain user interface queries in string format. 
%       terror          Displays various Error messages.
%       inpt18          Load, Save, Create or Edit the geometry struct.
%       statesetup2     Load, Save, Create or Edit the state struct.
%       setrefpoint     Set the reference point position.
%       fLattice_setup  Generate the computational lattice.
%       solverloop5     Solver loop options.
%       postproc        Post processor options.
%       intro           Display code changes history
%       copyright       Display the copyright message
%
% Author: Tomas Melin <dr.tomas.melin@gmail.com.se>
% Keywords: Tornado text based user interface
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%   Bristol, 2007-06-27:  Changed main to function. TM
%   Spånga, 2021-09-19:   Updated to MATLAB R2020 R2020  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function main
clear
%splash                      %Display spash screen     
clear
%initializing variables
geo.version=136;            %version of geometry file
geo.name=[];                 %Geometry name string
geo.project=[];
geo.allmove=0;              %Allmoving surface set bit
geo.allmove_origin=[0 0 0]; %Origin of rotation for allmoving surface
geo.allmove_axis=[0 0 0];   %Hingeline for allmoving surface
geo.allmove_symetric=0;     %Symmetry for allmoving deflection
geo.allmove_def=0;          %Deflection of allmovin surface
geo.fnx=0;				    %number of panels on flap chords (2d array)
geo.ny=1;					%number of panels in span (2d array)
geo.nx=1;					%number of panels on chord (2d array)
geo.fsym=0;				    %flap deflection symmetry boolean bit  (2d array)
geo.fc=0;					%flap chord in percent of wingchord (2d array)
geo.flapped=0;			    %flapped partition(wing part) boolean bit (2d array)
geo.TW(:,:,1)=0;	    	%partition twist (3d array)<1 inboard, 2 outboard>
geo.TW(:,:,2)=0;
geo.foil(:,:,1)={'0'};		%Partition airfoils (3d array)	
geo.foil(:,:,2)={'0'};		%1 inboard, 2 outboard
geo.T=1;					%Taper ratio (2d array)
geo.SW=0;					%Sweep (2d array)
geo.c=1;					%Root chord (2d array)
geo.dihed=0;				%Dihedral (2d array)	
geo.b=1;					%span(distance root->tip chord) (2d array)
geo.symetric=1;			    %Wing symmetry boolean bit (2d array)
geo.startx=0;		    	%Partition starting coordinate (2d array)
geo.starty=0;		    	% ---"----
geo.startz=0;		    	% ---"----
geo.nwing=1;				%number of wings (scalar)
geo.nelem=1;				%number of partitions on each wing (1d array)
geo.flap_vector=0;          %Flap deflection vector 
geo.ref_point=[0 0 0];      %System reference point
geo.CG=[0 0 0];             %System center of gravity (around which all rotations occur)
geo.meshtype=0;
geo.structure=[];           %Internal structure of wing number 1;
geo.prop=[];                %propeller geometry

state.AS=0;					%airspeed
state.alpha=0;				%angle of attack
state.betha=0;				%angle of sideslip
state.P=0;					%roll angluar rate	
state.Q=0;					%pitch angular rate
state.R=0;					%Yaw angular rate
state.alphadot=0;           %Angle of attack time derivative
state.bethadot=0;           %Angle of sidesliptime derivative
state.ALT=0;                %Altitude, meters.
state.rho=0;                %Air density, kg/m^3.
state.pgcorr=0;             %Prandtl-Glauert compressibillity correction.

lattice.XYZ=0;				%panel corner matrix (2d array)
lattice.COLLOC=0;           %collocation point matrix
lattice.VORTEX=0;           %Vortex sling cornerpoint position matrix
lattice.N=0;                %Airfoil collocation point normal direction matrix

ref.S_ref=0;                %reference area;
ref.C_mac=0;                %mean aerodynamic choord
ref.mac_pos=0;              %start position of mac
ref.C_mgc=0;                %mean geometric chord
ref.b_ref=0;                %reference span

results.dwcond=0;           %computation result memory structure.    

loop=1;                     %program run bit

stat=0;                     %Status variable, 0 if geometry and lattice is inconsistent
	
while loop==1
   answ=questions(1);       %question string generator function
   if isempty(answ)
      answ=-1;
      terror(9)               %Error message string generator function
   end
   
   switch (answ)
   	  case 1
         %enter new geometry variables 
         [geo]=inpt19(geo);    %New input function called. TM20161123
         stat=0;
      case 2
         %Enter new state variables   
         [state]=statesetup2(state);
         stat=0;
         
         if exist('state.alphadot')
         else
             state.alphadot=0;
             state.bethadot=0;
         end
             
             
      case 3
         %Change a control surface deflection
         
         geo=setcontrol(geo);
         
%          disp(' ')
%          if sum(sum(geo.flapped'))>0
%               rudder=input('Change rudder number: [1..]: ');         
%               [n,m]=find(geo.flapped');
%               def=input('New absolute control deflection [deg]: ')*pi/180;
%               geo.flap_vector(m(rudder),n(rudder))=def;
%               disp(' Remember to regenerate lattice before solving!')
%               stat=0;
%          else
%              disp(' No trailing edge control surfaces in the current geometry.')
%          end
         
         
     case 4
         disp(' ')
         %disp(strcat('Current reference point: ',num2str(geo.ref_point)))
         [geo]=setrefpoint(ref,geo);
            
     case 5

         %try
            disp(' ')
            quest=questions(12);
            [lattice,ref]=fLattice_setup2(geo,state,quest);
            stat=1;
         %catch
         %   terror(23) 
         %end
            
      case 6
       
         if stat==1
              processor(results,lattice,state,geo,ref); %prepare run
         else
               terror(11) %Wrong lattice
         end
      case 7
      
      	postproc(lattice,geo,ref)
      
   	case 8
      	disp(' ');
      	disp('Keyboard access enabled. Return to program with command "dbcont" ');
      	disp(' ')
      	keyboard;
    case 9
       
       JID=input('\nEnter Job IDentity tag (JID): ','s'); %***
        if isempty(JID) %***
            JID=('trial'); %***
            disp(' ') %***
            disp(' JID defaulted to "trial" '); %***
            disp(' ') %***
        end %***

        revdrag14edit4(JID)  %*** 
           
           
           
      
       case 10
         questions(9);
         selected=input(' Enter choice from above please: ');
         
         if isempty(selected)
         else
            if selected==1
            	intro(0);
         	elseif selected==2
            	copyright(0);
            end
         end
         
         
       case 100
           questions(15)
           selected=input(' Enter choice from above please: ');
           
            if isempty(selected)
            else
                if selected==1
                    definitions; 
                end
            end
           
         
      case 0
   	   loop=0;
      
       case -1
       % donothing
         
      otherwise
         
   end
   
end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%INLINE FUNCTIONS
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function []=intro(~)
disp('******************************************************');
disp('*                                                    *');
disp('*              Welcome to Tornado 136.005            *');
disp('*                                                    *');
disp('*                  by: Tomas Melin                   *');
disp('*                Copyright 1999-2021                 *');
disp('******************************************************');
disp('*                                                    *')
disp('* Release 136.005   -  BETA RELEASE - 2021 08 13     *')
disp('* Changes since last revision:                       *')
disp('* Code cleanup                                       *')
disp('* Unsteady solver removed                            *')
disp('*                                                    *')
disp('* Release 136.004   -  BETA RELEASE - 2021 02 28     *')
disp('* Changes since last revision:                       *')
disp('* Code cleanup                                       *')
disp('*                                                    *')
disp('*                                                    *')
disp('*                                                    *')
disp('* Release 136.003   -  BETA RELEASE - 2020 06 30     *')
disp('* Changes since last revision:                       *')
disp('*                                                    *')
disp('* Code revision to fit Matlab2020                    *')
disp('* Rudder moment computation added in postprocessor   *')
disp('* Error in ruddersweep fixed                         *')
disp('* Menus changed                                      *')
disp('* Bending moment computation correcected             *')
disp('* Strip theory modified                              *')
disp('* General code cleanup                               *')
disp('* Menus changed                                      *')
disp('* Unsteady solver added                              *')
disp('* Strip theory added                                 *')   
disp('* Unsteady solver added                              *')
disp('* Find static margin function added                  *')
disp('* Falling back to slope2 in fcreate lattice          *')
disp('* Move cg to ac def                                  *')
disp('* Put allmoving in                                   *')
disp('* Move apex to wing def in inpt19                    *')
disp('* Reshuffeling input order                           *')
disp('* More input data sanity checks                      *')
disp('* Added geometry name to geo in inpt19               *')
disp('* Added project reference to geo in inpt19           *')
disp('* Added allmoving surface to geo in inpt19           *') 
disp('* Added geometry Version to geo in inpt19            *')
disp('* Bodyinput.m moved inline with inpt19.              *')
disp('* Changed text in rudder deflection to show abolute  *') 
disp('*  setting.                                          *')
disp('* Fixed geometry plotting from geometry edit menu    *')
disp('* Optional Update of old geometry files added.       *')

%
disp('* Release 135.005      2008-11-17                    *')
disp('* Release 135.004      2008-09-26                    *')
disp('* Work in progress release                           *')
disp('* DO NOT DISTRIBUTE                                  *')
disp('*                                                    *')
disp('* Release 135.003      2008-04-28                    *')
disp('* Work in progress release                           *')
disp('* DO NOT DISTRIBUTE                                  *')

disp('*                                                    *')
disp('* Release 135.002      2008-03-20                    *')
disp('* Work in progress release                           *')
disp('* DO NOT DISTRIBUTE                                  *')

disp('*                                                    *')
disp('* Release 134 Beta      2007-11-15                   *')
disp('* Change directory command changed to functional form*')
disp('* Variable directory names added in config.m         *')
disp('* Aircraft trim function added                       *')

disp('*                                                    *')
disp('* Release 133, never released to public (2007-11-14) *')
disp('* New solver added, vectorised 6 times faster        *')
disp('* Symmetric solver removed                           *')
disp('* Simple solution now computes derivatives as well   *')
disp('* Zero lift drag prediction added                    *')
disp('* Definition plots added                             *')
disp('* Result data exporter added                         *')
disp('*                                                    *')


disp('* Release 1.32      2007-08-02                       *')
disp('* Rotation in boundary condition error fixed         *')
disp('* New Camber model added, more accurate than before  *')
disp('* Viscosity added to ISA                             *')
disp('* Subfunctions moved inline                          *')
disp('* Symmetric solver added.                            *')
disp('* Editor fixed to vork with new foil standad         *')
disp('* Batch function added.                              *')
disp('*                                                    *')

disp('* Release 1.31 Beta 2007-02-06                       *')
disp('* Trefftz plane analysis added                       *')
disp('* More airfoils added to library                     *')
disp('* Geo.foil chage type from double to cell            *')
disp('* Splash screen added                                *')
disp('* The way that geometry18 calls slope2 is changed    *')
disp('*                                                    *')

disp('* Release 1.30 Beta 2005-12-04                       *')
disp('* Wind alignment modified                            *')
disp('*                                                    *')

disp('* Release 1.29 Beta 2005-11-04                       *')
disp('* Wake alignment fixed                               *')
disp('* Normal position on panel fixed                     *')
disp('* Wind to body transform modified                    *')
disp('* Sideslip computation modified                      *')
disp('* Standard (Horseshoe) VL method added               *')
disp('*                                                    *')

disp('* Release 1.28 Beta 2004-11-04                       *')
disp('* Rudder setting problem fixed                       *')


disp('* Release 1.27 Beta 2004-07-20                       *')
disp('*  Mop up job after memory structure change.         *')

disp('* Release 1.26 Beta 2004-03-07                       *')
disp('*  Memory structure changed                          *')
disp('*  geometry, state, lattice and result variables     *')
disp('*  moved to structures                               *')
disp('*  major changes in all functions                    *')


disp('*  Old gromertry files obsolete                      *')
disp('*  Geometry file conversion function added           *')


disp('*                                                    *')
disp('* Release 1.25 Beta 2004-01-21                       *')
disp('*  Not distributed                                   *')
disp('*                                                    *')


disp('* Release 1.24 Beta 2003-08-27                       *')
disp('*  More spell-check                                  *')
disp('*  result plotting functions made as one fcn         *')
disp('*  Sideforce computation corrected                   *')
disp('*  flapped_vector format corrected                   *')
disp('*                                                    *')


disp('* Release 1.23 Beta 2002-10-09                       *')
disp('*  Reference point position changed to geometry      *')
disp('*  property.                                         *')
disp('*  Downwash matrix condition computed in solver6     *')
disp('*  Input function shanged to accommodate new variable*')
disp('*  Diffbatch changed to accommodate new variables.   *')
disp('*  fOld2New function created to translate old        *')
disp('*  geometry files into new.                          *')
disp('*  fLattice_setup created. Its a Wrapper for         *')
disp('*  geosetup and wake stetup etc.                     *')
disp('*                                                    *')
disp('*                                                    *')

disp('* Release 1.22 Beta 2002-10-01                       *')
disp('*  Rudder setting changed to geometry property       *')
disp('*  Current rudder setting displayed in result plot   *')
disp('*  Input error for profile taper fixed               *')
disp('*  continious camber error fixed                     *')
disp('*  Note: New variable in geometry file: flap_setting *')
disp('*                                                    *')
disp('*                                                    *')
disp('* Release 1.21 Beta 2002-08-07                       *')
disp('*  Comments added,                                   *')
disp('*  Code cleanup                                      *')
disp('*                                                    *')

disp('* Release 1.20 Beta 2002-02-10                       *')
disp('*  Fixes since last revision                         *')
disp('*  New Cmac computation added                        *')
disp('*  Grapical C_mac and refpoint added                 *')
disp('*  Spanload computation and graph added              *')
disp('*  Beta sign inconsistency corrected                 *')
disp('*  Wake roll inconsistency corrected                 *')
disp('*  Wake length edited.					           *')
disp('*  NACA profile slope error corrected                *')
disp('*  Refpoint move to MAC added                        *')
disp('*  Input questions fixed for flapped assymetric wings*')
disp('*  Mean geometric chord added (not used yet)         *')
disp('*                                                    *')

disp('* Release 1.19 Beta 2001-11-07                       *')
disp('*  Fixes since last revision                         *')
disp('*  Geometry plot sped up                             *')
disp('*                                                    *')


disp('* Release 1.18 Beta 2001-11-05                       *')
disp('*  Fixes since last revision                         *')
disp('*  rudder sweep misarrayment fixed                   *')
disp('*  Main function picture removed                     *')


disp('*                                                    *')
disp('* Release 1.17 Beta 2001-10-18                       *')
disp('*  Fixes since last revision                         *')
disp('*  Hotfix for matlab6                                *')
disp('*                                                    *')
disp('* Release 1.16 Beta 2001-10-12                       *')
disp('*  Fixes since last revision                         *')
disp('*  Plotting functions enabeled for old jobs.         *')
disp('*  Job IDentifiers (JID) added.                      *')
disp('*  More spellcheck                                   *')
disp('*  Vorticity plot removed.                           *')
disp('*  Output files with variable names enabeled.        *')
disp('*                                                    *')
disp('* Release 1.15 Beta 2001-10-11                       *')
disp('*  Fixes since last revision                         *')
disp('*  State input stabilised.                           *')
disp('*  State save change according to geometry.          *')
disp('*                                                    *')
disp('* Release 1.14 Beta 2001-10-10                       *')
disp('*  Fixes since last revision                         *')
disp('*  Geomtry input stabilised                          *')
disp('*  Meshing added as separate function.               *')
disp('*  Main menu changed                                 *')
disp('*  Minor changes in output plots.                    *')
disp('*                                                    *')
disp('* Release 1.13 Alpha 2001-01-15                      *')
disp('*  Fixes since last revision                         *')
disp('*  Changes in solver 4, to handle matrices better.   *')
disp('*  Solver loop, extra input variable to handle case  *')
disp('*  calls.                                            *')
disp('*  Diffbatch, now works through solverloop.          *')
disp('*                                                    *')
disp('* Release 1.13 Beta 2000-12-5                        *')
disp('*  Fixes since last revision                         *')
disp('*	 Error in sideslip BC corrected                   *')
disp('*   Error in solverloop, variable resetting -        *')
disp('*   corrected                                        *')
disp('*   S_ref & panel Area now computed for first wing   *')
disp('*                                                    *')
disp('* Release 1.12 Beta 2000-10-22                       *')
disp('*  Fixes since last revision                         *')
disp('*	 minor variable name changes                      *')                                                    
disp('*                                                    *')
disp('* Release 1.11 Beta 2000-10-10                       *')
disp('*  Fixes since last revision                         *')
disp('*	 in inpt11.m: interface cleaned up                *')                                                    
disp('*   in several functions: Spelcheck ;-)              *')                                                  
disp('*                                                    *')                                                     
disp('*                                                    *')
disp('* Release 1.10 Beta 2000-10-02                       *')
disp('*  Fixes since last revision                         *')
disp('*	divisions change name to partitions for better    *')                                                    
disp('*  clearity.                                         *')                                                  
disp('*  Bug in setrudder fixed for multiple wing AC       *')                                                     
disp('*                                                    *')
disp('*                                                    *')
disp('* Release 1.09 Beta 2000-09-17                       *')
disp('*                                                    *')
disp('*  Fixes since last revision                         *')		                                                  
disp('*     Directorys decapitated Unix users.             *')
disp('*     interface cleaned up/stabilized                *')                                                
disp('*                                                    *')
disp('*                                                    *')
disp('* Release 1.08 Beta 2000-08-10                       *')
disp('*                                                    *')
disp('* Fixes since release 1.07                           *')
disp('*  in inpt11                                         *')
disp('*    old wing features deleted before new added      *')
disp('*    -prventing aliasing of older designs            *')
disp('*                                                    *')
disp('*  copyright.m                                       *')
disp('*     copyright information added with this function *')
disp('*                                                    *')
disp('* Release 1.07 Beta 2000-07-28                       *')
disp('*                                                    *')
disp('* Fixes since release 1.06                           *')
disp('*                                                    *')
disp('*  In main                                           *')
disp('*    terror call corrected  (error -> terror)        *')
disp('*    Main menu changed                               *')
disp('*                                                    *')
disp('*  In Wakesetup                                      *')
disp('*    Length of wake moved to config                  *')
disp('*                                                    *')
disp('*  In geoinput                                       *')
disp('*    All property vectors made to the same length    *')
disp('*    -Preventing out of index crash in geosetup      *')
disp('*                                                    *')
disp('*  In tedit                                          *')
disp('*    Remove division made operational                *')
disp('*                                                    *')
disp('* Release 1.06 Beta 2000-06-28                       *')
disp('*                                                    *')
disp('* Fixes since release 1.05                           *')
disp('*                                                    *')
disp('* In Solver                                          *')
disp('*   Induced drag now calculated correctly            *')
disp('*                                                    *')
disp('* New function "config.m"                            *')
disp('*   Edit this function to enable static              *')
disp('*   reference variables, s.a. S_ref, rho, C_mac      *')
disp('*                                                    *')
disp('* In geosetup                                        *')
disp('*    b_ref calculation corrected for symmetric wings *')
disp('*                                                    *')
disp('* In rplot             	                           *')
disp('*    figure 5 edited to show z-component             *')
disp('*    of every panel load                             *')
disp('*                                                    *')
disp('* Fixes since relese 1.04                            *')
disp('*                                                    *')
disp('* In Dnwash                                          *')
disp('*      bug-parasite line erased                      *')
disp('* In rplot                                           *')
disp('*      Handling of b_ref fixed                       *')
disp('*                                                    *')
disp('* Fixes since release 1.03                           *');
disp('*                                                    *')
disp('* In solverloop,                                     *');
disp('*     delta deflection in  [deg] rather than [deg/s] *');
disp('*     Ruddersweep corrected                          *');
disp('*     Title added on sweep plots                     *');
disp('*     Row reduction                                  *');
disp('*     Taylor expansion fixed                         *');
disp('* In MAIN                                            *');
disp('*     b_ref handling fixed according to S_ref        *');
disp('* Specfun                                            *');
disp('*     edited for 3-dimensions and batch              *');
disp('* Taylorbatch                                        *');
disp('*     fixed for new function calls                   *');
disp('*     renamed to diffbatch (more appropriate)        *');
disp('* dnwash                                             *');
disp('*     functions tcros & tnorm sped innermost         *');
disp('*     loop up with 25 percent                        *');
disp('*                                                    *');
disp('*                                                    *');
disp('*                                  Happy Hacking  /T *');
disp('******************************************************');
disp('*                                                    *');
disp('* Tornado, Copyright (C) 2000, Tomas Melin           *');
disp('* Tornado comes with ABSOLUTELY NO WARRANTY;         *');
disp('*                                                    *');
disp('* This is free software,and you are welcome to       *');
disp('* redistribute it under certain conditions;          *');
disp('* see Release Info for details.                      *');                                               
disp('*                                                    *');   
disp('******************************************************');
disp(' ');
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [output]=copyright(~)
disp(' ');
disp('**************************************************************')
disp(' ');
disp('Tornado, a Vortex lattice program for conceptual aircraft design.');
disp(' ');
disp('    Copyright (C) 2000  Tomas Melin');
disp(' ');
disp('    This program is free software; you can redistribute it and/or modify');
disp('    it under the terms of the GNU General Public License as published by');
disp('    the Free Software Foundation; either version 2 of the License, or');
disp('    (at your option) any later version.');
disp(' ');
disp('    This program is distributed in the hope that it will be useful,');
disp('    but WITHOUT ANY WARRANTY; without even the implied warranty of');
disp('    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
disp('    GNU General Public License for more details.'); 
disp(' ');
disp('    You should have received a copy of the GNU General Public License');
disp('    along with this program; if not, write to the Free Software');
disp('    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA');
disp(' ');
disp('	The Author can be reached by: ');
disp('	E-mail:	dr.tomas.melin@gmail.com');
disp(' ');
disp('**************************************************************')
output=0;
end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function[geo]=setrefpoint(ref,geo)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% usage: [REF_POINT] = setrefpoint (REF_POINT,REF)
%
% Allows the user to manipulate the reference point position through
% a simple interface. The [x,y,z] position of the REF_POINT is either set
% directly, or as a percentage of the MAC in ther structure REF (Ref.C_mac)
%
% Example:
%
%  [ref_point]=setrefpoint(ref_point,ref);
%
% Calls:
%       questions       Contain user interface queries in string
%                       format. 
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado text based user interface
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
no=questions(5);
if isempty(no)
   no=4;
   error(9)
end
disp(' ');
switch no
	case 1
   	%move in x
   	geo.ref_point(1)=input('Move reference point to x= 	');
    
	case 2
   	%move in y
	geo.ref_point(2)=input('Move reference point to y=	');
   	
	case 3
  		%move in z
  	 	geo.ref_point(3)=input('Move reference point z= 	');
    case 4
      %Move ref point to percentage of MAC
      point=input('Move reference point to percent of MAC  [ % ] : 	');
      geo.ref_point=ref.mac_pos+[point*ref.C_mac/100 0 0];
     
    case 5
      %Move GC point to percentage of MAC
      point=input('Move CG point to percent of MAC  [ % ] : 	');
      geo.CG=ref.mac_pos+[point*ref.C_mac/100 0 0];
     
	otherwise
end
end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function splash
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% usage: [-] =  SPLASH(-)
%
% Displays the splash screen on startup of Tornado. This
% Should usually contain the latest development info and 
% Important code changes. The image is displayed in figure
% 100.
%
% Example:
%
%  [void] = splash (void);
%
% Calls:
%       none
%
% Author: Tomas Melin <dr.tomas.melin@gmail.se>
% Keywords: Splash screen.
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header.   TM
%                         Made into a function.     TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clear
A=imread('splash.jpg');
h=figure(100);
set(gca,'Position',[0 0 1 1])
set(h,'ToolBar','none');
set(h,'MenuBar','none');
set(h,'Color','White');
set(h,'Name','Tornado startup splash screen.');
axis off
image(A);
axis off
end%function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
