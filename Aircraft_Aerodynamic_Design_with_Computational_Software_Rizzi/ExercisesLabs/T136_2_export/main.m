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
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado text based user interface
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%   Bristol, 2007-06-27:  Changed main to function. TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%function main
clear
%splash                      %Display spash screen     
clear
%initializing variables
geo.version=136;            %version of geometry file
geo.allmove=0;              %Allmoving surface set bit
geo.allmove_origin=[0 0 0]; %Origin of rotation for allmoving surface
geo.allmove_axis=[0 0 0];   %Hingeline for allmoving surface
geo.fnx=0;				    %number of panels on flap chords (2d array)
geo.ny=1;					%number of panels in span (2d array)
geo.nx=1;					%number of panels on chord (2d array)
geo.fsym=0;				    %flap deflection symmetry boolean bit  (2d array)
geo.fc=0;					%flap chord in percent of wingchord (2d array)
geo.flapped=[0];			%flapped partition(wing part) boolean bit (2d array)
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
	
while loop==1;
   answ=questions(1);       %question string generator function
   if isempty(answ)
      answ=10;
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
         disp(strcat('Current reference point: ',num2str(geo.ref_point)))
         geo.ref_point=setrefpoint(geo.ref_point,ref);
            
     case 5
         try
            disp(' ')
            quest=questions(12);
            [lattice,ref]=fLattice_setup2(geo,state,quest);
            stat=1;
         catch
            terror(23) 
         end
            
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
      	disp('Keyboard access enabled. Return to program with command "return" ');
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
         	if selected==1;
            	intro(0);
         	elseif selected==2;
            	copyright(0);
            end
         end
         
         
       case 100
           questions(15)
           selected=input(' Enter choice from above please: ');
           
            if isempty(selected)
            else
                if selected==1;
                    definitions; 
                end
            end
           
         
      case 0
   	   loop=0;

         
      otherwise
         
   end
   
end
%end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%INLINE FUNCTIONS
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
