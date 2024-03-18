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
% usage: [STATE]=statesetup(STATE)
%
% Enables a user to manipulate the STATE structure.
%
% Example:
%
%  [state]=statesetup(state);
%
% Calls:
%       questions       Contain user interface queries in string format. 
%       terror          Displays various Error messages.
%       isinptok        Checks format of imput. 
%       ISAtmosphere    Table inter polation of standard atmosphere.
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado text based user interface
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [state]=statesetup(state);
settings=config('startup');


q2=questions(4);
   if isempty(q2)
      q2=0;
     	terror(9)
   end
     
switch q2
%% Load file
    case 2

       ok=0;  
       while ok==0;  
          try
           	cd(settings.sdir)
   				disp(' ')   
            	ls
      	   	fname=input('Load file: ','s');
   				load(fname);
		   		cd(settings.hdir)
         	ok=1;
      	catch
         	cd(settings.hdir)
         	terror(4)
            ok=1;
      	end
   	end
   
      
%% Create State
	case 1
   		disp(' ')
         disp(' ')
         
         stepper=0;
         while stepper<99
            switch stepper
            case -1
               stepper=1;
            case 0
               data=input('Alpha [deg]: ','s');
               if isinptok(data,1)==1;
                  state.alpha=str2num(data)*pi/180;
               end
               
            case 1
               data=input('Beta [deg]: ','s');
					 if isinptok(data,1)==1;
                     state.betha=str2num(data)*pi/180;
               end

				case 2
					data=input('Roll angular velocity [deg/s]: ','s');
               if isinptok(data,1)==1;
                  state.P=str2num(data)*pi/180;
               end
   
      		case 3
               data=input('Pitch angular velocity [deg/s]: ','s');
               if isinptok(data,1)==1;
                  state.Q=str2num(data)*pi/180;
               end               
               
      		case 4
               data=input('Yaw angular velocity [deg/s]: ','s');
               if isinptok(data,1)==1;
                  state.R=str2num(data)*pi/180;
               end
               
      		case 5
               data=input('Angle of attack time derivative, (Alpha_dot), [deg/s]:','s');
               if isinptok(data,1)==1;
                  state.adot=str2num(data)*pi/180;
               end
                
            case 6
               data=input('Angle of sideslip time derivative, (Beta_dot), [deg/s]:','s');
               if isinptok(data,1)==1;
                  state.bdot=str2num(data)*pi/180;
               end
            case 7
                    
 
             speedtype=questions(11);
             disp(' ')
             
%% Different speedtypes             
             switch speedtype 
                  %International units 
                  case 1                     
                        data=input('True airspeed [m/s]: ','s');
                            if isinptok(data,1)==1;
                                state.AS=str2num(data);
                                state.rho=1.225;
                                state.ALT=0;    
                                
                            end
                   case 2
                            data=input('True airspeed [m/s]: ','s');
                            
                            if isinptok(data,1)==1;
                                state.AS=str2num(data,1); 
                            end

                            data2=input('Altitude [m]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2);  
                                state.rho=ISAtmosphere(state.ALT);
                            end
                            
                       disp(' ')
                   case 3
                       data=input('Equivalent airspeed [m/s]: ','s');
                            
                            if isinptok(data,1)==1;
                                groundspeed=str2num(data);
                            end

                            data2=input('Altitude [m]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2);  
                                state.rho=ISAtmosphere(state.ALT);

                            end
                            
                            state.AS=sqrt(ISAtmosphere(0)*groundspeed^2/state.rho);                           
                   case 4
                       data=input('Calibrated airspeed [m/s]: ','s');
                       gamma=1.4;     
                            if isinptok(data,1)==1;
                                speed=str2num(data);
                            end

                            data2=input('Altitude [m]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2);  
                                [state.rho a p_1]=ISAtmosphere(state.ALT);

                            end
                            p_0=p_1+(101300)*(((((speed^2*(gamma-1))/(2*340.3^2))+1)^((gamma)/(gamma-1)))-1);%Stagnation pressure
                            state.AS=sqrt(2*a^2/(gamma-1)*((p_0/p_1)^((gamma-1)/(gamma))-1)); %True airspeed
                   case 5
                           data=input('Mach number [-]: ','s');
                            
                            if isinptok(data,1)==1;
                                speed=str2num(data);
                            end
                            
                            if speed>0.3
                                terror(15)
                            end

                            data2=input('Altitude [m]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2);  
                                [state.rho a]=ISAtmosphere(state.ALT);

                            end
                            
                            
                            state.AS=speed*a;
                       
                       
                       
                       disp(' ')
                       
                       
                       %%Imperial
                       case 6
                            data=input('True airspeed [kts]: ','s');
                            
                            if isinptok(data,1)==1;
                                state.AS=str2num(data)*1852/3600; 
                            end

                            data2=input('Altitude [ft]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2)*0.3048;  
                                state.rho=ISAtmosphere(state.ALT);

                            end
                            
                       disp(' ')
                   case 7
                       data=input('Equivalent airspeed [kts]: ','s');
                            
                            if isinptok(data,1)==1;
                                groundspeed=str2num(data)*1852/3600;
                            end

                            data2=input('Altitude [ft]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2)*0.3048;  
                                state.rho=ISAtmosphere(state.ALT);

                            end
                            
                            state.AS=sqrt(ISAtmosphere(0)*groundspeed^2/state.rho);                           
                   case 8
                       data=input('Calibrated airspeed [kts]: ','s');
                       gamma=1.4;     
                            if isinptok(data,1)==1;
                                speed=str2num(data)*1852/3600;
                            end

                            data2=input('Altitude [ft]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2)*0.3048;  
                                [state.rho a p_1]=ISAtmosphere(state.ALT);

                            end
                            p_0=p_1+(101300)*(((((speed^2*(gamma-1))/(2*340.3^2))+1)^((gamma)/(gamma-1)))-1);%Stagnation pressure
                            state.AS=sqrt(2*a^2/(gamma-1)*((p_0/p_1)^((gamma-1)/(gamma))-1)); %True airspeed
                   case 9
                           data=input('Mach number [-]: ','s');
                            
                            if isinptok(data,1)==1;
                                speed=str2num(data);
                            end
                            
                            if speed>0.3
                                terror(15)
                            end

                            data2=input('Altitude [ft]: ','s');
                            
                            if isinptok(data2,1)==1;
                                state.ALT=str2num(data2)*0.3048;  
                                [state.rho a]=ISAtmosphere(state.ALT);

                            end
                            
                            
                            state.AS=speed*a;
               end
                       
%% Back to ordinary state            
             
               case 8
                    disp(' ')
                    % Out in version 136 disp('Caution, only use this option if you are sure, really sure, what you are doing.')
                    data=input('Apply Prandtl-Glauert Correction [0 1]: ','s');
                    if isinptok(data,1)==1;
                        state.pgcorr=str2num(data);
                        stepper=100;
                    end        
                    disp(' ')
                 
           
            
            	
               
               
            end%caseblock
            
            if isinptok(data,1)==1
               stepper=stepper+1;
            elseif isinptok(data,1)==-1
                stepper=stepper-1;
            elseif isinptok(data,1)==-2
                stepper=99;
            else
            end
            
         end %whileblock
         
         %state.rho=config('rho'); %standard sealevel density

%% Save State        
  case 3 
         
     disp(' ');
     disp(' ');
     disp(' ');
     
     cd(settings.sdir)
     sfname=input('Save state as file: ','s');
     
     if isempty(sfname)==1;
         disp('+++ no name, not saved +++')
         cd(settings.hdir)
     else
    		save(sfname,'state');
          cd(settings.hdir)
          disp(' ');
          disp('*** File Saved. ****');

          
     end

     %% CHANGE ALPHA
     case(4)
     disp(' ');
     disp(' ');
     disp('Changing alpha ');
     disp(strcat('Current alpha is: ',num2str(state.alpha*180/pi),' degrees.'))
     state.alpha=input('New alpha [deg]: ')*pi/180;
     
 %% exiting menu    
   case 0
      stat=1;
      return
   otherwise
      terror(9);
      stat=1;
      return    
   end
    stat=0;