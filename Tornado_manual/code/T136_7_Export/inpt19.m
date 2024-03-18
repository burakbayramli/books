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
% usage: [GEO] = INPT (GEO)
%
% Invokes the geometry manipulation script of Tornado to allow 
% the user to load, save, edit or input a new geometry structure.
% Geo is the structure containing all necessary geometrical 
% definitions.
%
% Example:
%
%  [geo]=inpt19(geo);
%
% Calls:
%       questions                   Contain user interface queries in string format. 
%       terror                      Displays various Error messages.
%       isinptok                    Checks format of imput. 
%       tedit                       Edit geo interface.
%       f_version_geo_transform     converts old geometry files to new standard.
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Tornado text based user interface
%
% Revision History:

%   Stockholm,2004 01 18:  Changed all geometry variables to new format. TM.
%                          Added a "convert geometry file" function. TM.
%   Bristol,  2007 06 27:  Addition of new header. TM.
%   Spånga, 2021-09-19:   Updated to MATLAB R2020, TM  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [geo]=inpt19(geo)

 settings=config('startup'); %setting directories


loop1=1;
while loop1==1
q=questions(2);
%stat=0;

if isempty(q)
   q=-1; %will go to otherwise section
end








switch q
	case 1 %enter new geometry
      
        disp(' ')
        %ok1=0;
        geo=[];
        geo.version=136;        
        
        disp('_______________________________________________________')
        disp('                                                      ')
        disp('              New Aircraft Definition                 ')
        disp('                -Global Parameters-                   ')
        disp('_______________________________________________________')
        disp(' ');
        disp('    b - Back one question. ')
        disp('    q - Abort input sequence. ')
        disp(' ')
        disp('-------------------------------------------------------')
        
        stepper=-1;
        while (stepper < 99)

           switch stepper
                case -1
                    data=input('Geometry name: ','s');
                    type=2;
                    if isinptok(data,type)==1
                         geo.name=(data);
                    end
                    
                case 0
                    data=input('Project Reference: ','s');
                    type=2;
                    if isinptok(data,type)==1
                         geo.project=(data);
                    end
                       
                case 1
                    disp(' ')
                    data=input('Center of gravity x-coordinate: ','s');
                    type=1;
                    if isinptok(data,type)==1
                         geo.CG(1)=str2double(data);
                    end
                
                case 2
                     data=input('Center of gravity y-coordinate: ','s');
                     type=1;
                     if isinptok(data,type)==1
                        geo.CG(2)=str2double(data);
                     end
               
                case 3
                    data=input('Center of gravity z-coordinate: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.CG(3)=str2double(data);
                    end
                    
               case 4
                   disp (' ')
                   data=input('Reference point x-coordinate: ','s');
                   type=1;
                   if isinptok(data,type)==1
                        geo.ref_point(1)=str2double(data);
                   end
                   
               case 5
                   data=input('Reference point y-coordinate: ','s');
                   type=1;
                   if isinptok(data,type)==1
                        geo.ref_point(2)=str2double(data);
                   end
                   
               case 6
                   data=input('Reference point z-coordinate: ','s');
                   type=1;
                   if isinptok(data,type)==1
                        geo.ref_point(3)=str2double(data);
                   end
                    
                case 7
                    disp (' ')
                    data=input('Number of Wings: ','s');
                    type=1;
                    if isinptok(data,type)==1
                       geo.nwing=str2double(data);
                       stepper=99;
                    end
                    disp (' ')
                    
           end  %case  
           
           if isinptok(data,type)==1
               stepper=stepper+1;
           elseif isinptok(data,type)==-1
               stepper=stepper-1;
           elseif isinptok(data,type)==-2
               stepper=99;
               return
           end %If     
        end  %while
    
     
  	for s=1:geo.nwing			%Stepping over the wings.
   		%home	
  		 	disp('______________________________________________________')
			disp('                                                      ')
            disp(strcat('  Data regarding wing number :', num2str(s),'  '))
			disp('______________________________________________________')
            disp(' ');
         
         stepper3=1;
         while stepper3 <99
             switch stepper3
             
                 case 1     
                 
                    data=input('Number of semispanwise partitions for this wing: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.nelem(s)=str2double(data);     
                    end
                    
                 case 2
            
                   data=input('Is the wing mirrored in the xz-plane [1 0]: ','s');
                   type=1;
                   if isinptok(data,type)==1
                    geo.symetric(s)=str2double(data);
                   end
                   

                   
                 case 3
                    disp(' ')
                    data=input('Apex x-coordinate: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.startx(s)=str2double(data);
                    end
                    
                 case 4
                     
                    data=input('Apex y-coordinate: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.starty(s)=str2double(data);
                    end
             
                 case 5
                     
                    data=input('Apex z-coordinate: ','s'); 
                    type=1;
                    if isinptok(data,type)==1
                        geo.startz(s)=str2double(data);
                    end  
                    
                case 6
                    disp(' ') 
                    data=input('Is this wing an all-moving control surface [1 0]: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.allmove(s)=str2num(data);
                    end
                    if geo.allmove==0
                         stepper=100;
                    end
                  
                 case 7
                     
                        if geo.allmove(s)==1
            				data=input('Hinge Line Origin [x y z]: ','s');
                            type=1;
                            if isinptok(data,type)==1
                               geo.allmove_origin(s,:)=str2num(data);
                            end
                        else
                            geo.allmove_origin(s,:)=[0 0 0];
                        end
                 
                case 8
                     
                 if geo.allmove(s)==1
            			data=input('Hinge Line Axis [x y z]: ','s');
                        type=1;
                        if isinptok(data,type)==1
                           geo.allmove_axis(s,:)=str2num(data);
                        end
                 else
                     geo.allmove_axis(s,:)=[0 1 0];
                 end
                 geo.allmove_def(s,:)=0;
                    
             
                 case 9
                    if geo.allmove(s)==1
                        if geo.symetric(s)==1
                			data=input('Symmetric deflection [0 1]: ','s');
                            type=1;
                            if isinptok(data,type)==1
                               geo.allmove_symetric(s,:)=str2num(data);
                            else
                               geo.allmove_symetric(s,:)=str2num(data);
                            end
                        end
                    else
                     geo.allmove_symetric(s,:)=0;
                    end
             stepper3=99;
             end

             
             
             
                       
            if isinptok(data,type)==1
               stepper3=stepper3+1;
            elseif isinptok(data,type)==-1
               stepper3=stepper3-1;
            elseif isinptok(data,type)==-2
               stepper3=99;
               return
            else
            end
         end
        
         
   		t=0;
        for s2=1:geo.nelem(s)
         	t=t+1;
         
         	disp(' ')
  		 	   disp('_____________________________________________________')
			   disp('                                                    ')
                disp(strcat(' Wing number:  ',num2str(s),('                  ')))
               disp(strcat(' Data entry for partition number:  ',num2str(t),('                  ')))
            
         	disp(' ')   
        
            stepper=4;
            while stepper<99
                 switch stepper 
          
                  case 4
                     if t==1  %Only define inboard profile on innermost partition  
                        data=input('Root chord: ','s');
                        type=1;
                        if isinptok(data,type)==1
                           geo.c(s)=str2num(data);
                        end
                     end
                  
                  case 5
                     if t==1  %Only define inboard profile on innermost partition      
                        ok=0;  
                        while ok==0
                            try
                                cd(settings.afdir)
      

                                disp(' ')
                                disp('____________________________ ')
                                disp(' ')
                                disp(' AVAILABLE AIRFOILS: ')
                                ls
                                disp('____________________________ ')
                                disp(' ')
                                disp('Enter profile filename from the list above (ex CLARKY.DAT) ')
                                disp('OR any NACA four digits series numer (ex: 2412)')
                                disp('0 (zero) for a flat plate. ')
                                disp(' ')
                                
                                
                                data=input('Base chord airfoil: ','s');
                                
                                
                                if isempty(str2num((cell2mat({data}))))==0
                                   geo.foil(s,t,1)={data};%Naca xxxx profile
                                   ok=1;
                                   cd(settings.hdir)
                                else
                                    try
                                        %foo=str2num(data);
                                        %cd(settings.afdir)
                                             load(data)  %Testload to see that the file exists
                                        cd(settings.hdir)
                                        geo.foil(s,t,1)={data};
                                        data=0;
                                        ok=1;
                                    catch    
                                       cd(settings.hdir)
                                        disp(' ')
                                       disp(' + + + ')
                                        disp(' ')
                                        disp(' No such file! ')
                                       disp(' ')
                                    end
                                end  
                                
                            catch
                                cd(settings.hdir)
                                terror(4)
                            end
                        end
                     end 
                  
                	 case 6   
                     if t==1  %Only on first partition
                        data=input('Number of panels chord wise: ','s');
                        type=1;
                        if isinptok(data,type)==1
                           geo.nx(s,1)=str2num(data);
                        end
                        
                     end


                    case 7
                     if s==1	% First wing base chord is reference for twist 
              				geo.TW(s,1,1)=0;
                     else
                            if t==1
                                data=input('Base chord twist [deg]: ','s');
                                type=1;
                                if isinptok(data,type)==1
                                    geo.TW(s,1,1)=str2num(data)*pi/180;
                                end                          
                            end
                            
                        
                     end
                     
                  case 8
                     %if t==1;
                     
        		         data=input('Partition dihedral [deg]: ','s');
                         type=1;
                          if isinptok(data,type)==1
                             geo.dihed(s,t)=str2num(data)*pi/180;
                          end
                     %end
                     
                  case 9
         				data=input('Number of panels semi-span wise: ','s');
                        type=1;
                     if isinptok(data,type)==1
                        geo.ny(s,t)=str2num(data);
                     end
                     
                  case 10
                     data=input('Span of partition: ','s');
                     type=1;
                     if isinptok(data,type)==1
                        geo.b(s,t)=str2num(data);
                     end
                     
                  case 11
                     data=input('Taper ratio: ','s');
                     type=1;
                     if isinptok(data,type)==1
                        geo.T(s,t)=str2num(data);
                     end
                     
                  case 12
                             
                        ok=0;  
                        while ok==0
                            try
                                cd(settings.afdir)
                                disp(' ')
                                disp('____________________________ ')
                                disp(' ')
                                disp(' AVAILABLE AIRFOILS: ')
                                ls
                                disp('____________________________ ')
                                disp(' ')
                                disp('Enter profile filename from the list above (ex CLARKY.DAT) ')
                                disp('OR any NACA four digits series numer (ex: 2412)')
                                disp('0 (zero) for a flat plate. ')
                                disp(' ')
                                data=input('Tip chord airfoil: ','s');
                                
                                                                
                                if isempty(str2num((cell2mat({data}))))==0
                                   geo.foil(s,t,2)={data};%Naca xxxx profile
                                   ok=1;
                                   cd(settings.hdir)
                                else
                                    try
                                        %foo=str2num(data);
                                        %cd(settings.afdir)
                                             load(data)  %Testload to see that the file exists
                                        cd(settings.hdir)
                                        geo.foil(s,t,2)={data};
                                        data=0;
                                        ok=1;
                                    catch    
                                       cd(settings.hdir)
                                        disp(' ')
                                       disp(' + + + ')
                                        disp(' ')
                                        disp(' No such file! ')
                                       disp(' ')
                                    end
                                end
                                
                            catch
                                cd(settings.hdir)
                                terror(4)
                            end
                        end
                      

                  case 13
                     data=input('Quarter chord line sweep [deg]: ','s');
                     type=1;
                     if isinptok(data,type)==1
                        geo.SW(s,t)=str2num(data)*(pi/180);
                     end
                     
                  case 14   
 					  data=input('Outboard twist [deg]: ','s');
                      type=1;
                      if isinptok(data,type)==1
                         geo.TW(s,t,2)=str2num(data)*(pi/180);
                      end

                  case 15
                      disp(' ')
                      disp(' _______________________ ')
                      disp(' Available mesh distribution types:')
                      disp('   [1] Linear')
                      disp('   [2] Spanwise half-cosine')
                      disp('   [3] Spanwise half-cosine, chordwise cosine')
                      disp('   [5] Spanwise cosine')
                      disp('   [6] Chordwise cosine')% (Added 22/08/2008 AT)
                      disp('   [7] 3:rd order centerpacking. (Not for wings)') 
                      disp(' ')
 					  data=input('Mesh type: ','s');
                      type=1;
                      if isinptok(data,type)==1
                         geo.meshtype(s,t)=str2num(data);
                      end
                      
                   case 16
                      disp(' _______________________ ')
                      disp(' ')
                      data=input('Is partition flapped [1 0]:','s');
                      type=1;
                      if isinptok(data,type)==1
                         geo.flapped(s,t)=str2num(data);
                         
                         if geo.flapped==0
                             stepper=100;
                             geo.fc(s,t)=0;
                             geo.fnx(s,t)=0;
                             geo.fsym(s,t)=0;
                         end
                      end
                   case 17    
                  	if geo.flapped(s,t)==1
            				data=input('Flap chord in fraction of local chord (0..1): ','s');
                            type=1;
                        if isinptok(data,type)==1
                           geo.fc(s,t)=str2num(data);
                        end
                        
                        if geo.fc(s,t)> 0.9
                            disp(' + + + Warning + + +  Are you sure? Consider an allmoving surface instead.')
                        end
                        
                    end
                                       
                  case 18
                     if geo.flapped(s,t)==1
                        data=input('Number of chord wise panels on flap: ','s');
                        type=1;
                        if isinptok(data,type)==1
                           geo.fnx(s,t)=str2num(data);
                        end
                        
                     end

                     case 19
                     if and(geo.symetric(s),geo.flapped(s,t))
              				data=input('Do control surfaces deflect symmetrically [1 0]:','s');
                            type=1;
                          if isinptok(data,type)==1
                             geo.fsym(s,t)=str2num(data);
                          end
                          
                     else
               			geo.fsym(s,t)=0;
                     end
                     
    
                  

                     
                  end %caseblock
                  
                  if isinptok(data,type)==1
                     stepper=stepper+1;
                  elseif isinptok(data,type)==-1
                     stepper=stepper-1;
                  elseif isinptok(data,type)==-2
                     stepper=99;
                     return
                  else
                  end
            end% while partitionblock
               
               if geo.flapped(s,t)==0
                  stepper=100;
               	geo.fc(s,t)=0;
                geo.fnx(s,t)=0;
               	geo.fsym(s,t)=0;
               end
         		geo.nx(s,t)=geo.nx(s,1);
            	%dihed(s,t)=dihed(s,1);
                if t~=1
            		geo.TW(s,t,1)=geo.TW(s,t-1,2);	%continious twist
            		geo.foil(s,t,1)=geo.foil(s,t-1,2); %continious camber
                end
             
      	end%partitionblock
    end%wingblock
      
      geo.flap_vector=zeros(size(geo.flapped));
      if isinptok(data,type)~=-2
         loop1=0;				%loop if aborted
      end
  
      
      
      
%% Load Geometry      
case 2
    %geo=[];
    %lattice=[];
  	try
        cd(settings.acdir)
        ls
     	fname1=input('Load file: ','s');
        load(fname1);  
        cd(settings.hdir)
      
        resave=0;
        try geo.version;
        catch
           resave=1;
           geo.name=('Undefined');
           geo.project=('Undefined');
           geo.version=136;
           geo.allmove=zeros(size(geo.symetric));
           geo.allmove_origin=0;
           geo.allmove_axis=0;
           geo.allmove_symetric=zeros(size(geo.symetric));
           geo.allmove_def=zeros(size(geo.symetric));
        end
        
        
        try geo.meshtype;
        catch
            resave=1;
            geo.meshtype=ones(size(geo.nx));
        end
        
        if resave
           disp(' ')
           disp(' + + + Warn + + +  Old file format detected ') 
           q5=input('Do you wish to rewrite in the new format? [1 0]: ','s');
           disp(' ')
           if q5
               cd(settings.acdir)
                    save(fname1,'geo');
               cd(settings.hdir)
               
           else
               disp(' No file saved. ')
           end
        end
        
        
      
      loop1=0;
    catch
        cd(settings.hdir)
        terror(4)        
    end
      
case(3)   
   [geo]=tedit(geo);
   
case(4)
   cd(settings.acdir)
      ls
      fname2=input('Save file as: ','s');
      if isempty(fname2)==1
         disp('+++ no name, not saved +++')
         cd(settings.hdir)
      else
          try
        	  save(fname2,'geo') ;  
              disp('*** File Saved. ****');
              cd(settings.hdir)
      		catch
        			cd(settings.hdir)
        			terror(5)   
          end
      end
   
  case(5)
     
     if config('expert')  
        geo=bodyinput(geo);
     end
     
  case(6)
     
     if config('expert') 
        geo.structure=fStrucinput(); %Add internal structure to geo struct.
     end
     
    case(7)
     
     if config('expert') 
        geo=fPropInput(geo); %Add internal structure to geo struct.
     end   
        
     
   case 0
      loop1=0;
   
	otherwise
   	terror(9);	
	end
end
end




function[geo]=bodyinput(geo)
disp(' ')
disp(' This will add blunt body data to the geo struct. ')
disp(' ')
nbod=input('Number of blunt bodies: ');

for i=1:nbod
    
    
    
    disp('________________________________')
    disp(strcat('Data regarding blunt body number :',(num2str(i))));
    data=input('Body length, [m]: ');
    if isinptok(data,1)==1
       geo.body.length(i)=data;    
    end
    
    data=input('Body diameter, [m]: ');
    if isinptok(data,1)==1
       geo.body.diam(i)=data;    
    end
    
    data=input('Body interference factor: ');
    if isinptok(data,1)==1
       geo.body.inter(i)=data;    
    end
    
    
    
    
end
end





function [geo]=tedit(geo)


if geo.nwing==0
   disp(' ')
   disp(' No geometry loaded')
   disp(' ')
else

try
	loop=1;  
   while loop==1   
      	disp(' ')
		   disp(strcat('Number of wings are :  ',num2str(geo.nwing)))
		   disp(strcat('Number of partition per wing are :  ',num2str(geo.nelem)))
			
         choice1=questions(6);
         
         if isempty(choice1)
      		choice1=10;
      		terror(9)
         end
            disp(' ')
      
        switch choice1
      
    			case 1
      			disp('New wing will be added as higest wing number. ')
      			disp(' ')
      			disp('Dont forget to set the new wings properties')
     	 		
                geo.nwing=geo.nwing+1;  
     	 		geo.nelem(geo.nwing)=1;            
                geo.startx(geo.nwing)=0;
      			geo.starty(geo.nwing)=0;
      			geo.startz(geo.nwing)=0;
      			geo.symetric(geo.nwing)=0;              
                geo.c(geo.nwing)=1;
                geo.foil(geo.nwing,:,1)={'0'};  %inboard profile
      			geo.foil(geo.nwing,:,2)={'0'};  %outboard profile              
                geo.nx(geo.nwing,:)=1;
                geo.TW(geo.nwing,:,1)=0;        %inboard twist
      			geo.TW(geo.nwing,:,2)=0;        %outboard twist
                geo.dihed(geo.nwing,:)=0;
                geo.ny(geo.nwing,:)=1;
                geo.b(geo.nwing,:)=1;
                geo.T(geo.nwing,:)=1;
                geo.SW(geo.nwing,:)=0;                
                geo.meshtype(geo.nwing,:)=1;    
      			geo.fc(geo.nwing,:)=0;
                geo.fnx(geo.nwing,:)=0;
                geo.fsym(geo.nwing,:)=0;
                geo.flap_vector(geo.nwing,:)=0;
      			geo.flapped(geo.nwing,:)=0;
                %T136 additions
                geo.allmove(geo.nwing)=0;
                geo.allmove_origin(geo.nwing,:)=[0 0 0];
                geo.allmove_axis(geo.nwing,:)=[0 0 0];
                geo.allmove_symetric(geo.nwing,:)=1;
      			 
                
   			case 2
   	   		disp(' ')
   	   		choice2=input('Remove wing no: ');
   	   		    geo.nwing=geo.nwing-1;
                geo.nelem(choice2)=[];
   	   		    geo.SW(choice2,:)=[];
   	   		    geo.T(choice2,:)=[];
                geo.flap_vector(choice2,:)=[];
       	   		geo.TW(choice2,:,:)=[];
   	       		geo.b(choice2,:)=[];
   	   	    	geo.c(choice2)=[];
       	   		geo.dihed(choice2,:)=[];
   	       		geo.fc(choice2,:)=[];
   	   	    	geo.flapped(choice2,:)=[];
   	   		    geo.fnx(choice2,:)=[];
   	   		    geo.foil(choice2,:,:)=[];
     			geo.fsym(choice2,:)=[];
     			
     			geo.nx(choice2,:)=[];
      			geo.ny(choice2,:)=[];
      			geo.startx(choice2)=[];
      			geo.starty(choice2)=[];
      			geo.startz(choice2)=[];
      			geo.symetric(choice2)=[];
                geo.meshtype(choice2,:)=[];
                
                %T136 additions
                geo.allmove(choice2)=[];
                geo.allmove_origin(choice2,:)=[];
                geo.allmove_axis(choice2,:)=[];
                geo.allmove_symetric(geo.nwing,:)=[];

			 	case 3
      			disp(' ')
       			disp('Partition will be added at wingtip ')
      			choice3=input('Add partition to wing no: ');
      
         
      			geo.nelem(choice3)=geo.nelem(choice3)+1;
      			i=geo.nelem(choice3);
      			geo.b(choice3,i)=0;
				geo.SW(choice3,i)=0;
      			geo.T(choice3,i)=0;
      			geo.TW(choice3,i,1)=0;
      			geo.TW(choice3,i,2)=0;
      			geo.dihed(choice3,i)=0;
      			geo.fc(choice3,i)=0;
      			geo.flapped(choice3,i)=0;
                geo.flap_vector(choice3,i)=0;
                geo.fnx(choice3,i)=0;
      			geo.foil(2*choice3-1,i,:)={'0'};
      			geo.foil(2*choice3,i,:)={'0'};
      			geo.fsym(choice3,i)=0;
      			geo.nx(choice3,i)=0;
      			geo.ny(choice3,i)=0;
                geo.meshtype(choice3,i)=1;
         
				case 4
    				disp(' ')
      
     			choice3=input('Remove partition from wing no: ');
      			choice4=input('Remove partition no: ');
      			no_of_elements=geo.nelem(choice3);
      
      			for step=choice4:no_of_elements %step from selected element outwards.   
         			if step<geo.nelem(choice3)
         				geo.SW(choice3,step)=geo.SW(choice3,step+1);	%moving properties one step inwards
      					geo.T(choice3,step)=geo.T(choice3,step+1);
                        geo.flap_vector(choice3,step)=geo.flap_vector(choice3,step+1);
      					geo.TW(choice3,step,1)=geo.TW(choice3,step+1,1);
      					geo.TW(choice3,step,2)=geo.TW(choice3,step+1,2);
      					geo.b(choice3,step)=geo.b(choice3,step+1);
     				    geo.dihed(choice3,step)=geo.dihed(choice3,step+1);
      					geo.fc(choice3,step)=geo.fc(choice3,step+1);
    				    geo.flapped(choice3,step)=geo.flapped(choice3,step+1);
      					geo.fnx(choice3,step)=geo.fnx(choice3,step+1);
                        geo.foil(choice3,step,1)=geo.foil(choice3,step+1,1);
      					geo.foil(choice3,step,2)=geo.foil(choice3,step+1,2);
                        geo.fsym(choice3,step)=geo.fsym(choice3,step+1);
      					geo.nx(choice3,step)=geo.nx(choice3,step+1);
      					geo.ny(choice3,step)=geo.ny(choice3,step+1);
                        
                        geo.meshtype(choice3,step)=geo.meshtype(choice3,step+1);
         			end
      			end
                       	geo.SW(choice3,no_of_elements)=0;%Deleting properties on most outboard element		
     	 				geo.T(choice3,no_of_elements)=0;
                        geo.flap_vector(choice3,no_of_elements)=0;         
     	 			    geo.TW(choice3,no_of_elements,1)=0;
     	 				geo.TW(choice3,no_of_elements,2)=0;  
     	 				geo.b(choice3,no_of_elements)=0	;
     	 			    geo.dihed(choice3,no_of_elements)=0;
     	 				geo.fc(choice3,no_of_elements)=0;
    	 				geo.flapped(choice3,no_of_elements)=0;
     	 				geo.fnx(choice3,no_of_elements)=0;
     	 				geo.foil(choice3,no_of_elements,1)={'0'};
     	 				geo.foil(choice3,no_of_elements,2)={'0'};
     	 				geo.fsym(choice3,no_of_elements)=0;
     	 				geo.nx(choice3,no_of_elements)=0;
         	   		    geo.ny(choice3,no_of_elements)=0;
                        geo.meshtype(choice3,no_of_elements)=0;
                        
                
                
                
      
      			if sum(geo.b(:,no_of_elements))==0
		      	    geo.SW(:,no_of_elements)=[];%Deleting zeroz column[]		
   		  	 		geo.T(:,no_of_elements)=[];
                    geo.flap_vector(:,no_of_elements)=[];    
	   	  	 		geo.TW(:,no_of_elements,:)=[];
   	  		 		geo.b(:,no_of_elements)=[];
     		 		geo.dihed(:,no_of_elements)=[];
     	 			geo.fc(:,no_of_elements)=[];
 	   	 			geo.flapped(:,no_of_elements)=[];
  		   	 		geo.fnx(:,no_of_elements)=[];
     			 	geo.foil(:,no_of_elements,:)=[];
     	 			geo.fsym(:,no_of_elements)=[];
     	 			geo.nx(:,no_of_elements)=[];
  	 	   	        geo.ny(:,no_of_elements)=[];
                    geo.meshtype(:,no_of_elements)=[];
   		   	end
      
         
               geo.nelem(choice3)=geo.nelem(choice3)-1;
               
               
            case 5   
      			disp(' ')
					wedit=input('View wing no: ');   
   				disp(' ')
  					disp('______________________________________________________')
					disp('                                                    ')
   				disp('   WING DATA                                            ')
                disp(strcat('     Wing number:  ',num2str(wedit),('                  ')))
  					disp('______________________________________________________')
   
   				disp(strcat('   Number of Partitions	    	:  ',num2str(geo.nelem(wedit))))
      			disp(' ')   
      			disp('Global entries ')
                disp(' ')
                disp(strcat('   Geometry name       	        :  ',num2str(geo.name)))
                disp(strcat('   Project reference   	        :  ',num2str(geo.project)))
                disp(' ');
                disp(strcat('   Reference point position     :  ',num2str(geo.ref_point)))
                disp(strcat('   Center of gravity position   :  ',num2str(geo.CG)))
               
                
                
                disp(' ')
                
                disp('Wing specific entries ')   

                disp(' ')
		 		disp(strcat('   Wing Symmetric		    	:  ',num2str(geo.symetric(wedit))))
 			    disp(strcat('   Apex coordinates	        	:  ',num2str([geo.startx(wedit) geo.starty(wedit) geo.startz(wedit) ])))
  			    disp(strcat('   Base chord 			        :  ',num2str(geo.c(wedit))))
                disp(strcat('   All moving surface	        :  ',num2str(geo.allmove(wedit))))
                disp(strcat('       Origin	                :  ',num2str(geo.allmove_origin(wedit,:))))
                disp(strcat('       Axis         	        :  ',num2str(geo.allmove_axis(wedit,:))))
                disp(strcat('       Deflection symmetry     :  ',num2str(geo.allmove_symetric(wedit))))
   			    disp(' ')    
   			    disp('Partition specific entries')   
   			    disp(' ')   
  
  				disp(strcat('   Partitions half-span      	:  ',num2str(geo.b(wedit,:))))
   				disp(strcat('   Partitions sweep 		    :  ',num2str(geo.SW(wedit,:))))
                disp(strcat('   Partitions Dihedral          :  ',num2str(geo.dihed(wedit,:))))

   				disp(strcat('   Partitions taper 		    :  ',num2str(geo.T(wedit,:))))
                
   				disp('   Partitions inner airfoil 	:  '),disp(geo.foil(wedit,:,1))
     			disp('   Partitions inner airfoil 	:  '),disp(geo.foil(wedit,:,2))
            
				disp(strcat('   Partition inner twists      	:  ',num2str(geo.TW(wedit,:,1))))
   				disp(strcat('   Partition outer twists 	    :  ',num2str(geo.TW(wedit,:,2))))
   				disp(' ')
   				disp(strcat('   Partition flapped  		    :  ',num2str(geo.flapped(wedit,:))))
   				disp(strcat('   Flap chords (Parts)		    :  ',num2str(geo.fc(wedit,:))))
   				disp(strcat('   Flaps deflect symmetric	    :  ',num2str(geo.fsym(wedit,:))))
   				disp(' ') 
   				disp(strcat('   No. Chord-wise panels  	    :  ',num2str(geo.nx(wedit,:))))
   				disp(strcat('   No. Span-wise panels         :  ',num2str(geo.ny(wedit,:))))
   				disp(strcat('   No. Flap-chord panels  	    :  ',num2str(geo.fnx(wedit,:))))
                disp(strcat('   Panel distribution            :  ',num2str(geo.meshtype(wedit,:))))
                disp(strcat('   Flap setting           	    :  ',num2str(geo.flap_vector(wedit,:))))
      			disp(' ') 
      			disp(' Paused, press space to continue. ') 
               pause
               
            case 6
   	   		disp(' ')
   	   		wedit=input('Edit wing no: ');  
   	   		eedit=input('Edit partition no: '); 
   	   		loop2=1;
            while loop2==1
   	   			disp(' ')
   					disp('_________________________________________________')
   					disp('                                                 ')
   					disp('   Wing Data                                     ')
   					disp('_________________________________________________')
   	
   					disp(strcat('    Number of Partitions       :',num2str(geo.nelem(wedit))))
   					disp(' ')
                    disp('Global entries ')
                    disp(' ')
                    disp(strcat(' [1] Geometry name       	        :  ',num2str(geo.name)))
                    disp(strcat(' [2] Project reference   	        :  ',num2str(geo.project)))
                    disp(' ');
                    disp(strcat(' [3] Reference point position       :',num2str(geo.ref_point)))
                    disp(strcat(' [4] Center of gravity position	    :  ',num2str(geo.CG)))

                    
                    disp(' ')
   					disp('Wing specific entries ')
   					disp(' ')
   	   			    disp(strcat(' [5] Wing Symmetric                 :',num2str(geo.symetric(wedit))))
 					disp(strcat(' [6] Apex coordinates               :',num2str([geo.startx(wedit) geo.starty(wedit) geo.startz(wedit) ])))
  					disp(strcat(' [7] Base chord                     :',num2str(geo.c(wedit))))
                    disp(strcat(' [8] All moving surface	            :  ',num2str(geo.allmove(wedit))))
                    disp(strcat(' [9]      Origin	                :  ',num2str(geo.allmove_origin(wedit,:))))
                    disp(strcat(' [10]     Axis         	            :  ',num2str(geo.allmove_axis(wedit,:))))
                    
   	   			    disp(' ')
   	   			    disp('Partition specific entries ')
   	   			    disp(' ')
                    disp(strcat(' [11] Dihedral                      :',num2str(geo.dihed(wedit,eedit))))
			   	    disp(strcat(' [12] Partition half-span           :',num2str(geo.b(wedit,eedit))))
	   				disp(strcat(' [13] Partition sweep               :',num2str(geo.SW(wedit,eedit))))
   					disp(strcat(' [14] Partition taper               :',num2str(geo.T(wedit,eedit))))
   					   
                    disp(' [15] Partition inner airfoil       :'),disp((geo.foil(wedit,eedit,1)))
                    disp(' [16] Partition outer airfoil       :'),disp((geo.foil(wedit,eedit,2)))

                    disp(strcat(' [17] Partition inner twist         :',num2str(geo.TW(wedit,eedit,1))))
   					disp(strcat(' [18] Partition outer twist         :',num2str(geo.TW(wedit,eedit,2))))
   					disp(' ')
   					disp(strcat(' [19] Partition flapped             :',num2str(geo.flapped(wedit,eedit))))
   					disp(strcat(' [20] Flap chords (Parts)           :',num2str(geo.fc(wedit,eedit))))
   					disp(strcat(' [21] Flaps deflect symmetric       :',num2str(geo.fsym(wedit,eedit))))
   					disp(' ') 
   					disp(strcat(' [22] No. of chord-wise panels      :',num2str(geo.nx(wedit,eedit))))
   					disp(strcat(' [23] No. of span-wise panels       :',num2str(geo.ny(wedit,eedit))))
   					disp(strcat(' [24] No. of flap-chord panels      :',num2str(geo.fnx(wedit,eedit))))
                    
                    
                    disp(strcat(' [25] Panel Distribution            :',num2str(geo.meshtype(wedit,eedit))))
                    

                    disp(' ')
   					disp('[0]	EXIT ') 
   					disp(' ') 	

					edit2=input('Edit Menu Item [1-25]: ');
                    if isempty(edit2)
      					edit2=18;
      					terror(9)
                    end
  	 
                    switch edit2
                        
                        case 1
                            geo.name=input('Geometry Name [FooBar]: ','s');
                            
                        case 2
                            geo.project=input('Project Reference [FooBar]: ','s');
                            
                        case 3
                            geo.ref_point=str2num(input('Reference Point [x y z]: ','s'));
                                
                        case 4                 
        	 				geo.CG=str2num(input('CG position [x y z]: ','s'));
                            
                        case 5
                            geo.symetric(wedit)=input('Wing symmetry bit [1 0]: ');
                            
                            
                        case 6
        	 				geo.startx(wedit)=input('Apex x-coord: ');
         					geo.starty(wedit)=input('Apex y-coord: ');
         					geo.startz(wedit)=input('Apex z-coord: ');
                            
      					case 7
                            geo.c(wedit)=input('Base chord: ');
                            
                        case 8
                            geo.allmove(wedit)=input('All Moving Surface bit [1 0]: ');
                            
                        case 9
                            geo.allmove_origin(wedit,:)=str2num(input('All Moving Surface origin [x y z]: ','s'));
                            
                        case 10
                            geo.allmove_axis(wedit,:)=str2num(input('All Moving Surface axis [x y z]: ','s'));

                        case 11
         				    geo.dihed(wedit,eedit)=input('Partition dihedral: ');	
                            
                        case 12
                            geo.b(wedit,eedit)=input('Partition Span: ');
                            
      					case 13
         					geo.SW(wedit,eedit)=input('Partition sweep [rad]: ');
                            
      					case 14
         					geo.T(wedit,eedit)=input('Partition taper : ');
                            
                        case 15
         					data=input('Partition inner foil : ','s');
                            geo.foil(wedit,eedit,1)={data};
                            disp('Remember to change adjacent partition to ensure continuity');
                            
                        case 16
                            data=input('Partition outer foil : ','s');
                            geo.foil(wedit,eedit,2)={data};
                            disp('Remember to change adjacent partition to ensure continuity');
                            
      					case 17  
         					geo.TW(wedit,eedit,1)=input('Partition inner twist [rad]: ');
                            disp('Remember to change adjacent partition to ensure continuity');
                            
     		 		    case 18
      						geo.TW(wedit,eedit,2)=input('Partition outer twist [rad]: ');
                            disp('Remember to change adjacent partition to ensure continuity');
                            
      					case 19
         					 geo.flapped(wedit,eedit)=input('Is Partition flapped : ');
                             
      					case 20
         					 geo.fc(wedit,eedit)=input('Flap chord (parts) : ');
                             
      					case 21
         					 geo.fsym(wedit,eedit)=input('Symmetric deflection : ');
                             
 					    case 22
      						 geo.nx(wedit,eedit)=input('No: Chordwise panels  : ');
                             
                        case 23
                            geo.ny(wedit,eedit)=input('No: spanwise panels  : ');
                            
   						case 24
      						geo.fnx(wedit,eedit)=input('No: Chordwise panels on flap  : ');
              
                        case 25
                            geo.meshtype(wedit,eedit)=input('Meshtype  [1 - 7]: ');
                            
                        case 0
      	      			loop2=0; 
      					otherwise
      		   			terror(9)   
                    end
            end

		case 7 %draw geometry
                %stat=0;     %Just assuming values to start the lattice setup
                state.AS=1;
                state.P=0;
                state.Q=0;
                state.R=0;
                state.alpha=0;
                state.betha=0;
                
                lattice.XYZ=[];
                
                [lattice,ref]=fLattice_setup2(geo,state,0);
                
                geometryplot(lattice,geo,ref);
                
            case 8
                keyboard

		    case 0
  					loop=0;
                case -1
                    %donothing
                    terror(9) 
            case 10
                
            otherwise
            	terror(9)     
        end
   end
catch   
end
end
end




    