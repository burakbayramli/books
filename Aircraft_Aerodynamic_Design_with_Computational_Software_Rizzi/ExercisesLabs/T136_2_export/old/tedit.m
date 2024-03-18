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
% usage: [GEO] = TEDIT (GEO)
%
% Invokes the geo struct editor of Tornado to allow 
% manipulation of the GEO structure. 
%
% Example:
%
%  [geo] = tedit (geo);
%
% Calls:
%       questions       Contain user interface queries in string format. 
%       terror          Displays various Error messages.
%       fLattice_setup  Generate the computational lattice.
%       geometryplot    Plots the lattice structure.
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Geo struct menu based editor.
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header. TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [geo]=tedit(geo)


if geo.nwing==0;
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
                geo.c(geo.nwing)=0;
                geo.foil(geo.nwing,:,1)={'0'};  %inboard profile
      			geo.foil(geo.nwing,:,2)={'0'};  %outboard profile              
                geo.nx(geo.nwing,:)=0;
                geo.TW(geo.nwing,:,1)=0;        %inboard twist
      			geo.TW(geo.nwing,:,2)=0;        %outboard twist
                geo.dihed(geo.nwing,:)=0;
                geo.ny(geo.nwing,:)=0;
                geo.b(geo.nwing,:)=0;
                geo.T(geo.nwing,:)=0;
                geo.SW(geo.nwing,:)=0;                
                geo.meshtype(geo.nwing,:)=1;    
      			geo.fc(geo.nwing,:)=0;
                geo.fnx(geo.nwing,:)=0;
                geo.fsym(geo.nwing,:)=0;
                geo.flap_vector(geo.nwing,:)=0;
      			geo.flapped(geo.nwing,:)=0;
      			 
                
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
     	 				geo.b(choice3,no_of_elements)=0	
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
  					disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""')
					disp('"                                                    "')
   				disp('"   Wing data                                        "')
   				disp('"                                                    "')
  					disp('""""""""""""""""""""""""""""""""""""""""""""""""""""""')
   
   				disp(strcat(' Number of Partitions		:  ',num2str(geo.nelem(wedit))))
      			disp(' ')   
      			disp('Global entries ')
                disp(' ')
                disp(strcat(' Reference point position	:  ',num2str(geo.ref_point)))
                disp(strcat(' Center of gravity position	:  ',num2str(geo.CG)))
                disp(' ')
                
                disp('Wing specific entries ')   

                disp(' ')
		 		disp(strcat(' Wing Symmetric		    	:  ',num2str(geo.symetric(wedit))))
 			    disp(strcat(' Apex coordinates	    	:  ',num2str([geo.startx(wedit) geo.starty(wedit) geo.startz(wedit) ])))
  			    disp(strcat(' Base chord 			    :  ',num2str(geo.c(wedit))))
   			    disp(' ')    
   			    disp(' Partition specific entries')   
   			    disp(' ')   
  
  				disp(strcat(' Partitions half-span      	:  ',num2str(geo.b(wedit,:))))
   				disp(strcat(' Partitions sweep 		    :  ',num2str(geo.SW(wedit,:))))
                disp(strcat(' Partitions Dihedral 		:  ',num2str(geo.dihed(wedit,:))))

   				disp(strcat(' Partitions taper 		    :  ',num2str(geo.T(wedit,:))))
                
   				disp(' Partitions inner airfoil 	:  '),disp(geo.foil(wedit,:,1))
     			disp(' Partitions inner airfoil 	:  '),disp(geo.foil(wedit,:,2))
            
				disp(strcat(' Partition inner twists  	:  ',num2str(geo.TW(wedit,:,1))))
   				disp(strcat(' Partition outer twists 	:  ',num2str(geo.TW(wedit,:,2))))
   				disp(' ')
   				disp(strcat(' Partition flapped  		:  ',num2str(geo.flapped(wedit,:))))
   				disp(strcat(' Flap chords (Parts)		:  ',num2str(geo.fc(wedit,:))))
   				disp(strcat(' Flaps deflect symmetric	:  ',num2str(geo.fsym(wedit,:))))
   				disp(' ') 
   				disp(strcat(' No. Chord-wise panels  	:  ',num2str(geo.nx(wedit,:))))
   				disp(strcat(' No. Span-wise panels 		:  ',num2str(geo.ny(wedit,:))))
   				disp(strcat(' No. Flap-chord panels  	:  ',num2str(geo.fnx(wedit,:))))
                disp(strcat(' Panel distribution        :  ',num2str(geo.meshtype(wedit,:))))
                disp(strcat(' Flap setting           	:  ',num2str(geo.flap_vector(wedit,:))))
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
   					disp('"""""""""""""""""""""""""""""""""""""""""""""""" ')
   					disp('" ')
   					disp('"   Wing Data ')
   					disp('" ')
   					disp('"""""""""""""""""""""""""""""""""""""""""""""""" ')
   	
   					disp(strcat('    Number of Partitions       :',num2str(geo.nelem(wedit))))
   					disp(' ')
                    disp('Global entries ')
                    disp(' ')
                    disp(strcat(' Reference point position      :',num2str(geo.ref_point)))
                    disp(strcat('[18] Center of gravity position	:  ',num2str(geo.CG)))
                    
                    disp(' ')
   					disp(' Wing specific entries ')
   					disp(' ')
   	   			    disp(strcat('[1] Wing Symmetric             :',num2str(geo.symetric(wedit))))
 					disp(strcat('[2] Apex coordinates           :',num2str([geo.startx(wedit) geo.starty(wedit) geo.startz(wedit) ])))
  					disp(strcat('[3] Base chord                 :',num2str(geo.c(wedit))))
   	   			    disp(' ')
   	   			    disp(' Partition specific entries ')
   	   			    disp(' ')
                    disp(strcat('[4] Dihedral                   :',num2str(geo.dihed(wedit,eedit))))
			   	    disp(strcat('[5] Partition half-span        :',num2str(geo.b(wedit,eedit))))
	   				disp(strcat('[6] Partition sweep            :',num2str(geo.SW(wedit,eedit))))
   					disp(strcat('[7] Partition taper            :',num2str(geo.T(wedit,eedit))))
   					   
                    disp('[8] Partition inner airfoil    :'),disp((geo.foil(wedit,eedit,1)))
                    disp('[9] Partition outer airfoil    :'),disp((geo.foil(wedit,eedit,2)))

                    disp(strcat('[10] Partition inner twist     :',num2str(geo.TW(wedit,eedit,1))))
   					disp(strcat('[11] Partition outer twist     :',num2str(geo.TW(wedit,eedit,2))))
   					disp(' ')
   					disp(strcat('[12] Partition flapped         :',num2str(geo.flapped(wedit,eedit))))
   					disp(strcat('[13] Flap chords (Parts)       :',num2str(geo.fc(wedit,eedit))))
   					disp(strcat('[14] Flaps deflect symmetric   :',num2str(geo.fsym(wedit,eedit))))
   					disp(' ') 
   					disp(strcat('[15] No. of chord-wise panels  :',num2str(geo.nx(wedit,eedit))))
   					disp(strcat('[16] No. of span-wise panels   :',num2str(geo.ny(wedit,eedit))))
   					disp(strcat('[17] No. of flap-chord panels  :',num2str(geo.fnx(wedit,eedit))))
                    
                    try%Old geometries do't have the meshtype field
                        disp(strcat('[19] Panel Distribution        :',num2str(geo.meshtype(wedit,eedit))))
                    end

                     disp(' ')
   					disp('[0]	EXIT ') 
   					disp(' ') 	

					edit2=input('Edit Menu Item [1-19]: ');
					if isempty(edit2)
      					edit2=18;
      					terror(9)
   					end
  	 
   					switch edit2
                        
                        case 18
                                              
        	 				geo.CG=input('CG position [x y z]: ');
   						case 1
     		 			    geo.symetric(wedit)=input('Wing symmetric [0 1]: ');    
                        case 2
        	 				geo.startx(wedit)=input('Apex x-coord: ');
         					geo.starty(wedit)=input('Apex y-coord: ');
         					geo.startz(wedit)=input('Apex z-coord: ');
      					case 5
         					geo.b(wedit,eedit)=input('Partition Span: ');
                            
                        case 3
      						geo.c(wedit)=input('Base chord: ');
                            
                        case 4
         				    geo.dihed(wedit,eedit)=input('Partition dihedral: ');
						
      					case 6
         					geo.SW(wedit,eedit)=input('Partition sweep [rad]: ');
      					case 7
         					geo.T(wedit,eedit)=input('Partition taper : ');
                            
                        case 8
         					data=input('Partition inner foil : ','s');
                            geo.foil(wedit,eedit,1)={data};
                            
                           
                            disp('Remember to change adjacent partition to ensure continuity');
                        case 9
                            data=input('Partition outer foil : ','s');
                            geo.foil(wedit,eedit,2)={data};
                            
                            disp('Remember to change adjacent partition to ensure continuity');
      					case 10  
         					geo.TW(wedit,eedit,1)=input('Partition inner twist [rad]: ');
                            disp('Remember to change adjacent partition to ensure continuity');
     		 		    case 11
      						geo.TW(wedit,eedit,2)=input('Partition outer twist [rad]: ');
                            disp('Remember to change adjacent partition to ensure continuity');
      					case 12
         					 geo.flapped(wedit,eedit)=input('Is Partition flapped : ');
      					case 13
         					 geo.fc(wedit,eedit)=input('Flap chord (parts) : ');
      					case 14
         					 geo.fsym(wedit,eedit)=input('Symmetric deflection : ');
 					    case 15
      						 geo.nx(wedit,eedit)=input('No: Chordwise panels  : ');
                        case 16
                            geo.ny(wedit,eedit)=input('No: spanwise panels  : ');
   						case 17
      						geo.fnx(wedit,eedit)=input('No: Chordwise panels on flap  : ');
                        case 18
                            
                        case 19
                            geo.meshtype(wedit,eedit)=input('Meshtype  : ');
                            
                        case 0
      	      			loop2=0; 
      					otherwise
      		   			terror(9)   
      				    end
	    			end

				case 7
                stat=0;     %Just assuming values to start the lattice setup
                state.AS=0;
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
            case 10     
            otherwise
            	terror(9)     
            end
         end       
	end
end
end
      

   