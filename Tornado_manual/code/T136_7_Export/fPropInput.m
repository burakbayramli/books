function [geo]=fPropInput(geo)

loop1=1;
while loop1==1


      
        disp(' ')
        %ok1=0;      
        
        disp('_______________________________________________________')
        disp('                                                      ')
        disp('              New Propeller Definition                 ')
        disp('_______________________________________________________')
        disp(' ');
        disp('    b - Back one question. ')
        disp('    q - Abort input sequence. ')
        disp(' ')
        disp('-------------------------------------------------------')
        
        data=input('Number of propellers on centerline ANDOR Starboard side: ','s');
        
        for i=1:str2double(data)
                disp('                                                      ')
                disp(strcat('              Data for propeller number: ',32, num2str(i)));
                disp('                                                      ')
            stepper=-1;
            while (stepper < 99)
            
            
            


           switch stepper
                case -1
                    data=input('Number of propeller blades: ','s');
                    type=1;
                    if isinptok(data,type)==1
                         geo.prop.n(i)=str2double(data);
                    end
                    
                case 0
                    data=input('Propeller Diameter [m]: ','s');
                    type=1;
                    if isinptok(data,type)==1
                         geo.prop.dia(i)=str2double(data);
                    end
                       
                case 1
                    data=input('Propeller RPM [RPM]: ','s');
                    type=1;
                    if isinptok(data,type)==1
                         geo.prop.rpm(i)=str2double(data);
                    end
                
                case 2
                     data=input('Propeller Thrust [N]: ','s');
                     type=1;
                     if isinptok(data,type)==1
                        geo.prop.T(i)=str2double(data);
                     end
               
                case 3
                    data=input('Propeller hub X-position [m]: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.prop.pos(i,1)=str2double(data);
                    end
                    
                    
                case 4
                    data=input('Propeller hub Y-position [m]: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.prop.pos(i,2)=str2double(data);
                    end   
                    
                    
               case 5
                    data=input('Propeller hub Z-position [m]: ','s');
                    type=1;
                    if isinptok(data,type)==1
                        geo.prop.pos(i,3)=str2double(data);
                    end   
                  
               case 6
                   disp (' ')
                   disp (' Right hand positive, [-1 0 0] rotates clockwise from behind propeller. ')
                   data=input('Propeller rotation axis X-Coordinate: ','s');
                   type=1;
                   if isinptok(data,type)==1
                        geo.prop.rot(i,1)=str2double(data);
                   end
                   
               case 7
                   data=input('Propeller rotation axis Y-Coordinate: ','s');
                   type=1;
                   if isinptok(data,type)==1
                        geo.prop.rot(i,2)=str2double(data);
                   end
                   
               case 8
                   data=input('Propeller rotation axis Z-Coordinate: ','s');
                   type=1;
                   if isinptok(data,type)==1
                        geo.prop.rot(i,3)=str2double(data);
                   end
                   
                   
               case 9
                   data=input('Is the prop symmetrically placed wtr the XZ plane?  [1 0]: ','s');
                   type=1;
                   if isinptok(data,type)==1
                        geo.prop.Psym(i)=str2double(data);
                        if geo.prop.Psym(i)
                            
                            data=input('Is the prop rotation symmetric [1 0]: ','s');
                            type=1;
                            if isinptok(data,type)==1
                                geo.prop.Rsym(i)=str2double(data);
                            end
                            
                        else   
                            geo.prop.Rsym(i)=0;
                        end
                        
                   end
                   
               
                    
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
        disp('-------------------------------------------------------')
        end
        
        
        
        
loop1=0;
end