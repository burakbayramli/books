function []=processor(results,lattice,state,geo,ref);

 questions(17);
 quest=input('Enter choice from above please: ');

    if isempty(quest)
       quest=0;
       terror(9)
       return
               
    else
       JID=input('Enter Job IDentity tag (JID): ','s');
    end
    
    
    if isempty(JID)
        JID=('trial');
        disp(' ')
        disp(' JID defaulted to "trial" ');
        disp(' ')
    end
               
        
   switch quest
        case 1    
             solverloop5(results,1,JID,lattice,state,geo,ref);%Simple solution
        case 2
            %Parameter sweep loop
            quest2=questions(18);
             switch quest2
                case 1 %Alpha sweep
                    solverloop5(results,2,JID,lattice,state,geo,ref);
                case 2 %Beta sweep
                    solverloop5(results,3,JID,lattice,state,geo,ref);
                case 3 %Delta sweep
                    solverloop5(results,4,JID,lattice,state,geo,ref);
                case 4 %P sweep
                    solverloop5(results,5,JID,lattice,state,geo,ref);
                case 5 %Q sweep
                    solverloop5(results,6,JID,lattice,state,geo,ref);
                case 6 %R sweep
                    solverloop5(results,7,JID,lattice,state,geo,ref);
                otherwise
             end
       case 3
           solverloop5(results,13,JID,lattice,state,geo,ref); %Trimmed point
       case 4
           solverloop5(results,22,JID,lattice,state,geo,ref); %Trimmed polar
       case 5
           solverloop5(results,16,JID,lattice,state,geo,ref); %Unsteady values
       case 6
           solverloop5(results,17,JID,lattice,state,geo,ref); %Unsteady all values
      
       
       case 7
            %Friction loop
            quest2=questions(19);
            
            switch quest2
                case 1
                    solverloop5(results,10,JID,lattice,state,geo,ref);%Flat plate
                case 2    
                    solverloop5(results,18,JID,lattice,state,geo,ref);%Strip theory, inline
                case 3 
                    solverloop5(results,19,JID,lattice,state,geo,ref);%Strip theory, external
                case 4 
                    solverloop5(results,23,JID,lattice,state,geo,ref);%Blunt body estimation
                otherwise
            end
       case 8
           solverloop5(results,14,JID,lattice,state,geo,ref);% Grid convergence
       case 9
           solverloop5(results,15,JID,lattice,state,geo,ref);% Stall angle
       
       case 10
           solverloop5(results,20,JID,lattice,state,geo,ref);%
       case 11
           solverloop5(results,21,JID,lattice,state,geo,ref);% Static margin
       case 12
            solverloop5(results,24,JID,lattice,state,geo,ref);% Strip alpha sweep
            
       
       case 13
           structure=[];
           ok=1;
           while ok ==1
            %aeroelast loop
            quest2=questions(22);
            
            switch quest2
                case 1
                    structure=fStrucinput();  %enter wing spar data
                
                case 3
                    try
                        cd aircraft
                            cd structure
                            ls 
                            disp(' ')
                            file=input('Save internal wingstructure as: ','s')
                            save(file,'structure')
                        
                            cd ..
                        cd ..
                    catch
                        cd ..
                        cd ..
                        
                    end
                    %%--------
                    
                case 2
                    try
                        cd aircraft
                            cd structure
                            ls 
                            disp(' ')
                            file=input('Load internal wingstructure file: ','s')
                            load(file)
                        
                            cd ..
                        cd ..
                    catch
                             cd ..
                        cd ..
                    end
                      %%--------
                      
                case 5  %Display geometry
                    wingno=1;
                    [output] = generate_struct_mesh( geo,lattice,structure,wingno);

                    
                    figure(5)
                    T=output.profile.t*1000;
                    x=cumsum(output.element_length);
                    plot(x,T)
                    ylabel('Skin Thickness, t, [mm]')
                    xlabel('Beam station, L, [m]')
                    axis([0 x(end) 0 max(T)*1.1])
                    title('Wing box wall thickness.')
                    
                    
                    figure(6)
                    h=output.profile.h*1000;
                    w=output.profile.w*1000;
                    x=cumsum(output.element_length);
                    plot(x,h);
                    hold on
                    plot(x,w,'r');
                    ylabel('Dimensions, (h,w), [mm]')
                    xlabel('Beam station, L, [m]')
                    axis([0 x(end) 0 max(w)*1.1])
                    title('Wing box height and width.')
                    legend('Box height','Box width')

                    
                case 6
                    wingno=1;
                    mesh=generate_struct_mesh( geo,lattice,structure,wingno);
                    
                    [results]=solver9(results,state,geo,lattice,ref);
                    [results]=coeff_create3(results,lattice,state,ref,geo);  
                    
                    FEMresult=FEMsolver(100,geo,lattice,state,ref,structure,mesh,results,wingno);
                    FEMresultplot2(FEMresult,mesh);
                    
                
                case 7
                    disp('Not available in this version')
                    
                case 8
                    disp('Not available in this version')
                    
                case 9
                    disp('Not available in this version')
                    
                case 88
                    keyboard
                    
                
                    
                case 0
                    ok=0;
                otherwise
                    ok=0;
            end
                    
           end

        case 25
           solverloop5(results,25,JID,lattice,state,geo,ref);% FEM TEST

       otherwise
   end
                
               
                
                    
      
                
                
                    

   