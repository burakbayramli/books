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
           solverloop5(results,20,JID,lattice,state,geo,ref);% Find alpha at prescribed CL
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
                    geo.structure=fStrucinput();  %enter wing spar data
                
                case 3
                    try
                        cd aircraft
                            cd structure
                            ls 
                            disp(' ')
                            disp('Saving structure as a separate file ')
                            file=input('Save internal wingstructure as: ','s')
                            structure=geo.structure;
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
                            load(file);
                            geo.structure=structure;
                        
                            cd ..
                        cd ..
                    catch
                             cd ..
                        cd ..
                    end
                      %%--------
                      
                case 5  %Display geometry
                    wingno=1;
                    [output] = generate_struct_mesh(geo,lattice,wingno);

                    disp('row 136 in processor.m')
                    figure(305)
                    T=output.profile.t*1000;
                    x=cumsum(output.element_length);
                    plot(x,T)                        %Something fishy here
                    ylabel('Skin Thickness, t, [mm]')
                    xlabel('Beam station, L, [m]')
                    axis([0 x(end) 0 max(T)*1.1])
                    title('Wing box wall thickness.')
                    
                    
                    figure(306)
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
                    
                    geometryplot(lattice,geo,ref)

                    
                case 6
                    wingno=1;
                    mesh=generate_struct_mesh(geo,lattice,wingno);
                    
                    [results]=solver9(results,state,geo,lattice,ref);
                    [results]=coeff_create3(results,lattice,state,ref,geo);  
                    
                    
                    FEMresult=FEMsolver(100,geo,lattice,state,ref,mesh,results,wingno);
                    FEMresultplot2(FEMresult,mesh);
                    
                
                case 7
                    g=9.82;         %Gravity
                    n=2.5;          %Loadfactor
                    wingno=1;       %First wing only so foar
                    disp('  ')
                    disp('---')
                    disp('Experimental / Under development')
                    disp('  ')
                    disp('--')
                    disp(' Altitude set to 3000m '); state.alt=3000;
                    [rho,a,p,mu]=ISAtmosphere(state.alt);
                    disp(' Speed set to M=0.78 ');    state.AS=0.78*a;
                    disp('  ')
                    ADM=input('Aircraft design pullup mass [kg]:');
                    
                    L=ADM*g*n;
                    CL_target=L/(rho*state.AS^2/2*ref.S_ref);
                    [results,alpha]=fFindAlphaAtCL(geo,state,1,CL_target);
      
                    disp(' ')
                    disp(strcat('Target CL of   :',num2str(CL_target)));
                    disp(strcat(strcat('at an alpha of :',num2str(alpha)),' [rad]'));

                    state.alpha=alpha;
                    
                    mesh=generate_struct_mesh( geo,lattice,wingno);
                    [results]=solver9(results,state,geo,lattice,ref);
                    [results]=coeff_create3(results,lattice,state,ref,geo);   
                    FEMresult=FEMsolver(100,geo,lattice,state,ref,mesh,results,wingno);
                    FEMresultplot2(FEMresult,mesh);
                    
                    
                    
                    
                    
                    
                    
                    
                    
                case 8
                    %solverloop5(results,25,JID,lattice,state,geo,ref);% Aeroelast
                    g=9.82;         %Gravity
                    %n=2.5;          %Loadfactor
                    wingno=1;       %First wing only so far
                    
                    %load('aircraft\structure\test_structure_11.mat')
                    
                    mesh=generate_struct_mesh( geo,lattice,wingno);
                    
                    disp('  ')
                    disp('---')
                    disp('Experimental / Under development')
                    disp('  ')
                    disp('--')
                    
                    %disp(' Altitude set to 3000m '); state.alt=3000;
                    %[state.rho,a,p,mu]=ISAtmosphere(state.alt);
                    %disp(' Speed set to M=0.78 ');    state.AS=0.78*a;
                    %disp('  ')
                    
                    
                    
                    ADM=input('Aircraft design pullup mass [kg]:');
                    n=input('Load Factor, n, [-]:');
                    
                    L=ADM*g*n;
                    CL_target=L/(state.rho*state.AS^2/2*ref.S_ref);
                    [results,alpha]=fFindAlphaAtCL(geo,state,1,CL_target);
                    
                    
                                        rotate3d on 
                    colormap(hot);
                    fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.cp')
                    title('Delta cp distribution')
                    colorbar('vert')
                    axis equal
      
                    disp(' ')
                    disp(strcat('Target CL of   :',num2str(CL_target)));
                    disp(strcat(strcat('at an alpha of :',num2str(alpha)),' [rad]'));

                    state.alpha=alpha;
                  
                    
                    
                    [results_1,lattice,convergence,FEMresult]=aeroelast2(wingno,geo,state,mesh);
                    FEMresultplot2(FEMresult,mesh)
                    
                    figure(4)% 	Delta cp plot
                    rotate3d on 
                    colormap(hot);
                    fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)',results.cp')
                    title('Delta cp distribution')
                    colorbar('vert')
                    axis equal
                    
                    
                    
                case 9
                    
                    
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
                
               
                
                    
      
                
                
                    

   