function[geo]=setcontrol(geo)

         disp(' ')
         if sum(sum(geo.flapped'))>0
         elseif sum(geo.allmove)>0
         else    
             disp(' No trailing edge control surfaces in the current geometry.')
             return
         end
         
         disp('__________________________________________')
         disp(' ')
         disp(strcat('      The current geometry has ',032, num2str(sum(sum(geo.flapped))), ' trailing edge control surfaces'))
         disp(strcat('      and ',032, num2str(sum(geo.allmove)), ' all moving control surfaces'))
         disp(' ')
        
         disp(' Available operations:')
         disp(' [1]. Change rudder/Trailinge edge effector setting')
         disp(' [2]. Change all moving surface setting')
         disp(' ')
         disp(' [3]. Cancel / Upmenu.')
         
         disp(' ')
         quest=input('option from above, please: ');
         
         if quest==1
              rudder=input('Change rudder number: [1..]: ');
              if rudder==0
                  return
              end
              [n,m]=find(geo.flapped');
              def=input('New absolute control deflection [deg]: ')*pi/180;
              geo.flap_vector(m(rudder),n(rudder))=def;
              disp(' Remember to regenerate lattice before solving!')
             
         elseif quest==2
              rudder=input('Change surface number: [1..]: ');
              if rudder==0
                  return
              end
              [n,m]=find(geo.allmove');
              def=input('New absolute control deflection [deg]: ')*pi/180;
              geo.allmove_def(n(rudder))=def;
              disp(' Remember to regenerate lattice before solving!')
              
         
         else
             return
         end
         
  
         



end