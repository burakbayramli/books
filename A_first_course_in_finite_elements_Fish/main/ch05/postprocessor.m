% postprocessing function 
function postprocessor(d)
include_flags;


    fprintf(1,'\n         Print stresses at the gauss points \n')
    fprintf(1,'Element\t\t x(gauss1) \t\t x(gauss2) \t\t stress(gauss1) \t\t stress(gauss2)\n')
    fprintf(1,'--------------------------------------------------------------------------------- \n')

    % loop over elements to compute the stress
    for e = 1:nel    
    % compute stresses and displacements for each element    
        disp_and_stress(e,d);             
    end
   
