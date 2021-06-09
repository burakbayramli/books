% plot deformed configuration and print stresses in gauss points
function postprocess(d);
include_flags

% plot the deformed configuration
displacements(d);

% Compute strains and stresses at gauss points
s = zeros(neq,1);
if strcmpi(compute_stress,'yes')==1;  
    fprintf(1,'\n                     Stress at Gauss Points \n')
    fprintf(1,'----------------------------------------------------------------------------- \n')
    for e=1:nel
    fprintf(1,'Element  %d \n',e)
    fprintf(1,'-------------\n')       
        get_stress(d,e);
        nodal_stress(d,e);
    end
    global sxx
    stress_contours;
    
    
end











