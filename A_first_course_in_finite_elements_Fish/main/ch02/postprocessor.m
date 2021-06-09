% Postprocessing function 
function postprocesser(d)
include_flags;
 
% prints the element number and corresponding stresses
    fprintf(1,'element\t\t\tstress\n');
    % Compute stress for each element
    for e=1:nel   
        de    = d(LM(:,e));    % nodal displacements for each element
        const = E(e)/leng(e);  % constant coefficient for each element 
        
        if ndof == 1     % ror 1D truss element
            stress(e) = const*([-1 1]*de);
        end
        if ndof == 2     % for 2D truss element
            p = phi(e)*pi/180;                             % Ccnverts degrees to radians
            c = cos(p);    s = sin(p);          
            stress(e) = const*[-c -s c s]*de;              % computes stress
        end
 
        fprintf(1,'%d\t\t\t%f\n',e,stress(e));
    end
