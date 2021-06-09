% plot temperature and flux
function postprocess(d);
include_flags

% plot the temperature field
if strcmpi(plot_temp,'yes')==1;  
   d1 = d(ID);
   figure(2); 
   for e=1:nel
       XX = [x(IEN(1,e))  x(IEN(2,e))  x(IEN(3,e))  x(IEN(4,e))  x(IEN(1,e))];
       YY = [y(IEN(1,e))  y(IEN(2,e))  y(IEN(3,e))  y(IEN(4,e))  y(IEN(1,e))];
       dd = [d1(IEN(1,e)) d1(IEN(2,e)) d1(IEN(3,e)) d1(IEN(4,e)) d1(IEN(1,e))];
       patch(XX,YY,dd);hold on;  
   end
title('Temperature distribution'); xlabel('X'); ylabel('Y'); colorbar;
end

%Compute flux at gauss points
if strcmpi(compute_flux,'yes')==1;  
    fprintf(1,'\n                     Heat Flux at Gauss Points \n')
    fprintf(1,'----------------------------------------------------------------------------- \n')
    for e=1:nel
    fprintf(1,'Element  %d \n',e)
    fprintf(1,'-------------\n')       
        get_flux(d,e);
    end
end