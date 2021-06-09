function stress_contours;
include_flags;


if strcmpi(plot_stress_xx,'yes')==1;  
   figure(2); 
   for e=1:nel
       XX = [x(IEN(1,e))  x(IEN(2,e))  x(IEN(3,e))  x(IEN(4,e))  x(IEN(1,e))];
       YY = [y(IEN(1,e))  y(IEN(2,e))  y(IEN(3,e))  y(IEN(4,e))  y(IEN(1,e))];

       sxx = nodestress(IEN(:,e),1)./counter(IEN(:,e));
       dd  = [sxx'   sxx(1)];
       patch(XX,YY,dd);hold on;  
   end
   title('\sigma_x_x contours'); xlabel('X'); ylabel('Y'); colorbar
end

if strcmpi(plot_mises,'yes')==1;  
   for e=1:nel
       XX = [x(IEN(1,e))  x(IEN(2,e))  x(IEN(3,e))  x(IEN(4,e))  x(IEN(1,e))];
       YY = [y(IEN(1,e))  y(IEN(2,e))  y(IEN(3,e))  y(IEN(4,e))  y(IEN(1,e))];

       sxx = nodestress(IEN(:,e),1)./counter(IEN(:,e));
       syy = nodestress(IEN(:,e),2)./counter(IEN(:,e));
       sxy = nodestress(IEN(:,e),3)./counter(IEN(:,e));
             
       S1 = 0.5*(sxx+syy) + sqrt( (0.5*(sxx-syy)).^2 + sxy.^2);
       S2 = 0.5*(sxx+syy) - sqrt( (0.5*(sxx-syy)).^2 + sxy.^2);
        
       mises = sqrt( S1.^2 + S2.^2 - S1.*S2 );    % for the plane-stress case
       
       
       dd = [mises' mises(1)];
        
       figure(3); 
       patch(XX,YY,dd);hold on;  

   end
   title('Von Mises \sigma contours'); xlabel('X'); ylabel('Y'); colorbar
end


