% postprocessing function 
function postprocessor(d)
include_flags;

% loop over elements to plot displacements, moments and shear forces
for e = 1:nel  
    
de        	= d(LM(:,e));              	% extract element nodal displacements
IENe    	= IEN(:,e);               	% extract element connectivity information
xe        	= x(IENe);                	% extract element coordinates
J  	= (xe(nen) - xe(1))/2;    	        % Jacobian 
[w , gp]	= gauss(ngp);             	% extract Gauss points and weights 
    
% compute displacements, moments and shear forces 
xplot 	= linspace(xe(1),xe(nen),nplot);      % equally distributed coordinate within an element                  
xplotgauss= (2*xplot-xe(1)-xe(nen))/(xe(nen) - xe(1));
for i = 1:nplot
      xi   	= xplotgauss(i);                % current coordinate  
      N    = NmatrixBeam(xi,xe);   	        % shape functions 
      B    = BmatrixBeam(xi,xe)*1/J^2;      % first derivative of shape functions 
      S    = SmatrixBeam(xi,xe)*1/J^3;       % second derivative of shape functions
      Ee   = E(e);                          % Young's modulus  
      displacement(i) = N*de ;              % displacement output
      moment(i)       	= Ee*B*de;          % moment output
      shear(i)          = Ee*S*de;          % Shear force output
    end
    
% plot displacements, moment and shear forces
[x_plot,S_ex,M_ex,w_ex]=exact;
figure(2)
plot(xplot,displacement,'-.r'); hold on;
plot(x_plot,w_ex,'-k'); legend('FE','Exact Solution'); hold on;
ylabel('displacement');  xlabel('x'); title('Displacements: FE versus analytical beam solutions');
    
figure(3)
plot(xplot,moment,'-.r'); hold on;
plot(x_plot,M_ex,'-k'); legend('FE','Exact Solution'); hold on;
ylabel('moment'); xlabel('x'); title('Moments: FE versus analytical beam solutions');

figure(4)
plot(xplot,shear,'-.r'); hold on;
plot(x_plot,S_ex,'-k'); legend('FE','Exact Solution'); hold on;
ylabel('shear'); xlabel('x');   title('Shear: FE versus analytical beam solutions');
    
end
   