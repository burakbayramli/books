function get_flux(d,e);
include_flags;

de = d(LM(:,e));    % extract temperature at  element nodes

% get coordinates of element nodes 
je = IEN(:,e);  
C  = [x(je); y(je)]'; 

[w,gp] = gauss(ngp);   % get Gauss points and weights

% compute flux vector
ind = 1;
for i=1:ngp
   for j=1:ngp
       eta = gp(i);  psi = gp(j);
      
       N             = NmatHeat2D(eta,psi);
       [B, detJ]     = BmatHeat2D(eta,psi,C);

       X(ind,:) =  N*C;                      % gauss points in physical coordinates  
       q(:,ind) =  -D*B*de;                  % compute the flux 
       ind     = ind + 1;
   end
end
q_x = q(1,:);
q_y = q(2,:);

%          #x-coord     y-coord    q_x(eta,psi)  q_y(eta,psi)
flux_e1  = [X(:,1)       X(:,2)        q_x'              q_y'];
fprintf(1,'\t\t\tx-coord\t\t\t\ty-coord\t\t\t\tq_x\t\t\t\t\tq_y\n');
fprintf(1,'\t\t\t%f\t\t\t%f\t\t\t%f\t\t\t%f\n',flux_e1');

if strcmpi(plot_flux,'yes')==1 & strcmpi(plot_mesh,'yes') ==1;  
    figure(1); 
    quiver(X(:,1),X(:,2),q_x',q_y','k');
    plot(X(:,1),X(:,2),'rx');
    title('Heat Flux');
    xlabel('X');
    ylabel('Y');
end

