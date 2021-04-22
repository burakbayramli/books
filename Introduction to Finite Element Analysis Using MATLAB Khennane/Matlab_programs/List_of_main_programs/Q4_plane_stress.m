% THIS PROGRAM USES AN 4-NODED QUADRILATERAL ELEMENT FOR THE LINEAR ELASTIC 
% STATIC ANALYSIS OF A TWO DIMENSIONAL PROBLEM
%
% Make these variables global so they can be shared by other functions
%
clc
clear all
global nnd nel nne nodof eldof n ngp
global geom connec dee nf Nodal_loads
%
 format long g
%
% To change the size of the problem or change the elastic properties
% supply another input file
%
Q4_COARSE_MESH_DATA  
%
%%%%%%%%%%%%%%%%%%%%%%%%%% End of input%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%
% Assemble the global force vector
%
fg=zeros(n,1);
for i=1: nnd
    if nf(i,1) ~= 0   
        fg(nf(i,1))= Nodal_loads(i,1);
    end
    if nf(i,2) ~= 0   
        fg(nf(i,2))= Nodal_loads(i,2);
    end
end 
%
%  Form the matrix containing the abscissas and the weights of Gauss points
%
ngp = 2;
samp=gauss(ngp);
%
% Numerical integration and assembly of the global stiffness matrix
%
%  initialise the global stffness matrix to zero
kk = zeros(n, n);
%
for i=1:nel
    [coord,g] = elem_q4(i) ;   % coordinates of the nodes of element i, 
                               % and its steering vector 
    ke=zeros(eldof,eldof) ;    % Initialise the element stiffness matrix
                               % to zero
    for ig=1: ngp
        wi = samp(ig,2);
    for jg=1: ngp
        wj=samp(jg,2);
        [der,fun] = fmlin(samp, ig,jg);  % Derivative of shape functions 
                                         %in local coordinates
        jac=der*coord;                   % Compute Jacobian matrix
        d=det(jac);                      % Compute determinant of Jacobian
                                         % matrix
        jac1=inv(jac);                   % Compute inverse of the Jacobian
        deriv=jac1*der;                  % Derivative of shape functions 
                                         % in global coordinates
        bee=formbee(deriv,nne,eldof);    %  Form matrix [B]
        ke=ke + d*thick*wi*wj*bee'*dee*bee; % Integrate stiffness matrix
    end
    end
    kk=form_kk(kk,ke, g);               % assemble global stiffness matrix
end
%
%
%%%%%%%%%%%%%%%%%%%%%%%  End of assembly %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
delta = kk\fg ;                          % solve for unknown displacements

disp('node       x_disp       y_disp ')     %
for i=1: nnd                                %
    if nf(i,1) == 0                         %
        x_disp =0.;                         %
    else
        x_disp = delta(nf(i,1));            %
    end
%    
    if nf(i,2) == 0                         %
        y_disp = 0.;                        %
    else
        y_disp = delta(nf(i,2));            %
    end
disp([i  x_disp  y_disp])              % Display displacements of each node
DISP(i,:) = [x_disp  y_disp]
end

%
%
ngp=1;                              % Calculate stresses and strains at 
                                    %the centre of each element
samp=gauss(ngp);
%
for i=1:nel
    [coord,g] = elem_q4(i);    % coordinates of the nodes of element i, 
                               % and its steering vector 
    eld=zeros(eldof,1);        % Initialise element displacement to zero
    for m=1:eldof              %
        if g(m)==0             %
            eld(m)=0.;         %
        else                   %
            eld(m)=delta(g(m));  % Retrieve element displacement from the 
                                 % global displacement vector
        end
    end
%
    for ig=1: ngp
        wi = samp(ig,2);
    for jg=1: ngp
        wj=samp(jg,2);
        [der,fun] = fmlin(samp, ig,jg);    % Derivative of shape functions
                                           % in local coordinates
        jac=der*coord;                     % Compute Jacobian matrix
        jac1=inv(jac);                     % Compute inverse of the Jacobian
        deriv=jac1*der;                    % Derivative of shape functions
                                           % in global coordinates
        bee=formbee(deriv,nne,eldof);      % Form matrix [B]
        eps=bee*eld                        % Compute strains
        sigma=dee*eps                      % Compute stresses
    end
    end
   SIGMA(i,:)=sigma ;           % Store stresses for all elements
end
%
% Average stresses at nodes
%
[ZX, ZY, ZT, Z1, Z2]=stresses_at_nodes_Q4(SIGMA);
%
%
% Plot stresses in the x_direction 
%
U2 = DISP(:,2);
cmin = min(U2);
cmax = max(U2);
caxis([cmin cmax]);
patch('Faces', connec, 'Vertices', geom, 'FaceVertexCData',U2,...
                             'Facecolor','interp','Marker','.');
colorbar;
