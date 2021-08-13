%% 2D Steady Navier-Stokes problem
% Based on "Finite Element Methods for flow problems" of
% Jean Donea and Antonio Huerta

% Andrea La Spina
% https://it.linkedin.com/in/andrealaspina
% https://independent.academia.edu/AndreaLaSpina
% https://www.researchgate.net/profile/Andrea_La_Spina

%% Equation

% -v*div(grad(u))+(u.div)u+grad(p)=b      in W
% div(u)=0                                in W
% u=u_D                                   on W_D

%% Initialization

initialization

%% Input

% Model parameters --------------------------------------------------------
v=1;                                    % Kinematic viscosity
b_fun=@(x,y) ...                        % Body force
    [(12-24*y)*x^4+(-24+48*y)*x^3+(-48*y+72*y^2-48*y^3+12)*x^2+...
     (-2+24*y-72*y^2+48*y^3)*x+(1-4*y+12*y^2-8*y^3),...
     (8-48*y+48*y^2)*x^3+(-12+72*y-72*y^2)*x^2+...
     (4-24*y+48*y^2-48*y^3+24*y^4)*x+(-12*y^2+24*y^3-12*y^4)];
% -------------------------------------------------------------------------

% Space -------------------------------------------------------------------
x_i=0;                                  % Initial point (x)
x_f=1;                                  % Final point (x)
y_i=0;                                  % Initial point (y)
y_f=1;                                  % Final point (y)
% -------------------------------------------------------------------------

% FEM ---------------------------------------------------------------------
n_el_x=10;                              % Number of finite elements (x)
n_el_y=10;                              % Number of finite elements (y)
n_gauss_side=3;                         % Number of Gauss points per side
body_force_selection='gauss';           % Selection points of body force
                                        % ('centre','gauss','nodes_v')
toll_p=1e-6;                            % Tollerance for pressure                                        
toll_v=1e-6;                            % Tollerance for velocity
% -------------------------------------------------------------------------


% Boundary and initial conditions -----------------------------------------
bound_cond_p=0;                         % Boundary conditions (pressure)
bound_cond_v=0;                         % Boundary conditions (velocity)
dof_constrained_string_p=...            % DOFs constrained (pressure)
    ['[1]'];
dof_constrained_string_v=...            % DOFs constrained (velocity)
    ['[2:n_np_x_v-1,',...
     'n_np_v-n_np_x_v+2:n_np_v-1,'...
     '1:n_np_x_v:n_np_v-n_np_x_v+1,'...
     'n_np_x_v:n_np_x_v:n_np_v]'];       
% -------------------------------------------------------------------------

% Plots -------------------------------------------------------------------
plot_geometry='yes';                    % Plot geometry ('yes','no')
plot_shape_functions='yes';             % Plot shape functions ('yes','no')
plot_test_functions='yes';              % Plot test functions ('yes','no')
% -------------------------------------------------------------------------

%% Derived parameters

% Common parameters -------------------------------------------------------
n_sd=2;                                 % Number of dimensional space
L_x=x_f-x_i;                            % Domain length (x)
L_y=y_f-y_i;                            % Domain length (y)
n_el=n_el_x*n_el_y;                     % Number of finite elements
n_gauss=n_gauss_side^2;                 % Number of Gauss points
L_el_x=L_x/n_el_x;                      % Length of a finite element (x)
L_el_y=L_y/n_el_y;                      % Length of a finite element (y)
% -------------------------------------------------------------------------

% Pressure ----------------------------------------------------------------
n_np_x_p=n_el_x+1;                      % Number of nodal points (x)
n_np_y_p=n_el_y+1;                      % Number of nodal points (y)
n_np_p=n_np_x_p*n_np_y_p;               % Number of nodal points
dof_el_p=4;                             % Number of DOFs per element
dof_p=n_np_p;                           % Total number of DOFs
h_x_p=L_el_x;                           % Spatial step (x)
h_y_p=L_el_y;                           % Spatial step (y)
x_p=x_i:h_x_p:x_f;                      % Space vector (x)
y_p=y_i:h_y_p:y_f;                      % Space vector (y)
% -------------------------------------------------------------------------

% Velocity ----------------------------------------------------------------
n_np_x_v=2*n_el_x+1;                    % Number of nodal points (x)
n_np_y_v=2*n_el_y+1;                    % Number of nodal points (y)
n_np_v=n_np_x_v*n_np_y_v;               % Number of nodal points
dof_el_v=9;                             % Number of DOFs per element
dof_v=n_np_v;                           % Total number of DOFs
h_x_v=L_el_x/2;                         % Spatial step (x)
h_y_v=L_el_y/2;                         % Spatial step (y)
x_v=x_i:h_x_v:x_f;                      % Space vector (x)
y_v=y_i:h_y_v:y_f;                      % Space vector (y)
% -------------------------------------------------------------------------

% Body force --------------------------------------------------------------
n_np_x_b=n_el_x;                        % Number of nodal points (x)
n_np_y_b=n_el_y;                        % Number of nodal points (y)
n_np_b=n_np_x_b*n_np_y_b;               % Number of nodal points
h_x_b=L_el_x;                           % Spatial step (x)
h_y_b=L_el_y;                           % Spatial step (y)
x_b=x_i+L_el_x/2:h_x_b:x_f-L_el_x/2;    % Space vector (x)
y_b=y_i+L_el_y/2:h_y_b:y_f-L_el_y/2;    % Space vector (y)
% -------------------------------------------------------------------------

%% Gauss parameters

gauss=[];
[gauss]=Gauss_parameters_2D(n_gauss_side,gauss);

% Trasformation of coordinated for the Gauss integration points
for n=1:n_gauss
    x_gauss(n)=L_el_x/2*(1+gauss(n).csi);
    y_gauss(n)=L_el_y/2*(1+gauss(n).eta);
end

% Jacobian of the transformation
J_mat=[L_el_x/2    0
          0    L_el_y/2];
J=det(J_mat);

% Computation of shape and test functions (and derivatives) at Gauss points
% for pressure
[gauss]=shape_functions_Gauss_points_2D_p(gauss);
[gauss]=test_functions_Gauss_points_2D_p(gauss);

% Computation of shape and test functions (and derivatives) at Gauss points
% for velocity
[gauss]=shape_functions_Gauss_points_2D_v(gauss);
[gauss]=test_functions_Gauss_points_2D_v(gauss);

%% Plot of geometry

if strcmp(plot_geometry,'yes')==1
    if n_el<=100
        plot_geometry_2D(x_p,y_p,x_v,y_v,L_x,L_y,n_gauss,...
                                            L_el_x,L_el_y,x_gauss,y_gauss);
    end
end

%% Plot of shape and test functions

% Normalized domain
csi_plot=-1:0.1:1;
eta_plot=-1:0.1:1;

if strcmp(plot_shape_functions,'yes')==1
    % Shape functions for pressure
    N_plot_p=f_N_plot_2D_p(csi_plot,eta_plot);
    plot_shape_functions_2D(csi_plot,eta_plot,N_plot_p,dof_el_p);
    
    % Shape functions for velocity
    N_plot_v=f_N_plot_2D_v(csi_plot,eta_plot);
    plot_shape_functions_2D(csi_plot,eta_plot,N_plot_v,dof_el_v);
end

if strcmp(plot_test_functions,'yes')==1
    % Test functions for pressure
    W_plot_p=f_W_plot_2D_p(csi_plot,eta_plot);
    plot_test_functions_2D(csi_plot,eta_plot,W_plot_p,dof_el_p);
    
    % Test functions for velocity
    W_plot_v=f_W_plot_2D_v(csi_plot,eta_plot);
    plot_test_functions_2D(csi_plot,eta_plot,W_plot_v,dof_el_v);
end


%% Evaluate matrices and vectors

% Afference matrix for pressure
[A_p]=afference_matrix_2D_p(n_np_x_p,n_np_y_p,dof_el_p);

% Afference matrix for velocity
[A_v]=afference_matrix_2D_v(n_np_x_v,n_np_y_v,dof_el_v);

% Strain rate - velocity matrix
[gauss]=strain_rate_velocity_matrix_2D(dof_el_v,gauss,L_el_x,L_el_y);

% Element viscosity matrix
for n=1:n_el
    el(n).K=element_viscosity_matrix_2D(v,dof_el_v,gauss,J);
end

% Element gradient operator matrix
for n=1:n_el
    el(n).G=element_gradient_operator_matrix_2D(dof_el_v,dof_el_p,gauss,...
                                                          J,L_el_x,L_el_y);
end

% Element load vector for velocity
for n=1:n_el
    [r,c]=row_column(n,n_el_x);
    x_c=x_i+L_el_x/2+L_el_x*(c-1);
    y_c=y_i+L_el_y*(n_el_y-1/2)-L_el_y*(r-1);
    if strcmp(body_force_selection,'centre')==1
        b_c=feval(b_fun,x_c,y_c);
        el(n).f=element_load_vector_centre_2D(b_c,dof_el_v,gauss,J);
    elseif strcmp(body_force_selection,'gauss')==1
        for k=1:n_gauss
            x_g(k)=x_c-L_el_x/2+x_gauss(k);
            y_g(k)=y_c-L_el_y/2+y_gauss(k);
            b_g(k,:)=feval(b_fun,x_g(k),y_g(k));
        end
        el(n).f=element_load_vector_gauss_2D(b_g,dof_el_v,gauss,J);
    elseif strcmp(body_force_selection,'nodes_v')==1
        x_n(1)=x_c-L_el_x/2; y_n(1)=y_c-L_el_y/2;
        x_n(2)=x_c+L_el_x/2; y_n(2)=y_c-L_el_y/2;
        x_n(3)=x_c+L_el_x/2; y_n(3)=y_c+L_el_y/2;
        x_n(4)=x_c-L_el_x/2; y_n(4)=y_c+L_el_y/2;
        x_n(5)=x_c;          y_n(5)=y_c-L_el_y/2;
        x_n(6)=x_c+L_el_x/2; y_n(6)=y_c;
        x_n(7)=x_c;          y_n(7)=y_c+L_el_y/2;
        x_n(8)=x_c-L_el_x/2; y_n(8)=y_c;
        x_n(9)=x_c;          y_n(9)=y_c;
        for k=1:dof_el_v
            b_n(k,:)=feval(b_fun,x_n(k),y_n(k));
        end
        el(n).f=element_load_vector_nodes_v_2D(b_n,dof_el_v,gauss,J);
    end
end

% Element load vector for pressure
for n=1:n_el
    el(n).h=zeros(dof_el_p,1);
end

%% Assemblate matrices and vectors

% Assemblage of viscosity matrix
[K]=assemble_viscosity_matrix(el,dof_v,n_el,dof_el_v,A_v);

% Assemblage of gradient operator matrix
[G]=assemble_gradient_operator_matrix(el,dof_v,dof_p,n_el,dof_el_v,...
                                                         dof_el_p,A_v,A_p);

% Divergence operator matrix
[G_T]=G';

% Zero matrix
ZERO=zeros(dof_p,dof_p);

% Assemblage of load vector for velocity
[f]=assemble_load_vector_v(el,dof_v,n_el,dof_el_v,A_v);

% Assemblage of load vector for pressure
[h]=assemble_load_vector_p(el,dof_p,n_el,dof_el_p,A_p);

%% Conversion of the matrices from full to sparse

K=sparse(K);
G=sparse(G);
G_T=sparse(G_T);
f=sparse(f);
h=sparse(h);

%% Boundary conditions

% Definition of the constrained DOFs for the pressure
dof_constrained_p=eval(dof_constrained_string_p);
dof_free_p=dof_p-length(dof_constrained_p);
dof_constrained_p=sort(dof_constrained_p);
n_dof_constrained_p=length(dof_constrained_p);

% Definition of the constrained DOFs for the velocity
dof_constrained_v=eval(dof_constrained_string_v);
dof_free_v=dof_v-length(dof_constrained_v);
dof_constrained_v=sort(dof_constrained_v);
n_dof_constrained_v=length(dof_constrained_v);
dof_constrained_v_X_Y=sort([dof_constrained_v.*2-1,dof_constrained_v.*2]);
dof_free_v_X_Y=dof_v*n_sd-length(dof_constrained_v_X_Y);
n_dof_constrained_v_X_Y=length(dof_constrained_v_X_Y);

% Evaluation of boundary conditions for velocity
u_p=zeros(n_dof_constrained_v_X_Y,1);
for n=1:n_dof_constrained_v_X_Y
    u_p(n,1)=bound_cond_v;
end

% Evaluation of boundary conditions for pressure
p_p=zeros(n_dof_constrained_p,1);
for n=1:n_dof_constrained_p
    p_p(n,1)=bound_cond_p;
end

% Viscosity matrix
[K_ff,K_fp,K_pf,K_pp]=constrain_matrix(K,...
                              dof_constrained_v_X_Y,dof_constrained_v_X_Y);

% Gradient operator matrix
[G_ff,G_fp,G_pf,G_pp]=constrain_matrix(G,...
                                  dof_constrained_v_X_Y,dof_constrained_p);

% Divergence operator matrix
[G_T_ff,G_T_fp,G_T_pf,G_T_pp]=constrain_matrix(G_T,...
                                  dof_constrained_p,dof_constrained_v_X_Y);

% Zero matrix
[ZERO_ff,ZERO_fp,ZERO_pf,ZERO_pp]=constrain_matrix(ZERO,...
                                      dof_constrained_p,dof_constrained_p);

% Load vector for velocity
[f_f,f_p]=constrain_vector(f,dof_constrained_v_X_Y);

% Load vector for pressure
[h_f,h_p]=constrain_vector(h,dof_constrained_p);

%% Steady Navier-Stokes problem

% Initialization of velocities
u=zeros(dof_v*n_sd,1);
p=zeros(dof_p,1);

norm_err_p(1)=toll_p+1;
norm_err_v(1)=toll_v+1;

% Convergence plots
figure('Color',[1 1 1])
axes('FontSize',14)
subplot(1,2,1,'YScale','log')
hold on
title('Convergence of pressure','FontSize',14)
xlabel('Iteration','FontSize',14)
ylabel('Norm((p_k-p_k_-_1)/p_k)','FontSize',14)
grid on
grid minor
subplot(1,2,2,'YScale','log')
hold on
title('Convergence of velocity','FontSize',14)
xlabel('Iteration','FontSize',14)
ylabel('Norm((u_k-u_k_-_1)/u_k)','FontSize',14)
grid on
grid minor
pause(eps)

k=1;
disp('NON-LINEAR ITERATIONS')
disp('-------------------------------------------------------------------')
while norm_err_p(end)>toll_p || norm_err_v(end)>toll_v
    
    % Previous evaluation
    p_old=p;
    u_old=u;
    
    % Element convection matrix
    for n=1:n_el
        u_el=zeros(dof_el_v*n_sd,1);
        for a=1:dof_el_v
            for i=1:n_sd
                r=n_sd*(a-1)+i;
                u_el(r,1)=u(A_v(n,r),1);
            end
        end
        el(n).C=element_convection_matrix_2D(u_el,dof_el_v,gauss,J,...
                                                            L_el_x,L_el_y);
    end
    
    % Assemblage of convection matrix
    [C]=assemble_convection_matrix(el,dof_v,n_el,dof_el_v,A_v);
    
    % Conversion of the convection matrix from full to sparse
    C=sparse(C);
    
    % Viscosity+Convection matrix
    D=K+C;
    
    % Constrain viscosity+convection matrix
    [D_ff,D_fp,D_pf,D_pp]=constrain_matrix(D,...
                              dof_constrained_v_X_Y,dof_constrained_v_X_Y);
    
    % Evaluation of inv(D_ff)
    tic
    D_ff_inv=inv(D_ff);
    time_inversion(k)=toc; 
    
    % 1st step: solving for the pressure
    tic
    p_f=(G_T_ff*D_ff_inv*G_ff)\...
        (G_T_ff*D_ff_inv*(f_f-D_fp*u_p-G_fp*p_p)+G_T_fp*u_p-h_f);
    time_solution_p(k)=toc;
    
    
    % 2nd step: solving for the velocity
    tic
    u_f=D_ff_inv*(f_f-G_ff*p_f-D_fp*u_p-G_fp*p_p);
    time_solution_v(k)=toc;
    
    time_analysis(k)=time_inversion(k)+time_solution_p(k)+...
                                                        time_solution_v(k);
    
    % Data for all dof
    [u]=data_all_dof(u_f,u_p,dof_constrained_v_X_Y);
    [p]=data_all_dof(p_f,p_p,dof_constrained_p);
    
    % Current evaluation
    u_new=u;
    p_new=p;
    
    % Evaluation of errors
    norm_err_p(k)=norm((p_new-p_old)/p_new);
    norm_err_v(k)=norm((u_new-u_old)/u_new);
        
    % Display results
    fprintf('\nIteration = %d',k-1)
    fprintf('\tNorm_p = %.2e',norm_err_p(k))
    fprintf('\tNorm_v = %.2e',norm_err_v(k))
    fprintf('\tElapsed time %.2f sec',sum(time_analysis(1:k)))
    fprintf('\tReynolds = %.1f',max(u)*mean(L_x,L_y)/v)
    
    % Convergence plot update
    subplot(1,2,1)
    semilogy(0:k-1,norm_err_p,'g','LineWidth',2)
    plot([0,k+1],[toll_p,toll_p],'r','LineWidth',2)
    xlim([0,k+1])
    ylim([min(toll_p/10,min(norm_err_p)),max(10,max(norm_err_p)*10)])
    subplot(1,2,2)
    semilogy(0:k-1,norm_err_v,'b','LineWidth',2)
    plot([0,k+1],[toll_v,toll_v],'r','LineWidth',2)
    xlim([0,k+1])
    ylim([min(toll_v/10,min(norm_err_v)),max(10,max(norm_err_v)*10)])
    pause(eps)
    
    k=k+1;
end
fprintf('\n')
disp('-------------------------------------------------------------------')

% Conversion of data from vector to matrix
for j=1:n_np_y_v
    [u_x_matrix(:,j)]=u(((n_np_y_v-j)*n_np_x_v+1)*2-1:2:...
                                     ((n_np_y_v-j)*n_np_x_v+n_np_x_v)*2-1);
    [u_y_matrix(:,j)]=u(((n_np_y_v-j)*n_np_x_v+1)*2-0:2:...
                                     ((n_np_y_v-j)*n_np_x_v+n_np_x_v)*2-0);
end
u_matrix=sqrt(u_x_matrix.^2+u_y_matrix.^2);

for j=1:n_np_y_p
    [p_matrix(:,j)]=p((n_np_y_p-j)*n_np_x_p+1:...
                                           (n_np_y_p-j)*n_np_x_p+n_np_x_p);
end

%% Plots

% Grid matrices
[X_v,Y_v]=meshgrid(x_v,y_v);
[X_p,Y_p]=meshgrid(x_p,y_p);
[X_b,Y_b]=meshgrid(x_b,y_b);

% Evaluation of the body force field
for n=1:n_np_b
    [r,c]=row_column(n,n_np_x_b);
    xp=x_i+L_el_x*(c-1)+L_el_x/2;
    yp=y_i+L_el_y*n_el_y-L_el_y*(r-1)-L_el_y/2;
    b(n,:)=feval(b_fun,xp,yp);
end
b_x=b(:,1);
b_y=b(:,2);

for j=1:n_np_y_b
    [b_x_matrix(:,j)]=b_x((n_np_y_b-j)*n_np_x_b+1:...
                                           (n_np_y_b-j)*n_np_x_b+n_np_x_b);
    [b_y_matrix(:,j)]=b_y((n_np_y_b-j)*n_np_x_b+1:...
                                           (n_np_y_b-j)*n_np_x_b+n_np_x_b);
end
b_matrix=sqrt(b_x_matrix.^2+b_y_matrix.^2);

% Limit values
u_x_min=min(min(u_x_matrix));
u_x_max=max(max(u_x_matrix));
u_y_min=min(min(u_y_matrix));
u_y_max=max(max(u_y_matrix));
p_min=min(min(p_matrix));
p_max=max(max(p_matrix));

% Body force vector field
figure('Color',[1 1 1])
axes('FontSize',14)
quiver(X_b',Y_b',b_x_matrix,b_y_matrix,...
       'LineWidth',1,'Color',[1 0 0],'AutoScaleFactor',1.5)
title('Body force vector field','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
grid on
grid minor
xlim([x_i,x_f])
ylim([y_i,y_f])

% Body force field
figure('Color',[1 1 1])
axes('FontSize',14)
contourf(X_b',Y_b',b_matrix,'LineWidth',1)
title('Body force field','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
grid on
grid minor
xlim([x_i,x_f])
ylim([y_i,y_f])

% Solution of velocity in x
figure('Color',[1 1 1])
axes('FontSize',14)
surf(X_v',Y_v',u_x_matrix)
title('Solution of u_x','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
zlabel('u_x(x,y)','FontSize',14)
grid on
grid minor
xlim([x_i,x_f])
ylim([y_i,y_f])
zlim([u_x_min,u_x_max])

% Solution of velocity in y
figure('Color',[1 1 1])
axes('FontSize',14)
surf(X_v',Y_v',u_y_matrix)
title('Solution of u_y','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
zlabel('u_y(x,y)','FontSize',14)
grid on
grid minor
xlim([x_i,x_f])
ylim([y_i,y_f])
zlim([u_y_min,u_y_max])

% Solution of velocity vector field
figure('Color',[1 1 1])
axes('FontSize',14)
quiver(X_v',Y_v',u_x_matrix,u_y_matrix,...
       'LineWidth',1,'Color',[1 0 0],'AutoScaleFactor',1.5)
title('Solution of velocity vector field','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
grid on
grid minor
xlim([x_i,x_f])
ylim([y_i,y_f])

% Solution of velocity field
figure('Color',[1 1 1])
axes('FontSize',14)
contourf(X_v',Y_v',u_matrix,'LineWidth',1)
title('Velocity field','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
grid on
grid minor
xlim([x_i,x_f])
ylim([y_i,y_f])

% Solution of pressure
figure('Color',[1 1 1])
axes('FontSize',14)
surf(X_p',Y_p',p_matrix)
title('Solution of p','FontSize',14)
xlabel('x','FontSize',14)
ylabel('y','FontSize',14)
zlabel('p(x,y)','FontSize',14)
grid on
grid minor
xlim([x_i,x_f])
ylim([y_i,y_f])
zlim([p_min,p_max])

%% Display in command window

disp('MODEL PARAMETERS')
disp('-------------------------------------------------------------------')
fprintf('Length (x)\t\t\t=\t%.1f\n',L_x)
fprintf('Length (y)\t\t\t=\t%.1f\n',L_y)
fprintf('Body force\t\t\t=\t%s\n',char(b_fun))
fprintf('Kinematic viscosity\t\t=\t%.2f\n',v)
fprintf('DOFs constrained (velocity)\t=\t%s\n',dof_constrained_string_v)
fprintf('DOFs constrained (pressure)\t=\t%s\n',dof_constrained_string_p)
fprintf('Boundary conditions (velocity)\t=\t%.1f\n',bound_cond_v)
fprintf('Boundary conditions (pressure)\t=\t%.1f\n',bound_cond_p)
disp('-------------------------------------------------------------------')

disp(' ')

disp('FEM PARAMETERS')
disp('-------------------------------------------------------------------')
fprintf('Number of finite elements (x)\t=\t%d\n',n_el_x)
fprintf('Number of finite elements (y)\t=\t%d\n',n_el_y)
fprintf('Length of a finite element (x)\t=\t%.2f\n',L_el_x)
fprintf('Length of a finite element (y)\t=\t%.2f\n',L_el_y)
fprintf('Gauss integration points\t=\t%d\n',n_gauss)
fprintf('Number of nodes (velocity)\t=\t%d\n',n_np_v)
fprintf('Number of nodes (pressure)\t=\t%d\n',n_np_p)
fprintf('DOFs per element (velocity)\t=\t%d\n',dof_el_v)
fprintf('DOFs per element (pressure)\t=\t%d\n',dof_el_p)
fprintf('Total number of DOF (velocity)\t=\t%d\n',dof_v)
fprintf('Total number of DOF (pressure)\t=\t%d\n',dof_p)
disp('-------------------------------------------------------------------')

disp(' ')

disp('ANALYSIS TIME')
disp('-------------------------------------------------------------------')
fprintf('Total analysis time\t\t=\t%.2f sec\n',sum(time_analysis(1:end)))
disp('-------------------------------------------------------------------')