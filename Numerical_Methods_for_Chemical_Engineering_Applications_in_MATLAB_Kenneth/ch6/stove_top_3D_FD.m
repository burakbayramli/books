% stove_top_3D_FD.m
%
% This MATLAB program uses the finite difference method
% to solve a 3-D BVP that models heat transfer within
% a 3-D stove top element.
%
% K. Beers. MIT ChE. 12/1/02. v 10/4/03
function iflag_main = stop_top_3D_FD();
iflag_main = 0;

% ----- CHECK FOR RESTART ------------------------------------------
% If this is a restart, we do not need to compute matrix again.
% Note, that we do not read in old value as initial guess.
% We only have restart to test difference solution methods
% quickly.
isrestart = input('Is this a restart, 0(no) or 1(yes) : ');
if(isrestart)
    load stove_top_3D_FD.mat;  % load previous results
    
    % Next, choose solver.  Either use direct method
    % (MATLAB elimination solver) or CG-type.
    isolver = input('Choose direct (0), bicgstab (1), or gmres(2) solver : ');
    % select preconditioner
    if(isolver)
        mode_precond = ...
            input('Use preconditioner? : none (0), Jacobi (1), or ILU (2)? : ');
    end
    if(isolver >= 2)
        restart = input('Enter RESTART for GMRES solver : ');
    end
    if(mode_precond >= 2)
        mode_fill = ...
            input('ILU: no-fill (0) or non-zero drop tol (1) : ');
        if(mode_fill)
            drop_tol = input('Enter drop tol : ');
            ILU_Options.droptol = drop_tol;
        end
        ILU_Options.milu = 1;
        ILU_Options.udiag = 1;
    end
    
else

    % ----- ENTER SYSTEM PARAMETERS ------------------------------------
    sigma = input('Enter sigma (e.g. 1) : ');

    % Next, set the physical parameters of the
    % problem.
    a_geom = input('Enter a (L/D) (e.g. 2/3) : ');  % L/D
    b_geom = input('Enter b (d1/L) (e.g. 0.5) : ');  % d1/L 
    % inner radius and thickness of first annular region
    r1_L = input('Enter r1_L (in. rad. of 1st annulus) (e.g. 0.1) : ');
    t1_L = input('Enter t1_L (thick. of 1st annulus  (e.g. 0.05) : ');
    % inner radius and thickness of second annular region
    r2_L = input('Enter r2_L (in. rad. of 2nd annulus) (e.g. 0.25) : ');
    t2_L = input('Enter t2_L (thick. of 2nd annulus) (e.g. 0.05) : ');

    % Enter grid information
    N_xy = input('Enter # of grid points in x and y directions (e.g. 51) : ');
    x_grid = linspace(0,0.5,N_xy);  dx = x_grid(2) - x_grid(1);
    y_grid = linspace(0,0.5,N_xy);  dy = y_grid(2) - y_grid(1);
    N_z = input('Enter # of grid points in z direction (e.g. 25) : ');
    % Now, as we expect strongest gradients to occur near surface, we place 1/4
    % of the points in the range z =0 to z = -0.2.
    Nz_up = round(N_z/4);
    dz_fine = 0.2/(Nz_up-1);
    z_grid_fine = linspace(0,0.2-dz_fine,Nz_up)';
    % place remaining points in lower part of domain
    z_grid_coarse = linspace(0.2, 1/a_geom,N_z - Nz_up)';
    z_grid = [z_grid_fine; z_grid_coarse];
    N_z = length(z_grid);

    % Set total domension of problem
    Ntot = N_z*(N_xy^2);  % total number of grid points
    disp(['Grid size, N_xy = ', int2str(N_xy), ...
            ', N_z = ', int2str(N_z), ...
            ', Ntot = ', int2str(Ntot)]);
    i_go_on = input('Stop (0) or continue (1)? : ');
    if(~i_go_on)
        return;
    end

    % Next, choose solver.  Either use direct method
    % (MATLAB elimination solver) or CG-type.
    isolver = input('Choose direct (0), bicgstab (1), or gmres(2) solver : ');
    % select preconditioner
    if(isolver)
        mode_precond = ...
            input('Use preconditioner? : none (0), Jacobi (1), or ILU (2)? : ');
    end
    if(isolver >= 2)
        restart = input('Enter RESTART for GMRES solver : ');
    end
    if(mode_precond >= 2)
        mode_fill = ...
            input('ILU: no-fill (0) or non-zero drop tol (1) : ');
        if(mode_fill)
            drop_tol = input('Enter drop tol : ');
            ILU_Options.droptol = drop_tol;
        end
        ILU_Options.milu = 1;
        ILU_Options.udiag = 1;
    end


    % ----- SET MATRIX AND VECTOR FOR PROBLEM ---------------------------
    % Allocate space for matrix using sparse matrix
    % notation. Also allocate space for right hand
    % side vector.
    A = spalloc(Ntot,Ntot,7*Ntot);
    b = zeros(Ntot,1);

    % We now set the linear system grid point by
    % grid point, starting first with the interior
    % points.

    % First, get factors we will need many times
    factor_x_up = zeros(size(x_grid));
    factor_x_lo = zeros(size(x_grid));
    for ix=2:(N_xy-1)
        xmid_up = 0.5*(x_grid(ix+1)+x_grid(ix));
        xmid_lo = 0.5*(x_grid(ix)+x_grid(ix-1));
        factor_x_up(ix) = 1 / (xmid_up - xmid_lo) / ...
            (x_grid(ix+1) - x_grid(ix));
        factor_x_lo(ix) = 1 / (xmid_up - xmid_lo) / ...
            (x_grid(ix) - x_grid(ix-1));
    end
    factor_y_up = zeros(size(y_grid));
    factor_y_lo = zeros(size(y_grid));
    for iy=2:(N_xy-1)
        ymid_up = 0.5*(y_grid(iy+1)+y_grid(iy));
        ymid_lo = 0.5*(y_grid(iy)+y_grid(iy-1));
        factor_y_up(iy) = 1 / (ymid_up - ymid_lo) / ...
            (y_grid(iy+1) - y_grid(iy));
        factor_y_lo(iy) = 1 / (ymid_up - ymid_lo) / ...
            (y_grid(iy) - y_grid(iy-1));
    end 
    factor_z_up = zeros(size(z_grid));
    factor_z_lo = zeros(size(z_grid));
    for iz=2:(N_z-1)
        zmid_up = 0.5*(z_grid(iz+1)+z_grid(iz));
        zmid_lo = 0.5*(z_grid(iz)+z_grid(iz-1));
        factor_z_up(iz) = 1 / (zmid_up - zmid_lo) / ...
            (z_grid(iz+1) - z_grid(iz));
        factor_z_lo(iz) = 1 / (zmid_up - zmid_lo) / ...
            (z_grid(iz) - z_grid(iz-1));
    end

    % Now construct the matrix elements for the
    % interior point rows from these values.
    for ix=2:(N_xy-1)
        for iy=2:(N_xy-1)
            for iz=2:(N_z-1)
                % get master label
                m = calc_label(ix,iy,iz,N_xy,N_z);
  
                % set matrix elements for this row
                % (ix-1,iy,iz)
                n = calc_label(ix-1,iy,iz,N_xy,N_z);
                A(m,n) = -factor_x_lo(ix);
                % (ix+1,iy,iz)
                n = calc_label(ix+1,iy,iz,N_xy,N_z);
                A(m,n) = -factor_x_up(ix);
                % (ix,iy-1,iz)
                n = calc_label(ix,iy-1,iz,N_xy,N_z);
                A(m,n) = -factor_y_lo(iy);
                % (ix,iy+1,iz)
                n = calc_label(ix,iy+1,iz,N_xy,N_z);
                A(m,n) = -factor_y_up(iy);
                % (ix,iy,iz-1)
                n = calc_label(ix,iy,iz-1,N_xy,N_z);
                A(m,n) = -factor_z_lo(iz);
                % (ix,iy,iz+1)
                n = calc_label(ix,iy,iz+1,N_xy,N_z);
                A(m,n) = -factor_z_up(iz);
                % (ix,iy,iz)
                A(m,m) = factor_x_lo(ix) + factor_x_up(ix) + ...
                    factor_y_lo(iy) + factor_y_up(iy) + ...
                    factor_z_lo(iz) + factor_z_up(iz);
    
                % set right hand side vector component
                H_loc = get_H_heat_region(x_grid(ix),y_grid(iy),z_grid(iz),...
                    a_geom,b_geom,r1_L,t1_L,r2_L,t2_L);
                b(m) = sigma*H_loc;
            end
        end
    end
    disp('Finished setting interior matrix/vector');

    disp('Setting boundary conditions ...');
    mode_BC = 1;
    % BC # 6 at z = 0
    iz=1;
    for ix=1:N_xy
        for iy=1:N_xy
            m = calc_label(ix,iy,iz,N_xy,N_z);
            A(m,m) = 1; b(m) = 0;
        end
    end
    % BC # 5 at z = N
    iz = N_z;
    for ix=1:N_xy
        for iy=1:N_xy
            m = calc_label(ix,iy,iz,N_xy,N_z);
            n1 = calc_label(ix,iy,iz-1,N_xy,N_z);
            if(mode_BC==0);
                A(m,m) = 1; A(m,n1) = -1; b(m) = 0;
            else
                n2 = calc_label(ix,iy,iz-2,N_xy,N_z);
                A(m,m)=3; A(m,n1)=-4; A(m,n2)=1; b(m) = 0;
            end
        end
    end
    % BC # 1 at y = 0
    iy = 1;
    for ix=1:N_xy
        for iz=2:(N_z-1)
            m = calc_label(ix,iy,iz,N_xy,N_z);
            n1 = calc_label(ix,iy+1,iz,N_xy,N_z);
            if(mode_BC==0);
                A(m,m) = 1; A(m,n1) = -1; b(m) = 0;
            else
                n2 = calc_label(ix,iy+2,iz,N_xy,N_z);
                A(m,m)=3; A(m,n1)=-4; A(m,n2)=1; b(m) = 0;
            end
        end
    end 
    % BC # 2 at y = 1
    iy = N_xy;
    for ix=1:N_xy
        for iz=2:(N_z-1)
            m = calc_label(ix,iy,iz,N_xy,N_z);
            n1 = calc_label(ix,iy-1,iz,N_xy,N_z);
            if(mode_BC==0);
                A(m,m) = 1; A(m,n1) = -1; b(m) = 0;
            else
                n2 = calc_label(ix,iy-2,iz,N_xy,N_z);
                A(m,m)=3; A(m,n1)=-4; A(m,n2)=1; b(m) = 0;
            end
        end
    end
    % BC # 3 at x = 0
    ix = 1;
    for iy=2:(N_xy-1)
        for iz=2:(N_z-1)
            m = calc_label(ix,iy,iz,N_xy,N_z);
            n1 = calc_label(ix+1,iy,iz,N_xy,N_z);
            if(mode_BC==0);
                A(m,m) = 1; A(m,n1) = -1; b(m) = 0;
            else
                n2 = calc_label(ix+2,iy,iz,N_xy,N_z);
                A(m,m)=3; A(m,n1)=-4; A(m,n2)=1; b(m) = 0;
            end
        end
    end
    % BC # 4 at x = 1
    ix = N_xy;
    for iy=2:(N_xy-1)
        for iz=2:(N_z-1)
            m = calc_label(ix,iy,iz,N_xy,N_z);
            n1 = calc_label(ix-1,iy,iz,N_xy,N_z);
            if(mode_BC==0);
                A(m,m) = 1; A(m,n1) = -1; b(m) = 0;
            else
                n2 = calc_label(ix-2,iy,iz,N_xy,N_z);
                A(m,m)=3; A(m,n1)=-4; A(m,n2)=1; b(m) = 0;
            end
        end
    end
end


% ---- CALLING LINEAR SOLVER ------------------------------
% compute the temperature profile by solving the
% linear system

% direct solver
if(isolver==0)

    % solve using MATLAB's built-in elimination solver
    disp('calling direct solver ...');
    theta = A\b;


% otherwise, use bicgstab solver
elseif(isolver==1)
    
    tol = 1e-8;
    maxit = round(1.1*Ntot);
    % select preconditioner
    disp('generating precondioner ...');
    if(mode_precond==0)
        disp('calling bicgstab() solver ...');
        [theta,iflag,relres,iter] = bicgstab(A,b,tol,maxit);
    elseif(mode_precond==1)  % Jacobi preconditioner
        M_PC = diag(diag(A));
        disp('calling bicgstab() solver ...');
        [theta,iflag,relres,iter] = bicgstab(A,b,tol,maxit,M_PC);
    else
        if(mode_fill==0)
            [Linc,Uinc] = luinc(A,'0');
        else
            [Linc,Uinc] = luinc(A,ILU_Options);
        end
        disp('calling bicgstab() solver ...');
        [theta,iflag,relres,iter] = bicgstab(A,b,tol,maxit,Linc,Uinc);
    end
    % report results
    disp(' ');  disp('bicgstab results : ');
    disp(['iflag = ', int2str(iflag), ', relres = ', num2str(relres), ...
            ', iter = ', int2str(iter)]);

    
elseif(isolver==2)  % GMRES method
    
    tol = 1e-8;
    maxit = round(1.1*Ntot);
    % select preconditioner
    disp('generating precondioner ...');
    if(mode_precond==0)
        disp('calling gmres() solver ...');
        [theta,iflag,relres,iter] = gmres(A,b,restart,tol,maxit);
    elseif(mode_precond==1)  % Jacobi preconditioner
        M_PC = diag(diag(A));
        disp('calling gmres() solver ...');
        [theta,iflag,relres,iter] = gmres(A,b,restart,tol,maxit,M_PC);
    else
        if(mode_fill==0)
            [Linc,Uinc] = luinc(A,'0');
        else
            [Linc,Uinc] = luinc(A,ILU_Options);
        end
        disp('calling gmres() solver ...');
        [theta,iflag,relres,iter] = gmres(A,b,restart,tol,maxit,Linc,Uinc,theta_guess);
    end
    % report results
    disp(' ');  disp('gmres results : ');
    disp(['iflag = ', int2str(iflag), ', relres = ', num2str(relres), ...
            ', iter = ', int2str(iter)]);

    
end  % for isolver


% ---- PLOTTING RESULTS ------------------------------
% Now, plot results for plane at y = 0
iy = 1;
Theta_slice = zeros(N_z,N_xy);
for ix=1:N_xy
for iz=1:N_z
    % get local temperature
    m = calc_label(ix,iy,iz,N_xy,N_z);
    Theta_slice(iz,ix) = theta(m);
    % check if point is within heat generating region
    if(iz==1)
        H_loc = get_H_heat_region(x_grid(ix),y_grid(iy),...
                    z_grid(iz),a_geom,b_geom, ...
                    r1_L,t1_L,r2_L,t2_L);
        S_slice(iz,ix) = sigma*H_loc;
    else  % if interior point
        S_slice(iz,ix) = b(m);
    end
end
end

% make plot of temperature distribution
disp('Plotting temperature distribution ...');
figure;
contourf(x_grid,-z_grid,Theta_slice,20);
colorbar;
xlabel('\chi');
ylabel('\zeta');
title_phrase = ['\theta(\chi,\eta,\zeta), \sigma = ', ...
        num2str(sigma), ', N_{xy} = ', int2str(N_xy), ...
        ', N_z = ', int2str(N_z)];
title(title_phrase);
theta_max = max(max(Theta_slice));
gtext(['\theta_{max} = ', num2str(theta_max)]);

% make plot of local heat generation rate
disp('Plotting heat generation rate ...');
figure;
contourf(x_grid,-z_grid,S_slice,20);
colorbar;
xlabel('\chi');
ylabel('\zeta');
title_phrase = ['S(\chi,\eta,\zeta), \sigma = ', ...
        num2str(sigma), ...
        ', N_{xy} = ', int2str(N_xy), ...
            ', N_z = ', int2str(N_z)];
title(title_phrase);

% make plot of heat flux at the surface
disp('Plotting heat flux at surface ...');
Theta_flux_surf = zeros(N_xy,N_xy);
for ix=1:N_xy
for iy=1:N_xy
    m = calc_label(ix,iy,1,N_xy,N_z);
    n1 = calc_label(ix,iy,2,N_xy,N_z);
    n2 = calc_label(ix,iy,3,N_xy,N_z);
    Theta_flux_surf(iy,ix) = (1/2/dz_fine)*...
        (-theta(n2) + 4*theta(n1) - 3*theta(m));
end
end
figure;
contourf(x_grid,y_grid,Theta_flux_surf,20);
colorbar;
xlabel('\chi');
ylabel('\eta');
title('Surface heat flux');

% compute total heat flux using trapezoid rule
DI_y = zeros(size(x_grid));
for ix=1:N_xy
    % get all values at this x value
    integrand_y = Theta_flux_surf(ix,:);
    DI_y(ix)  = trapz(y_grid,integrand_y');
end
total_flux = trapz(x_grid,DI_y);
total_flux = total_flux * 4;  % correct for quadrant domain
disp(' ');
disp(['Total heat flux out of surface = ', ...
    num2str(total_flux)]);

% calculate total heat generation rate
vol_an1 = b_geom*pi*((r1_L+t1_L)^2 - r1_L^2);
vol_an2 = b_geom*pi*((r2_L+t2_L)^2 - r2_L^2);
total_heat_gen = (vol_an1 + vol_an2)*sigma;
disp(['Total heat generation rate = ', ...
        num2str(total_heat_gen)]);

% add results to plot
gtext({['Tot. heat flux out = ', num2str(total_flux)], ...
    ['Tot. heat gen. rate = ', num2str(total_heat_gen)]});


% extract solution into 3D array
Theta_3D = zeros(N_xy,N_xy,N_z);
%[X,Y,Z] = meshgrid(x_grid,y_grid,z_grid);
for ix=1:N_xy
for iy=1:N_xy
for iz=1:N_z
    m = calc_label(ix,iy,iz,N_xy,N_z);
    Theta_3D(ix,iy,iz) = theta(m);
end
end
end

% generate a few slices at points of interest
[X,Y,Z] = meshgrid(x_grid,y_grid,z_grid);

% mid-slice in x
figure;
[SY,SZ] = meshgrid(y_grid,z_grid);
SX = x_grid(round(N_xy/2))*ones(size(SY));
slice(X,Y,Z,Theta_3D,SX,SY,SZ);
hold on;
% mid-slice in y
[SX,SZ] = meshgrid(x_grid,z_grid);
SY = y_grid(round(N_xy/2))*ones(size(SX));
slice(X,Y,Z,Theta_3D,SX,SY,SZ);
% mid-slice in z
[SX,SY] = meshgrid(x_grid,y_grid);
SZ = z_grid(round(N_z/2))*ones(size(SY));
slice(X,Y,Z,Theta_3D,SX,SY,SZ);
xlabel('x'); ylabel('y'); zlabel('z');
colorbar;

% save results to disk
save stove_top_3D_FD.mat;

iflag_main = 1;
return;


% =====================================================
% This function computes the location in the master
% vector of the temperature at grid point (i,j,k)
function label = calc_label(ix,iy,iz,N_xy,N_z);

label = (iz-1)*N_xy*N_xy + (ix-1)*N_xy + iy;

return;



% =====================================================
% This function checks to see if the local point is
% within a heat generating region. If so, the function
% returns a 1, else a 0.
function [H_loc,iflag] = ...
    get_H_heat_region(x,y,z,a_geom,b_geom, ...
        r1_L,t1_L,r2_L,t2_L);

iflag = 0;

% First, check to see whether the point is
% close enough to the surface.
if(z > b_geom)
    H_loc = 0;
    return;
end

% Next, compute radius in (x,y) polar coordinates
r = sqrt(x^2 + y^2);

% If in first annular region
if(and((r >= r1_L),(r <= (r1_L+t1_L))))
    H_loc = 1;
    return;
% else if in second annular region
elseif(and((r >= r2_L),(r <= (r2_L+t2_L))))
    H_loc = 1;
    return;
else
    H_loc = 0;
end

iflag = 1;
return;
