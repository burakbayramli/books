% TDGL_A_2D.m
% This MATLAB program simulates the dynamics of the
% Landau phi^4 free energy model in 2-D using
% Ginzburg-Landau TDGL model A dynamics.
%
% K. Beers. MIT ChE. 1/16/04. v 7/12/05

function iflag_main = TDGL_A_2D();
iflag_main = 0;
disp('Running TDGL_A_2D : ');
disp('Sample default values are in () in input prompts.');

% Ask the user to specify the length of the
% system.
disp(' '); disp('Enter system parameters : ');
Grid.L = input('----- Enter side length of simulation domain (100) : ');
Grid.V = Grid.L^2;  % the total 2-D "volume"

% Next, ask the user to specify the number of
% grid points in each direction of the 2D grid.
Grid.N_side = input('----- Enter # of grid points in each direction (50) : ');
Grid.N_tot = Grid.N_side^2;
Grid.x = linspace(0,Grid.L,Grid.N_side);
Grid.y = linspace(0,Grid.L,Grid.N_side);
Grid.d = 2;  % dimension of the lattice

% compute the grid spacing and the 2-D volume per cell
Grid.h = Grid.L/Grid.N_side;  % we use periodic BC
Grid.v_cell = Grid.h^2;

% To parameterize the Landau model, we use the Ising lattice
% model in 2-D.  We define energy scale such that
% kb*Tc = J*z = 1.
Sim.k_b = 1;  % Boltzmann constant
Sim.z = 2*Grid.d;  % # of nearest neighbors per site
Sim.J = 1/Sim.z;  % set coupling constant such that J*z = 1
Sim.Tc_MF = Sim.J*Sim.z/Sim.k_b;  % mean-field critical temperature
Sim.Tc_RG = Sim.J/Sim.k_b/0.44069;  % renormalization-group Tc

% We now ask the user to input the temperature
disp(' '); disp('Next, the temperature is entered, in comparison to : ');
disp(['     mean-field Tc = ', num2str(Sim.Tc_MF)]);
disp(['     Renormalization group Tc = ', num2str(Sim.Tc_RG)]);
Sim.T = input('----- Enter simulation T (0.35) : ');

% enter field strength in units of kb*Tc
disp(' ');  disp('H is the external field strength, spin moment = 1.');
var1 = input('----- Enter H in units of kb*Tc (0) : ');
Sim.H = var1*Sim.k_b*Sim.Tc_MF;

% We now compute the parameters of the Landau
% phenomenological free energy for this Ising
% lattice.  This is obtained by expanding the
% free energy around phi = 0, and retaining up
% to the fourth power terms (leading error is
% then 6th power due to symmetry).
Sim.r0 = Sim.k_b/Grid.v_cell;
Sim.r = Sim.r0*(Sim.T-Sim.Tc_MF);
Sim.u = Sim.k_b*Sim.T/12/Grid.v_cell;
Sim.c = Sim.J*Grid.h^2/Grid.v_cell;
Sim.w = Sim.H / Grid.v_cell;

% Ask the user to input the transport coefficient
disp(' '); disp('Gamma is the transport coefficient.');
Sim.gamma = input('----- Enter transport coefficient Gamma (1) : ');

% We now ask the user to input the time step of the simulation,
% after reporting an approximate upper limit time step of an
% explicit integrator for a 2-D diffusion problem.  Since
% an implicit algorithm is used, this time step limitation
% is not enforced, however.
Sim.dt_max_explicit = (Grid.h^2)/(Sim.gamma*Sim.c);
disp(' '); disp('Next, you are asked to set time step, in comparison to : ');
disp(['     Explicit integrator critical dt (upper bound) = ', ...
        num2str(Sim.dt_max_explicit)]);
Sim.dt = input('----- Enter time step for implicit integrator (0.5) : ');
Sim.t_end = input('----- Enter the end time for the simulation (2000) : ');
Sim.num_steps = 1 + floor(Sim.t_end / Sim.dt);
Sim.plot_freq = input('----- Enter # of time steps between plots (10) : ');

% Generate std of random noise
Sim.rand_noise_std = sqrt(2*Sim.k_b*Sim.T*Sim.gamma/Grid.v_cell*Sim.dt);

% We now allocate a vector to store the field values at each
% grid point.  We initialize this vector to store the initial
% values of all zero.
phi = zeros(Grid.N_tot,1);

% We next specify a numerical stencil operator that approximates
% the negative 2-D Laplacian on a grid with unit spacing.  Here, we use the
% traditional finite difference formula; however, a better choice would
% be to include corner sites to obtain a more isotropic stencil operator
% to lessen the effect of the lattice orientation upon the results.
Grid.A = spalloc(Grid.N_tot,Grid.N_tot,5*Grid.N_tot);
for ix=1:Grid.N_side
    for iy=1:Grid.N_side
        % center site
        l = get_label_2D(ix,iy,Grid.N_side);
        Grid.A(l,l) = 4;
        % -x neighbor
        jx = ix-1;  jy = iy;
        if(jx < 1)  % periodic BC
            jx = jx + Grid.N_side;
        end
        k = get_label_2D(jx,jy,Grid.N_side);
        Grid.A(l,k) = -1;
        % +x neighbor
        jx = ix+1;  jy = iy;
        if(jx > Grid.N_side)
            jx = jx - Grid.N_side;
        end
        k = get_label_2D(jx,jy,Grid.N_side);
        Grid.A(l,k) = -1;
        % -y neighbor
        jx = ix;  jy = iy - 1;
        if(jy < 1)
            jy = jy + Grid.N_side;
        end
        k = get_label_2D(jx,jy,Grid.N_side);
        Grid.A(l,k) = -1;
        % +y neighbor
        jx = ix;  jy = iy + 1;
        if(jy > Grid.N_side)
            jy = jy - Grid.N_side;
        end
        k = get_label_2D(jx,jy,Grid.N_side);
        Grid.A(l,k) = -1;        
    end
end

% We next compute the initial chemical potential field
mu = calc_chem_pot(phi,Grid,Sim);

% Finally, we open a figure that shows the order parameter
% field dynamics.
fig_order = figure;
% and add figure for plot of average order parameter vs. time
fig_order_mean = figure;

% We now begin time simulation.
time = 0;  iframe = 0;
for itime = 1:Sim.num_steps
    time = time + Sim.dt;
    
    % generate random noise for each grid point
    rand_noise = Sim.rand_noise_std*randn(Grid.N_tot,1);
    
    % get new field values using explicit Euler method
    phi = phi - Sim.gamma*Sim.dt*mu + rand_noise;
    
    % compute average order parameter
    phi_mean = mean(phi);
    
    % compute new chemical potential field
    mu = calc_chem_pot(phi,Grid,Sim);
    
    % periodically print out the results of the simulation to
    % the screen, for use in making a movie.
    if(~mod(itime,Sim.plot_freq))
        iframe = iframe + 1;
        figure(fig_order);
        % extract results into 2-D array for plotting
        Phi_plot = zeros(Grid.N_side,Grid.N_side);
        for ix=1:Grid.N_side
            for iy=1:Grid.N_side
                l = get_label_2D(ix,iy,Grid.N_side);
                Phi_plot(ix,iy) = phi(l);
            end
        end
        phi_max = norm(phi,inf);
        contourf(Grid.x,Grid.y,Phi_plot);
        title_phrase = 'TDGL-A \phi^4 model: ';
        title_phrase = [title_phrase, 'T = ', num2str(Sim.T)];
        title_phrase = [title_phrase, ', H = ', num2str(Sim.H)];
        title_phrase = [title_phrase, ', time = ', num2str(time)];
        xlabel('x');  ylabel('y');  title(title_phrase);
        colorbar;
        Movie_order(iframe) = getframe;
        % add plot of average order parameter vs. time
        figure(fig_order_mean);
        hold on;  plot(time,phi_mean,'o');
        xlabel('time');  ylabel('< \phi >');
        title(title_phrase);
    end
    
end

% show movie and convert to avi
repeat_movie = 1;
fig_movie = figure;
while(repeat_movie)
    figure(fig_movie);
    movie(Movie_order,0);
    repeat_movie = input('Repeat movie (0=n, 1=y) : ');
end

iflag_main = 1;
return;


% --------------------------------------------
% This routine returns an index label for each
% point in a 2-D grid, based on the same
% numbering system used in meshgrid and the
% plotting of results.  The x-axis goes from
% left to right, and the y-axis from top to
% bottom, in the same ordering as the elements
% of a matrix.
function label = get_label_2D(ix,iy,N_side);

label = (ix-1)*N_side + iy;

return;


% ---------------------------------------------
% This routine returns the real-space coordinates
% of a grid point in the 2-D lattice, using the
% same numbering system as meshgrid.  (x_O,y_O)
% are the coordinates of the origin, located at the
% upper left corner of the domain.
function [x_c,y_c] = get_coord_2D(ix,iy,h,x_O,y_O);

x_c = x_0 + h*(ix-1);
y_c = y_O - h*(iy-1);

return;


% ------------------------------------------------------
% This routine computes the chemical potential field
% for the Landau phi^4 model on a 2-D lattice grid.
function mu = calc_chem_pot(phi,Grid,Sim);

mu = Sim.c/(Grid.h^2)*Grid.A*phi + Sim.r*phi + ...
    4*Sim.u*phi.^3 - Sim.w*ones(size(phi));

return;
