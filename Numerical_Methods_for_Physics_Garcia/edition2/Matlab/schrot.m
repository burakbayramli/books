%  Program to solve the Schrodinger equation using sparce matrix
%  Crank-Nicolson scheme (Particle-in-a-box version)
clear all;  help schrot;  % Clear memory and print header

%* Initialize parameters (grid spacing, time step, etc.)
i_imag = sqrt(-1);
N = input('Enter number of grid points: ');
L = 100;                % System extends from -L/2 to L/2
h = L/(N-1);            % Grid size
x = h*(0:N-1) - L/2;    % Coordinates of grid points
h_bar = 1;  mass = 1;   % Natural units
tau = input('Enter time step: ');

%*  Set up the Hamiltonian operator matrix
coeff = -h_bar^2/(2*mass*h^2);
for i=2:(N-1)
  ham(i,1) = coeff;
  ham(i,2) = -2*coeff;
  ham(i,3) = coeff;
end
% First and last rows for Direchlet boundary conditions
ham(1,1)=0; ham(1,2)=0; ham(1,3)=0;
ham(N,1)=0; ham(N,2)=0; ham(N,3)=0;

%* Set up the matrix Q
tri_eye = zeros(N,3);
tri_eye(:,2) = ones(N,1);   % Identity matrix in packed format
Q = 0.5*( tri_eye + 0.5*i_imag*tau/h_bar * ham );

%* Initialize the wavefunction 
x0 = 0;         % Location of the center of the wavepacket
velocity = 0.5; % Average velocity of the packet
k0 = mass*velocity/h_bar;       % Average wavenumber
sigma0 = L/10;  % Standard deviation of the wavefunction
Norm_psi = 1/(sqrt(sigma0*sqrt(pi)));  % Normalization
psi = Norm_psi * exp(i_imag*k0*x') .* ...
                      exp(-(x'-x0).^2/(2*sigma0^2));
% Set phi to zero as boundary condition, i.e. perfect reflector
psi(1) = 0;  psi(N)=0;

%* Plot the initial wavefunction (real and imaginary parts)
figure(1); clf;
plot(x,real(psi),x,imag(psi));
title('Initial wave function');
xlabel('x');  ylabel('\psi_0(x)'); legend('Real','Imag');
drawnow;  pause(1);

%* Initialize loop and plot variables 
max_iter = L/(velocity*tau);   % Particle should return to origin
plot_iter = max_iter/100;        % Produce 100 curves
p_plot(:,1) = abs(psi).^2;      % Record initial condition
iplot = 1;
figure(2); clf;
axisV = [-L/2 L/2 0 2*max(p_plot)]; % Fix axis min and max

%* Loop over desired number of steps (packet returns to origin)
for iter=1:max_iter

  %* Compute new wave function using the sparse matrix version
  %  of the Crank-Nicolson scheme
  chi = tri_ge(Q,psi);  
  psi = chi - psi;      

  %* Periodically record values for plotting
  if( rem(iter,plot_iter) < 1 )  % Every plot_iter steps record 
    iplot = iplot+1;
    tplot(iplot) = iter*tau;           % Record current time
    p_plot(:,iplot) = psi.*conj(psi);  % and P(x,t) for plots
    plot(x,p_plot(:,iplot));  % Display snap-shot of P(x,t)
    xlabel('x'); ylabel('P(x,t)');
    title(sprintf('Finished %g of %g iterations\n',iter,max_iter));
    axis(axisV); drawnow;
  end
end

%* Plot probability versus position at various times
pFinal = psi.*conj(psi);
plot(x,p_plot(:,1:5:iplot),x,pFinal);
xlabel('x'); ylabel('P(x,t)');
title('Probability density at various times');
figure(3); clf;
mesh(tplot,x,p_plot);  % Plot P(x,t) vs. x and t
view([85 25]);
ylabel('x');  xlabel('t'); zlabel('P(x,t)');
colormap([0 0 0]);