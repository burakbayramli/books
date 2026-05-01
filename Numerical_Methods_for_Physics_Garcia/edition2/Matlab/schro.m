%  schro - Program to solve the Schrodinger equation 
%  for a free particle using the Crank-Nicolson scheme
clear all;  help schro;   % Clear memory and print header

%* Initialize parameters (grid spacing, time step, etc.)
i_imag = sqrt(-1);    % Imaginary i
N = input('Enter number of grid points: ');
L = 100;              % System extends from -L/2 to L/2
h = L/(N-1);          % Grid size
x = h*(0:N-1) - L/2;  % Coordinates  of grid points
h_bar = 1;  mass = 1; % Natural units
tau = input('Enter time step: ');

%* Set up the Hamiltonian operator matrix
ham = zeros(N);  % Set all elements to zero
coeff = -h_bar^2/(2*mass*h^2);
for i=2:(N-1)
  ham(i,i-1) = coeff;
  ham(i,i) = -2*coeff;  % Set interior rows
  ham(i,i+1) = coeff;
end
% First and last rows for periodic boundary conditions
ham(1,N) = coeff;   ham(1,1) = -2*coeff; ham(1,2) = coeff;
ham(N,N-1) = coeff; ham(N,N) = -2*coeff; ham(N,1) = coeff;

%* Compute the Crank-Nicolson matrix
dCN = ( inv(eye(N) + .5*i_imag*tau/h_bar*ham) * ...
             (eye(N) - .5*i_imag*tau/h_bar*ham) );
			 
%* Initialize the wavefunction 
x0 = 0;          % Location of the center of the wavepacket
velocity = 0.5;  % Average velocity of the packet
k0 = mass*velocity/h_bar;       % Average wavenumber
sigma0 = L/10;   % Standard deviation of the wavefunction
Norm_psi = 1/(sqrt(sigma0*sqrt(pi)));  % Normalization
psi = Norm_psi * exp(i_imag*k0*x') .* ...
                      exp(-(x'-x0).^2/(2*sigma0^2));

%* Plot the initial wavefunction
figure(1); clf;
plot(x,real(psi),'-',x,imag(psi),'--');
title('Initial wave function');
xlabel('x');  ylabel('\psi(x)'); legend('Real  ','Imag  ');
drawnow;  pause(1);

%* Initialize loop and plot variables 
max_iter = L/(velocity*tau);      % Particle should circle system
plot_iter = max_iter/20;          % Produce 20 curves
p_plot(:,1) = psi.*conj(psi);     % Record initial condition
iplot = 1;
figure(2); clf;
axisV = [-L/2 L/2 0 max(p_plot)]; % Fix axis min and max

%* Loop over desired number of steps (wave circles system once)
for iter=1:max_iter
	
  %* Compute new wave function using the Crank-Nicolson scheme
  psi = dCN*psi;  
  
  %* Periodically record values for plotting
  if( rem(iter,plot_iter) < 1 )   
    iplot = iplot+1;
    p_plot(:,iplot) = psi.*conj(psi); 
    plot(x,p_plot(:,iplot));     % Display snap-shot of P(x)
    xlabel('x'); ylabel('P(x,t)');
    title(sprintf('Finished %g of %g iterations',iter,max_iter));
    axis(axisV); drawnow;
  end

end

%* Plot probability versus position at various times
pFinal = psi.*conj(psi);
plot(x,p_plot(:,1:3:iplot),x,pFinal);
xlabel('x'); ylabel('P(x,t)');
title('Probability density at various times');
   
