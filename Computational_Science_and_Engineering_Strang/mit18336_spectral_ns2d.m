function mit18336_spectral_ns2d
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Navier-Stokes equations in vorticity/stream function formulation on the torus %
% Version 1.0                                                                   %
% (c) 2008 Jean-Christophe Nave - MIT Department of Mathematics                 %             
%  jcnave (at) mit (dot) edu                                                    %
%                                                                               %
%  Dw/Dt = nu.Laplacian(w)                                                      % 
%  Laplacian(psi) = -w                                                          %
%  u = psi_y                                                                    %
%  v =-psi_x                                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all;

nu=1.0e-3;  % viscosity
NX=128;     % resolution in x
NY=128;     % resolution in y
dt=1e-1;    % time step
TF=1000.0;  % final time
TSCREEN=25; % sreen update interval time (NOTE: plotting is usually slow)
method='crank_nicholson';       % 'Forward_Euler' or 'crank_nicholson'
initial_condition='vortices';   % 'vortices' or 'random'

I=sqrt(-1);
dx=2*pi/NX;
dy=2*pi/NY;
t=0.;

switch lower(method)
   case {'forward_euler'}
      disp('Forward Euler method')
      disp('WARNING: only conditionally stable !!! Lower the time step if unstable...')
   case {'crank_nicholson'}
      disp('Crank-Nicholson Method')
      disp('Unconditionally stable up to CFL Condition')
    otherwise
      disp('Unknown method!!!');
      return
end

% Define initial vorticity distribution
switch lower(initial_condition)
   case {'vortices'}
      [i,j]=meshgrid(1:NX,1:NY);
      w=exp(-((i*dx-pi).^2+(j*dy-pi+pi/4).^2)/(0.2))+exp(-((i*dx-pi).^2+(j*dy-pi-pi/4).^2)/(0.2))-0.5*exp(-((i*dx-pi-pi/4).^2+(j*dy-pi-pi/4).^2)/(0.4));
   case {'random'}
      w=random('unif',-1,1,NX,NY);
   otherwise
      disp('Unknown initial conditions !!!');
      return
end

kx=I*ones(1,NY)'*(mod((1:NX)-ceil(NX/2+1),NX)-floor(NX/2)); % matrix of wavenumbers in x direction 
ky=I*(mod((1:NY)'-ceil(NY/2+1),NY)-floor(NY/2))*ones(1,NX); % matrix of wavenumbers in y direction 

dealias=kx<2/3*NX&ky<2/3*NY; % Cutting of frequencies using the 2/3 rule

ksquare_viscous=kx.^2+ky.^2;        % Laplacian in Fourier space
ksquare_poisson=ksquare_viscous;    
ksquare_poisson(1,1)=1;             % fixed Laplacian in Fourier space for Poisson's equation
w_hat=fft2(w);

k=0;
while t<TF
    k=k+1;
    % Compute the stream function and get the velocity and gradient of vorticity
    psi_hat = -w_hat./ksquare_poisson;  % Solve Poisson's Equation
    u  =real(ifft2( ky.*psi_hat));      % Compute  y derivative of stream function ==> u
    v  =real(ifft2(-kx.*psi_hat));      % Compute -x derivative of stream function ==> v
    w_x=real(ifft2( kx.*w_hat  ));      % Compute  x derivative of vorticity
    w_y=real(ifft2( ky.*w_hat  ));      % Compute  y derivative of vorticity
    
    conv     = u.*w_x + v.*w_y;         % evaluate the convective derivative (u,v).grad(w)   
    conv_hat = fft2(conv);              % go back to Fourier space
    
    conv_hat = dealias.*conv_hat;   % Perform spherical dealiasing 2/3 rule
    
    % Compute Solution at the next step
    switch lower(method)
       case {'forward_euler'}
          w_hat_new = w_hat + dt*( nu*ksquare_viscous.*w_hat-conv_hat);
       case {'crank_nicholson'}
          w_hat_new = ((1/dt + 0.5*nu*ksquare_viscous)./(1/dt - 0.5*nu*ksquare_viscous)).*w_hat - (1./(1/dt - 0.5*nu*ksquare_viscous)).*conv_hat;
    end
    
    t=t+dt;
    % Plotting the vorticity field
    if (k==TSCREEN) 
        % Go back in real space omega in real space for plotting
        w=real(ifft2(w_hat_new));
        contourf(w,50); colorbar; shading flat;colormap('jet'); 
        title(num2str(t));
        drawnow
        k=0;
    end
    w_hat=w_hat_new;
end