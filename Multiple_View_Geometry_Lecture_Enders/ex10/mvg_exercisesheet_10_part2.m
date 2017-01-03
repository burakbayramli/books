%% Exercise Sheet 10 - Part II
% 2) and 3)

clc
close all;
clear all;

% read image
f = double(imread('lena.jpg'));

% Initialize optimization Variable with the input image.
u  = f;     % corresponding to Exercise 2)
u2 = f;     % corresponding to Exercise 3)

% set parameters
maxits = 300;
eps = 0.1;      % perturbation parameter because of non-differentiability of the TV term
tau = 0.21;     % Step size    
lambda = 0.02;  % Smoothness parameter

for step = 1:maxits  
  % gradient of u using forward differences
  u_x = dx_forward(u);
  u_y = dy_forward(u);
  
  u2_x = dx_forward(u2);
  u2_y = dy_forward(u2);
  
  
  % norm of the gradient of u2
  norm_grad_u2 = sqrt( u2_x.^2 + u2_y.^2 + eps.^2);
  
  % normalize the gradient of u2 
  u2_x_normalized = u2_x ./ norm_grad_u2;
  u2_y_normalized = u2_y ./ norm_grad_u2;
  
  
  % compute the divergence using backward differences
  div  = dx_back(u_x) + dy_back(u_y); 
  div2 = dx_back(u2_x_normalized) + dy_back(u2_y_normalized); 
  
  % update u
  u  = u  + tau * (lambda * (f - u ) + div);
  u2 = u2 + tau * (lambda * (f - u2) + div2);
  
  % display everything
  if mod(step, 10) == 0
    fprintf('it = %d \n',step)
    
    subplot(1,3,1), imshow(f,[]), title('Input Image f'), drawnow
    subplot(1,3,2), imshow(u,[]), title('Smooth Image u'), drawnow
    subplot(1,3,3), imshow(u2,[]), title('Smooth Image u2'), drawnow
  end 
end