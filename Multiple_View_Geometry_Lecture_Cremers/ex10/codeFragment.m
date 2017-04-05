%% Exercise Sheet 10 - Part II
% 2)

clc
close all;
clear all;

% read image
f = double(imread('lena.jpg'));

% Initialize optimization Variable with the input image.
u = f;

% set parameters
maxits = 1000;
% ...

for step = 1:maxits  
  % gradient of u using forward differeneces
  % ...

  % compute the divergence using backward differences
  % ...
  
  % update u
  % ...
  
  % display everything
  if mod(step, 10) == 0
    fprintf('it = %d \n',step)
    
    subplot(1,2,1), imshow(f,[]), title('Input Image f'), drawnow
    subplot(1,2,2), imshow(u,[]), title('Smooth Image u'), drawnow
  end 
end