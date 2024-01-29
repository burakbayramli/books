% plot the 2-D domain defined in PSLG format
clear all; 
load shape.dat;
n_shapes=shape(1,1)         % number of shapes
n_outer_pts=shape(2,1)      % number of vertices in the outer loop
n_inner_pts(1:n_shapes-1)=shape(3:1+n_shapes,1) % inner vertices
start_pt=n_shapes+2;        % starting point of the outer loop
end_pt=n_shapes+1+n_outer_pts;  % ending point of the outer loop

plot(shape([start_pt:end_pt start_pt],1),...   % plot the 
     shape([start_pt:end_pt start_pt],2),'-'); % outer loop
hold on;

% for-loop block: plot the inner loop(s)
for i=1:n_shapes-1      % loop over each inner loop
	start_pt=end_pt+1;    % starting point of the current inner loop
	end_pt=start_pt+ n_inner_pts(i)-1;  % ending point of the inner loop
	plot(shape([start_pt:end_pt start_pt],1),...    % plot the current 
       shape([start_pt:end_pt start_pt],2),'-');  % inner loop
end
hold off;