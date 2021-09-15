% animate_2D_vib.m
%
% This MATLAB m-file "animates" a vibrational mode
% for the 2-D lattice.
% K. Beers. MIT ChE. 10/4/2002
function iflag_main = animate_2D_vib();
iflag_main = 0;

% read in results of normal mode analysis
load lattice_2D_vib.mat;

% display normal modes and their frequencies
disp(' ');
disp('Normal mode frequencies (rad/sec) : ');
disp('-----------------------------------');
for j=1:length(freq)
    disp([int2str(j), '  ', num2str(freq(j))]);
end
disp(' ');
    
% select normal mode to animate
k_show = input('Enter number of mode to visualize : ');

% Create vector of theta values for a single
% oscillation.
num_frames = 50;
theta_vect = linspace(0,2*pi,num_frames);
[sin_max,iframe_max] = max(sin(theta_vect));
if(length(iframe_max) > 1)
    iframe_max = iframe_max(1);
end

% Set amplitude of oscillation so that the
% maximum displacement is equal to 1/3 times
% the equilibirum bond length.
displace_max = max(abs(V_eig(:,k_show)));
a = l_b/3/displace_max;

% Create movie by plotting 2-D lattice positions
title_phrase = ['2-D lattice, N = ', int2str(N), ...
        ', mode # = ', int2str(k_show), ...
        ', \omega = ', num2str(freq(k_show))];

movie_fig = figure;
for iframe = 1:num_frames
    hold off;
    % compute departure vector
    delta = a*sin(theta_vect(iframe))*V_eig(:,k_show);
    q = q_min + delta;
    % plot positions of atoms
    for i=1:N
        for j=1:N
            [k,kx,ky] = get_master_index(i,j,N);
            plot(q(kx),q(ky),'o');
            hold on;
        end
    end
    axis([ (-l_b) (N+l_b) (-l_b) (N+l_b) ]);
    title(title_phrase);
    % get frame for movie
    Movie_of_mode(iframe) = getframe;
    % if maximum amplitude, make separate plot
    if(iframe == iframe_max)
        figure;
        for i=1:N
        for j=1:N
            [k,kx,ky] = get_master_index(i,j,N);
            plot(q(kx),q(ky),'o');
            hold on;
            plot(q_min(kx),q_min(ky),'x');
            x_line = [q_min(kx), q(kx)];
            y_line = [q_min(ky), q(ky)];
            plot(x_line,y_line);
        end
        end
        axis([ (-l_b) (N+l_b) (-l_b) (N+l_b) ]);
        title([title_phrase, ', max. amplitude']);
        figure(movie_fig);
    end
    
end

% Show several periods of oscillation, repeat
% again and again until stop.
iquit = 1;
times_repeat = 10;
FPS = num_frames/2;  % frames per second
while(iquit)
    movie(Movie_of_mode,times_repeat,FPS);
    iquit = input('Quit (0) or Show again (1) : ');
end

iflag_main = 1;
return;



% ==============================================================
% get_master_index.m
%
% This function calculates the master index label of
% a site in the 2D lattice of NxN sites.
function [k,kx,ky] = get_master_index(i,j,N);

k = (i-1)*N+j;
kx = 2*k-1;
ky = 2*k;

return;


