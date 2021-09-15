% make_Ising_lattice_MC_movie.m
% This MATLAB program makes an .avi movie of 
% a Monte Carlo simulation of a 2-D Ising
% lattice.
% K. Beers. MIT ChE. 11/7/03
function iflag = make_Ising_lattice_MC_movie();
iflag = 0;

% First, set simulation parameters to get just above
% Curie critical point
MCOpts.N = 50;
MCOpts.mu = 1;
MCOpts.H = 0;
MCOpts.J = 1;
MCOpts.kb_T = 5;  % for longer-range fluct, try 2.275

% set initial simulation to do equilibration
MCOpts.Nequil = 50*(MCOpts.N^2);
MCOpts.Nsamples = 5000;
MCOpts.freq_sample = MCOpts.N;
MCOpts.make_plots = 1;

% perform equilibration and short run to check that we indeed have
% equilibration
[mag_PROPS,ener_PROPS,S,iflag_main] = Ising_lattice_MC(MCOpts);

% Now, continue to run simulation and make movie out of results.
num_frames = 200;
freq_frame = MCOpts.N;  % # of MC moves between frames of movie

% new simulation parameters for each "frame" run
MCOpts.Nequil = 0;
MCOpts.Nsamples = freq_frame;
MCOpts.freq_sample = freq_frame;
MCOpts.make_plots = 0;

% Now, generate frames for movie
figure;
for iframe=1:num_frames

    % set initial state
    MCOpts.S_init = S;
    
    % perform simulation to get new final state
    [m_PROPS,ener_PROPS,S] = Ising_lattice_MC(MCOpts);
    
    % make plot of final state
    spy(S + ones(size(S)));
    xlabel('lattice x-site #i');  ylabel('lattice y-site #j');
    title_phrase = ['Ising lattice + spins: ', ...
            'N = ', int2str(MCOpts.N), ...
            ', \mu = ', num2str(MCOpts.mu), ...
            ', H = ', num2str(MCOpts.H), ...
            ', J = ', num2str(MCOpts.J), ...
            ', k_bT = ', num2str(MCOpts.kb_T)];
    title(title_phrase);
    MC_MOVIE(iframe) = getframe;
    
end
movie(MC_MOVIE);
% convert to .avi file
movie2avi(MC_MOVIE,'MC_MOVIE.avi','quality',50);

iflag = 1;
return;
