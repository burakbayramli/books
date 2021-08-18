% Run this on Octave using 'octave runDriver.m'
addpath('../Codes2D:../Grid/Euler2D:../Codes1D:../ServiceRoutines');
EulerDriver2D;
figure; PlotField2D(N, x, y, Q(:,:,4)); drawnow; pause(.1);
%page_screen_output(0);
%page_output_immediately(1);
