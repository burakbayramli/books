function saveFig( filename, fig )
%SAVEFIG   Saves the current figure as filename.fig and filename.pdf in the
%  output/ subfolder.
%
% Input:
% filename   Name of output file.
% fig        Handle to figure (optional; default is current figure)
	

	% Prepend output folder
	if isunix
		filename = ['output/' filename];
	else
		filename = ['output\\' filename];
	end
	
	% Set specified figure as current figure
	if nargin > 1
		set(0, 'CurrentFigure', fig);
	end
	
	% Save to .fig
	hgsave([filename, '.fig']);
	
	% Save to .pdf by first saving as .eps and then converting to .pdf
	% using epstopdf.
	epsFileName = [filename, '.eps'];
	if isunix
		print('-depsc', epsFileName);
		unix(['unset LD_LIBRARY_PATH; epstopdf ', epsFileName]);
		unix(['rm ', epsFileName]);
	else
		print('-depsc', epsFileName);
		dos(['epstopdf "', epsFileName, '"']);
 		dos(['del "', epsFileName, '"']);
	end