function style = plotstyle( newstyle )
%PLOTSTYLE   Set/get the plot style for 2D rectangular meshes
%   The "plot style" is the function used to plot 2D results. It can be any
%   of the following:
%		pcolor, contour, mesh, surf.
%	The function call plotstyle() returns the current plot style (stored in
%	plotstyle.mat), while plotstyle(newstyle) changes the plot style to
%	'newstyle'.

	narginchk(0, 1);

	
	default = 'pcolor';
	filename = '+Plot/plotstyle.mat';
	validstyles = { 'pcolor', 'contour', 'mesh', 'surf', 'element' };
	
	if nargin == 0
		try
			% Try to load the style stored in 'filename'
			S = load(filename);
			style = S.style;
			return;
		catch
			% If the file doesn't exist, return the default value
			newstyle = default;
		end
	end
	
	style = newstyle;
	% Is the new style a valid function name?
	if ~any(cellfun(@(s)strcmp(s, style), validstyles))
		error(['''' style ''' is not a valid plot style name.']);
	end
	save(filename, 'style');
end