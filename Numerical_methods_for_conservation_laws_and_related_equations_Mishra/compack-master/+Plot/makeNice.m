function makeNice()
%MAKENICE   Prepares the current figure for printing.
%   This function changes certain properties of the current figure, such as
%   line widths and font sizes, to make the figure more readable after
%   writing it to an image file (such as .pdf).


	fontsize = 14;
	fontname = 'arial';
	linewidth = 2;
	
	set(gca, 'FontName', fontname);
	set(gca, 'FontSize', fontsize);

	axes = findall(gcf, 'type', 'axes');
	for h = axes
		set(h, 'fontsize', fontsize);
		set(findall(h, 'type', 'text'), 'fontsize', fontsize);
		set(findall(h, 'type', 'text'), 'fontname', fontname);
		set(findobj(h, 'type', 'line'), 'linewidth', linewidth);
	end
end