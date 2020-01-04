
function [pitch, voicing] = fread_pitch(filename, display)
%
% [pitch, voicing] = fread_pitch(filename, display)
%
%
% read pitch and voicing as floats
%
% filename is input filename
% display = 0:plot; 1:noplot
%
%
			% options
			% open file
	fid = fopen([filename '.pitch'],'r');

			% write
	[data, count] = fread(fid, 'float');

	add_frm = 10;
	add_on = add_frm*80;
	data = [data' zeros(1,add_frm*2)]; 
	lgh  = length(data);
	n = 1:2:lgh;
	pitch = data(n);
	% pitch = 10*data(n);
	n = 2:2:lgh;
	voicing = data(n);


	x = load24(filename);
	x = x/max(abs(x));
	x = [x' zeros(1, add_on)];
	lghx = length(x);
	x = [zeros(1, 160), x(1:lghx-160)];
	lghx = length(x);
	lghx_ms = lghx/80;

	if display == 1
		clf;
		subplot(311)
		plot((1:lghx)/8000, x)
		axis([ 0 lghx_ms/100 -1.25 1.25])
		ylabel('Amplitude')
		title('(a)')
		grid

		n = (1:lgh/2);
		subplot(312)
		plot(n/100, pitch(1:lgh/2))
		axis([0 lgh/2/100 0 450])
		ylabel('Freq (Hz)')
		title('(b)')
		%title('PITCH')
		grid
		subplot(313)
		plot(n/100, voicing(1:lgh/2))
		axis([ 0 lgh/2/100 0 1.5])
		%title('VOICING')
		title('(c)')
		grid
		xlabel('Time (s)')
		ylabel('Probability')
		pause(.5)
	end;


	fclose(fid);



