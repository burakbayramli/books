
function y = hilbertq(x)
% HILBERTQ: interpolation
%
%       y = hilbertq(x)
%
% 	x is input
%	bhil is the hilbert filter
% 	y is output waveform

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

				% options
	filter_plot = 0;

	f = [.05 .95];
	m = [1 1];
	N = 200;

	b = remez(N, f, m, 'hilbert');

	if filter_plot == 1
		clg;
		subplot(311);
		plot(b);
		fb = fft(b, 1024);
		subplot(312);
		plot(20*log(abs(fb(1:512))));
	end;

	yh = conv2(x,b,'same');
	y = x + j*yh;

	if filter_plot == 1;
		subplot(313);
		plot(abs(y));
		pause(1);
	end;




	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



