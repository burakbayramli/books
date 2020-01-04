
function y = uniform_bank(spacing, prototype_filter, plot_factor)
% UNIFORM_BANK: design a set of uniformlyspaced bandpass filters
%
%       y = uniform_bank(spacing, prototype_filter, plot_factor)
%
%       spacing is the filter spacing in Hz
%       prototype_filter is the "analysis filter"
% 	plot_factor is a normalization factor for plotting 
% 	y is output

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


						% set up some parameters

	test = [plot_factor, zeros(1,511)];
	plot (test);
	hold on;

	number_filters = (5000/spacing) + 1;
	lengthp = length(prototype_filter);
						% do "end" filters
							
		n = number_filters;
		modfreq = 2.0*pi*(n-1)*spacing/10000;
	        b =  prototype_filter.*(1.0*cos ( modfreq*([1:lengthp]-1)));

		y(:, n) = b';            

		ft = fft(b, 1024);
		mft = abs(ft(1:512));
		plot(mft);


		n = 1;
		modfreq = 2.0*pi*(n-1)*spacing/10000;
	        b =  prototype_filter.*(1.0*cos ( modfreq*([1:lengthp]-1)));

		y(:, n) = b';            

		ft = fft(b, 1024);
		mft = abs(ft(1:512));
		plot(mft);


	for n = 2:(number_filters-1);

						% modulate prototype_filter 

		modfreq = 2.0*pi*(n-1)*spacing/10000;
	        b =  prototype_filter.*(2.0*cos ( modfreq*([1:lengthp]-1)));

		y(:, n) = b';            

						% plot

		ft = fft(b, 1024);
		mft = abs(ft(1:512));
		plot(mft);

	end

	xlabel ('Frequency (FFT samples)');

	hold off;

		
