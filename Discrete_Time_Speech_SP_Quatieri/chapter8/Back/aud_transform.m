
function  [h, logmag] = aud_transform(filename, phase_type) 
%
% function  [h, logmag] = aud_transform(filename, phase_type)
%
% AUD_TRANSFORM converts raw auditory (complex) spectra into 
%               auditory impulse responses
%
% filename is file input
% phase_type is 0: zero-phase; 1: min-phase
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	iplot = 1;

				% initialize
	lfft = 4096;
	lfft2 = lfft/2;
	lfft2p1 = lfft2 + 1;

	resp_lgh = 1000; 	% response length
	resp_lgh2 = resp_lgh/2;

				% read file
	filename
	fid = fopen(filename,'r');
	resp = fread(fid,inf,'float');
	fclose(fid);

	%[resp] = bloadx(filename,'float');
	%[resp,header] = bload(filename,'float',rheader);

	reala = resp(1:2:lfft+2);
	imaga = resp(2:2:lfft+2);
	mag = sqrt(reala.*reala+imaga.*imaga);
	phase = atan(imaga./(reala + .0000001));

				% hermetrisize
	mags = zeros(lfft,1);
	mags = mag(1:lfft2p1);
	mags(lfft:-1: lfft2p1 + 1) = mag(2:lfft2);

	phases = zeros(lfft,1);
	phases = phase(1:lfft2p1);
	phases(lfft:-1: lfft2p1 + 1) =  -phase(2:lfft2);



				% generate internal impulse response
	fth = mags.*exp(j*phases);
	h = ifft(fth, lfft);
	h = real(h);

if phase_type ==  0 		% zero-phase

	cmin = real(ifft(mags, lfft));
	phase_min = zeros(1,lfft);

	hmin = zeros(1,resp_lgh);
	hmin(resp_lgh2+1:resp_lgh) = cmin(1:resp_lgh2);
	hmin(1:resp_lgh2) = cmin(lfft-(resp_lgh2-1):lfft);

	h = hmin;


elseif phase_type == 1 		% minimum-phase

				% generate min-phase impulse response
	%[cmin, hmin] = rceps(h);
	%phase_min = imag(fft(cmin, lfft));

	cmin = ifft(log(mags+.0000001), lfft);
	cmin(2:lfft2) = 2.0*cmin(2:lfft2);
	cmin(lfft2p1:lfft) = zeros(1,lfft2);
	phase_min = imag(fft(cmin, lfft));
	aux = real(ifft(exp(fft(cmin, lfft))));

	hmin = zeros(1,resp_lgh);
	hmin(resp_lgh2+1:resp_lgh) = aux(1:resp_lgh2);
	hmin(1:resp_lgh2) = aux(lfft-(resp_lgh2-1):lfft);

	h = hmin;
end;

	logmag = 20*log10(mag(1:lfft2) + 0.0000001);



if iplot == 1
				% plot
	clf; 
	subplot(411);
	plot(mags(1:lfft2));
	ylabel('amplitude');

	subplot(412);
	plot(20*log10(mag +0.0000001));
	ylabel('amplitude (db)');
%	xlabel('freq (DFT samples)')
	pause(1);

	subplot(413);
	plot(phase_min(1:lfft2));
	ylabel('phase');

	%subplot(312);
	%plot(unwrap(phase));
	%title('marc phase');
%	xlabel('freq (DFT samples)')

	subplot(414);
	plot(hmin(1:resp_lgh))
	ylabel('response');
%	xlabel(' time (samples)')
	pause(1);

end;

%end;
