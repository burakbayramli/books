% display_data    Read and plot dsp signals in dsp data folder.

% Note: You may need to modify the path to the signal in "fid":

% speech
fid=fopen('speech.bin','r');
speech=fread(fid,'int');
fclose(fid);

% seismic
fid=fopen('seismic.bin','r');
seismic=fread(fid,'int');
fclose(fid);

% friends
pals=zeros(256,256,3);
fid=fopen('friends.bin','r');
pals(:)=fread(fid,'int');
fclose(fid);

% Plots
sp_fig(1,9,6);
subplot(2,2,1);
plot(speech,'g'); grid on;
title('Speech');
subplot(2,2,3);
plot(seismic,'r'); grid on;
title('Seismic');
subplot(1,2,2);
image(pals/255);
axis square;
title('Friends');
