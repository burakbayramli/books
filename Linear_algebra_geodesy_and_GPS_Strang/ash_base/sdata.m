function [h] = sdata(masterfile,roverfile)
%SDATA Reading of antenna offsets.
%   	 The 2 antenna heights are saved
%	    as h = ["rover"; "master"]
%	    Typical call: sdata('s0810a94.076','s0005a94.076')

%Kai Borre  03-30-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/23  $

% Unit in meters
antrad = 0.135;  % Antenna radius  (this value fits Ashtech antennas)
h = zeros(2,1);

for k = 1:2
   if k == 1, fid(k) = fopen(roverfile); end
   if k == 2, fid(k) = fopen(masterfile); end
   a = fscanf(fid(k),'%s',inf);
   q = findstr(a,'BEFORE');
   h_bef = a(q+6:q+10);
   q = findstr(a,'AFTER');
   h_aft = a(q+5:q+9);
   if h_aft == 0, h_aft = h_bef; end
   h(k,1) = mean(str2num([h_bef; h_aft]));
end
h = sqrt(h.^2-antrad^2);
fidu = fopen('sdata.dat','w');
count = fwrite(fidu,[h],'double');
fclose('all');
%%%%%%%%% end sdata.m %%%%%%%%%
