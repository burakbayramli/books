function [tow,prn,P1,Phi1,P2,Phi2] = readGril;
%READGRIL Reading a binary GRIL files.
%         The following message was sent to receiver:
%             em,,/msg/jps/{GT,SI,R1,P1,R2,P2}
%             dm
%
%	    

%Kai Borre May 6, 2006
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2006/04/23 $

global EPH fidr

%masterfile = '27apr06r.log';
%fidr = fopen(masterfile);

%for ii = 1:100 %%%

% Constants
c = 299792458;  % vacuum speed of light, m/s
f1 = 154*10.23E6;			% L1 frequency Hz
f2 = 120*10.23E6;			% L2 frequency Hz
lambda1 = c/f1;	     % wavelength on L1:  .19029367  m
lambda2 = c/f2;	     % wavelength on L2:  .244210213 m
%alpha1 = f1^2/(f1^2-f2^2);	  % parameters for ionosphere free
%alpha2 = -f2^2/(f1^2-f2^2);	% combination
%omegae = 7.292115147e-5;	  % rotation rate of the earth rad/s

% GPS value for pi
Pi = 3.1415926535898; 

% struct GPSEphemeris
et = fread(fidr,5,'uint8');
while char(et(1:2)') == 'GE'
    %et2 = hex2dec(et1(3:end));
    sv = fread(fidr,1,'uint8'); 
    i = sv;
    EPH(1,i) = sv;
    tow = fread(fidr,1,'uint32');
    flags = fread(fidr,1,'uint8');
    iodc = fread(fidr,1,'int16');
    EPH(21,i) = fread(fidr,1,'int32'); % toc
    ura = fread(fidr,1,'int8');
    healthS = fread(fidr,1,'uint8');
    wn = fread(fidr,1,'int16');
    tgd = fread(fidr,1,'single');
    EPH(2,i) = fread(fidr,1,'single'); % af2
    EPH(20,i) = fread(fidr,1,'single'); %af1
    EPH(19,i) = fread(fidr,1,'single'); %af0
    EPH(18,i) = fread(fidr,1,'int32'); %toe
    iode = fread(fidr,1,'int16');
    EPH(4,i) = fread(fidr,1,'double'); %rootA
    EPH(6,i) = fread(fidr,1,'double'); %ecc
    EPH(3,i) = fread(fidr,1,'double')*Pi; %m0
    EPH(16,i) = fread(fidr,1,'double')*Pi; %Omega0
    EPH(12,i) = fread(fidr,1,'double')*Pi; %i0
    EPH(7,i) = fread(fidr,1,'double')*Pi; %omega
    EPH(5,i) = fread(fidr,1,'single')*Pi; %deltan
    EPH(17,i) = fread(fidr,1,'single')*Pi; %Omegadot
    EPH(13,i) = fread(fidr,1,'single')*Pi; %idot
    EPH(10,i) = fread(fidr,1,'single'); %crc
    EPH(11,i) = fread(fidr,1,'single'); %crs
    EPH(8,i) = fread(fidr,1,'single'); %cuc
    EPH(9,i) = fread(fidr,1,'single'); %cus
    EPH(14,i) = fread(fidr,1,'single'); %cic
    EPH(15,i) = fread(fidr,1,'single'); %cis
    cs = fread(fidr,1,'uint8');
    dummy = fread(fidr,1,'uint8');    
    et = fread(fidr,5,'char'); 
    if char(et(1:2)') ~= 'GE', break; end
end


% struct GPSTime
if char(et(1:2)') ~= 'GT'
    gt = fread(fidr,5,'uint8=>char');
else
    gt = et;
end
gt1 = char(gt');
%gt2 = str2num(gt1(3:end));
tow = fread(fidr,1,'uint32')/1000; %211 588 seconds
wn = fread(fidr,1,'uint16'); % 347 correct
cs = fread(fidr,1,'uint8');
dummy = fread(fidr,1,'uint8');

% struct SatIndex
si = fread(fidr,5,'char');
si1 = char(si');
si2 = hex2dec(si1(3:end));
for i = 1:si2-1
    prn(i) = fread(fidr,1,'int8'); %uint8
end
cs = fread(fidr,1,'uint8');
dummy = fread(fidr,1,'uint8');
PRN = prn;

% struct PR_P1
pr = fread(fidr,5,'char');
pr1 = char(pr');
pr2 = hex2dec(pr1(3:end));
for i = 1:si2-1
    prange1(i) = fread(fidr,1,'double')*c;
end
cs = fread(fidr,1,'uint8');
dummy = fread(fidr,1,'uint8');
P1 = prange1;

% struct PhaseP1
ph = fread(fidr,5,'char');
ph1 = char(ph');
ph2 = hex2dec(ph1(3:end));
for i=1:si2-1
    phase1(i) = fread(fidr,1,'double')*lambda1;
end
cs = fread(fidr,1,'uint8');
dummy = fread(fidr,1,'uint8');
Phi1 = phase1;

% struct PR_P2
pr = fread(fidr,5,'char');
pr1 = char(pr');
pr2 = hex2dec(pr1(3:end));
for i = 1:si2-1
    prange2(i) = fread(fidr,1,'double')*c;
end
cs = fread(fidr,1,'uint8');
dummy = fread(fidr,1,'uint8');
P2 = prange2;

% struct PhaseP2
ph = fread(fidr,5,'char');
ph1 = char(ph');
ph2 = hex2dec(ph1(3:end));
for i = 1:si2-1
    phase2(i) = fread(fidr,1,'double')*lambda2;
end
cs = fread(fidr,1,'uint8');
dummy = fread(fidr,1,'uint8');
Phi2 = phase2;

%%%%%%%%% end readGril.m %%%%%%%%%%%%%%%%%%%%%%%%
