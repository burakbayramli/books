function [tow,prn,P1,Phi1,P2,Phi2,eof] = readGrilR
%READGRIL Reading a binary GRIL files.
%         The following message was sent to receiver:
%             em,,/msg/jps/{GT,SI,R1,P1,R2,P2}
%             dm
	    
%Kai Borre November 12, 2007
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 2007/11/12 $

global fidr

% Constants
c = 299792458;  % vacuum speed of light, m/s
f1 = 154*10.23E6;			% L1 frequency Hz
f2 = 120*10.23E6;			% L2 frequency Hz
lambda1 = c/f1;	     % wavelength on L1:  .19029367  m
lambda2 = c/f2;	     % wavelength on L2:  .244210213 m
Pi = 3.1415926535898; % GPS value for pi
eof = 0;

% struct GPSEphemeris
et = fread(fidr,5,'uint8');
if isempty(et) == 1 
    eof = 1; 
    tow = 0;
    prn = [];
    P1 = [];
    Phi1 = [];
    P2 = P1;
    Phi2 = Phi1;
    return; 
end

while char(et(1:2)') == 'GE'
    %et2 = hex2dec(et1(3:end));
    sv = fread(fidr,1,'uint8'); 
    i = 1;% sv;
    eph(1,i) = sv;
    tow = fread(fidr,1,'uint32');
    flags = fread(fidr,1,'uint8');
    iodc = fread(fidr,1,'int16');
    eph(21,i) = fread(fidr,1,'int32'); % toc
    ura = fread(fidr,1,'int8');
    healthS = fread(fidr,1,'uint8');
    wn = fread(fidr,1,'int16');
    tgd = fread(fidr,1,'single');
    eph(2,i) = fread(fidr,1,'single'); % af2
    eph(20,i) = fread(fidr,1,'single'); %af1
    eph(19,i) = fread(fidr,1,'single'); %af0
    eph(18,i) = fread(fidr,1,'int32'); %toe
    iode = fread(fidr,1,'int16');
    eph(4,i) = fread(fidr,1,'double'); %rootA
    eph(6,i) = fread(fidr,1,'double'); %ecc
    eph(3,i) = fread(fidr,1,'double')*Pi; %m0
    eph(16,i) = fread(fidr,1,'double')*Pi; %Omega0
    eph(12,i) = fread(fidr,1,'double')*Pi; %i0
    eph(7,i) = fread(fidr,1,'double')*Pi; %omega
    eph(5,i) = fread(fidr,1,'single')*Pi; %deltan
    eph(17,i) = fread(fidr,1,'single')*Pi; %Omegadot
    eph(13,i) = fread(fidr,1,'single')*Pi; %idot
    eph(10,i) = fread(fidr,1,'single'); %crc
    eph(11,i) = fread(fidr,1,'single'); %crs
    eph(8,i) = fread(fidr,1,'single'); %cuc
    eph(9,i) = fread(fidr,1,'single'); %cus
    eph(14,i) = fread(fidr,1,'single'); %cic
    eph(15,i) = fread(fidr,1,'single'); %cis
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
    prn(i) = fread(fidr,1,'uint8');
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
for i = 1:si2-1
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
%%%%%%%%% end readGrilR.m %%%%%%%%%%%%%%%%%%%%%%%%
