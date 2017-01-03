function [tow,prn,P1,Phi1,P2,Phi2,eof] = readGrilM;
%READGRIL Reading a binary GRIL files.
%         The following message was sent to receiver:
%             em,,/msg/jps/{GT,SI,R1,P1,R2,P2}
%             dm

%Kai Borre November 12, 2007
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 2007/10/07 $

global EPH fidm

% Constants
c = 299792458;  % vacuum speed of light, m/s
f1 = 154*10.23E6;			% L1 frequency Hz
f2 = 120*10.23E6;			% L2 frequency Hz
lambda1 = c/f1;	     % wavelength on L1:  .19029367  m
lambda2 = c/f2;	     % wavelength on L2:  .244210213 m
Pi = 3.1415926535898; % GPS value for pi
eof = 0;

% struct GPSEphemeris
et = fread(fidm,5,'uint8');
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
    sv = fread(fidm,1,'uint8');
    i = sv;
    EPH(1,i) = sv;
    tow = fread(fidm,1,'uint32');
    flags = fread(fidm,1,'uint8');
    iodc = fread(fidm,1,'int16');
    EPH(21,i) = fread(fidm,1,'int32'); % toc
    ura = fread(fidm,1,'int8');
    healthS = fread(fidm,1,'uint8');
    wn = fread(fidm,1,'int16');
    tgd = fread(fidm,1,'single');
    EPH(2,i) = fread(fidm,1,'single'); % af2
    EPH(20,i) = fread(fidm,1,'single'); %af1
    EPH(19,i) = fread(fidm,1,'single'); %af0
    EPH(18,i) = fread(fidm,1,'int32'); %toe
    iode = fread(fidm,1,'int16');
    EPH(4,i) = fread(fidm,1,'double'); %rootA
    EPH(6,i) = fread(fidm,1,'double'); %ecc
    EPH(3,i) = fread(fidm,1,'double')*Pi; %m0
    EPH(16,i) = fread(fidm,1,'double')*Pi; %Omega0
    EPH(12,i) = fread(fidm,1,'double')*Pi; %i0
    EPH(7,i) = fread(fidm,1,'double')*Pi; %omega
    EPH(5,i) = fread(fidm,1,'single')*Pi; %deltan
    EPH(17,i) = fread(fidm,1,'single')*Pi; %Omegadot
    EPH(13,i) = fread(fidm,1,'single')*Pi; %idot
    EPH(10,i) = fread(fidm,1,'single'); %crc
    EPH(11,i) = fread(fidm,1,'single'); %crs
    EPH(8,i) = fread(fidm,1,'single'); %cuc
    EPH(9,i) = fread(fidm,1,'single'); %cus
    EPH(14,i) = fread(fidm,1,'single'); %cic
    EPH(15,i) = fread(fidm,1,'single'); %cis
    cs = fread(fidm,1,'uint8');
    dummy = fread(fidm,1,'uint8');
    et = fread(fidm,5,'char');
    if char(et(1:2)') ~= 'GE', break; end
end

% struct GPSTime
if char(et(1:2)') ~= 'GT'
    gt = fread(fidm,5,'uint8=>char');
else
    gt = et;
end
gt1 = char(gt');
%gt2 = str2num(gt1(3:end));
tow = fread(fidm,1,'uint32')/1000; %211 588 seconds
wn = fread(fidm,1,'uint16'); % 347 correct
cs = fread(fidm,1,'uint8');
dummy = fread(fidm,1,'uint8');

% struct SatIndex
si = fread(fidm,5,'char');
si1 = char(si');
si2 = hex2dec(si1(3:end));
for i = 1:si2-1
    prn(i) = fread(fidm,1,'uint8');
end
cs = fread(fidm,1,'uint8');
dummy = fread(fidm,1,'uint8');
PRN = prn;

% struct PR_P1
pr = fread(fidm,5,'char');
pr1 = char(pr');
pr2 = hex2dec(pr1(3:end));
for i = 1:si2-1
    prange1(i) = fread(fidm,1,'double')*c;
end
cs = fread(fidm,1,'uint8');
dummy = fread(fidm,1,'uint8');
P1 = prange1;

% struct PhaseP1
ph = fread(fidm,5,'char');
ph1 = char(ph');
ph2 = hex2dec(ph1(3:end));
for i = 1:si2-1
    phase1(i) = fread(fidm,1,'double')*lambda1;
end
cs = fread(fidm,1,'uint8');
dummy = fread(fidm,1,'uint8');
Phi1 = phase1;

% struct PR_P2
pr = fread(fidm,5,'char');
pr1 = char(pr');
pr2 = hex2dec(pr1(3:end));
for i = 1:si2-1
    prange2(i) = fread(fidm,1,'double')*c;
end
cs = fread(fidm,1,'uint8');
dummy = fread(fidm,1,'uint8');
P2 = prange2;

% struct PhaseP2
ph = fread(fidm,5,'char');
ph1 = char(ph');
ph2 = hex2dec(ph1(3:end));
for i = 1:si2-1
    phase2(i) = fread(fidm,1,'double')*lambda2;
end
cs = fread(fidm,1,'uint8');
dummy = fread(fidm,1,'uint8');
Phi2 = phase2;
%%%%%%%%% end readGrilM.m %%%%%%%%%%%%%%%%%%%%%%%%
