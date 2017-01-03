function [eph] = edata(ephemerisfile);
%EDATA    Reads a binary ephemeris file and stores it in
%     	 a matrix with 21 rows; column number is
%	       the number of ephemerides
%	       Typical call: edata('e0810a94.076')

%Kai Borre 04-10-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/23 $

GPS_pi = 3.1415926535898; 	 % Exact GPS value for pi

% Units are either seconds, meters, or radians
fide = fopen(ephemerisfile);
A = fread(fide,Inf,'char');   % The ephemeris file has no EOF mark
[m,n] = size(A);
clear A
noeph = m/129;		    % Each ephemeris contains 129 bytes
frewind(fide);
eph = zeros(21,noeph);

% Set aside memory for the input
svprn    = zeros(1,noeph);
wn       = zeros(1,noeph);
t0c      = zeros(1,noeph);
tgd      = zeros(1,noeph);
aodc     = zeros(1,noeph);
toe      = zeros(1,noeph);
af2      = zeros(1,noeph);
af1      = zeros(1,noeph);
af0      = zeros(1,noeph);
aode     = zeros(1,noeph);
deltan   = zeros(1,noeph);
M0       = zeros(1,noeph);
ecc      = zeros(1,noeph);
roota    = zeros(1,noeph);
toe      = zeros(1,noeph);
cic      = zeros(1,noeph);
crc      = zeros(1,noeph);
cis      = zeros(1,noeph);
crs      = zeros(1,noeph);
cuc      = zeros(1,noeph);
cus      = zeros(1,noeph);
Omega0   = zeros(1,noeph);
omega    = zeros(1,noeph); % argument of perigee
i0       = zeros(1,noeph);
Omegadot = zeros(1,noeph);
idot     = zeros(1,noeph);
accuracy = zeros(1,noeph);
health   = zeros(1,noeph);
fit      = zeros(1,noeph);

for i = 1:noeph
	 svprn(i) = fread(fide,1,'char');
	 wn(i)	  = fread(fide,1,'ushort');
    sec_of_week(i) = fread(fide,1,'long');
	 tgd(i)   = fread(fide,1,'float');
	 aodc(i)  = fread(fide,1,'long');
	 t0c(i)   = fread(fide,1,'long');
	 af2(i)   = fread(fide,1,'float');
	 af1(i)   = fread(fide,1,'float');
	 af0(i)   = fread(fide,1,'float');
	 aode(i)  = fread(fide,1,'long');
	 deltan(i) = fread(fide,1,'float')*GPS_pi;
	 M0(i)	  = fread(fide,1,'double')*GPS_pi;
	 ecc(i)   = fread(fide,1,'double');
	 roota(i) = fread(fide,1,'double');
	 toe(i)   = fread(fide,1,'long');
	 cic(i)   = fread(fide,1,'float');
	 crc(i)   = fread(fide,1,'float');
	 cis(i)   = fread(fide,1,'float');
	 crs(i)   = fread(fide,1,'float');
	 cuc(i)   = fread(fide,1,'float');
	 cus(i)   = fread(fide,1,'float');
	 Omega0(i) = fread(fide,1,'double')*GPS_pi;
	 omega(i) = fread(fide,1,'double')*GPS_pi;
	 i0(i)	  = fread(fide,1,'double')*GPS_pi;
	 Omegadot(i) = fread(fide,1,'float')*GPS_pi;
	 idot(i)  = fread(fide,1,'float')*GPS_pi;
	 accuracy(i) = fread(fide,1,'short');
	 health(i) = fread(fide,1,'ushort');
	 fit(i) = fread(fide,1,'ushort');
end
status  =  fclose(fide);
%  Description of variable eph.
eph(1,:) = svprn;
eph(2,:) = af2;
eph(3,:) = M0;
eph(4,:) = roota;
eph(5,:) = deltan;
eph(6,:) = ecc;
eph(7,:) = omega;
eph(8,:) = cuc;
eph(9,:) = cus;
eph(10,:) = crc;
eph(11,:) = crs;
eph(12,:) = i0;
eph(13,:) = idot;
eph(14,:) = cic;
eph(15,:) = cis;
eph(16,:) = Omega0;
eph(17,:) = Omegadot;
eph(18,:) = toe;
eph(19,:) = af0;
eph(20,:) = af1;
eph(21,:) = t0c;

fidu = fopen('edata.dat','w');
count = fwrite(fidu,[eph],'double');
fclose('all');
%%%%%%%%% end edata.m %%%%%%%%%
