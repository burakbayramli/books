function iono_delay(n_prn)
%	This script file is for the iono-delay calculation
%
%	The GPS users continuously receive navigation information from
%	the sapace vihicles in the form of modulated data bits.  The
%	The received information includes the satellites' time, its
%	clock correction and ephemeris parameters, almanacs and health
%	for all GPS space vehicles, and text messages.  The precise
%	postion and clock offset of the space vehicles's antenna phase
%	center in the ECEF coordicates can be computed by receiving
%	this information.
%
%	Constants:
%		pi				=	3.1415926535898;
%		mu				=	WGS-84 value of the earth's universal gravitational 
%							parameter
%						=	3.986005e14 (meters^3/sec^2);
%		Omegae_dot 	= 	WGS-84 value of the earth's rotation rate
%						=	7.2921151467e-5 (rad/sec);
%		F				=	-2*mu^.5/c^2 (sec/meter^.5);
%		c				=	speed of light (meters/sec);
%
	pi				=	3.1415926535898;
	mu				=	3.986005e14; 
   Omegae_dot 	=	7.2921151467e-5;
   F				=	-4.442807633e-10;			% sec/meter^.5
   c 				=	2.99792458e+8;				% meters/sec
   freq1			=  154.*10.23*10^6;			% Hz
   freq2			=	120.*10.23*10^6;			% Hz
   lamda1		=	c/freq1;						% meters
   lamda2		=	c/freq2;						% meters
%
%	Initialization
%
	for i = 1:32
%
      iflag(i)    = 0;
      e(i) 			= 0.;
      toc(i) 		= 0;
      toe(i) 		= 0;
      Wn(i) 		= 0.;
      Omega_dot(i)= 0.;
      Omega0(i)	= 0.;
      M0(i)			= 0.;
      af0(i)		= 0.;
      af1(i)		= 0.;
      delta_n(i)	= 0.;
      A(i)			= 0.;
      i0(i)			= 0.;
%
		af2(i)		= 0.;
      I_dot(i)		= 0.;
%
   end;
%   
%	Input $ALMA Data
%
%
%	load in data
%
	fid = fopen('alma.dat','r');
%
	for i_prn = 1:27
		id_alma			= fscanf(fid, '%s5',1);
      prn 				= fscanf(fid, '%d',1);
      e(prn)			= fscanf(fid, '%f',1);
      toc(prn) 		= fscanf(fid, '%d',1);
      Wn(prn) 			= fscanf(fid, '%d',1);
      Omega_dot(prn)	= fscanf(fid, '%f',1);
      Omega0(prn)		= fscanf(fid, '%f',1);
      omega(prn)		= fscanf(fid, '%f',1);
      M0(prn)			= fscanf(fid, '%f',1);
      af0(prn)			= fscanf(fid, '%f',1);
      af1(prn)			= fscanf(fid, '%f',1);
      delta_n(prn)	= fscanf(fid, '%f',1);
      A(prn)			= fscanf(fid, '%f',1);
      i0(prn)			= fscanf(fid, '%f',1);
      health4			= fscanf(fid, '%d',1);
      health5			= fscanf(fid, '%d',1);
      checksum1		= fscanf(fid, '%s4',1);
   end
%
	fclose(fid);
%
%	Input $RGEA Data
%
%
	fid = fopen('rgea_temp.dat','r');
%
i_time = 1;
for i_time = 1:200
   for i = 1:32       
      tsv(i)		= 0.;
      psr(i)		= 0.;
      psr_std(i) 	= 0.;
      adr(i) 		= 0.;
      adr_std(i)	= 0.;
      dopp(i)		= 0.;
      CN0(i)		= 0.;
      locktime(i)	= 0.;
      psr2(i)		= 0.;
      psr2_std(i) = 0.;
      adr2(i) 		= 0.;
      adr2_std(i)	= 0.;
      dopp2(i)		= 0.;
      CN02(i)		= 0.;
      locktime2(i)= 0.;
   end
%      
%	while (feof(fid) ~= 1)
		id_rgea			= fscanf(fid, '%s5',1);
      Wn1				= fscanf(fid, '%d',1);
      trc(i_time)		= fscanf(fid, '%f',1);
      Nobs				= fscanf(fid, '%d',1);
      Rec_status		= fscanf(fid, '%s7',1);
      Nobs_prn			= Nobs/2;
%     
      for i = 1:Nobs_prn
         prn1 				= fscanf(fid, '%d',1);
         iflag(prn1)		= 1;
      	psr(prn1)		= fscanf(fid, '%f',1);
         psr_std(prn1) 	= fscanf(fid, '%f',1);
         adr(prn1) 		= fscanf(fid, '%f',1);
      	adr_std(prn1)	= fscanf(fid, '%f',1);
      	dopp(prn1)		= fscanf(fid, '%f',1);
      	CN0(prn1)		= fscanf(fid, '%f',1);
         locktime(prn1)	= fscanf(fid, '%f',1);
         ch_tr_status   = fscanf(fid, '%s9',1);
         prn2 				= fscanf(fid, '%d',1);
      	psr2(prn2)		= fscanf(fid, '%f',1);
         psr2_std(prn2) 	= fscanf(fid, '%f',1);
         adr2(prn2) 		= fscanf(fid, '%f',1);
      	adr2_std(prn2)	= fscanf(fid, '%f',1);
      	dopp2(prn2)		= fscanf(fid, '%f',1);
      	CN02(prn2)		= fscanf(fid, '%f',1);
         locktime2(prn2)	= fscanf(fid, '%f',1);
         ch_tr_status2  = fscanf(fid, '%s9',1);
		end;
%
%	A complete set of ephemeris parameters received by teh GPS
%	receiver for a give satellite includes:
%
%		Wn			=	GPS week number;
%		tow		=	Time of GPS week (sec);
%		tgd		=	Time of group delay (sec);
%		AODC		=	clock data issue;
%		toc		=	clock data reference time (sec);
%		af2		=	GPS clock drift (sec/(sec*sec));
%		af1		=	GPS clock drift (sec/sec);
%		af0		=	GPS clock drift (sec);
%		AODE		=	orbit data issue;
%		delta_n	=	mean motion difference from computed 
%						value (semi-circle/sec);	
%		M0			=	mean anomaly at reference time (semi-circle);
%		e			=	eccentricity;
%		A_half	=	square root of the semi-major axis 
%						(meter_half);	
%		toe		=	emphemeris reference time (sec);
%		Cuc		=	amplitude of the cosine correction term to
%						the argument of latitude (radians);
%		Cus		=	amplitude of the sine correction term to
%						the argument of latitude (radians);
%		Crc		=	amplitude of the cosine correction term to
%						the orbit radius (meters);
%		Crs		=	amplitude of the sine correction term to
%						the orbit radius (meters);
%		Cic		=	amplitude of the cosine correction term to
%						the angle of inclination (radians);
%		Cis		=	amplitude of the sine correction term to
%						the angle of inclination (radians);
%		Omega0	=	right ascension at reference time (semi-circle);
%		omega		=	argument of perigee (semi-circle);
%		io			=	inclination angle at reference time (semi-circle);
%		Omega_dot=	rate of right ascension (semi-circle/sec);
%		I_dot		=	rate of inclination angle (semi-circle/sec);
%		ACCURACY	=	accuracy;
%		HEALTH	=	health;
%		FIT		=	fit;
%		PRN		=	SV PRN;
%		RES		=	res;
%		CHECKSUM = 	checksum;
%
%	Input Data:
%
%		Wn			=	850;  					
%		tow		=	248670;					% sec
%		tgd		=	9.313226e-10;			% sec
%		AODC		=	133;
%		toc		=	252000;					% sec
%		af2		=	0.000000e+00;			% sec/(sec*sec)
%		af1		=	-3.183231e-12;			% sec/sec
%     af0		=	-2.502208e-04;			% sec
%		AODE		=	133;
%	 	delta_n	=	4.908419e-09;			% semi-circle/sec)	
%		M0			=	-3.682930e-01;			% semi-circle
%		e			=	1.521692e-02;
%		A_half	=	5.153688e+03;			% meter^.5 
%		toe		=	252000;					% sec
		Cic		=	6.146729e-08;			% radians
      Crc		=	2.259375e+02;			% meters	
      Cis		=	2.086163e-07;			% radians
      Crs		=	7.321875e+01;			% meters
      Cuc		=	4.017726e-06;			% radians	
      Cus		=	7.698312e-06;			% radians
%		Omega0	=	1.384688e+00;			% semi-circle
%		omega		=	-2.466272e+00;			% semi-circle
%		i0			=	9.464037e-01;			% semi-circle
%		Omega_dot=	-8.464281e-09;			% semi-circle/sec
% 		I_dot(prn)	=	9.178953e-11;		% semi-circle/sec
%		ACCURACY	=	7;
%		HEALTH	=	0;
%		FIT		=	0;
%		PRN		=	2;
%		RES		=	1;
%     CHECKSUM =	62874;
%		
%		tsv		=	Effective SV PRN code phase time at 
%						message transmission time (seconds);
%		delta_tsv=	SV PRN code phase time offset (seconds);
%		delta_tr =	the relativistic correction term (seconds);
%		t			=	GPS system time (seconds);
%
for prn = find(iflag)
   iono_psr(i_time,prn) = 0;
   iono_adr(i_time,prn) = 0;
%   while (iflag(prn) == 1)
		I_dot(prn)	=	9.178953e-11;
      tsv(prn)	=	trc(i_time)-psr(prn)/c;		
      Ek(prn)	=	M0(prn);
      delta_tr(prn) = F*e(prn)*A(prn)^.5*sin(Ek(prn));
      delta_tsv(prn)= af0(prn)+af1(prn)*(tsv(prn)-toc(prn))+af2(prn)*(tsv(prn)-toc(prn))^2.+delta_tr(prn);
      t(prn)	= 	tsv(prn) - delta_tsv(prn);
%
%	A = semi-major axis (meters);
%
%	n0 = computed mean motion (rad/sec);
%
	n0(prn) 	= (mu/A(prn)^3.)^(1/2);
%
   toe(prn) =	toc(prn);
	tk(prn)	= t(prn)-toe(prn);
   n(prn) 	= delta_n(prn);
   Mk(prn) 	= M0(prn)+n(prn)*tk(prn);
%   
   Ek_new(prn) = Mk(prn);
   
   for jx = 1:100
         Ek_old(prn) 	= Ek_new(prn);
         f(prn) 			= Ek_old(prn)-e(prn)*sin(Ek_old(prn))-Mk(prn);
         f_p(prn) 		= 1.-e(prn)*cos(Ek_old(prn));
         f_pp(prn) 		= e(prn)*sin(Ek_old(prn));
         Ek_new(prn) 	= Ek_old(prn)-(f(prn)/(f_p(prn)-f_pp(prn)*f(prn)/(2.*f_p(prn))));
        	jx 				= jx+1;
   end
   
   Ek(prn) = Ek_new(prn);
%     
%
	numerator(prn) 	= (1-e(prn)^2.)^(1/2.)*sin(Ek(prn));
	denominator(prn) 	= cos(Ek(prn))-e(prn);
   nuk(prn) 			= atan(numerator(prn)/denominator(prn));
   Phik(prn) 			= nuk(prn)+omega(prn);
%
%	Second Harmonic Perturbations   
%  
	delta_uk(prn) = Cus*sin(2*Phik(prn))+Cuc*cos(2*Phik(prn));
   delta_rk(prn) = Crs*sin(2*Phik(prn))+Crc*cos(2*Phik(prn));
   delta_ik(prn) = Cis*sin(2*Phik(prn))+Cic*cos(2*Phik(prn));
%   
   uk(prn) = Phik(prn)+delta_uk(prn);
   rk(prn) = A(prn)*(1.-e(prn)*cos(Ek(prn)))+delta_rk(prn);
   ik(prn) = i0(prn)+delta_ik(prn)+I_dot(prn)*tk(prn);
%
%	Position in Orbital Plane
%
	xk_pline(prn) = rk(prn)*cos(uk(prn));
   yk_pline(prn) = rk(prn)*sin(uk(prn));
%
	Omegak(prn) = Omega0(prn)+(Omega_dot(prn)-Omegae_dot)*tk(prn)-Omegae_dot*toe(prn);
%
%	ECEF coordinates
%
	xk(i_time,prn) = xk_pline(prn)*cos(Omegak(prn))-yk_pline(prn)*cos(ik(prn))*sin(Omegak(prn));
   yk(i_time,prn) = xk_pline(prn)*sin(Omegak(prn))+yk_pline(prn)*cos(ik(prn))*cos(Omegak(prn));
   zk(i_time,prn) = yk_pline(prn)*sin(ik(prn));
%
%	iono correction
%
	iono_psr(i_time,prn) = (psr2(prn)-psr(prn))/(-1+(freq1^2/freq2^2));
	iono_adr(i_time,prn) = (lamda1*adr(prn)-lamda2*adr2(prn))/(-1+(freq1^2/freq2^2));
	end
%end
%  i_time = i_time + 1;
end
fclose(fid);
%figure(1);
%subplot(2,2,1);
%title('PRN 18 : x-position');
%plot(trc,xk(:,18));
%subplot(2,2,2);
%title('PRN 18 : y-position');
%plot(trc,yk(:,18));
%subplot(2,2,3);
%title('PRN 18 : z-position');
%plot(trc,zk(:,18));
figure;
plot(trc(10:200), iono_psr(10:200,n_prn),trc(10:200), iono_adr(10:200,n_prn));
title('iono-delay');
Xlabel('gps-time');
Ylabel('iono-delay (meters)');
legend('psuedo-range','carrier-phase');
hold on;
%   
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
 
	
   