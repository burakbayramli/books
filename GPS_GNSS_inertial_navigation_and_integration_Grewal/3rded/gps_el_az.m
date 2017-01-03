function gps_el_az(n_prn, phi_user, lamda_user)
%	This function file is for the GPS satellite position calculation
%
%	User location:	Cal. State Fullerton
%	phi_user		=	geodetic latitude of the user (deg)
%	lamda_user	=	geodetic longitude of the user (deg)
%	phi_user		= 	33.8825;										% deg
%   lamda_user	= 	-117.8833;										% deg
%
%	Convert the user's position from the ellipsoidal coordinate system
%	to the earth-center-earth_fixed (ECEF) coordinates
%
%	h = geodetic height of the user;
%	a = semimajor axis of the earth (WGS 84)
%	b = semiminor axis of the earth (WGS 84)
	h = 0.;
   a = 6378137.0;                     % meters
   b = 6356752.3142;						  % meters
   N = a^2./((a*cos(phi_user*pi/180.))^2+(b*sin(phi_user*pi/180.))^2);
   r_user = [(N+h)*cos(phi_user*pi/180.)*cos(lamda_user*pi/180.); ...
      		 (N+h)*cos(phi_user*pi/180.)*sin(lamda_user*pi/180.); ...
             ((b^2/a^2)*N+h)*sin(phi_user*pi/180.)];
%	T_mat  = local tangent plan transition matrix
	T_mat(1,1)	 =	-sin(lamda_user*pi/180.);
   T_mat(1,2)	 =	 cos(lamda_user*pi/180.);
   T_mat(1,3)	 =  0.;
   T_mat(2,1)	 =	-cos(lamda_user*pi/180.)*sin(phi_user*pi/180.);
   T_mat(2,2)	 =	-sin(lamda_user*pi/180.)*sin(phi_user*pi/180.);
   T_mat(2,3)	 =	 cos(phi_user*pi/180.);
   T_mat(3,1)	 =	 cos(lamda_user*pi/180.)*cos(phi_user*pi/180.);
   T_mat(3,2)	 =	 sin(lamda_user*pi/180.)*cos(phi_user*pi/180.);
   T_mat(3,3)	 =	 sin(phi_user*pi/180.);
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
%figure;
	pi				=	3.1415926535898;
	mu				=	3.986005e14; 
   Omegae_dot 	=	7.2921151467e-5;
   F				=	-4.442807633e-10;			% sec/meter^.5
   c 				=	2.99792458e+8;				% meters/sec
%
%	Initialization
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
%
%	Input Data:
%
	fid = fopen('alma.dat','r');
%
	for i_sv = 1:27
		id_alma					= fscanf(fid, '%s5',1);
      i_prn						= fscanf(fid, '%d',1);
      alma_e(i_prn)			= fscanf(fid, '%f',1);
      alma_toc(i_prn) 		= fscanf(fid, '%d',1);
      alma_Wn(i_prn) 		= fscanf(fid, '%d',1);
      alma_Omega_dot(i_prn)= fscanf(fid, '%f',1);
      alma_Omega0(i_prn)	= fscanf(fid, '%f',1);
      alma_omega(i_prn)		= fscanf(fid, '%f',1);
      alma_M0(i_prn)			= fscanf(fid, '%f',1);
      alma_af0(i_prn)		= fscanf(fid, '%f',1);
      alma_af1(i_prn)		= fscanf(fid, '%f',1);
      alma_delta_n(i_prn)	= fscanf(fid, '%f',1);
      alma_A(i_prn)			= fscanf(fid, '%f',1);
      alma_i0(i_prn)			= fscanf(fid, '%f',1);
      alma_health4			= fscanf(fid, '%d',1);
      alma_health5			= fscanf(fid, '%d',1);
      alma_checksum1			= fscanf(fid, '%s4',1);
      %
   end
   fclose(fid);
		prn = n_prn;
%		prn = 18;
		Wn			=	alma_Wn(prn);  					
%		tow		=	248670;					% sec
%		tgd		=	9.313226e-10;			% sec
%		AODC		=	133;
		toc		=	alma_toc(prn);			% sec
		af2		=	0.000000e+00;			% sec/(sec*sec)
		af1		=	alma_af1(prn);			% sec/sec
      af0		=	alma_af0(prn);			% sec
%		AODE		=	133;
	 	delta_n	=	alma_delta_n(prn);	% semi-circle/sec)	
		M0			=	alma_M0(prn);			% semi-circle
		e			=	alma_e(prn);
		A			=	alma_A(prn);			% meter^.5 
		toe		=	alma_toc(prn);			% sec
		Cic		=	6.146729e-08;			% radians
      Crc		=	2.259375e+02;			% meters	
      Cis		=	2.086163e-07;			% radians
      Crs		=	7.321875e+01;			% meters
      Cuc		=	4.017726e-06;			% radians	
      Cus		=	7.698312e-06;			% radians
		Omega0	=	alma_Omega0(prn);		% semi-circle
		omega		=	alma_omega(prn);		% semi-circle
		i0			=	alma_i0(prn);			% semi-circle
		Omega_dot=	alma_Omega_dot(prn);	% semi-circle/sec
 		I_dot		=	9.178953e-11;			% semi-circle/sec
%		
%		tsv		=	Effective SV PRN code phase time at 
%						message transmission time (seconds);
%		delta_tsv=	SV PRN code phase time offset (seconds);
%		delta_tr =	the relativistic correction term (seconds);
%		t			=	GPS system time (seconds);
figure;
h = polar([0 2*pi],[0 90]);
hold on;
delete(h);
for i = 1:289
   t_plot(i) = i/12.;
 	tsv = 170417 + (i-1)*300.;
      Ek			=	M0;
      delta_tr = F*e*A^.5*sin(Ek);
      delta_tsv= af0+af1*(tsv-toc)+af2*(tsv-toc)^2.+delta_tr;
      t			= 	tsv - delta_tsv;
%
%	A = semi-major axis (meters);
%	n0 = computed mean motion (rad/sec);
%
	n0 	= 	(mu/A^3.)^(1/2);
%
   toe 	=	toc;
	tk		= 	t-toe;
   n 		= 	delta_n;
   Mk 	= 	M0+n*tk;
%   
   Ek_new= Mk;
   
   for jx = 1:100
         Ek_old 	= Ek_new;
         f 			= Ek_old-e*sin(Ek_old)-Mk;
         f_p 		= 1.-e*cos(Ek_old);
         f_pp 		= e*sin(Ek_old);
         Ek_new 	= Ek_old-(f/(f_p-f_pp*f/(2.*f_p)));
        	jx 		= jx+1;
   end
%   
   Ek = Ek_new;
%     
%
	numerator 	= (1-e^2.)^(1/2.)*sin(Ek);
	denominator = cos(Ek)-e;
   nuk 			= atan(numerator/denominator);
   if (denominator <= 0.)
      nuk = nuk + pi;
   end
   Phik 			= nuk+omega;
%
%	Second Harmonic Perturbations   
%  
	delta_uk = Cus*sin(2*Phik)+Cuc*cos(2*Phik);
   delta_rk = Crs*sin(2*Phik)+Crc*cos(2*Phik);
   delta_ik = Cis*sin(2*Phik)+Cic*cos(2*Phik);
%   
   uk = Phik+delta_uk;
   rk = A*(1.-e*cos(Ek))+delta_rk;
   ik = i0+delta_ik+I_dot*tk;
%
%	Position in Orbital Plane
%
	xk_pline = rk*cos(uk);
   yk_pline = rk*sin(uk);
%
	Omegak = Omega0+(Omega_dot-Omegae_dot)*tk-Omegae_dot*toe;
%
%	ECEF coordinates
%
	xk(i) = xk_pline*cos(Omegak)-yk_pline*cos(ik)*sin(Omegak);
   yk(i) = xk_pline*sin(Omegak)+yk_pline*cos(ik)*cos(Omegak);
   zk(i) = yk_pline*sin(ik);
   r_sv = [xk(i); yk(i); zk(i)];
   r_relative = r_sv - r_user;
   ENU = T_mat * r_relative;
%	elevation angle
   numerator 	= ENU(3);
   denominator = (ENU(1)^2+ENU(2)^2)^.5;
   el(i)			= atan(numerator/denominator)*180./pi;
   if (el(i) <= 0.)
      el(i) = 0.;
   end
%	azmuth
   numerator 	= ENU(1);
   denominator = ENU(2);
   az(i)			= atan(numerator/denominator)*180./pi;
   if (denominator <= 0.)
      az(i) = az(i) + 180.;
   end
   az(i) = mod(az(i)+360, 360.);
y = -(el(i)-90)*sin((az(i)+90.)*pi/180.);
x = -(el(i)-90)*cos((az(i)+90.)*pi/180.);
hold on;
if (el(i) ~= 0.)
   plot(x,y,'*r');
end
end
%end
%fclose(fid);
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  

   