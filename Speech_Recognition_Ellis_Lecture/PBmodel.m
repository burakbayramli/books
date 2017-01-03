% Pbmodel1.m
%
%
%
%   		1D Linear Cochlear Model for the gerbil
%
%					R. Naidu and D. Mountain
%				Cochlear Biophysics Laboratory
%			Boston University Hearing Research Center
%
% The model computes Pressure difference and the BM volume
% velocity by picking a number for the fluid volume velocity
% through the helicotrema and then working back towards
% the base.  At each section the fluid volume velocity is computed
% by adding the fluid volume velocity from the previous section to
% the basilar membrane volume velocity.  The pressure drop across the
% fluid due to this velocity is then computed and added to the pressure
% difference across the cochlear partition.  Since the model is linear,
% the results above can then be scaled to the pressure difference at the
% stapes.

%The following are for gerbil, other frequency-place maps are available at:
%   http://earlab.bu.edu/anatomy/cochlea/freq_place_maps/Greenwood.htm

L   = 1.21;		% length of cochlea (cm)
a   = 398.;		%Greenwood coefficient
b   = 2.2;		%Greenwood coefficient
k   = 0.631;	%Greenwood coefficient
 
N = 512; 		% No. of sections
T = L/N; 		% spatial step size

rho = 1.02;		% Fluid density (gm/cm^3)

P  = zeros(1,N);	% Pressure difference array (dyne/cm)
Zp = zeros(1,N);	% Cochlear Partition Impedance array (cgs Ohms)
Vbm= zeros(1,N);	% Basilar membrane volume velocity array (cm^3/s)
						% To get to mechanical velocity (cm/s), this needs to be mulitiplied
						% by 1/(T*Wbm) where Wbm is the effective width of the BM

f = input('Frequency of tone ==>');
q = input('Quality factor (try 10) ==>');

w = 2*pi*f;

for i = 1:N					% Compute model parameters
   
	Wbm(i) = 0.0175;		%Width of the BM (cm)
	y(i)   = 0.01;			%Thickness of the organ of Corti (cm)
	As(i)  = .0002;		%Cross-sectional area of scalae (cm^2)
	Q(i)   = q;				%Quality factor of the RLC circuit
	x(i)   = i*T;			%Distance from base (cm)

	Ls(i)  = rho*T/As(i);						%Mass of scalae
	CF(i)  = a*(10^(((L-x(i))/L)*b) - k);	%Frequency-place map
	Cbm(i) =  exp(-30.9 + 4.04*x(i));		%Volume compliance of cochlear partition
	Lbm(i) = 1/(Cbm(i)*(2*pi*CF(i))^2);		%Effective Mass of cochlear partition
	Rbm(i) = (1/Q(i))*(Lbm(i)/Cbm(i))^0.5;	%viscous resistance of cochlear partition
       
%Cochlear partition impedance
	Zp(i)  = (-w^2*Lbm(i)*Cbm(i) + 1 + Rbm(i)*j*w*Cbm(i))/(j*w*Cbm(i));   %j = sqrt(-1)
	end
      
% define the conditions at the helicotrema modeling it as a resistance:
%
Rh  = (Ls(N)/Cbm(N))^0.5;		%Match the cochlear impedance	
Vh  = 1;								%Pick a velocity at the helicotrema
Ph = Rh*Vh;
            
for i = N:-1:1						% Start from the apex and solve for P and vBM
    if i == N
       P(i) = Ph;    			% Boundary conditions at helicotrema
       V(i) = Vh + P(i)/Zp(i);
      
    else 
       P(i) = P(i+1) + V(i+1)*j*w*Ls(i+1);
       V(i) = V(i+1) + P(i)/Zp(i);
    end

    Vbm(i)= P(i)/Zp(i);
   
end
norm_Vbm = abs(Vbm)/abs(P(1));	% Scale for input pressure of 1 dyne/cm^2 inside stapes
norm_P  = abs(P)/abs(P(1));		% which corresponds to approximately 40 dB SPL
norm_Zp = abs(Zp);


theta_P=unwrap(angle(P))-angle(P(1));
theta_Vbm=unwrap(angle(Vbm)-angle(P(1)));
theta_Zp=unwrap(angle(Zp));

MagFig=figure;				%Start generating the magnitude figure (assume 1024x768 display
set(MagFig, 'Position',[5  200   500   500])	%Position it on the left side of screen
shg;

subplot(3,1,1)
semilogy(x, norm_Vbm);
axis([0 1.4 1e-10 1e-6]);
grid;
ylabel('BM Volume Velocity');
title('Magnitude (cgs units)');

subplot(3,1,2)
semilogy(x, norm_P);
axis([0 1.4 0.01 1]);
grid;
ylabel('Pressure Difference');

subplot(3,1,3)
semilogy(x, norm_Zp);
axis([0 1.4 1e6 1e10]);
grid;
xlabel('Distance from Base (cm)');
ylabel('CP Impedance');

PhaseFig=figure;				%Start generating the magnitude figure (assume 1024x768 display
set(PhaseFig, 'Position',[512  200   500   500])	%Position it on the left side of screen

subplot(3,1,1)
plot(x, theta_Vbm);
%axis([0 1.4 -5 5]);
grid;
ylabel('BM Velocity');
title('Phase (radians)');

subplot(3,1,2)
plot(x, theta_P);
%axis([0 1.4 -4 0 ]);
grid;
ylabel('Pressure Difference');

subplot(3,1,3)
plot(x, theta_Zp);
axis([0 1.4 -2 2]);
grid;
xlabel('Distance from Base (cm)');
ylabel('CP Impedance');
shg;