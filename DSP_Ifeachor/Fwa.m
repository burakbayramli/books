function FWA

% Program for a limited finite wordlength analysis on user-specified IIR 		 
% filters in cascade, parallel or direct form	 
% Analysis performed are for:					 
% (1) ADC quantization noise 					 
% (2) Coefficient quantization noise				 
% (3) Product Roundoff noise					 
% (4) Scaling							 
%									 
% For details see:						 
% Digital Signal Processing: A Practical Approach			 
% E C Ifeachor and B W Jervis, Addison Wesley, 1993 		 

clear all;

isel=3;	%0	for ADC noise analysis
			%1	for coefficient quantization error analysis
         %2	for product roundoff noise analysis
         %3	for scale factors computations
FM = 1 ;		%filter multiplier (set to 1 if there is none)
b1 = [1 0.0339 1];  %numerators of the second order sections
b2 = [1 -0.7563 1];
b3 = [1 0.5331 1];
b4 = [1 -1.1489 1];
a1 = [1 -0.1743 0.9662];  %denominators of the second order sections
a2 = [1 -0.5588 0.9675];
a3 = [1 -0.2711 0.9028];
a4 = [1 -0.4441 0.9045];
b = [b1; b2; b3; b4]; a = [a1; a2; a3; a4];
%above coeff are for example 7.16 (quantization error analysis)

b=[1.2916 -0.08407 0];a=[1 -0.131 0.3355];
b=[7.5268 0 0];a=[1 -0.049 0];
%above coeff are for example 7.19 (scaling factor)
b1 = [1 0.2189 1];  %numerators of the second order sections
b2 = [1 -0.5291 1];
b3 = [1 1.5947 1];
a1 = [1 -0.0127 0.9443];  %denominators of the second order sections
a2 = [1 -0.1731 0.7252];
a3 = [1 -0.6152 0.2581];
b = [b1; b2; b3]; a = [a1; a2; a3];
%above coeff are for example 7.18 (scaling factor)
b1=[1 -2 1]; b2=[1 -0.707 1];
a1=[1 0.777 0.3434]; a2=[1 0.01877 0.801];
b=[b1 ; b2]; a = [a1 ; a2];
%above coeff are in user guide: example 7.21
b=[0.1436 0.2872 0.1436];a=[1 -1.8353 0.9747];
%above coeff are for example 7.20 (scaling factor)
b1 = [1 0.2189 1];  %numerators of the second order sections
b2 = [1 -0.5291 1];
b3 = [1 1.5947 1];
a1 = [1 -0.0127 0.9443];  %denominators of the second order sections
a2 = [1 -0.1731 0.7252];
a3 = [1 -0.6152 0.2581];
b = [b1; b2; b3]; a = [a1; a2; a3];

%b=[1 2 1];a=[1 -1.0581359 0.338544];
isel = 3;
if isel==0
   n=300;	% length of impulse response
   FM = 1;
	ADCNoise(b,a,n,FM);
elseif 	isel==1
	Fs = 1000; %sampling frequency in Hz
	maxbits = 8; %maximum wordlength 
	ftype = 2; %filter type: 0 - LPF, 1 - HPF, 2 - BPF, 3 - BSF
	f = [0 100 120 300 320 500]; %bandedge frequencies(Hz) (4 or 6) of filters
	CoeffQuantizeErr(b,a,maxbits,ftype,f,Fs); %coefficient quantization error analysis
	Stability(b,a,maxbits);
elseif isel==2
	iopt= 2; 	% scaling type: 0 - L1, 1 - L2, 2 - Loo, 3 - no scaling
   n = 300;		% time points(300) for L1 and L2, frequency points(256) for Loo
	nbits = 30;	% word length of the coefficients
	RoundoffNoise(b,a,n,nbits,iopt); 
else  
   structure = 0 ; % 0 - canonic, 1 - direct
   nstep = 256;
   size = 300;
   ScaleFactor(b,a,nstep,size,structure); 
end