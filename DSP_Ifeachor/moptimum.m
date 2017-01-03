function moptimum

%Program moptimum is for designing I-stage optimum decimator 		
%or interpolator (I=1,2,3 or 4). The program computes the decimation	
%factors, filter characteristics, and decimator efficiencies	
								
%The following parameters must be provided by the user:		
%	Fs	-	input sampling frequency			
%	M	-	overall decimation factor			
%	fp	-	passband edge frequency				
%	dp	-	overall passband deviation in +ve dB		
%	ds	-	overall stopband deviation in +ve dB

clear all;
Fs = 96000;		% sampling frequency in Hz
fp = 450;		% passband edge frequency in Hz
dp = 0.0864;	% overall passband deviation in +ve dB
ds = 60;			% overall stopband deviation in +ve dB
M = 96;			% overall decimation factor

EvalNStageDecimator(Fs,fp,dp,ds,M); % evaluate single stage decimator
for i=2:4	% evaluate all possible 2-, 3- and 4-stage decimators
   R = GetFactors(M,i);
	for j=1:size(R,1);
   	EvalNStageDecimator(Fs,fp,dp,ds,R(j,:));
	end
end