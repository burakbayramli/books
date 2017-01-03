function LMSADF
								
%program to illustrate adaptive filtering using	the LMS algorithms	

% X   delayed input data vector				
% Y   measured signal				
% W   coefficient vector				
% E   enhanced signal

N=30;   % filter length 
M=0;    % delay 
w0=1; 	% initial value for adaptive filter coefficients 
SF=2048;	% factor for reducing the data samples - 11 bit ADC assumed 
mu=0.04;
X = zeros(N,1); 
delay = zeros(1,M+1);
W = w0*ones(N,1);
in = fopen('ADF.dat','r');	%read input data from specified data file
Y = fscanf(in,'%g',inf)/SF;
fclose(in);
if w0==0
   sf = SF;	% scaling factor for display
else
   sf = SF/N/w0;
end	
for i=1:length(Y)
	if M>0
		delay(2:M+1) = delay(1:M);	% shift data for delay
   end
   delay(1) = Y(i);
	X(2:N) = X(1:N-1);	% update buffer
	X(1) = delay(M+1);
   E(i) = Y(i)-W'*X;		% the enhanced signal
   W = W + 2*mu*E(i)*X;	% update the weights
end
subplot(2,1,1),plot(1:length(Y),Y*SF); title('Input Signal');
subplot(2,1,2),plot(1:length(E),E*sf); title('Enhanced Signal');