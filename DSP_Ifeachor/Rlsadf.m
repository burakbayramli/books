function RLSadf								
%	program to illustrate adaptive filtering using		
%	the RLS algorithm 		

% X  delayed input signal
% Y  measured signal
% W  coefficient vector				
% E  enhanced signal

N = 30;  	% filter length 
M = 1;     	% stages of delay 
SF = 2048;	% 12-bit ADC scaling 
p0 = 0.05;
w0 = 100;
gamma = 0.98;
RemoveMean = 0;	% 1 to remove the mean, 0 otherwise
W = w0*ones(N,1);	% adaptive filter weights
X = zeros(N,1);  
delay = zeros(1,M+1);
P = p0*diag(ones(1,N),0);
if w0==0
   sf = SF;	% scaling factor for display
else
   sf = SF/N/w0;
end

in = fopen('ADF.dat','r');	%read input data from specified data file
Y = fscanf(in,'%g',inf)/SF;
fclose(in);

if RemoveMean			% remove the mean from the data if required
   Y = Y - sum(Y)/length(Y);
end

for i=1:length(Y)
	if M>0
		delay(2:M+1) = delay(1:M);	% shift input data in delay registers
   end
   delay(1) = Y(i);
	X(2:N) = X(1:N-1);			% update buffer
   X(1) = delay(M+1);
   
	E(i) = Y(i) - X'*W;			% the enhanced signal
   G = P*X/(gamma + X'*P*X);	
   P = (P - G*X'*P)/gamma;
	W = W + G*E(i);				% update the weights
end
subplot(2,1,1),plot(1:length(Y),Y*SF); title('Input Signal');
subplot(2,1,2),plot(1:length(E),E*sf); title('Enhanced Signal');










