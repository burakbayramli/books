function SQRTADF							
%	program to illustrate adaptive filtering using		
%	the square root RLS algorithm 		

% X   delayed input data vector				
% Y   measured signal				
% W   coefficient vector				
% E   enhanced signal

N = 30;  	% filter length 
M = 1;     	% delay 
npt = N*(N+1)/2;
SF = 2048;	% 12-bit ADC scaling 
p0 = 0.05;
w0 = 1;
gamma = 0.98;
RemoveMean = 0;	% 1 - remove the mean from the data, 0 - otherwise

delay = zeros(1,M);
W = w0*ones(N,1);
X = zeros(N,1);
S = zeros(1,npt);
S(1)=p0;
for i=1:N-1
	ik=(i*(i+1)-2)/2+1;
	S(ik)=p0;
end

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

	W = sqrtflt(W,X,E(i),S,gamma,N);
end
subplot(2,1,1),plot(1:length(Y),Y*SF); title('Input Signal');
subplot(2,1,2),plot(1:length(E),E*sf); title('Enhanced Signal');