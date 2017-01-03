function EvalNStageDecimator(Fs,fp,dp,ds,R)

format long;
a1 = 0.005309; a2 = 0.07114; a3 = -0.4761; a4 = -0.00266;
a5 = -0.5941; a6 = -0.4278; a7 = 11.01217; a8 = 0.51244;
dp = 10^(dp/20.0)-1; ds = 10^(-ds/20.0); 
Ftemp = Fs; 

dp = dp/length(R);
MPS = 0; TSR = 0;
fprintf('stage\tfactor\tFi\tfp\tfs\tdp\tds\tN\n');
for i=1:length(R)	% n=size(R,1) possible k-stages decimators
   F = Ftemp/R(i);
   fs = F - Fs/2/prod(R);
   df = (fs - fp)/Ftemp;
   Ftemp = F;
   N = round((log10(ds)*(a1*log10(dp)^2+a2*log10(dp)+a3)+...
       a4*log10(dp)^2+a5*log10(dp)+a6)/df-df*(a7+a8*(log10(dp)-log10(ds)))+1);
   fprintf('%d\t%d\t%d\t%d\t%d\t%-7.4f\t%-7.4f\t%d\n',i,R(i),F,fp,fs,dp,ds,N);
   MPS = MPS + N*F;
   TSR = TSR + N;
end
fprintf('MPS = %d,  TSR = %d\n\n',MPS,TSR);
format;
