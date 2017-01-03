function solveSpeed
% solveSpeed  Measure elapsed time and flop rate for solving Ax=b
%
% Synopsis:  solveSpeed
%
% Input:  none
%
% Output:  Print out of elapsed time, flop rate and memory use
%          as function of matrix dimension.  Flop rate vs. matrix
%          dimension is plotted
if ~isstudent
  n = [8 16 32 64 128 256 512 1024];   %  solve this range of n by n systems
else
  n = [8 16 32 64 128];                %  student edition is limited to n=128
end

fprintf('      n          et     Mflop rate   MBytes for A\n');
for i=1:length(n);
  A = rand(n(i),n(i));  b = rand(n(i),1);
  flops(0);
  tic;
  x = A\b;
  elapsedTime(i) = toc;
  frate(i) = flops/elapsedTime(i)/1e6;
  fprintf('  %6d   %10.3f   %8.3f   %8.2f\n',...
           n(i),elapsedTime(i),frate(i),8*n(i)*n(i)/1e6);    
end

loglog(n,frate,'o');
xlabel('Matrix dimension');   ylabel('Megaflop rate');
