% PURPOSE: demo of unsort() 
%          returns sorted vectors or matrices
%          in unsorted (orginal) form 
% 
%---------------------------------------------------
% USAGE: unsort_d
%---------------------------------------------------



n = 10; k = 4;
tt = randn(n,1);       % generate a vector of random numbers
[tts tti] = sort(tt);  % sort them

ttu = unsort(tts,tti); % unsort them
fprintf(1,'These two vectors should be the same \n');
[tt ttu]


tt = randn(n,k);            % generate a matrix
[junk tti] = sort(tt(:,1)); % sort by first column
tts = tt(tti,:);

ttu = unsort(tts,tti); % unsort them

fprintf(1,'These two matrices should be the same \n');
in.fmt = strvcat('%8.2f');
mprint(tt,in);
mprint(tt,in);
