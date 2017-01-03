function [wh,wo,tstcol]=doclassif(trndat,trnlab,tstdat,tstlab,U,I,E,EL,R)
% [H,O,TO] = doclassif(D,L,TD,TL,U,I,E,EL,R)  Try classification
%     D is rows of training data, with labels given by L.
%     TD is rows of test data, with oracle results given by TL.
%     Train a neural net on D with E epochs of I iterations, 
%     starting with learning rate = R, then halving it each epoch after
%     the first EL.  Use U hidden units.
%     Returns the neural net in H and O, and the test output classes in TO
% 2001-03-27 dpwe@ee.columbia.edu

if nargin < 5
  U = 5;
end
if nargin < 6
  I = 29;
end
if nargin < 7
  E = 4;
end
if nargin < 8
  EL = 1;
end
if nargin < 9
  R = 0.08;
end

% Make actual target matrix
[ntrn,ndim] = size(trndat);
trg = sparse([1:ntrn],trnlab,1);

% Calculate norms
nrm = [mean(trndat);std(trndat)];

% Do training
wh = U;
wo = [];

for ep = 1:E
   if ep > EL
     R = R / 2;
   end
   [wh,wo] = nntrain(trndat,nrm,trg,R,I,wh,wo,16,10);

   % Do testing

   % Measure accuracy on train data to start
   trnop = nnfwd(trndat,nrm,wh,wo);
   trncol = findmaxcol(trnop);
   tfac = mean(trncol == trnlab);

   % Now do real test on test data

   % Calculate net output for test data (using norms from trn data)
   tstop = nnfwd(tstdat,nrm,wh,wo);

   plot(tstop);

   % Find 'winning' output for each pattern
   tstcol = findmaxcol(tstop);

   % Calculate proportion of matches to oracle labels
   efac = mean(tstcol == tstlab);
   disp(['Ep ',num2str(ep), ' lr=', num2str(R), ' frame accuracy on trn = ', num2str(100*tfac),'% test = ', num2str(100*efac),'%']);

end
