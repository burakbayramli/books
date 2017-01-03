echo on; clc;

colordef none;

% Load simulated vowels data
load data; whos

clc;
% Generation of a random sequence from the given HMMs

[X,stateSeq] = genhmm(hmm1);
f1 = figure('name','Random sequence from HMM1'); plotseq(X,stateSeq);
f2 = figure('name','Random sequence from HMM1, 2D view'); plotseq2(X,stateSeq,hmm1);

pause; % Press a key...

[X,stateSeq] = genhmm(hmm2);
figure(f1);clf;
set(f1,'name','Random sequence from HMM2'); plotseq(X,stateSeq);
figure(f2); clf;
set(f2,'name','Random sequence from HMM2, 2D view'); plotseq2(X,stateSeq,hmm2);

pause; % Press a key...

[X,stateSeq] = genhmm(hmm3);
figure(f1);clf;
set(f1,'name','Random sequence from HMM3'); plotseq(X,stateSeq);
figure(f2); clf;
set(f2,'name','Random sequence from HMM3, 2D view'); plotseq2(X,stateSeq,hmm3);

pause; % Press a key...

[X,stateSeq] = genhmm(hmm4);
figure(f1);clf;
set(f1,'name','Random sequence from HMM4'); plotseq(X,stateSeq);
figure(f2); clf;
set(f2,'name','Random sequence from HMM4, 2D view'); plotseq2(X,stateSeq,hmm4);

pause; % Press a key...

[X,stateSeq] = genhmm(hmm5);
figure(f1);clf;
set(f1,'name','Random sequence from HMM5'); plotseq(X,stateSeq);
figure(f2); clf;
set(f2,'name','Random sequence from HMM5, 2D view'); plotseq2(X,stateSeq,hmm5);

pause; % Press a key...

[X,stateSeq] = genhmm(hmm6);
figure(f1);clf;
set(f1,'name','Random sequence from HMM6'); plotseq(X,stateSeq);
figure(f2); clf;
set(f2,'name','Random sequence from HMM6, 2D view'); plotseq2(X,stateSeq,hmm6);

pause; % Press a key...

close([f1 f2]);clc;

% Classification of sequences of observations

% e.g. logProb(1,3) = logfwd(X1,hmm3). Computing...

echo off;
for i=1:6,
  for j=1:6,
    stri = num2str(i);
    strj = num2str(j);
    eval( [ 'logProb(' , stri , ',' , strj , ')=logfwd(X' , stri , ',hmm' , strj , ');' ] ); 
  end;
end;
logProb

echo on;
pause; % Press a key...

clc;

% Alignment (best path) of the observation sequences

[STbest,bestProb] = logvit(X1,hmm1);
f1 = figure; compseq(X1,ST1,STbest);

pause; % Press a key...

[STbest,bestProb] = logvit(X2,hmm3);
clf; compseq(X2,ST2,STbest);

pause; % Press a key...
    
[STbest,bestProb] = logvit(X3,hmm5);
clf; compseq(X3,ST3,STbest);

pause; % Press a key...
    
[STbest,bestProb] = logvit(X4,hmm4);
clf; compseq(X4,ST4,STbest);

pause; % Press a key...
    
[STbest,bestProb] = logvit(X5,hmm6);
clf; compseq(X5,ST5,STbest);

pause; % Press a key...
    
[STbest,bestProb] = logvit(X6,hmm2);
clf; compseq(X6,ST6,STbest);

pause; % Press a key...
    

close(f1); clc;

% Classification of sequences according to the best path probability

% e.g. [STbest,bestProb(1,3)] = logvit(X1,hmm3). Computing...

echo off;
for i=1:6,
  for j=1:6,
    stri = num2str(i);
    strj = num2str(j);
    eval([ '[stbest,bestProb(' , stri , ',' , strj , ')]=logvit(X' , stri , ',hmm' , strj , ');' ]); 
  end;
end;
bestProb

echo on;
pause; % Press a key...

% Value of forward proba found earlier
logProb
    
% Difference between best path proba and forward proba

logProb - bestProb

echo off;
