clear all

% select image to process (from shape.jpg, Cameraman.bmp, Barbara.png)

%I = imread('shape.jpg');
%writefile='shape';
%fmt='gif';
%n=length(I); lbd =  0.045;

%I   =  imread('Cameraman.bmp');
%writefile='Cameraman';
%fmt='gif';
%n=length(I); lbd =  0.045;

I   =  imread('Barbara.png');
writefile='Barbara';
fmt='gif';
n=length(I); lbd =  0.045;

% number of rounds of testing
nTestRounds=1;

% will we write the results?
writeImages = 1;

% decide which algorithms to test
doChambolle=1;
doGPCL=1;
doGPLS=1;
doGPBB_M=1;
doGPBB_M2=0;
doGPBB_M3=1;
doGPBB_Mf=0;
doGPBB_M2f=0;
doGPBB_M3f=1;
doGPBB_NM=1;
doGPBB_Nalt=0;
doGPBB_N2alt=0;
doGPBB_N3alt=0;
doGPABB=1;
doGPBBsafe=1;
doSQPBB_NM=0;
doSQPBB_NM2=0;
doSQPBB_M=1;
doSQPBB_M2=0;
doPDHG=1;
doCGM=1;


NIT = 50000;    %max number of iterations
j=0;

% set verbosity: 0=no output, 1=print stuff every iteration
verbose=0;

if writeImages
  writefileCleanName=[writefile '-Clean.' fmt];
  imwrite(I,writefileCleanName,fmt);
end

% generate a noisy image to be used on the k=1 round of testing for all
% values of GapTol
f_first = double(imnoise(I,'gaussian',0,0.01)); 
if writeImages
  writefileNoisyName=[writefile '-Noisy.' fmt];
  imwrite(f_first,writefileNoisyName,fmt);
end

% choose sequence of gap tolerances to solve for
Gaps = [2 3 4 6];
for GapTolexp = Gaps
  GapTol = 10.^(-GapTolexp);
  slot=0;
  for k=1:nTestRounds
    % if k=1, use the standard noisy image, otherwise generate a new one
    if k==1
      f=f_first;
    else
      f = double(imnoise(I,'gaussian',0,0.01)); 
    end
    
    % upper bound on the next loop is the number of replications
    fprintf(' Solving for Convergence Tolerance %7.2e, Round %2d\n', ...
	GapTol, k);
    
    % Chambolle's method
    if doChambolle
      alpha = 0.248;	
      slot=slot+1;
      algName{slot}='Chambolle';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] ...
	  = TV_Chambolle(zeros(n),zeros(n),f,lbd,alpha,NIT,GapTol,verbose);
      Time_Cost(slot,k) = TimeSeq(end);
      if GapTolexp==Gaps(end)
	TimeChambolle=TimeSeq;
	DgapChambolle=Dgap;
      end
    end
    
    % GP with constant step length
    if doGPCL
      clear u w1 w2 Energy Dgap TimeSeq
      alpha = 0.248;
      slot=slot+1;
      algName{slot}='GPCL';
      [u, w1,w2,Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCL(zeros(n),zeros(n),f,lbd,alpha,NIT,GapTol,verbose);  
      Time_Cost(slot,k) = TimeSeq(end);
    end
      
    % GPLS
    if doGPLS
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      algName{slot}='GPLS';
      [u, w1,w2, Energy,Dgap,TimeSeq,  iterationCount(slot,k)] = ...
	  TV_GPLS(zeros(n),zeros(n),f,lbd,NIT,GapTol,verbose);
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % monotone GPBB (cycle length 1)
    if doGPBB_M
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=1.0;
      cycleLength=1;
      algName{slot}='GPBB-M';
      [u, w1,w2, Energy,Dgap,TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose);  
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % monotone cyclic GPBB (cycle length 2)
    if doGPBB_M2
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=1.0;
      cycleLength=2;
      algName{slot}='GPBB-M (2)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k)=TimeSeq(end);
    end
    
    % monotone cyclic GPBB (cycle length 3)
    if doGPBB_M3
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=1.0;
      cycleLength=3;
      algName{slot}='GPBB-M(3)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % monotone GPBB (cycle length 1) with fudge factor 0.5 in step length
    if doGPBB_Mf
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=0.5;
      cycleLength=1;
      algName{slot}='GPBB-M (fudge)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose);  
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % monotone cyclic GPBB (cycle length 2) with fudge factor 0.5 in step
    % length
    if doGPBB_M2f
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=0.5;
      cycleLength=2;
      algName{slot}='GPBB-M(2) fudge';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k)=TimeSeq(end);
    end
    
    % monotone cyclic GPBB (cycle length 3) with fudge factor of 0.5 
    if doGPBB_M3f
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=0.5;
      cycleLength=3;
      algName{slot}='GPBB-M(3) fudge';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % if requested, write out the cleaned up image
    if k==1 & writeImages
      writefileDenoisedName = [writefile '-Denoised-' int2str(GapTolexp) '.' ...
	    fmt];
      imwrite(u,writefileDenoisedName,fmt);
    end

    % nonmonotone GPBB (cycle length 1)
    if doGPBB_NM
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=0;
      BBformula=1;
      fudge=1.0;
      cycleLength=1;
      algName{slot}='GPBB-NM';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose);   
      Time_Cost(slot,k) = TimeSeq(end);
      % save for plotting
      if GapTolexp==Gaps(end)
	TimeGPBBNM=TimeSeq;
	DgapGPBBNM=Dgap;
      end
    end
    
    
    % nonmonotone GPBB (cycle length 1) with alternative BB formula
    if doGPBB_Nalt
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=0;
      BBformula=2;
      fudge=1.0;
      cycleLength=1;
      algName{slot}='GPBB-NM (alt)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose);   
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % nonmonotone cyclic GPBB (cycle length 2) with alternative BB formula
    if doGPBB_N2alt
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=0;
      BBformula=1;
      fudge=1.0;
      cycleLength=2;
      algName{slot}='GPCBB-NM(2)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % nonmonotone cyclic GPBB (cycle length 3) 
    if doGPBB_N3alt
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=0;
      BBformula=1;
      fudge=1.0;
      cycleLength=3;
      algName{slot}='GPCBB-NM(3)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPCBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end

    % GPBB with alternating step length formulae
    if doGPABB
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      algName{slot}='GPABB';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPABB(zeros(n),zeros(n),f,lbd,NIT,GapTol,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % safeguarded BB with nDecr=5;
    if doGPBBsafe
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      algName{slot}='GPBBsafe';
      BBformula=1;
      fudge=1.0;
      nDecr=5;
      [u, w1, w2, Energy, Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_GPBBsafe(zeros(n),zeros(n),f,lbd,nDecr,BBformula,fudge,NIT,GapTol,verbose);
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % SQP modification of BB - nonmonotone
    if doSQPBB_NM
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=0;
      BBformula=1;
      fudge=1.0;
      cycleLength=1;
      algName{slot}='SQPBB-NM';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_SQPBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % SQP modification of cyclic BB (cycle length 2) - nonmonotone 
    if doSQPBB_NM2
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=0;
      BBformula=1;
      fudge=1.0;
      cycleLength=2;
      algName{slot}='SQPBB-NM(2)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_SQPBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % SQP modification of BB - monotone
    if doSQPBB_M
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=1.0;
      cycleLength=1;
      algName{slot}='SQPBB-M';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_SQPBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % SQP modification of cyclic BB (cycle length 2) - monotone 
    if doSQPBB_M2
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      monotone=1;
      BBformula=1;
      fudge=1.0;
      cycleLength=2;
      algName{slot}='SQPBB-M(2)';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_SQPBB(zeros(n),zeros(n),f,lbd,NIT,GapTol,...
	  monotone,BBformula,fudge,cycleLength,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % Chan and Zhu's PDHG method
    if doPDHG
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      algName{slot}='PDHG';
      [u, w1,w2, Energy,Dgap, TimeSeq, iterationCount(slot,k)] = ...
	  TV_PDHG(zeros(n),zeros(n),f,lbd,NIT,GapTol,verbose); 
      Time_Cost(slot,k) = TimeSeq(end);
    end
    
    % Chan-Golub-Mulet algorithm
    if doCGM
      clear u w1 w2 Energy Dgap TimeSeq
      slot=slot+1;
      beta=100;
      u=f;
      algName{slot}='CGM';
      [u, w, Primal_CGM, Dual_CGM, Dgap, TimeSeq, iterationCount(slot,k),beta] = ...
	  CGM2d(u, f, lbd, beta, NIT,GapTol, verbose);
      Time_Cost(slot,k)=TimeSeq(end);
      if GapTolexp==Gaps(end)
	TimeCGM=TimeSeq;
	DgapCGM=Dgap;
      end
    end
    
  end
  
  % assemble statistics for reporting in the RT matrix
  if k>1
    TM = (sum(Time_Cost')/k)';
    IC = (sum(iterationCount')/k)';
  elseif k==1
    TM = Time_Cost;
    IC = iterationCount;
  end
  j = j+1;
  RT(:,j)=IC;
  j = j+1;
  RT(:,j)=TM;

end


fprintf('\n\n');
fprintf('%20s \t','     Tolerances');
for GapTolexp = Gaps
  GapTol=10^(-GapTolexp);
  fprintf('   %7.2e      ', GapTol);
end
fprintf('\n\n');

for i=1:slot;
  fprintf('%20s \t',algName{i});
  for l=1:2:j
    fprintf(' & %5d & %6.2f', RT(i,l), RT(i,l+1));
  end
  fprintf(' \\\\')
  fprintf('\n')
end
    
% truncate the arrays for plotting
TopTime=2*max([TimeGPBBNM(end); TimeCGM(end)]);
TimeChambolleTruncated=TimeChambolle(TimeChambolle<TopTime);
DgapChambolleTruncated= DgapChambolle(1:length(TimeChambolleTruncated));

figure(1)
semilogy(TimeGPBBNM,DgapGPBBNM,'b-','LineWidth',2);
hold on
semilogy(TimeChambolleTruncated,DgapChambolleTruncated,'r--','LineWidth',2);
semilogy(TimeCGM,DgapCGM,'k-.','LineWidth',2);
legend('BB-NM','Chambolle','CGM');
xlabel('CPU Time'); ylabel('Relative Duality Gap');

% print the figure
figFileName = [writefile '-fig.eps'];
print('-depsc',figFileName);

% save RT in case we need to generate the performance table again
% later. Also the plot data
RT(:,1:2:end)=round(RT(:,1:2:end));
RTfile = [writefile '-results'];
save(RTfile,'algName','RT','TimeGPBBNM','DgapGPBBNM',...
    'TimeChambolle','DgapChambolle','TimeCGM','DgapCGM');
