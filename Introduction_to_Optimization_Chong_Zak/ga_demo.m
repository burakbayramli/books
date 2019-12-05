function [x,N]=genetic_alg(funcname,xnew,options);
%Genetic algorithm demo
% function [x,N]=genetic_alg(funcname,xnew,options);
%
%Options:
%print = options(1);
%epsilon_x = options(2);
%epsilon_g = options(3);
%selection = options(5);
%max_iter=options(14);
%alpha = options(18);
%
%Selection:
% options(5) = 0 for roulette wheel, 1 for tournament

if nargin ~= 3
  options = [];
  if nargin ~= 2
    disp('Wrong number of arguments.');
    return;
  end
end

if length(options) >= 14 
  if options(14)==0
    options(14)=1000*length(xnew);
  end
else
  options(14)=1000*length(xnew);
end
if length(options) < 18
  options(18)=1.5; %optional step size
end

%clc;
format compact;
format short e;

options = foptions(options);
print = options(1);
epsilon_x = options(2);
epsilon_g = options(3);
selection = options(5);
max_iter=options(14);
alpha = options(18);

if funcname == 'f_r',
  ros_cnt
elseif funcname == 'f_p',
  pks_cnt;
end %if

if length(xnew) == 2
  plot(xnew(1),xnew(2),'o')
  text(xnew(1),xnew(2),'Start Point')
  xlower = [-2;-1];
  xupper = [2;3];
end

xbestcurr = xnew;
h_best=feval(funcname,xnew);

num_p = 10;
num_c = 5;
num_m = 5;

%initialize population
for p = 1:num_p,
  chrom=xnew+alpha*2*[rand(1)-0.5; rand(1)-0.5];
  for i=1:length(xnew), %project
    chrom(i) = max(chrom(i),xlower(i));
    chrom(i) = min(chrom(i),xupper(i));
  end %for
  popul(:,p)=chrom;
end %for

for k = 1:max_iter,

  %evaluation
  h_new=h_best;
  h_worst=h_best;
  avghandicap=0;
  for p = 1:num_p,
    %plot(popul(1,p),popul(2,p));
    handicap(p)=feval(funcname,popul(:,p));
    avghandicap = avghandicap + handicap(p);
    if handicap(p) > h_worst,
      h_worst=handicap(p);
    end %if
    if handicap(p) < h_new,
      xnew=popul(:,p);
      h_new=handicap(p);
    end %if
  end %for
  avghandicap=avghandicap/num_p;

  %if abs(h_best-avghandicap) <= epsilon_g*abs(h_best),
    %disp('Terminating: relative function difference less than');
    %disp(epsilon_g);
    %k=k-1;
      %break;
  %end %if

  if h_new < h_best,
    xbestold = xbestcurr;
    xbestcurr = xnew;
    h_best = h_new;
    pltpts(xbestcurr,xbestold);
    %check for stopping
    if norm(xbestcurr-xbestold) <= epsilon_x*norm(xbestold)
      disp('Terminating: Norm of difference between iterates less than');
      disp(epsilon_x);
      break;
    end %if
  else
    %disp('Warning: no improvement');
  end

  if print,
    disp('Iteration number k =')
    disp(k);  %print iteration index k
    disp('alpha =');
    disp(alpha);  %print alpha
    disp('New point =');
    disp(xnew'); %print new point
  end %if

  %selection
  if selection == 0,  %roulette wheel

  %construct CMF
  sumhandicap=sum((h_worst-handicap).^9);
  cmf(1) = ((h_worst-handicap(1))^9)/sumhandicap;
  for p=2:num_p,
    cmf(p) = cmf(p-1)+((h_worst-handicap(p))^9)/sumhandicap;
  end %for

  for p = 1:num_p,
    selectrand = rand(1);
    q = 1;
    while (selectrand > cmf(q)),
      q=q+1;
    end %while
    chrom=popul(:,q);
    for i=1:length(xnew), %project
      chrom(i) = max(chrom(i),xlower(i));
      chrom(i) = min(chrom(i),xupper(i));
    end %for
    matpool(:,p)=chrom;
    %plot(matpool(1,p),matpool(2,p));
  end %for

  else %tournament selection

  for p = 1:num_p,
	fighter1 = ceil(num_p*rand(1));
	fighter2 = ceil(num_p*rand(1));
    if handicap(fighter1) < handicap(fighter2),
      chrom=popul(:,fighter1);
    else
      chrom=popul(:,fighter2);
    end
    for i=1:length(xnew), %project
      chrom(i) = max(chrom(i),xlower(i));
      chrom(i) = min(chrom(i),xupper(i));
    end %for
    matpool(:,p)=chrom;
    %plot(matpool(1,p),matpool(2,p));
  end %for

  end %if

  %crossover
  for p = 1:num_c,
    mother=ceil(num_p*rand(1));
    father=ceil(num_p*rand(1));
    avgpt=(matpool(:,father)+matpool(:,mother))/2.0;
    chrom = avgpt+alpha*2*[rand(1)-0.5; rand(1)-0.5];
    for i=1:length(xnew), %project
      chrom(i) = max(chrom(i),xlower(i));
      chrom(i) = min(chrom(i),xupper(i));
    end %for
    matpool(:,father) = chrom;
    chrom = avgpt+alpha*2*[rand(1)-0.5; rand(1)-0.5];
    for i=1:length(xnew), %project
      chrom(i) = max(chrom(i),xlower(i));
      chrom(i) = min(chrom(i),xupper(i));
    end %for
    matpool(:,mother) = chrom;
  end %for

  %mutation
  for p = 1:num_m,
    mutant=ceil(num_p*rand(1));
    chrom = matpool(:,mutant)+alpha*2*[rand(1)-0.5; rand(1)-0.5];
    for i=1:length(xnew), %project
      chrom(i) = max(chrom(i),xlower(i));
      chrom(i) = min(chrom(i),xupper(i));
    end %for 
    matpool(:,mutant)=chrom;
  end %for
  
  popul = matpool;

  if k == max_iter
    disp('Terminating with maximum number of iterations');
  end %if
end %for

if nargout >= 1
  x=xnew;
  if nargout == 2
    N=k;
  end 
else
  disp('Final point ='); 
  disp(xbestcurr'); 
  disp('Number of iterations ='); 
  disp(k); 
end %if
