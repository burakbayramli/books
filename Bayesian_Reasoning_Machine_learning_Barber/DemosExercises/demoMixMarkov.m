function demoMixMarkov
%DEMOMIXMARKOV demo of training a mixture of Markov models
load sequences
for n=1:20 % convert the characters into (arbitrary) numerical values (from 1 to 4)
	v{n}(findstr(sequences{n},'A'))=1;
	v{n}(findstr(sequences{n},'C'))=2;
	v{n}(findstr(sequences{n},'G'))=3;
	v{n}(findstr(sequences{n},'T'))=4;
end
opts.maxit=50; opts.plotprogress=1;
[ph,pv1gh,pvgvh,loglikelihood,phgv]=mixMarkov(v,4,2,opts);
% For each sequence find the highest posterior hidden class:
c=ones(1,20);
class2sequences=[]; class1sequences=[];
for n=1:20
	if phgv{n}(1)>0.5; c(n)=2;
		class2sequences=vertcat(class2sequences,sequences{n});
	else
		class1sequences=vertcat(class1sequences,sequences{n});
	end
end
fprintf(1,' Sequences in group 1:\n'); disp(class1sequences)
fprintf(1,'\n Sequences in group 2:\n'); disp(class2sequences)
fprintf(1,'\n labels of each sequence:\n'); disp(c);
fprintf(1,'\n log likelihood for this run = %g\n',loglikelihood);