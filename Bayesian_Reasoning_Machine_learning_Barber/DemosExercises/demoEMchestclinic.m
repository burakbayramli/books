function demoEMchestclinic
%DEMOEMCHESTCLINIC demo of EM learning of chest clinic bayes net table
load chestclinic

x=ancestralsample(pot,50); % draw some samples from the correct distribution
[V N]=size(x);
% now make some missing data:
for n=1:N
	missmask(:,n) = real(rand(V,1)>0.2); % 20 percent missing data
	r=randperm(V);
	missmask(r(1),n)=1; % ensure at least one non-missing value
end
xmiss=x.*replace(missmask,0,nan);
imagesc(xmiss,[0 2]); colormap hot; title('data : black is missing')

empot=pot; % setup the BN to be learned to have the correct structure
PotentialsToUpdate=[asia smoker tub lcancer bronch xray dys]; % don't update tlc since this is a logic gate
[vars numstates]=potvariables(pot);
for i=PotentialsToUpdate;
	empot(i).table=myzeros(numstates(pot(i).variables)); % clear the potential tables
end
pars.tol=0.001; pars.maxiterations=10;pars.plotprogress=1;
pars.PotentialsToUpdate=PotentialsToUpdate;
empot=EMbeliefnet(empot,xmiss,pars); % do EM
for i=PotentialsToUpdate;
	str1=disptable(pot(i));   str2=disptable(empot(i));
	disp(['table ',num2str(i),' true and learned:']);
	disp([char(str1) char(str2)]); disp(' ');
end