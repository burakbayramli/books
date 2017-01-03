function demoCondindepEmp
%demoCondindepEmp demo of empirical conditional independence
figure
[x y z1 z2]=assign(1:4);
nstates([x y z1 z2])=[3 2 4 2];
Nsamples=20; % number of datapoints
NumExp=100; %number of experiments
pot(x).variables=[x z1];
pot(y).variables=[y z1];
pot(z1).variables=z1;
pot(z2).variables=[z2 x];
opts.Uxgz=1; opts.Uygz=1; opts.Uz=1; opts.Uxyz=1; % Dirichlet hyperparameters

draw_layout(dag(pot),{'x','y','z1','z2'}); drawnow
fprintf(1,'Test if I([%s],[%s]|[%s]) based on %d datapoints (%d experiments):\n',...
   'x','y','z1 z2',Nsamples,NumExp)
for typ=1:2
    for ex=1:NumExp % number of experiments
        pot(x).table=condp(rand(nstates([x z1])));
        pot(y).table=condp(rand(nstates([y z1])));
        pot(z1).table=condp(rand(nstates(z1),1));
        pot(z2).table=condp(rand(nstates([z2 x])));
        indeppot=multpots(pot([x y z1 z2]));
        
        deppot.variables=[x y z1 z2]; deppot.table=normp(rand(nstates)); % a random distribution
        switch typ
            case 1; truepot=indeppot;
            case 2; truepot=deppot;
        end
        data=potsample(truepot,Nsamples); z=[z1 z2];
        [dum logBayesFactor(ex) expMI(ex) miss(ex)]=condindepEmp(data(x,:),data(y,:),data(z,:),...
            nstates(x),nstates(y),nstates(z),0,opts);
        trueMI(ex)=condMI(truepot,x,y,[z1 z2]);
    end
    res{typ}=[logBayesFactor' expMI' trueMI' miss'];
end
fprintf('%1.2f: Fraction Bayes test is correct\n', mean(mean([res{1}(:,1)>0 res{2}(:,1)<0])));
mb=median([res{1}(:,1)' res{2}(:,1)']);
fprintf('%1.2f: Fraction Bayes test is correct (based on empirical median threshold)\n',...
    mean(mean([res{1}(:,1)>mb res{2}(:,1)<mb])));
md=median([res{1}(:,2)' res{2}(:,2)']);
fprintf('%1.2f: Fraction MI test (based on empirical MI median threshold) is correct\n',...
    mean(mean([res{1}(:,2)<md res{2}(:,2)>md])));
dof=(prod(nstates(x))-1)*(prod(nstates(y))-1)*prod(nstates([z1 z1]));
chi2 = mean([2*Nsamples*res{1}(:,2)'<chi2test(dof,0.05) 2*Nsamples*res{2}(:,2)'>chi2test(dof,0.05)]);
fprintf('%1.2f : Fraction of times the chi square test is correct\n',chi2);