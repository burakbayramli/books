function demoJTree
%DEMOJTREE  Chest Clinic example using Junction Tree algorithm
load chestclinic; 

[jtpot jtsep infostruct]=jtree(pot); % setup the Junction Tree
jtpot=absorption(jtpot,jtsep,infostruct); % do full round of absorption

figure; drawNet(dag(pot),variable); title('Belief Net');
figure; drawJTree(jtpot,infostruct,variable); title('Junction Tree (separators not shown)');

% p(dys=yes)
jtpotnum = whichpot(jtpot,dys,1); % find a single JT potential that contains dys
margpot=sumpot(jtpot(jtpotnum),dys,0); % sum over everything but dys

disp(['p(dys=yes) ' num2str(margpot.table(yes)./sum(margpot.table))]);

fprintf(1,'\np(dys|smoker=yes) computed using setevpot:\n')
[jtpot jtsep]=jtassignpot(setevpot(pot,smoker,yes),infostruct);
jtpot=absorption(jtpot,jtsep,infostruct); % do full round of absorption
disptable(condpot(jtpot(jtpotnum),dys),variable);

disp('As a check, compute using simple summation:')
disptable(condpot(sumpot(setpot(multpots(pot),smoker,yes),dys,0)),variable);

fprintf(1,'\n\nCompute a normalisation constant p(dys=yes) with a variable set:\n')
fprintf(1,'We can compute the normalisation without changing the structure by using setevpot:\n')
[jtpot jtsep infostruct]=jtree(setevpot(pot,dys,yes)); % setup the Junction Tree
[jtpot jtsep logZ]=absorption(jtpot,jtsep,infostruct);% do full round of absorption
disp('All the cliques have the same normalisation, namely p(dys=yes):')
for i=1:length(jtpot); Z(i) = table(sumpot(jtpot(i),[],0)); end; Z

disp('Alternatively, we can compute by using setpot, but this changes the structure:')
disp('First create a new potential p(dys=yes,rest)')
newpot=setpot(pot,dys,yes);
disp('To use the JT we must have no missing variables, so squeeze the potentials')
[newpot newvars oldvars]=squeezepots(newpot);
[jtpot2 jtsep2 infostruct2]=jtree(newpot); % setup the Junction Tree
[jtpot2 jtsep2 logZ2]=absorption(jtpot2,jtsep2,infostruct2); % do full round of absorption
disp('After absorbing, transform back to original variables')
jtpot2=changevar(jtpot2,newvars,oldvars);
disp('All the cliques have the same normalisation, namely p(dys=yes):')
clear Z; for i=1:length(jtpot2); Z2(i) = table(sumpot(jtpot2(i),[],0)); end; Z2