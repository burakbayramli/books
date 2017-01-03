function demogrouppot
%DEMOGROUPPOT:
% This demo shows how to group variables together to form potentials on
% these new variables. This can be useful, for example, to convert a
% dynamic bayes network to a hidden markov model.
% In the example below, there are four variables, 1,2,3,4 in the original
% set of potentials. Group variable 1 is formed from variables 1,2, and
% group variable 2 from variables 3,4. We then form new group potentials on
% these group variables. The code shows how also one can access the group
% state associated to an ungrouped state, and shows that this indexing is
% consistent.

pot(1).variables=[1 2];
pot(1).table=rand(2,3);

pot(2).variables=[3 4];
pot(2).table=rand(3,4);

disp('Define some potentials on associated (ungrouped) variables:')
for i=1:2;
    fprintf(1,'potential %i:\n',i)
    pot(i)
end

pot12=multpots(pot([1 2]));

disp('For variables [1 2 3 4] in state [1 2 2 1], the joint potential has value:')
evalpot(pot12,[1 2 3 4], [1 2 2 1])


disp('Now form group variable1 from variables 1,2, and group variable2 from variables 3,4.')
group(1).variables=[1 2]; 
group(2).variables=[3 4]; 

[gpot(1) group]=grouppot(pot(1),group);
[gpot(2) group]=grouppot(pot(2),group);

disp('Define the corresponding potentials on the grouped variables:')
for i=1:2;
    fprintf(1,'potential %i:\n',i)
    gpot(i)
end

disp('For variables [1 2 3 4] in state [1 2 2 1], this correspond to group variable 1 in state 3 and group variable 2 in state 2');
disp('The joint group potential has value:')

gpot12=multpots(gpot([1 2]));
[gpstate gpvar]=groupstate(group,[1 2 3 4],[1 2 2 1]);
evalpot(gpot12,gpvar,gpstate)
disp('This matches the value of the ungrouped joint')
