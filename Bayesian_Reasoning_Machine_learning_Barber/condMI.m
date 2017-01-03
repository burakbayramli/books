function MI=condMI(pot,x,y,z)
%CONDMI conditional mutual information I(x,y|z) of a potential. 
% Mutual information is given by condMI(pot,x,y,[])
tmppot = multpots([pot logpot(divpots(condpot(pot,[x y],z),multpots([condpot(pot,x,z) condpot(pot,y,z)])))]);
MI =abs(sum(tmppot.table(:)));