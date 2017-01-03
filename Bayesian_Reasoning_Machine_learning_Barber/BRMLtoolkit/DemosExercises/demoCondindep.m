function demoCondindep
%DEMOCONDINDEP Chest Clinic example : test independencies
load chestclinic

A=dag(pot); draw_layout(A); % get the DAG adjacency matrix for the Belief Network
X=[1 4]; Y=[2]; Z=[3 5];

if condindep(A,X,Y,Z); % test if X is indep of Y given Z in the graph A
	fprintf(1,'[%s] is indep of [%s] Given [%s]\n',num2str(X),num2str(Y),num2str(Z));
else
    fprintf(1,'[%s] is NOT indep of [%s] Given [%s]\n',num2str(X),num2str(Y),num2str(Z));
end
% check independence numerically:
fprintf('numerically independent if zero: %g\n',condindepPot(pot,X,Y,Z));