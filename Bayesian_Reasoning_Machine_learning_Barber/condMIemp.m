function empMI=condMIemp(dataX,dataY,dataZ,X,Y,Z)
%CONDMIEMP Compute the Conditional Mutual Information of the empirical distribution
% empMI=condMIEmp(dataX,dataY,dataZ,X,Y,Z)
% here dataX,dataY,dataZ are data matrices where each row contains the sample states
% with the number of their states in X,Y,Z
pxyz=reshape(normp(count(vertcat(dataX,dataY,dataZ),[X Y Z])),[X Y Z]); % empirical distribution
x = 1:length(X); y = x(end)+1:x(end)+length(Y); z = y(end)+1:y(end)+length(Z);
emppot.variables=[x y z]; emppot.table=pxyz; empMI = condMI(emppot,x,y,z);