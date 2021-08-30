stress=[0,7,0;7,0,0;0,0,0];
[axes,pstress]=eigs(stress);
axes1=axes(:,1);  %  takes the first column of 'axes' and puts it into 'axes1'
axes2=axes(:,2);
axes3=axes(:,3);