%2.7  trusscode.m

% Made by Yeunwoo Cho for the MIT 18.085 class homework5-------------------------
% Input files---------------------------------------------------------------------
load inputs.txt;    % x-coord/y-coord/initial horizontal displacemnt
% /initial vertical displacemnt/initial horizontal force/initial vertical force 
load bar.txt;       % existence of bars (1: Yes 0: No)
%--------------------------------------------------------------------------
temp=size(inputs);  %Size of inputs.txt to get the node number N by 6
N=temp(1,1);        %Number of nodes
n=2*N;              %Number of unknown displacments at nodes
m=(N-1)*N/2;        %Maximum number of bars
A=zeros(m,n);       %Initialization of matrix A
% Calculation of A----------------------------------------------------------
for j=1:N
    for k=j+1:N
        i=k-j+(j-1)*(N-j/2); %Bar numbering
    Angle=atan2(inputs(k,2)-inputs(j,2),inputs(k,1)-inputs(j,1));
    A(i,2*j-1)=-cos(Angle);
    A(i,2*j)=-sin(Angle);
    A(i,2*k-1)=cos(Angle);
    A(i,2*k)=sin(Angle);
    end
end
%--------------------------------------------------------------------------
ss=size(A); %Size of matrix A : m by 2n
% Make zero columns for the fixed nodes-------------------------------------
for j=1:N
if inputs(j,3)==0 & inputs(j,4)==0  %Fixed Nodes
    A(:,2*j-1)=0;
    A(:,2*j)=0;
end
end
%--------------------------------------------------------------------------
zero_columns=ss(1,2); % 2n
% Erase zero columns of A----------------------------------------------------
for j=zero_columns:-1:1
    if A(:,j)==0 %Fixed nodes
        A(:,j)=[];
        sss=size(A);
        zero_columns=sss(1,2);
    end
end
%--------------------------------------------------------------------------
% Make zero rows for the non-existent bars(c(i)=0)--------------------------
for j=1:m
if bar(j,1)==0
    A(j,:)=0;
end
end
%--------------------------------------------------------------------------
zero_rows=ss(1,1);%m
% Erase zero rows of A------------------------------------------------------
for j=zero_rows:-1:1
    if  A(j,:)==0
        A(j,:)=[];
        ssss=size(A);
        zero_rows=ssss(1,1);
    end
end
%--------------------------------------------------------------------------
%---End of calculation of A -----------------------------------------------
% size of A will be (m-number of unconnected bars) by (n-2(fixed nodes))
% Make force vector from input file inputs.txt-----------------------------
for i=1:N
    Force(2*i-1,1)=inputs(i,5);
    Force(2*i,1)=inputs(i,6);
end
%-------------------------------------------------------------------------
ff=size(Force); %2N
% Erase force-components whose corresponding bar do not exist--------------
for j=ff/2:-1:1
if inputs(j,3)==0 & inputs(j,4)==0 %For the non-existent bars (c(i)=0)
    Force(2*j,:)=[];
    Force(2*j-1,:)=[];
    ff=size(Force);
end
end
f=Force;
% Size of Force will be m-(number of unconnected bars)
%------------------------------------------------------------------------
% Make C matrix 
size_of_bar=size(bar);
bb=size_of_bar(1,1);
c=bar;

for i=1:m
    if bar(i)==0
        c(i)=0;
    end
end

for i=bb:-1:1
    if c(i)==0
        c(i)=[];
        bb=size(c);
    end
end
C=diag(c);
%-------------------------------------------------------------------------
% Outputs------------------------------------------------------------------
A                     %A matrix
K=A'*C*A              %K matrix
NullSpace=null(A,'r') %Nullspace of A from row echelon form
NullSpace2=null(A)    %Nullspce of A from SVD

% Statically determinate case output
if det(K)~=0          
displacement=K\f %inv(K)*f       %displacement of unsupported nodes
internal_force=C*A*displacement  %resultant internal force of bars
end
% End of program "truss"----------------------------------------------------
