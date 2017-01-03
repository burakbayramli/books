%2.4  gridresistcode.m

N=3; % N*N nodes and 2N*N - 2N edges in a square grid
col=[-1; zeros(N*N - 1,1)]; % -1 on diagonal of AH
row=[-1 1 zeros(1,N*N -2)]; % +1 next to the diagonal 
AH=toeplitz(col,row); % Incidence matrix/Horizontal edges
AH(N:N:N^2,:)=[]; % Remove N nonexistent edges/end nodes
ROW=[-1 zeros(1,N-1) 1 zeros(1,N*N-N-1)]; % off-diagonal 1
COL=[-1; zeros(N*N-N-1,1)]; % -1 on diagonal of AV
AV=toeplitz(COL,ROW); % Incidence matrix/Vertical edges
A=[AH;AV]; % Combine horizontal and vertical edges into A
ATA=A'*A; % Conductance matrix (singular) of order N*N
norm(ATA*ones(N*N,1)) % Check that ATA(1;...;1)=0

B=toeplitz([2 -1 zeros(1,N-2)]); B(1,1)=1; B(N,N)=1; 
fastATA=kron(B,eye(N)) + kron(eye(N),B); % 2D from 1D
norm(ATA - fastATA) % Check that both ATA's are correct
% Voltages 0 and 1 at nodes k and j/can remove columns j,k from A
% Easier way ! Create a current source f between nodes j and k
% Ground a node (which can be k) and find u=ATA\f and u(j)
% This is the voltage needed at j for unit current from j to k
ATA(:,k)=[]; ATA(k,:)=[]; % Ground node k to make ATA invertible
f=zeros(N*N - 1); f(j)=1; u=ATA\f; u(j) % Expect 1/2 for neighbors
