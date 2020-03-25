function [Sx,b,perm]=S(X)
% Computes the (possibly random) time at which a network becomes
% operational given the (random) times of repair X
% Inputs:  X    - a configuration of times of repair
%         GRAPH - structure containing the edges of the graph;
%                 defined as global variable
% Outputs: Sx  - the time at which the network becomes operational
%          b - the critical number for configuration X;
%          perm     - the permutation induced by sorting X;

global GRAPH
E=GRAPH.E;
n=max(E(:)); %number of nodes
A=zeros(n);  %incidence matrix
[x_sorted,perm]=sort(X); % find permutation pi
b=0; % critical number 

for i=perm(:)'
    b=b+1;
    e=E(i,:); % which edge is up
    A(e,e)=1; % indicate that the nodes of 'e' are connected
    % find with which other nodes e(1) and e(2) are connected
    y=A(e(1),:) | A(e(2),:); 
    A(y,y)=1; % indicate the complete connectivity of the nodes
    struc_func=A(1,n); % if nodes 1 and n are connected, set H(X)=1
    % struc_func=all(A(1,:)); use this for all-terminal rlbty
    % if structure function is one, exit
     if struc_func, break,  end 
end
Sx=x_sorted(b);







   
     
