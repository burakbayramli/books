% Create a regular mesh for a half annulus 
% Input: sr: inner radius, er: outer radius, 
% m: divisions in r direction, n: divisions in the angle direction, 
% stheta: starting angle, etheta: ending angle

function [nodes,elements,nids]=HalfAnnulusMesh(sr,er,stheta,etheta,m,n)

[r,theta]=meshgrid(sr:(er-sr)/m:er, stheta:(etheta-stheta)/n:etheta);  

nodes=zeros((m+1)*(n+1),3);  % empty nodes matrix
elements=zeros(m*n,5);       % empty elements matrix
nids=zeros(n+1,m+1);         % empty node ID matrix

% next code block: create nodes
k=1;
for i=1:n+1       % rows
  for j=1:m+1     % columns
    nids(i,j)=k;   % grid matrix of node IDs
    x=r(i,j)*cos(theta(i,j));
    y=r(i,j)*sin(theta(i,j));
    nodes(k,1:3)=[k x y];  
    k=k+1;
  end
end

% next code block: create elements
k=1;
for i=1:n         % rows
  for j=1:m       % columns
    elements(k,1)=k;
    elements(k,2:3)=[nids(i,j) nids(i,j+1)];
    elements(k,4:5)=[nids(i+1,j+1) nids(i+1,j)]; 
    k=k+1;
  end
end