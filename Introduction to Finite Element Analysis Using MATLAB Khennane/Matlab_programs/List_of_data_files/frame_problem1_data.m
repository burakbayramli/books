%               File:    frame_problem1_data.m
%
% The following variables are declared as global in order
% to be used by all the functions (M-files) constituting 
% the program
%
%
global nnd nel nne nodof eldof n geom connec F ...
       prop nf Element_loads Joint_loads force Hinge
%
format short e
%
nnd = 5 ;            % Number of nodes:  				
nel = 4 ;            % Number of elements: 				
nne = 2 ;            % Number of nodes per element:			
nodof =3;            % Number of degrees of freedom per node	
eldof = nne*nodof;   % Number of degrees of freedom per element 	
%
% Nodes coordinates x and y  
%
geom=zeros(nnd,2);
geom(1,1)=0.     ; geom(1,2)= 0.;       % x and y coordinates of node 1
geom(2,1)=0.     ; geom(2,2)= 5000.;    % x and y coordinates of node 2
geom(3,1)=6000.  ; geom(3,2)= 6000.;    % x and y coordinates of node 3
geom(4,1)=12000. ; geom(4,2)= 5000.;    % x and y coordinates of node 4
geom(5,1)=12000. ; geom(5,2)= 0.;       % x and y coordinates of node 4
%
% Element connectivity
%
connec=zeros(nel,2);
connec(1,1) = 1;  connec(1,2) =2 ;   % First and second node of element 1
connec(2,1) = 2;  connec(2,2) =3 ;   % First and second node of element 2
connec(3,1) = 3;  connec(3,2) =4 ;   % First and second node of element 3
connec(4,1) = 4;  connec(4,2) =5 ;   % First and second node of element 4
%
% Geometrical properties
%
prop=zeros(nel,3);
prop(1,1)=2.0e+5; prop(1,2)=5210; prop(1,3)=86.4e+6; % E,A and I element 1 
prop(2,1)=2.0e+5; prop(2,2)=5210; prop(2,3)=86.4e+6; % E,A and I element 2 
prop(3,1)=2.0e+5; prop(3,2)=5210; prop(3,3)=86.4e+6; % E,A and I element 3 
prop(4,1)=2.0e+5; prop(4,2)=5210; prop(4,3)=86.4e+6; % E,A and I element 4 
%
% Boundary conditions
%
nf = ones(nnd, nodof);                % Initialise the matrix nf to 1
nf(1,1) = 0; nf(1,2) =0; nf(1,3) = 0; % Prescribed nodal freedom of node 1
nf(5,1) = 0; nf(5,2)= 0; nf(5,3) = 0; % Prescribed nodal freedom of node 5     
%          
% Counting of the free degrees of freedom
%
n=0;
for i=1:nnd
    for j=1:nodof
        if nf(i,j) ~= 0 
            n=n+1;
           nf(i,j)=n;
        end
    end
end
%
% Internal Hinges
%
Hinge = ones(nel,2);
%
% loading
%
Joint_loads= zeros(nnd, 3);
%
% Joint loads are usually entered in global coordinates
% Enter here the forces in X and Y directions and any
% concentrated moment at node i
%
% Staticaly equivalent loads are entered in local 
% coordinates of the element
%
Element_loads= zeros(nel, 6);
Element_loads(2,:)= [0   36.4965e3    37e6    0    36.4965e3   -37e6];
Element_loads(3,:)= [0  -36.4965e3   -37e6    0   -36.4965e3    37e6];
%
%
%%%%%%%%%%%%   End of input    %%%%%%%%%%%% 