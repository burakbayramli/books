%               File:    frame_problem2_data.m
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
nnd = 9 ;               	% Number of nodes:  				
nel = 9 ;                	% Number of elements: 				
nne = 2 ;                	% Number of nodes per element:			
nodof =3;                	% Number of degrees of freedom per node	
eldof = nne*nodof;       	% Number of degrees of freedom per element 	
%
% Nodes coordinates x and y  
%
geom=zeros(nnd,2);
geom(1,1)=0.     ; geom(1,2)= 0.;    %   x and y coordinates of node 1
geom(2,1)=0.     ; geom(2,2)= 5.;    %   x and y coordinates of node 2
geom(3,1)=0.     ; geom(3,2)= 10.;   %   x and y coordinates of node 3
geom(4,1)=3.     ; geom(4,2)= 5.;    %   x and y coordinates of node 4
geom(5,1)=4.5    ; geom(5,2)= 10.;   %   x and y coordinates of node 5
geom(6,1)=6.     ; geom(6,2)= 5.;    %   x and y coordinates of node 6
geom(7,1)=9.     ; geom(7,2)= 10.;   %   x and y coordinates of node 7
geom(8,1)=9.     ; geom(8,2)= 5.;    %   x and y coordinates of node 8
geom(9,1)=9.     ; geom(9,2)= 0.;    %   x and y coordinates of node 9
%
% Element connectivity
%
connec=zeros(nel,2);
connec(1,1) = 1;  connec(1,2) =2 ;        % First and second node of element 1
connec(2,1) = 2;  connec(2,2) =3 ;        % First and second node of element 2
connec(3,1) = 2;  connec(3,2) =4 ;        % First and second node of element 3
connec(4,1) = 3;  connec(4,2) =5 ;        % First and second node of element 4
connec(5,1) = 4;  connec(5,2) =6 ;        % First and second node of element 5
connec(6,1) = 5;  connec(6,2) =7 ;        % First and second node of element 6
connec(7,1) = 6;  connec(7,2) =8 ;        % First and second node of element 7
connec(8,1) = 7;  connec(8,2) =8 ;        % First and second node of element 8
connec(9,1) = 8;  connec(9,2) =9 ;        % First and second node of element 9
%
% Geometrical properties
%
prop=zeros(nel,3);
prop(1,1)=35e+6;  prop(1,2)=0.16;  prop(1,3)=2.1333e-3;  %E,A and I of element 1 
prop(2,1)=35e+6;  prop(2,2)=0.16;  prop(2,3)=2.1333e-3;  %E,A and I of element 2 
prop(3,1)=70e+6;  prop(3,2)=0.1 ;  prop(3,3)=1.3333e-3;  %E,A and I of element 3 
prop(4,1)=70e+6;  prop(4,2)=0.1 ;  prop(4,3)=1.3333e-3;  %E,A and I of element 4
prop(5,1)=70e+6;  prop(5,2)=0.1 ;  prop(5,3)=1.3333e-3;  %E,A and I of element 5 
prop(6,1)=70e+6;  prop(6,2)=0.1 ;  prop(6,3)=1.3333e-3;  %E,A and I of element 6 
prop(7,1)=70e+6;  prop(7,2)=0.1 ;  prop(7,3)=1.3333e-3;  %E,A and I of element 7 
prop(8,1)=35e+6;  prop(8,2)=0.16;  prop(8,3)=2.1333e-3;  %E,A and I of element 8 
prop(9,1)=35e+6;  prop(9,2)=0.16;  prop(9,3)=2.1333e-3;  %E,A and I of element 9 
%
% Boundary conditions
%
nf = ones(nnd, nodof);                   % Initialise the matrix nf to 1
nf(1,1) = 0; nf(1,2) =0; nf(1,3) = 0;   % Prescribed nodal freedom of node 1
nf(9,1) = 0; nf(9,2)= 0; nf(9,3) = 0;   % Prescribed nodal freedom of node 9     
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
Hinge = ones(nel, 2);
Hinge(4,2) = 0;  		%Hinge accounted with element 4
%
% loading
%
% Joint loads are usually entered in global coordinates
% Enter here the forces in X and Y directions and any
% concentrated moment at node i
Joint_loads= zeros(nnd, 3);
Joint_loads(4,:)=[0   -20     0];
Joint_loads(6,:)=[0   -20     0];
%
% Staticaly equivalent loads are entered in local 
% coordinates of the element
%
Element_loads= zeros(nel, 6);
Element_loads(4,:)= [0   -56.25     -50.625    0     -33.75    0];
Element_loads(6,:)= [0   -33.75        0       0     -56.25    50.625];
%
%
%%%%%%%%%%%%   End of input            %%%%%%%%%%%% 