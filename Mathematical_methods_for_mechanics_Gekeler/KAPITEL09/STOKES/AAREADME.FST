In this section, some stationary Navier-Stokes problems
are solved by using the Taylor-Hood element
 
1. Make FIRST directory AAMESH permanent
2. Many options are commended out, Modfify !!
3. OPTIONS = 1/2: with/with MATLAB TOOLBOX
4. Recall bildxx for different figures
5. Restart with smaller values of NU

Quotation from Mathlab-Toolbox:

In the POINT MATRIX p, the first and second row contain x-and y-coordinates
of the points in the mesh.
ALLTOGETHER TWO ROWS
The sequence of p must not be changed because of the edge matrix e.

In the TRIANGLE MATRIX t, the first three rows contain indices to the corner points,
given in counter clockwise order, and the fourth row contains the subdomain number.
ALLTOGETHER FOUR ROWS 

In the Edge Matrix e, the first and second row contain indices of the starting
and ending point, the third and fourth rows contain the starting and ending
parameter values , the fifth row contains the edge segment number, and the sixth
and seventh row contain the left- and right-hand side subdomain numbers.
ALLTOGETHER SEVEN ROWS
Relative to the augmented node matrix [p,p1], the indices of midpoints of edges 
are put into row number eight

Let e(:,I) be the part of the edge matrix e concerning segment number I
and let IL be the length of I. Then the indices of the nodes in this segment are found 
as follows:

[e(1,I), e(2,I(LI)] indices with both end points (closed/closed)
e(1,I)              indices without right end point (closed/open)
e(2,I)              indices without left end point (open/closed)
e(1,I(2:LI))        indices without both end points (open/open)

!!See also additional options within the individual DEMO-Files!!



