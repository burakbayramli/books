# NS_Solver_SIMPLE
Implemented a NS solver using SIMPLE algorithm and solved the benchmark cavity problem. There are some small bugs that have to be fixed. But the thoery behind the code is well established. I used reference from a CFD book written by Versteeg. I found the explanation in this book to be better compared to the Patankar book. Next part is to implement the channel flow benchmark problem using this code.

The Hybrid difference scheme is the discretization scheme. The iterative solver is SOR.

The optimal grid for the code is less than 90. If you increase the grid points, then you have to decrease the relaxation parameter. If you want to increase the relaxation parameter then you have to decrease the number of grid points.

# The Contour plot obtained from the cavity problem with U=1.0m/s

![](Contour_U.png)
