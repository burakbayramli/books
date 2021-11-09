#include "sw.h"

// Residual
Eigen::VectorXd residual(sw &sw, Eigen::VectorXd &Q)
{
    Eigen::VectorXd R;
    R = Eigen::VectorXd::Zero(Q.rows());

    // Set cell conservative variables
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.cells[i].Q(0) = Q(3 * i);
        sw.cells[i].Q(1) = Q(3 * i + 1);
        sw.cells[i].Q(2) = Q(3 * i + 2);
    }

    // Set cell residuals to 0
    for (int i = 0; i < sw.N_cells; i++)
    {
        sw.cells[i].R(0) = 0;
        sw.cells[i].R(1) = 0;
        sw.cells[i].R(2) = 0;
    }

    // Set left and right conservative variables, apply boundary conditions
    ///////////////////////////////////////////////////////////////////////
    for (int i = 0; i < sw.N_edges; i++)
    {
        int celll, cellr;
        Eigen::Vector3d Ql, Qr;

        celll = sw.edges[i].celll;

        Ql = sw.cells[celll].Q;

        cellr = sw.edges[i].cellr;

        if (cellr != -1)
        {
            Qr = sw.cells[cellr].Q;
        }
        else
        {
            Qr = Ql;
            Qr(1) = -Qr(1);
            Qr(2) = -Qr(2);
        }

        sw.edges[i].Ql = Ql;
        sw.edges[i].Qr = Qr;
    }

    // Residual calculation
    ///////////////////////////////////////////////////////////////////////
    for (int i = 0; i < sw.N_edges; i++)
    {
        int celll, cellr;
        double l;
        Eigen::Vector3d n, Ql, Qr, delta, F;

        celll = sw.edges[i].celll;
        cellr = sw.edges[i].cellr;
        n = sw.edges[i].n;
        l = sw.edges[i].l;

        // Left conservative variables
        Ql = sw.edges[i].Ql;

        // Right conservative variables
        Qr = sw.edges[i].Qr;

        // Flux
        F = flux(Ql, Qr, n, sw.riemann_solver);

        // Subtract flux from left cell
        sw.cells[celll].R -= F * l;

        if (cellr != -1) // Check if right cell exists
        {
            // Add flux to right cell
            sw.cells[cellr].R += F * l;
        }
    }

    //Set residual vector
    for (int i = 0; i < sw.N_cells; i++)
    {
        R(3 * i) = -sw.cells[i].R(0) / sw.cells[i].S;
        R(3 * i + 1) = -sw.cells[i].R(1) / sw.cells[i].S;
        R(3 * i + 2) = -sw.cells[i].R(2) / sw.cells[i].S;
    }

    return R;
}