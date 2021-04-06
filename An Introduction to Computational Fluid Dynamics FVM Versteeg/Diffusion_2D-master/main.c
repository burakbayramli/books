//  2-D steady state heat conduction solved in Finite Volume method
// Author: Vignesh. R, CFD Engineer

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#define grid 30

int main(void)
{
    double aw[grid+2][grid+2], ae[grid+2][grid+2], an[grid+2][grid+2], as[grid+2][grid+2], ap[grid+2][grid+2];
    double su[grid+2][grid+2], sp[grid+2][grid+2], phi[grid+2][grid+2];
    float dx, dy, omega = 1.2, k = 1000.0, A = 0.01;
    int i, j, t, iter = 5000;

    dx = 1.0/(grid);
    dy = 1.0/(grid);

    //initialization
    for (i=0;i<=grid+1;i++)
    {
        for (j=0;j<=grid+1;j++)
        {
            aw[i][j] = 0.0;
            ae[i][j] = 0.0;
            an[i][j] = 0.0;
            as[i][j] = 0.0;
            ap[i][j] = 0.0;
            su[i][j] = 0.0;
            sp[i][j] = 0.0;
            phi[i][j] = 298.0;  //This is also important for setting temperature

        }
    }

    //Initial conditions on the boundary - Dirichlet
    for (i=1;i<grid+1;i++)
    {
        phi[i][0] = 298.0;  //Bottom
        phi[i][grid+1] = 400.0;   //Top
    }

    for (j=1;j<grid+1;j++)
    {
        phi[0][j] = 298.0;       //Left
        phi[grid+1][j] = 298.0;  //Right
    }

    //Corner points initialization
    phi[0][0] = (phi[0][1] + phi[1][0])/2.0;
    phi[grid+1][0] = (phi[grid+1][1] + phi[grid][0])/2.0;
    phi[0][grid+1] = (phi[0][grid] + phi[1][grid+1])/2.0;
    phi[grid+1][grid+1] = (phi[grid][grid+1] + phi[grid+1][grid])/2.0;

    //Internal cells
    for (i=2;i<grid;i++)
    {
        for (j=2;j<grid;j++)
        {
            sp[i][j] = 0.0;
            su[i][j] = 0.0;
            ae[i][j] = (k * A)/dx;
            aw[i][j] = (k * A)/dx;
            an[i][j] = (k * A)/dy;
            as[i][j] = (k * A)/dy;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }

    //West Boundary cell
    //Dirichlet BC
    for (i=1;i<2;i++)
    {
        for (j=2;j<grid;j++)
        {
            aw[i][j] = 0.0;
            sp[i][j] = -1.0 *(2.0 * k * A)/dx;
            ae[i][j] = (k * A)/dx;
            an[i][j] = (k * A)/dy;
            as[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[0][j])/dx;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }

    //East boundary cell
    for (i=grid;i<grid+1;i++)
    {
        for (j=2;j<grid;j++)
        {
            ae[i][j] = 0.0;
            sp[i][j] = -1.0 * (2.0 * k * A)/dx;
            aw[i][j] = (k * A)/dx;
            an[i][j] = (k * A)/dy;
            as[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[grid+1][j])/dx;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }


    //North boundary cell
    for (j=grid;j<grid+1;j++)
    {
        for (i=2;i<grid;i++)
        {
            an[i][j] = 0.0;
            sp[i][j] = -1.0 * (2.0 * k * A)/dy;
            aw[i][j] = (k * A)/dx;
            ae[i][j] = (k * A)/dx;
            as[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[i][grid+1])/dy;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }


    //South boundary cell
    for (j=1;j<2;j++)
    {
        for (i=2;i<grid;i++)
        {
            as[i][j] = 0.0;
            sp[i][j] = -1.0 * (2.0 * k * A)/dy;
            aw[i][j] = (k * A)/dx;
            ae[i][j] = (k * A)/dx;
            an[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[i][0])/dy;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }


    //Southwest
    for (j=1;j<2;j++)
    {
        for (i=1;i<2;i++)
        {
            as[i][j] = 0.0;
            sp[i][j] = -1.0 * (2.0 * k * A)/dy - 1.0 *(2.0 * k * A)/dx;
            aw[i][j] = 0.0;
            ae[i][j] = (k * A)/dx;
            an[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[i][0])/dy + (2.0 * k * A * phi[0][j])/dx;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }

    //Southeast
    for (j=1;j<2;j++)
    {
        for (i=grid;i<grid+1;i++)
        {
            as[i][j] = 0.0;
            sp[i][j] = -1.0 * (2.0 * k * A)/dy -1.0 * (2.0 * k * A)/dx;
            aw[i][j] = (k * A)/dx;
            ae[i][j] = 0.0;
            an[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[i][0])/dy + (2.0 * k * A * phi[grid+1][j])/dx;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }


    //Northwest
    for (j=grid;j<grid+1;j++)
    {
        for (i=1;i<2;i++)
        {
            an[i][j] = 0.0;
            sp[i][j] = -1.0 * (2.0 * k * A)/dy - 1.0 *(2.0 * k * A)/dx;
            aw[i][j] = 0.0;
            ae[i][j] = (k * A)/dx;
            as[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[i][grid+1])/dy + (2.0 * k * A * phi[0][j])/dx;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }

    //Northeast
    for (j=grid;j<grid+1;j++)
    {
        for (i=grid;i<grid+1;i++)
        {
            an[i][j] = 0.0;
            sp[i][j] = -1.0 * (2.0 * k * A)/dy -1.0 * (2.0 * k * A)/dx;
            aw[i][j] = (k * A)/dx;
            ae[i][j] = 0.0;
            as[i][j] = (k * A)/dy;
            su[i][j] = (2.0 * k * A * phi[i][grid+1])/dy + (2.0 * k * A * phi[grid+1][j])/dx;
            ap[i][j] = aw[i][j]+ae[i][j]+an[i][j]+as[i][j]-sp[i][j];
        }
    }

    //Iterative solver
    //0<Omega<2
    //over-relaxation if omega is greater than 1 and under-relaxation if it is lesser than 1
    for (t=1; t<=iter; t++){
        for (j=1;j<grid+1;j++){
            for(i=1;i<grid+1;i++){
                phi[i][j] = phi[i][j] + (omega/ap[i][j]) * (ae[i][j]*phi[i+1][j] + aw[i][j]*phi[i-1][j] + an[i][j]*phi[i][j+1] + as[i][j]*phi[i][j-1] - ap[i][j]*phi[i][j] + su[i][j]);
            }
        }
    }

    //print all the node temperatures
    for (j=0;j<=grid+1;j++)
    {
        for (i=0;i<=grid+1;i++)
        {
            printf("%f\t", phi[i][j]);
        }
        printf("\n");
    }

printf("\n");
/*
    for (j=0;j<=grid+1;j++)
    {
        for (i=0;i<=grid+1;i++)
        {
            printf("%f\t", ap[i][j]);
        }
        printf("\n");
    }*/

    // OUTPUT DATA - To visualize in paraview
	FILE *fout2;
	fout2 = fopen("temp.dat","w+t");

	if ( fout2 == NULL )
	{
    printf("\nERROR when opening file\n");
    fclose( fout2 );
	}

  else
	{
	fprintf( fout2, "VARIABLES=\"X\",\"Y\",\"T\"\n");
	fprintf( fout2, "ZONE  F=POINT\n");
	fprintf( fout2, "I=%d, J=%d\n", grid+2, grid+2 );

	for ( j = 0 ; j < (grid+2) ; j++ )
	{
    for ( i = 0 ; i < (grid+2) ; i++ )
    {
		double xpos, ypos;
		xpos = i*dx;
		ypos = j*dy;

		fprintf( fout2, "%5.8lf\t%5.8lf\t%5.8lf\n", xpos, ypos, phi[i][j]);
    }
	}
	}

	fclose( fout2 );

}
