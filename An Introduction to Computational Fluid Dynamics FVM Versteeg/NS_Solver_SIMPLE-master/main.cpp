//2D Navier stokes solver using SIMPLE algorithm
//Author: Vignesh R, CFD Engineer

//The program is limited by the number of the grids.
//If you get NaN please decrease the number of grids to get proper solution.

//Please refer to CFD book by Versteeg and Malalasekara for further info about the theory
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define grid 90 //Do not increase the grid anymore, as either there will be a program crash or a NaN value due to divergence. Keep the grid lower than this.

//Function to determine max of three numbers
double GetMax(double a, double b, double c){
    return std::max(a,std::max(b,c));

}

int main(void)
{
    double aw[grid+2][grid+2], ae[grid+2][grid+2], an[grid+2][grid+2], as[grid+2][grid+2], apu[grid+2][grid+2], apv[grid+2][grid+2], app[grid+2][grid+2];
    double source[grid+2][grid+2], pressure[grid+2][grid+2], pp[grid+2][grid+2], uu[grid+2][grid+2], vv[grid+2][grid+2];
    double u[grid+2][grid+2], v[grid+2][grid+2], uold[grid+2][grid+2], vold[grid+2][grid+2], p[grid+2][grid+2];
    double dx,dy,xmax,ymax,rho,mu,omegau,omegav,omegap,total,source_sum=0.0, Fe, Fw, Fn, Fs, De, Dw, Dn, Ds;
    int i, j, t = 1, outeriter = 2000, k, vel_iter = 10, press_iter = 100;

    //Fluid properties
    mu = 0.01;

    rho = 1.0;

    //Length of the domain
    xmax = 1.0;
    ymax = 1.0;

    //Length of the grid
    dx = xmax/(grid);
    dy = ymax/(grid);

    //initialization
    for (i=0;i<=grid+1;i++)
    {
        for (j=0;j<=grid+1;j++)
        {
            aw[i][j] = 0.0;
            ae[i][j] = 0.0;
            an[i][j] = 0.0;
            as[i][j] = 0.0;
            app[i][j] = 1.0;
            apu[i][j] = 1.0;
            apv[i][j] = 1.0;
            u[i][j] = 0.0;
            v[i][j] = 0.0;
            p[i][j] = 1.0;
            pp[i][j] = 0.0;
            source[i][j] = 0.0;
        }
    }

    //initializing top lid velocity for U
    for (i=0; i<=(grid+1); i++)
    {
        u[i][grid+1] = 1.0;
    }


    memcpy (uold, u, (grid+2)*(grid+2)*sizeof(double));
    memcpy (vold, v, (grid+2)*(grid+2)*sizeof(double));

    //setting relaxation parameters
    //try to keep the pressure relaxation low as this influences the solution a lot
    omegap = 0.1;
    omegau = 0.7;
    omegav = 0.7;

    while(t<=outeriter)
    {

        //u-momentum equations

        //coefficients for internal cells
        for(i=2;i<=grid;i++){
            for(j=1;j<=grid;j++){
                Fe = rho * dy * 0.5 * (uold[i+1][j] + uold[i][j]);
                Fw = rho * dy * 0.5 * (uold[i][j] + uold[i-1][j]);
                Fn = rho * dx * 0.5 * (vold[i][j+1] + vold[i-1][j+1]);
                Fs = rho * dx * 0.5 * (vold[i][j] + vold[i-1][j]);

                De = mu*(dy/dx);
                Dw = mu*(dy/dx);
                Dn = mu*(dx/dy);
                Ds = mu*(dx/dy);

                //Hybrid differencing for discretization //Change this to convert this to a power law scheme
                //This is devised for a 1D method by Patankar. Extending to a 2D method works to some extent
                ae[i][j] = GetMax(-Fe, (De - (0.5*Fe)), 0.0);
                aw[i][j] = GetMax(Fw, (Dw + (0.5*Fw)), 0.0);
                an[i][j] = GetMax(-Fn, (Dn - (0.5*Fn)), 0.0);
                as[i][j] = GetMax(Fs, (Ds + (0.5*Fs)), 0.0);
                apu[i][j] = ae[i][j] + aw[i][j] + an[i][j] + as[i][j]+ (Fe-Fw) + (Fn-Fs);;
                apu[i][j] = apu[i][j]/omegau;
            }
        }


        for(k=1;k<=vel_iter;k++){
            for(i=2;i<=grid;i++){
                for(j=1;j<=grid;j++){
                    u[i][j] = (1.0 - omegau) * uold[i][j] + (1.0/apu[i][j]) * (ae[i][j]*u[i+1][j] + aw[i][j]*u[i-1][j] + an[i][j]*u[i][j+1] + as[i][j]*u[i][j-1] + (p[i-1][j] - p[i][j])*dy);
                }
            }
        }

        //v-momentum equations

        //coefficients for internal cells
        for(i=1;i<=grid;i++){
            for(j=2;j<=grid;j++){
                Fe = rho * dy * 0.5 * (uold[i+1][j] + uold[i+1][j-1]);
                Fw = rho * dy * 0.5 * (uold[i][j] + uold[i][j-1]);
                Fn = rho * dx * 0.5 * (vold[i][j] + vold[i][j+1]);
                Fs = rho * dx * 0.5 * (vold[i][j] + vold[i][j-1]);


                De = mu*(dy/dx);
                Dw = mu*(dy/dx);
                Dn = mu*(dx/dy);
                Ds = mu*(dx/dy);

                //Hybrid differencing for discretization
                ae[i][j] = GetMax(-Fe, (De - (0.5*Fe)), 0.0);
                aw[i][j] = GetMax(Fw, (Dw + (0.5*Fw)), 0.0);
                an[i][j] = GetMax(-Fn, (Dn - (0.5*Fn)), 0.0);
                as[i][j] = GetMax(Fs, (Ds + (0.5*Fs)), 0.0);
                apv[i][j] = ae[i][j] + aw[i][j] + an[i][j] + as[i][j]+ (Fe-Fw) + (Fn-Fs);;
                apv[i][j] = apv[i][j]/omegav;
            }
        }


        for(k=1;k<=vel_iter;k++){
            for(i=1;i<=grid;i++){
                for(j=2;j<=grid;j++){
                    v[i][j] = (1.0 - omegav) * vold[i][j] + (1.0/apv[i][j]) * (ae[i][j]*v[i+1][j] + aw[i][j]*v[i-1][j] + an[i][j]*v[i][j+1] + as[i][j]*v[i][j-1] + (p[i][j-1] - p[i][j])*dx);
                }
            }
        }

        //Pressure correction equation

        for(i=1;i<=grid;i++){
            for(j=1;j<=grid;j++){
                ae[i][j] = (rho * dy * dy)/apu[i+1][j];
                aw[i][j] = (rho * dy * dy)/apu[i][j];
                an[i][j] = (rho * dx * dx)/apv[i][j+1];
                as[i][j] = (rho * dx * dx)/apv[i][j];
            }
        }

        //Boundary values for pressure coeffs

        for(j=0;j<=grid+1;j++){
            aw[1][j] = 0.0;
            ae[grid][j] = 0.0;
        }

        for(i=0;i<=grid+1;i++){
            an[i][grid] = 0.0;
            as[i][1] = 0.0;
        }

        for(i=0;i<=grid+1;i++){
            for(j=0;j<=grid+1;j++){
                app[i][j] = ae[i][j] + aw[i][j] + an[i][j] + as[i][j];
            }
        }

        app[1][1] = 1 * (10^30);

        //Calculating mass imbalance with the source term
        for(i=1;i<=grid;i++){
            for(j=1;j<=grid;j++){
                source[i][j] = rho*dy*(u[i+1][j]-u[i][j]) + rho*dx*(v[i][j+1]-v[i][j]);
                source_sum = source_sum + source[i][j] * source[i][j];
            }
        }

        total = sqrt(source_sum);

        std::cout<<"Iteration No:"<<t<<"\t"<<"Mass imbalance before correction:"<<total<<std::endl;

        source_sum = 0.0;

        for(k=1;k<=press_iter;k++){
            for(j=1;j<=grid;j++){
                for(i=1;i<=grid;i++){
                    pp[i][j] = pp[i][j] + (1.7/app[i][j])*(ae[i][j]*pp[i+1][j] + aw[i][j]*pp[i-1][j] + an[i][j]*pp[i][j+1] + as[i][j]*pp[i][j-1] - source[i][j] - pp[i][j]*app[i][j]);
                }
            }
        }

        //Applying pressure and velocity corrections

        for(i=1;i<=grid;i++){
            for(j=1;j<=grid;j++){
                p[i][j] = p[i][j] + omegap * pp[i][j];
            }
        }

        for(i=2;i<=grid;i++){
            for(j=1;j<=grid;j++){
                u[i][j] = u[i][j] + (dy/apu[i][j]) * (pp[i-1][j] - pp[i][j]);
            }
        }

        for(i=1;i<=grid;i++){
            for(j=2;j<=grid;j++){
                 v[i][j] = v[i][j] + (dx/apv[i][j]) * (pp[i][j-1] - pp[i][j]);
            }
        }

        //Use old and new velocities to calculate the residual to keep track
        //Use norm to determine the residual

        //copying u and v to uold and vold
        memcpy (uold, u, (grid+2)*(grid+2)*sizeof(double));
        memcpy (vold, v, (grid+2)*(grid+2)*sizeof(double));

        t = t+1;

        source_sum = 0.0;

    }

    //This part is not fully finished. This is a simplified method for the sake of post-processing
	for (i=0; i<=(grid-1); i++)
	{
		for (j=0; j<=(grid-1); j++)
		{
			uu[i][j] = 0.5*(u[i][j]+u[i][j+1]);
			vv[i][j] = 0.5*(v[i][j]+v[i+1][j]);
			pressure[i][j] = 0.25*(p[i][j]+p[i+1][j]+p[i][j+1]+p[i+1][j+1]);
		}
	}

    // OUTPUT DATA
	FILE *fout2;
	fout2 = fopen("UVP.dat","w+t");

	if ( fout2 == NULL )
	{
    printf("\nERROR when opening file\n");
    fclose( fout2 );
	}

  else
	{
	fprintf( fout2, "VARIABLES=\"X\",\"Y\",\"U\",\"V\",\"P\"\n");
	fprintf( fout2, "ZONE  F=POINT\n");
	fprintf( fout2, "I=%d, J=%d\n", grid, grid );

	for ( j = 0 ; j < (grid) ; j++ )
	{
    for ( i = 0 ; i < (grid) ; i++ )
    {
		double xpos, ypos;
		xpos = i*dx;
		ypos = j*dy;

		fprintf( fout2, "%5.8lf\t%5.8lf\t%5.8lf\t%5.8lf\t%5.8lf\n", xpos, ypos, uu[i][j], vv[i][j], pressure[i][j] );
    }
	}
	}

	fclose( fout2 );

}
