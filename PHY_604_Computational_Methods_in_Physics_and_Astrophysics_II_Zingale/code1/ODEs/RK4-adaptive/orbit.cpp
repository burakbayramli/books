#include <cmath>
#include <iostream>
#include <algorithm>

const double pi = 4.0*atan(1.0);
const double GM = 4.0*pi*pi;
const int NEQ = 4;

int rhs(const double *, double *);
double period(const double );
int single_step(const double *, double *, const double);
int orbit_advance(double *, const double, const double);
int orbit_adaptive(double *, const double, const double, const double);

int main() {

  // initial conditions
  const double a = 1.0;
  const double e = 0.95;

  const double x0 = 0.0;
  const double y0 = a*(1.0 - e);

  const double u0 = sqrt( (GM/a) * (1.0 + e)/(1.0 - e));
  const double v0 = 0.0;

  double X0[NEQ];
  X0[0] = x0;
  X0[1] = y0;
  X0[2] = u0;
  X0[3] = v0;

  // timestep and max time
  double dt = 0.001;
  double tmax = period(a);

  orbit_adaptive(X0, dt, tmax, 1.e-7);

}


double period(const double a) {
  // return the orbital period
  return sqrt(a*a*a);
}


int single_step(const double *X0, double*Xnew, const double dt) {

  double k1[NEQ];
  double k2[NEQ];
  double k3[NEQ];
  double k4[NEQ];

  double xtmp[NEQ];

  rhs(X0, k1);

  for (int m=0; m<NEQ; m++) {
    xtmp[m] = X0[m] + 0.5*dt*k1[m];
  }
  rhs(xtmp, k2);

  for (int m=0; m<NEQ; m++) {
    xtmp[m] = X0[m] + 0.5*dt*k2[m];
  }
  rhs(xtmp, k3);

  for (int m=0; m<NEQ; m++) {
    xtmp[m] = X0[m] + dt*k3[m];
  }
  rhs(xtmp, k4);

  for (int m=0; m<NEQ; m++) {
    Xnew[m] = X0[m] + dt/6.0*(k1[m] + 2*k2[m] + 2*k3[m] + k4[m]);
  }

  return 0;
}


int rhs(const double *X, double *Xdot) {

  // unpack our input vector
  double x = X[0];
  double y = X[1];
  double u = X[2];
  double v = X[3];

  // positions
  Xdot[0] = u;
  Xdot[1] = v;

  // velocities
  double r = sqrt(x*x + y*y);

  Xdot[2] = -GM*x/pow(r, 3);
  Xdot[3] = -GM*y/pow(r, 3);

  return 0;
}


int orbit_advance(double *X0, const double dt_in, const double tmax) {

  double t = 0;
  double Xnew[NEQ];

  double dt = dt_in;

  while (t < tmax) {
    single_step(X0, Xnew, dt);

    t = t + dt;
    for (int m=0; m < NEQ; m++) {
      X0[m] = Xnew[m];
    }

    std::cout << t << " " << X0[0] << " " << X0[1] << " " << X0[2] << " " << X0[3] << std::endl;

  }

  return 0;

}


int orbit_adaptive(double *X0, const double dt_in,
                   const double tmax, const double eps) {

  double t = 0;
  double Xnew[NEQ];

  double dt = dt_in;
  double dt_new = dt;

  const double S1 = 0.9;
  const double S2 = 4.0;

  while (t < tmax) {

    double rel_error = 1.e10;

    while (rel_error > eps) {
      dt = dt_new;
      if (t + dt > tmax)
        dt = tmax - t;

      // 2 half steps
      double Xtmp[NEQ];
      single_step(X0, Xtmp, 0.5*dt);
      single_step(Xtmp, Xnew, 0.5*dt);

      // a single step
      double Xsingle[NEQ];
      single_step(X0, Xsingle, dt);

      // estimate the error
      rel_error = -1.e20;
      for (int m=0; m < NEQ; m++) {
        rel_error = std::max(rel_error, fabs((Xnew[m] - Xsingle[m])/Xnew[m]));
      }

      double dt_est = dt*pow(fabs(eps/rel_error), 0.2);
      dt_new = std::min(std::max(S1*dt_est, dt/S2), S2*dt);
    }


    t = t + dt;
    for (int m=0; m < NEQ; m++) {
      X0[m] = Xnew[m];
    }

    std::cout << t << " " << X0[0] << " " << X0[1] << " " << X0[2] << " " << X0[3] << std::endl;

  }

  return 0;

}
