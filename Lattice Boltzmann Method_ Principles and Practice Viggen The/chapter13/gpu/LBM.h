/* This code accompanies
 *   The Lattice Boltzmann Method: Principles and Practice
 *   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
 *   ISBN 978-3-319-44649-3 (Electronic) 
 *        978-3-319-44647-9 (Print)
 *   http://www.springer.com/978-3-319-44647-9
 *
 * This code is provided under the MIT license. See LICENSE.txt.
 *
 * Author: Orest Shardt
 *
 */
#ifndef __LBM_H
#define __LBM_H

const unsigned int scale = 2;
const unsigned int NX = 32*scale;
const unsigned int NY = NX;

const unsigned int ndir = 9;
const size_t mem_size_0dir   = sizeof(double)*NX*NY;
const size_t mem_size_n0dir  = sizeof(double)*NX*NY*(ndir-1);
const size_t mem_size_scalar = sizeof(double)*NX*NY;

const double w0 = 4.0/9.0;  // zero weight
const double ws = 1.0/9.0;  // adjacent weight
const double wd = 1.0/36.0; // diagonal weight

const double nu = 1.0/6.0;
const double tau = 3.0*nu+0.5;

// Taylor-Green parameters
const double u_max = 0.04/scale;
const double rho0 = 1.0;

const unsigned int NSTEPS = 200*scale*scale;
const unsigned int NSAVE  =  50*scale*scale;
const unsigned int NMSG   =  50*scale*scale;

// compute L2 error and energy?
// disable for speed testing
const bool computeFlowProperties = true;

// suppress verbose output
const bool quiet = true;

void taylor_green(unsigned int,double*,double*,double*);
void stream_collide_save(double*,double*,double*,double*,double*,double*,bool);
void init_equilibrium(double*,double*,double*,double*,double*);
void compute_flow_properties(unsigned int,double*,double*,double*,double*,double*,double*);
void report_flow_properties(unsigned int,double*,double*,double*,double*,double*);
void save_scalar(const char*,double*,double*,unsigned int);

#endif /* __LBM_H */

