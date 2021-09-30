#pragma once
#ifndef _FLOW_VAR_H_
#define	_FLOW_VAR_H_

struct flow_var
{
	double ** d;	double ** uu;	double ** v;
	double ** T;	double ** p;	double **cc;

	double ***Fluxi;	double *** Fluxj;
};

struct cuda_flow_var
{
	double * d;	double * uu;	double * v;
	double * T;	double * p;	double *cc;

	double ***Fluxi;	double *** Fluxj;
};


#endif // !_FLOW_VAR_H_
