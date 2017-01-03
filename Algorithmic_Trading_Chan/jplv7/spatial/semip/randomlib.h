/* Prototypes for all user accessible RANDLIB routines */

extern int mexPrintf(
    const char	*fmt,	/* printf style format */
    ...				/* any additional arguments */
    );

extern void mexErrMsgTxt(
    const char	*error_msg	/* string with error message */
    );

#define printf mexPrintf

extern void beta_rndc(int n, double a, double b, double *array);
extern void beta_cdfc(int n, double a, double b, double *x, double *array);
extern void bino_rndc(int n, long a, double p, double *array);
extern void bino_cdfc(int n, double a, double b, double *x, double *array);
extern void chis_rndc(int n, double dof, double *array);
extern void chis_cdfc(int n, double dof, double *x, double *array);
extern void exp_rndc(int n, double dof, double *array);
extern void fdis_rndc(int n, double a, double b, double *array);
extern void fdis_cdfc(int n, double a, double b, double *x, double *array);
extern void gamm_rndc(int n, double a, double b, double *array);
extern void gamm_cdfc(int n, double dof, double *x, double *array);
extern void mdis_rndc(int n, int nevents, long ntrials, double *p, double **mult);
extern void nbino_rndc(int n, long a, double p, double *array);
extern void nbino_cdfc(int n, double a, double b, double *x, double *array);
extern void nchi_rndc(int n, double a, double b, double *array);
extern void nchi_cdfc(int n, double a, double b, double *x, double *array);
extern void nfdis_rndc(int n, double a, double b, double c, double *array);
extern void nfdis_cdfc(int n, double a, double b, double c, double *x, double *array);
extern void normal_rndc(double **vcov, int k, double *rout);
extern void normal_truncc(int n, double *mu, double *sigma2, double *left, double *right, double *out);
extern void snormal_rndc(int n, double *x);
extern void pois_rndc(int n, double a, double *array);
extern void pois_cdfc(int n, double dof, double *x, double *array);
extern void tdis_rndc(int n, double a, double *array);
extern void tdis_cdfc(int n, double dof, double *x, double *array);
extern void advnst(long k);
extern float genbet(float aa,float bb);
extern float genchi(float df);
extern float genexp(float av);
extern float genf(float dfn, float dfd);
extern float gengam(float a,float r);
extern void  genmn(float *parm,float *x,float *work);
extern void  genmul(long n,float *p,long ncat,long *ix);
extern float gennch(float df,float xnonc);
extern float gennf(float dfn, float dfd, float xnonc);
extern float gennor(float av,float sd);
extern void  genprm(long *iarray,int larray);
extern float genunf(float low,float high);
extern void  getsd(long *iseed1,long *iseed2);
extern void  gscgn(long getset,long *g);
extern long ignbin(long n,float pp);
extern long ignnbn(long n,float p);
extern long ignlgi(void);
extern long ignpoi(float mu);
extern long ignuin(long low,long high);
extern void initgn(long isdtyp);
extern long mltmod(long a,long s,long m);
extern void phrtsd(char* phrase,long* seed1,long* seed2);
extern float ranf(void);
extern void setall(long iseed1,long iseed2);
extern void setant(long qvalue);
extern void setgmn(float *meanv,float *covm,long p,float *parm);
extern void setsd(long iseed1,long iseed2);
extern float sexpo(void);
extern float sgamma(float a);
extern float snorm(void);
extern void setSeedTimeCore(char **msg);
extern void setSeedConstantCore(char **msg);
extern double phiinv(double x);
extern double erfinv(double y);
double algdiv(double*,double*);
double alngam(double*);
double alnrel(double*);
double apser(double*,double*,double*,double*);
double basym(double*,double*,double*,double*);
double bcorr(double*,double*);
double betaln(double*,double*);
double bfrac(double*,double*,double*,double*,double*,double*);
void bgrat(double*,double*,double*,double*,double*,double*,int*i);
double bpser(double*,double*,double*,double*);
void bratio(double*,double*,double*,double*,double*,double*,int*);
double brcmp1(int*,double*,double*,double*,double*);
double brcomp(double*,double*,double*,double*);
double bup(double*,double*,double*,double*,int*,double*);
void cdfbet(int*,double*,double*,double*,double*,double*,double*,
            int*,double*);
void cdfbin(int*,double*,double*,double*,double*,double*,double*,
            int*,double*);
void cdfchi(int*,double*,double*,double*,double*,int*,double*);
void cdfchn(int*,double*,double*,double*,double*,double*,int*,double*);
void cdff(int*,double*,double*,double*,double*,double*,int*,double*);
void cdffnc(int*,double*,double*,double*,double*,double*,double*,
            int*s,double*);
void cdfgam(int*,double*,double*,double*,double*,double*,int*,double*);
void cdfnbn(int*,double*,double*,double*,double*,double*,double*,
            int*,double*);
void cdfnor(int*,double*,double*,double*,double*,double*,int*,double*);
void cdfpoi(int*,double*,double*,double*,double*,int*,double*);
void cdft(int*,double*,double*,double*,double*,int*,double*);
void cumbet(double*,double*,double*,double*,double*,double*);
void cumbin(double*,double*,double*,double*,double*,double*);
void cumchi(double*,double*,double*,double*);
void cumchn(double*,double*,double*,double*,double*);
void cumf(double*,double*,double*,double*,double*);
void cumfnc(double*,double*,double*,double*,double*,double*);
void cumgam(double*,double*,double*,double*);
void cumnbn(double*,double*,double*,double*,double*,double*);
void cumnor(double*,double*,double*);
void cumpoi(double*,double*,double*,double*);
void cumt(double*,double*,double*,double*);
double dbetrm(double*,double*);
double devlpl(double [],int*,double*);
double dexpm1(double*);
double dinvnr(double *p,double *q);
static void E0000(int,int*,double*,double*,unsigned long*,
                  unsigned long*,double*,double*,double*,
                  double*,double*,double*,double*);
void dinvr(int*,double*,double*,unsigned long*,unsigned long*);
void dstinv(double*,double*,double*,double*,double*,double*,
            double*);
double dlanor(double*);
double dln1mx(double*);
double dln1px(double*);
double dlnbet(double*,double*);
double dlngam(double*);
double dstrem(double*);
double dt1(double*,double*,double*);
static void E0001(int,int*,double*,double*,double*,double*,
                  unsigned long*,unsigned long*,double*,double*,
                  double*,double*);
void dzror(int*,double*,double*,double*,double *,
           unsigned long*,unsigned long*);
void dstzr(double *zxlo,double *zxhi,double *zabstl,double *zreltl);
double erf1(double*);
double erfc1(int*,double*);
double esum(int*,double*);
double exparg(int*);
double fpser(double*,double*,double*,double*);
double gam1(double*);
void gaminv(double*,double*,double*,double*,double*,int*);
double gamln(double*);
double gamln1(double*);
double Xgamm(double*);
void grat1(double*,double*,double*,double*,double*,double*);
void gratio(double*,double*,double*,double*,int*);
double gsumln(double*,double*);
double psi(double*);
double rcomp(double*,double*);
double rexp(double*);
double rlog(double*);
double rlog1(double*);
double spmpar(int*);
double stvaln(double*);
double fifdint(double);
double fifdmax1(double,double);
double fifdmin1(double,double);
double fifdsign(double,double);
long fifidint(double);
long fifmod(long,long);
void ftnstop(char*);
extern int ipmpar(int*);

