#line 1 "oscillator.f"
/* oscillator.f -- translated by f2c (version 19991025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)

   produced by f2c -A -a -c -g oscillator.f 
*/

#include "f2c.h"

#line 1 "oscillator.f"
/* Common Block Declarations */

struct {
    doublereal m, b, c__, a, w, y0, tstop, dt;
    char func[20];
} data_;

#define data_1 data_

/* Table of constant values */

static integer c__5 = 5;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__2 = 2;

/*<       program oscillator >*/
/* Main program */ MAIN__(void)
{
    extern /* Subroutine */ int scan1_(void), timeloop_(void);

/*<       call scan1 >*/
#line 3 "oscillator.f"
    scan1_();
/*<       call timeloop >*/
#line 4 "oscillator.f"
    timeloop_();
/*<       end >*/
#line 5 "oscillator.f"
    return 0;
} /* MAIN__ */

/*<       subroutine scan1 >*/
/* Subroutine */ int scan1_(void)
{
    /* Builtin functions */
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(void);

    /* Local variables */
    extern /* Subroutine */ int scan2_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, char *, ftnlen);
    char func___[20];
    doublereal a___, b___, c___, m___, w___, y0___, tstop___, dt___;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 5, 0, 0, 0 };


/*<       real*8 m_, b_, c_, A_, w_, y0_, tstop_, dt_ >*/
/*<       character func_*20 >*/
/*<       read(*,*) m_, b_, c_, func_, A_, w_, y0_, tstop_, dt_ >*/
#line 10 "oscillator.f"
    s_rsle(&io___1);
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&m___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&b___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&c___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    do_lio(&c__9, &c__1, func___, (ftnlen)20);
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&a___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&w___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&y0___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&tstop___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    do_lio(&c__5, &c__1, (char *)&dt___, (ftnlen)sizeof(doublereal));
#line 10 "oscillator.f"
    e_rsle();
/*      write(*,*) 'scan1', ' m=', m_, ' b=', b_, ' c=', c_, ' A=', A_, */
/*     >           ' w=', w_, ' y0=', y0_, ' tstop=', tstop_, ' dt=', dt_, */
/*     >           ' c-term function:', func_ */
/*<       call scan2(m_, b_, c_, A_, w_, y0_, tstop_, dt_, func_) >*/
#line 15 "oscillator.f"
    scan2_(&m___, &b___, &c___, &a___, &w___, &y0___, &tstop___, &dt___, 
	    func___, (ftnlen)20);
/*<       return  >*/
#line 16 "oscillator.f"
    return 0;
/*<       end >*/
} /* scan1_ */

/*<       subroutine scan2(m_, b_, c_, A_, w_, y0_, tstop_, dt_, func_) >*/
/* Subroutine */ int scan2_(doublereal *m___, doublereal *b___, doublereal *
	c___, doublereal *a___, doublereal *w___, doublereal *y0___, 
	doublereal *tstop___, doublereal *dt___, char *func___, ftnlen 
	func__len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

/*<       real*8 m_, b_, c_, A_, w_, y0_, tstop_, dt_ >*/
/*<       character func_*(*) >*/
/*<       real*8 m, b, c, A, w, y0, tstop, dt >*/
/*<       character func*20 >*/
/*<       common /data/ m, b, c, A, w, y0, tstop, dt, func >*/
/*<       m = m_ >*/
#line 27 "oscillator.f"
    data_1.m = *m___;
/*<       b = b_ >*/
#line 28 "oscillator.f"
    data_1.b = *b___;
/*<       c = c_ >*/
#line 29 "oscillator.f"
    data_1.c__ = *c___;
/*<       A = A_ >*/
#line 30 "oscillator.f"
    data_1.a = *a___;
/*<       w = w_ >*/
#line 31 "oscillator.f"
    data_1.w = *w___;
/*<       y0 = y0_ >*/
#line 32 "oscillator.f"
    data_1.y0 = *y0___;
/*<       tstop = tstop_ >*/
#line 33 "oscillator.f"
    data_1.tstop = *tstop___;
/*<       dt = dt_ >*/
#line 34 "oscillator.f"
    data_1.dt = *dt___;
/*<       func = func_ >*/
#line 35 "oscillator.f"
    s_copy(data_1.func, func___, (ftnlen)20, func__len);
/*<       return >*/
#line 36 "oscillator.f"
    return 0;
/*<       end >*/
} /* scan2_ */

/*<       subroutine adv_fe(y, n, t, dt, scratch) >*/
/* Subroutine */ int adv_fe__(doublereal *y, integer *n, doublereal *t, 
	doublereal *dt, doublereal *scratch)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int rhs_(doublereal *, doublereal *, integer *, 
	    doublereal *);

/*<       integer n, i >*/
/*<       real*8 y(n), t, dt, scratch(n) >*/
/*<       call rhs(scratch, y, n, t) >*/
#line 43 "oscillator.f"
    /* Parameter adjustments */
#line 43 "oscillator.f"
    --scratch;
#line 43 "oscillator.f"
    --y;
#line 43 "oscillator.f"

#line 43 "oscillator.f"
    /* Function Body */
#line 43 "oscillator.f"
    rhs_(&scratch[1], &y[1], n, t);
/*     Forward Euler scheme: */
/*<       do 10 i = 1,n >*/
#line 45 "oscillator.f"
    i__1 = *n;
#line 45 "oscillator.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          y(i) = y(i) + dt*scratch(i) >*/
#line 46 "oscillator.f"
	y[i__] += *dt * scratch[i__];
/*<  10   continue >*/
#line 47 "oscillator.f"
/* L10: */
#line 47 "oscillator.f"
    }
/*<       return >*/
#line 48 "oscillator.f"
    return 0;
/*<       end >*/
} /* adv_fe__ */

/*<       subroutine adv_rk2(y, n, t, dt, scratch1, scratch2) >*/
/* Subroutine */ int adv_rk2__(doublereal *y, integer *n, doublereal *t, 
	doublereal *dt, doublereal *scratch1, doublereal *scratch2)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    doublereal t2;
    extern /* Subroutine */ int rhs_(doublereal *, doublereal *, integer *, 
	    doublereal *);

/*<       integer n, i >*/
/*<       real*8 y(n), t, dt, scratch1(n), scratch2(n), t2 >*/
/*<       call rhs(scratch1, y, n, t) >*/
#line 55 "oscillator.f"
    /* Parameter adjustments */
#line 55 "oscillator.f"
    --scratch2;
#line 55 "oscillator.f"
    --scratch1;
#line 55 "oscillator.f"
    --y;
#line 55 "oscillator.f"

#line 55 "oscillator.f"
    /* Function Body */
#line 55 "oscillator.f"
    rhs_(&scratch1[1], &y[1], n, t);
/*<       do 10 i = 1,n >*/
#line 56 "oscillator.f"
    i__1 = *n;
#line 56 "oscillator.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          scratch2(i) = y(i) + dt*scratch1(i) >*/
#line 57 "oscillator.f"
	scratch2[i__] = y[i__] + *dt * scratch1[i__];
/*<  10   continue >*/
#line 58 "oscillator.f"
/* L10: */
#line 58 "oscillator.f"
    }
/*<       t2 = t + dt >*/
#line 59 "oscillator.f"
    t2 = *t + *dt;
/*<       call rhs(scratch2, scratch2, n, t2) >*/
#line 60 "oscillator.f"
    rhs_(&scratch2[1], &scratch2[1], n, &t2);
/*<       do 20 i = 1,n >*/
#line 61 "oscillator.f"
    i__1 = *n;
#line 61 "oscillator.f"
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          y(i) = y(i) + 0.5*dt*(scratch1(i) + scratch2(i)) >*/
#line 62 "oscillator.f"
	y[i__] += *dt * .5f * (scratch1[i__] + scratch2[i__]);
/*<  20   continue >*/
#line 63 "oscillator.f"
/* L20: */
#line 63 "oscillator.f"
    }
/*<       return >*/
#line 64 "oscillator.f"
    return 0;
/*<       end >*/
} /* adv_rk2__ */

/*<       subroutine rhs(f, y, n, t) >*/
/* Subroutine */ int rhs_(doublereal *f, doublereal *y, integer *n, 
	doublereal *t)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    double sin(doublereal);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);
    double cos(doublereal);

    /* Local variables */
    doublereal cterm;

    /* Fortran I/O blocks */
    static cilist io___15 = { 0, 6, 0, 0, 0 };


/*<       integer n >*/
/*<       real*8 f(n), y(n), t >*/
/*<       real*8 cterm >*/
/*<       real*8 m, b, c, A, w, y0, tstop, dt >*/
/*<       character func*20 >*/
/*<       common /data/ m, b, c, A, w, y0, tstop, dt, func >*/
/*<       if (func .eq. 'y') then >*/
#line 76 "oscillator.f"
    /* Parameter adjustments */
#line 76 "oscillator.f"
    --y;
#line 76 "oscillator.f"
    --f;
#line 76 "oscillator.f"

#line 76 "oscillator.f"
    /* Function Body */
#line 76 "oscillator.f"
    if (s_cmp(data_1.func, "y", (ftnlen)20, (ftnlen)1) == 0) {
/*<          cterm = y(1) >*/
#line 77 "oscillator.f"
	cterm = y[1];
/*<       else if (func .eq. 'siny') then >*/
#line 78 "oscillator.f"
    } else if (s_cmp(data_1.func, "siny", (ftnlen)20, (ftnlen)4) == 0) {
/*<          cterm = sin(y(1)) >*/
#line 79 "oscillator.f"
	cterm = sin(y[1]);
/*<       else if (func .eq. 'y3') then >*/
#line 80 "oscillator.f"
    } else if (s_cmp(data_1.func, "y3", (ftnlen)20, (ftnlen)2) == 0) {
/*<          cterm = (y(1)**3)/6.0 >*/
/* Computing 3rd power */
#line 81 "oscillator.f"
	d__1 = y[1];
#line 81 "oscillator.f"
	cterm = d__1 * (d__1 * d__1) / 6.f;
/*<       else >*/
#line 82 "oscillator.f"
    } else {
/*<          write(*,*) 'Error: spring-term ',func,' illegal' >*/
#line 83 "oscillator.f"
	s_wsle(&io___15);
#line 83 "oscillator.f"
	do_lio(&c__9, &c__1, "Error: spring-term ", (ftnlen)19);
#line 83 "oscillator.f"
	do_lio(&c__9, &c__1, data_1.func, (ftnlen)20);
#line 83 "oscillator.f"
	do_lio(&c__9, &c__1, " illegal", (ftnlen)8);
#line 83 "oscillator.f"
	e_wsle();
/*<          cterm = 0.0 >*/
#line 84 "oscillator.f"
	cterm = 0.f;
/*<       endif >*/
#line 85 "oscillator.f"
    }
/*<       f(1) = y(2) >*/
#line 87 "oscillator.f"
    f[1] = y[2];
/*<       f(2) = ( -b*y(2) - c*cterm + A*cos(w*t) )/m >*/
#line 88 "oscillator.f"
    f[2] = (-data_1.b * y[2] - data_1.c__ * cterm + data_1.a * cos(data_1.w * 
	    *t)) / data_1.m;
/*<       return >*/
#line 89 "oscillator.f"
    return 0;
/*<       end >*/
} /* rhs_ */

/*<       subroutine timeloop >*/
/* Subroutine */ int timeloop_(void)
{
    /* System generated locals */
    doublereal d__1, d__2;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(olist *), s_wsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_wsle(void), f_clos(cllist *);

    /* Local variables */
    doublereal time, y[2];
    extern /* Subroutine */ int adv_rk2__(doublereal *, integer *, doublereal 
	    *, doublereal *, doublereal *, doublereal *);
    doublereal scratch1[2], scratch2[2];

    /* Fortran I/O blocks */
    static cilist io___20 = { 0, 21, 0, 0, 0 };


/*<       integer n >*/
/*<       parameter (n=2) >*/
/*<       real*8 time, y(n), scratch1(n), scratch2(n) >*/
/*<       real*8 m, b, c, A, w, y0, tstop, dt >*/
/*<       character func*20 >*/
/*<       common /data/ m, b, c, A, w, y0, tstop, dt, func >*/
/*      write(*,*) 'timeloop', ' m=', m, ' b=', b, ' c=', c, ' A=', A, */
/*     >           ' w=', w, ' y0=', y0, ' tstop=', tstop, ' dt=', dt, */
/*     >           ' c-term function:', func */
/*     initial conditions: */
/*<       y(1) = y0 >*/
#line 106 "oscillator.f"
    y[0] = data_1.y0;
/*<       y(2) = 0.0 >*/
#line 107 "oscillator.f"
    y[1] = 0.f;
/*<       open(21, FILE='sim.dat', status='unknown', form='formatted') >*/
#line 109 "oscillator.f"
    o__1.oerr = 0;
#line 109 "oscillator.f"
    o__1.ounit = 21;
#line 109 "oscillator.f"
    o__1.ofnmlen = 7;
#line 109 "oscillator.f"
    o__1.ofnm = "sim.dat";
#line 109 "oscillator.f"
    o__1.orl = 0;
#line 109 "oscillator.f"
    o__1.osta = "unknown";
#line 109 "oscillator.f"
    o__1.oacc = 0;
#line 109 "oscillator.f"
    o__1.ofm = "formatted";
#line 109 "oscillator.f"
    o__1.oblnk = 0;
#line 109 "oscillator.f"
    f_open(&o__1);
/*<       do 10 time = 0.0, tstop, dt >*/
#line 110 "oscillator.f"
    d__1 = data_1.tstop;
#line 110 "oscillator.f"
    d__2 = data_1.dt;
#line 110 "oscillator.f"
    for (time = 0.; d__2 < 0 ? time >= d__1 : time <= d__1; time += d__2) {
/*         call adv_fe (y, n, time, dt, scratch1) */
/*<          call adv_rk2 (y, n, time, dt, scratch1, scratch2) >*/
#line 112 "oscillator.f"
	adv_rk2__(y, &c__2, &time, &data_1.dt, scratch1, scratch2);
/*<          write(21,*) time, y(1) >*/
#line 113 "oscillator.f"
	s_wsle(&io___20);
#line 113 "oscillator.f"
	do_lio(&c__5, &c__1, (char *)&time, (ftnlen)sizeof(doublereal));
#line 113 "oscillator.f"
	do_lio(&c__5, &c__1, (char *)&y[0], (ftnlen)sizeof(doublereal));
#line 113 "oscillator.f"
	e_wsle();
/*<  10   continue >*/
#line 114 "oscillator.f"
/* L10: */
#line 114 "oscillator.f"
    }
/*<       close(21) >*/
#line 115 "oscillator.f"
    cl__1.cerr = 0;
#line 115 "oscillator.f"
    cl__1.cunit = 21;
#line 115 "oscillator.f"
    cl__1.csta = 0;
#line 115 "oscillator.f"
    f_clos(&cl__1);
/*<       return >*/
#line 116 "oscillator.f"
    return 0;
/*<       end >*/
} /* timeloop_ */

/* Main program alias */ int oscillator_ () { MAIN__ (); return 0; }
