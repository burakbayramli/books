include ../system.mk

INCLUDES = -I.

.SUFFIXES: .f

OBJS = \
dasum.o \
daxpy.o \
dbdsqr.o \
dcabs1.o \
dcopy.o \
ddot.o \
dgebak.o \
dgebal.o \
dgebd2.o \
dgebrd.o \
dgecon.o \
dgeev.o \
dgehd2.o \
dgehrd.o \
dgelq2.o \
dgelqf.o \
dgelss.o \
dgemm.o \
dgemv.o \
dgeqr2.o \
dgeqrf.o \
dger.o \
dgesv.o \
dgesvd.o \
dgetf2.o \
dgetrf.o \
dgetrs.o \
dhseqr.o \
dlabad.o \
dlabrd.o \
dlacon.o \
dlacpy.o \
dladiv.o \
dlae2.o \
dlaev2.o \
dlahqr.o \
dlahrd.o \
dlaln2.o \
dlamch.o \
dlange.o \
dlanhs.o \
dlanst.o \
dlansy.o \
dlanv2.o \
dlapy2.o \
dlarf.o \
dlarfb.o \
dlarfg.o \
dlarft.o \
dlarfx.o \
dlartg.o \
dlas2.o \
dlascl.o \
dlaset.o \
dlasq1.o \
dlasq2.o \
dlasq3.o \
dlasq4.o \
dlasq5.o \
dlasq6.o \
dlasr.o \
dlasrt.o \
dlassq.o \
dlasv2.o \
dlaswp.o \
dlatrd.o \
dlatrs.o \
dnrm2.o \
dorg2l.o \
dorg2r.o \
dorgbr.o \
dorghr.o \
dorgl2.o \
dorglq.o \
dorgql.o \
dorgqr.o \
dorgtr.o \
dorm2r.o \
dormbr.o \
dorml2.o \
dormlq.o \
dormqr.o \
dpbtf2.o \
dpbtrf.o \
dpbtrs.o \
dposv.o \
dpotf2.o \
dpotrf.o \
dpotrs.o \
drot.o \
drscl.o \
dscal.o \
dsteqr.o \
dsterf.o \
dstev.o \
dswap.o \
dsyev.o \
dsymv.o \
dsyr.o \
dsyr2.o \
dsyr2k.o \
dsyrfs.o \
dsyrk.o \
dsytd2.o \
dsytrd.o \
dtbsv.o \
dtrevc.o \
dtrmm.o \
dtrmv.o \
dtrsm.o \
dtrsv.o \
dzasum.o \
dzsum1.o \
idamax.o \
ieeeck.o \
ilaenv.o \
izamax.o \
izmax1.o \
lsame.o \
xerbla.o \
zaxpy.o \
zcopy.o \
zdotc.o \
zdotu.o \
zdrscl.o \
zdscal.o \
zgecon.o \
zgemm.o \
zgeru.o \
zgetf2.o \
zgetrf.o \
zlacon.o \
zladiv.o \
zlaswp.o \
zlatrs.o \
zscal.o \
zswap.o \
ztrsm.o \
ztrsv.o 

FCFLAGS = $(FCOPTIONS) $(OPTFLAGS)

.f.o:
	$(FC) $(FCFLAGS) -c $*.f 

libBlasLapack.a: $(OBJS)
	$(AR) $@ $(OBJS)
	$(RANLIB) $@
	$(CP) $@ ../../Libraries

clean:
	rm -f $(OBJS)

realclean: clean
	rm -rf libBlasLapack.a $(COMPILERTEMPFILES) ../../Libraries/libBlasLapack.a
