      COMMON /IBOUND/
     & NINL(NGR),IIS(NGR),IJI(NIA),IJPI(NIA),IJI1(NIA),IJI2(NIA),
     & NOUT(NGR),IOS(NGR),IJO(NOA),IJPO(NOA),IJO1(NOA),IJO2(NOA),
     & NSYM(NGR),ISS(NGR),IJS(NSA),IJPS(NSA),IJS1(NSA),IJS2(NSA),
     & NWAL(NGR),IWS(NGR),IJW(NWA),IJPW(NWA),IJW1(NWA),IJW2(NWA),
     & NWALI(NGR),IWAS(NGR),NWALA(NGR),
     & NOC(NGR),IOCS(NGR),IJL(NOCA),IJR(NOCA),IJOC1(NOCA),IJOC2(NOCA),
     & npre(ngr),ips(ngr),ijpb(npa),ijpp(npa),ijp1(npa),ijp2(npa)
C
      COMMON /RBOUND/
     &        FMI(NIA),FMO(NOA),fmp(npa),
     &        SRDS(NSA),XNS(NSA),YNS(NSA),
     &        SRDW(NWA),XTW(NWA),YTW(NWA),
     &        FOC(NOCA),FMOC(NOCA),AR(NOCA),AL(NOCA)

c
c     COMMON /IBOUND/ 
c    *  IJI(NIA),IJPI(NIA),NINL(NGR),IIS(NGR),IJI1(NIA),IJI2(NIA),
c    *  IJO(NOA),IJPO(NOA),NOUT(NGR),IOS(NGR),IJO1(NOA),IJO2(NOA),
c    *  IJS(NSA),IJPS(NSA),NSYM(NGR),ISS(NGR),IJS1(NSA),IJS2(NSA),
c    *  IJW(NWA),IJPW(NWA),NWAL(NGR),IWS(NGR),IJW1(NWA),IJW2(NWA),
c    *  IWAS(NGR),NWALI(NGR),NWALA(NGR),
c    *  IJL(NOCA),IJR(NOCA),NOC(NGR),IOCS(NGR),IJOC1(NOCA),IJOC2(NOCA),
c    *  ijpb(npa),ijpp(npa),npre(ngr),ips(ngr),ijp1(npa),ijp2(npa)
c     COMMON /RBOUND/ FMI(NIA),FMO(NOA),SRDS(NSA),XNS(NSA),
c    *       YNS(NSA),SRDW(NWA),XTW(NWA),YTW(NWA),FOC(NOCA),
c    *       FMOC(NOCA),AR(NOCA),AL(NOCA)
