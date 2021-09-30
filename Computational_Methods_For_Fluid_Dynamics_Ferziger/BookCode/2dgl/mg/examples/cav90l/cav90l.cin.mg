Steady lid-driven cavity flow (orthogonal grid)           TITLE
F T F F F F 1          LREAD,LWRITE,LTEST,LOUTS,LOUTE,LTIME,KIN
9 9 2 2 1 1            IMON,JMON,IPR,JPR,NPCOR,NIGRAD
1.0E-05 1.e4 0.92      SORMAX,SLARGE,ALFA
1.0  2.5E-03  1.0      DENSIT,VISC,PRANL
0. 0. 0. 0. 0. 0.      GRAVX,GRAVY,BETA,TH,TC,TREF
0. 0. 0. 0.  1.0       UIN,VIN,PIN,TIN,ULID
1 1 1.e20  0.          ITSTEP,NOTT,DT,GAMT
T   T   T   F                (LCAL(I),I=1,NFI)
0.8 0.8 .2 0.8        (URF(I),I=1,NFI)
0.2 0.2 0.2 0.2        (SOR(I),I=1,NFI)
1   1   10  1          (NSW(I),I=1,NFI)
1.  1.  1.  1.         (GDS(I),I=1,NFI)
100 5  4  4   3   3    (LSG(IK),IK=1,MNG)
8 6 5 5 4 4  (LSR(IK),IK=1,MNG)
3 3 3 3 3 3  (LSI(IK),IK=1,MNG)
1 20 20 20 20 20 20 10  (MIT(IK),IK=1,MNG)
