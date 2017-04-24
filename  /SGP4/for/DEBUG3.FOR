      IF (Help .eq. 'Y') THEN
         Write(14,*)  ' ------------------After DSINIT : ------------- '
         Write(14,*)   '    Inputs : '
         Write(14,26) 'Cosim',Cosim,'Emsq',Emsq,'Argpo',Argpo,'S1',S1,
     &                      'S2',S2,'S3',S3
         Write(14,26) 'S4',S4,'S5',S5,'Sinim',Sinim,'Ss1',Ss1,'Ss2',Ss2,
     &                      'Ss3',Ss3
         Write(14,26) 'Ss4',Ss4,'Ss5',Ss5,'Sz1',Sz1,'Sz3',Sz3,'Sz11',
     &                      Sz11,'Sz13',Sz13
         Write(14,26) 'Sz21',Sz21,'Sz23',Sz23,'Sz31',Sz31,'Sz33',Sz33,
     &                      'T',T,'Tc',Tc
         Write(14,24) 'GSTo',GSTo,'Mo',Mo,'MDot',MDOT,'No',xN,
     &                      'nodeo',nodeo,'nodeDt',nodeDOT
         Write(14,26) 'XPIDOT',XPIDOT,'Z1',Z1,'Z3',Z3,'Z11',Z11,'Z13',
     &                      Z13,'Z21',Z21
         Write(14,23) 'Z23',Z23,'Z31',Z31,'Z33',Z33
         Write(14,*)   '    In / Out : '
         Write(14,26) 'EM',EccM,'Argpm',Argpm,'Inclm',Inclm,'Mm',MM,
     &                      'Xn',Xn,'nodem',nodem
         Write(14,*)   '    Outputs : '
         Write(14,27) 'IREZ',IREZ,'Atime',Atime,'D2201',D2201,'D2211',
     &                      D2211,'D3210',D3210,'D3222',D3222
         Write(14,26) 'D4410',D4410,'D4422',D4422,'D5220',D5220,'D5232',
     &                      D5232,'D5421',D5421,'D5433',D5433
         Write(14,26) 'Dedt',Dedt,'Didt',Didt,'DMDT',DMDT,'DNDT',DNDT,
     &                      'DNODT',DNODT,'DOMDT',DOMDT
         Write(14,26) 'Del1',Del1,'Del2',Del2,'Del3',Del3,'Xfact',Xfact,
     &                      'Xlamo',Xlamo,'Xli',Xli
         Write(14,21) 'Xni',Xni
  21    FORMAT( (A7,f15.9) )
  23    FORMAT( 3(A7,f15.9) )
  24    FORMAT( 4(A7,f15.9) )
  26    FORMAT( 6(A7,f15.9) )
  27    FORMAT( A7,I15,5(A7,f15.9) )
       ENDIF

