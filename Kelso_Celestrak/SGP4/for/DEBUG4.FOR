      IF (Help .eq. 'Y') THEN
         Write(14,*)  ' ------------------After DSPACE :------------- '
         Write(14,*)   '    Inputs : '
         Write(14,27)'IRez',IRez,'D2201',D2201,'D2211',D2211,'D3210',
     &                       D3210,'D3222',D3222,'D4410',D4410
         Write(14,26) 'D4422',D4422,'D5220',D5220,'D5232',D5232,'D5421',
     &                       D5421,'D5433',D5433,'Dedt',Dedt
         Write(14,26)  'Del1',Del1,'Del2',Del2,'Del3',Del3,'Didt',Didt,
     &                       'Dmdt',Dmdt,'Dnodt',Dnodt
         Write(14,26)  'Domdt',Domdt,'Argp',Argpo,'ArgpDot',ArgpDot,'T',
     &                       T,'TC',TC,'GSTo',GSTo
         Write(14,23)  'Xfact',Xfact,'Xlamo',Xlamo,'No',No
         Write(14,*)   '    In / Out : '
         Write(14,26)  'Atime',Atime,'EM',EccM,'Argpm',Argpm,'Inclm',
     &                       Inclm,'Xli',XLi,'XMam',Mm
         Write(14,22)  'Xni',Xni,'nodem',nodem
         Write(14,*)   '    Outputs : '
         Write(14,22)  'Dndt',Dndt,'XN',XN
  22    FORMAT( 2(A7,f15.9) )
  23    FORMAT( 3(A7,f15.9) )
  26    FORMAT( 6(A7,f15.9) )
  27    FORMAT( A7,I15,5(A7,f15.9) )
       ENDIF

