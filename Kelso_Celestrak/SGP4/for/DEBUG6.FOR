      IF (Help .eq. 'Y') THEN
         Write(14,*)  ' ------------------After SGP4Init :----------- '
         Write(14,*)   '    Inputs  : '
         Write(14,27) 'Satn',Satn,'Yr',satn,'BStar',BStar,'Ecco',Ecco,
     &                      'Epoch',Epoch,'Argpo',Argpo
         Write(14,22) 'Inclo',Inclo,'Mo',Mo
         Write(14,*)  ' In and Out variables '
         Write(14,21)  'No',No
         Write(14,*)   '    Outputs  : '
         Write(14,28)'INIT',INIT,'Isimp',Isimp,'Method',Method,
     &                     'Aycof',Aycof
         Write(14,23) 'CON41',CON41,'Cc1',Cc1,'Cc4',Cc4
         Write(14,25) 'Cc5',Cc5,'D2',D2,'D3',D3,'D4',D4,'Delmo',Delmo
         Write(14,23) 'Eta',Eta,'ArgpDot',ArgpDot,'Omgcof',Omgcof
         Write(14,23) 'Sinmao',Sinmao,'T2cof',T2cof,'T3cof',T3cof
         Write(14,26) 'T4cof',T4cof,'T5cof',T5cof,'GSTo',GSTo,'X1mth2',
     &                      X1mth2,'X7thm1',X7thm1,'Xlcof',Xlcof
         Write(14,24) 'Xmcof',Xmcof,'MDot',MDOT,'Xnodcf',Xnodcf,
     &                      'nodeDt',nodeDOT
         Write(14,*)   '   IN and Outputs from Deep space satellites : '
         Write(14,22) 'T',T,'nodeo',nodeo
         Write(14,29) 'Irez',Irez,'Atime',Atime,
     &                      'D2201',D2201,'D2211',D2211,'D3210',D3210
         Write(14,26) 'D3222',D3222,'D4410',D4410,'D4422',D4422,'D5220',
     &                      D5220,'D5232',D5232,'D5421',D5421
         Write(14,25) 'D5433',D5433,'Dedt',Dedt,'Del1',Del1,
     &                      'Del2',Del2,'Del3',Del3
         Write(14,25) 'Didt',Didt,'Dmdt',Dmdt,'Dnodt',Dnodt,
     &                      'Domdt',Domdt,'E3',E3
         Write(14,26) 'Ee2',Ee2,'Peo',Peo,'Pgho',Pgho,
     &                      'Pho',Pho,'Pinco',Pinco,'Plo',Plo
         Write(14,26)'Se2',Se2,'Se3',Se3,'Sgh2',Sgh2,'Sgh3',Sgh3,'Sgh4',
     &                      Sgh4,'Sh2',Sh2
         Write(14,26) 'Sh3',Sh3,'Si2',Si2,'Si3',Si3,'Sl2',Sl2,'Sl3',Sl3,
     &                      'Sl4',Sl4
         Write(14,25) 'Xfact',Xfact,'Xgh2',Xgh2,'Xgh3',Xgh3,'Xgh4',
     &                      Xgh4,'Xh2',Xh2
         Write(14,25) 'Xh3',Xh3,'Xi2',Xi2,'Xi3',Xi3,'Xl2',
     &                      Xl2,'Xl3',Xl3
         Write(14,26) 'Xl4',Xl4,'Xli',Xli,'Xlamo',Xlamo,'Xni',Xni,
     &                      'Zmol',Zmol,'Zmos',Zmos
  21    FORMAT( (A7,f15.9) )
  22    FORMAT( 2(A7,f15.9) )
  23    FORMAT( 3(A7,f15.9) )
  24    FORMAT( 4(A7,f15.9) )
  25    FORMAT( 5(A7,f15.9) )
  26    FORMAT( 6(A7,f15.9) )
  27    FORMAT( 2(A7,I15),4(A7,f15.9) )
  28    FORMAT( A7,A15,A7,I15,A7,A15,A7,f15.9 )
  29    FORMAT( A7,I15,4(A7,f15.9) )
       ENDIF

