      IF (Help .eq. 'Y') THEN
         Write(14,*)  ' ------------------After SGP4   :------------- '
         Write(14,*)   '    Inputs : '
         Write(14,27) 'Isimp',Isimp,'Method',Method,'Aycof',Aycof,
     &                      'BSTAR',BSTAR,'CON41',CON41,'Cc1',Cc1
         Write(14,26)'Cc4',Cc4,'Cc5',Cc5,'D2',D2,'D3',D3,'D4',D4,
     &                      'Delmo',Delmo
         Write(14,26) 'Ecco',Ecco,'Eta',Eta,'Argpo',Argpo,'ArgpDot',
     &                      ArgpDot,'Omgcof',Omgcof,'Sinmao',Sinmao
         Write(14,26) 'T',T,'T2cof',T2cof,'T3cof',T3cof,'T4cof',T4cof,
     &                      'T5cof',T5cof,'X1mth2',X1mth2
         Write(14,26) 'X7thm1',X7thm1,'Inclo',Inclo,'Mo',Mo,'XMDOT',
     &                      MDOT,'XNO',xN,'nodeo',nodeo
         Write(14,24) 'XNODOT',nodeDOT,'Xlcof',Xlcof,'Xmcof',Xmcof,
     &                      'Xnodcf',Xnodcf
         Write(14,*)   '    Outputs : '
         Write(14,29) 'Error',error,'ri',r(1),'rj',r(2),'rk',r(3),
     &                     'vi',v(1),'vj',v(2)
         Write(14,21) 'vk',v(3)
         Write(14,*)   '    Extra Inputs for DS : '
         Write(14,28)  'IRez',IRez,'D2201',D2201,'D2211',D2211,
     &                      'D3210',D3210,'D3222',D3222
         Write(14,26) 'D4410',D4410,'D4422',D4422,'D5220',D5220,'D5232',
     &                      D5232,'D5421',D5421,'D5433',D5433
         Write(14,26) 'Dedt',Dedt,'Del1',Del1,'Del2',Del2,'Del3',Del3,
     &                      'Didt',Didt,'Dmdt',Dmdt
         Write(14,26) 'Dnodt',Dnodt,'Domdt',Domdt,'E3',E3,'Ee2',Ee2,
     &                      'Peo',Peo,'Pgho',Pgho
         Write(14,26) 'Pho',Pho,'Pinco',Pinco,'Plo',Plo,'Se2',Se2,'Se3',
     &                      Se3,'Sgh2',Sgh2
         Write(14,26) 'Sgh3',Sgh3,'Sgh4',Sgh4,'Sh2',Sh2,'Sh3',Sh3,'Si2',
     &                      Si2,'Si3',Si3
         Write(14,26) 'Sl2',Sl2,'Sl3',Sl3,'Sl4',Sl4,'GSTo',GSTo,'Xfact',
     &                      Xfact,'Xgh2',Xgh2
         Write(14,26) 'Xgh3',Xgh3,'Xgh4',Xgh4,'Xh2',Xh2,'Xh3',Xh3,'Xi2',
     &                      Xi2,'Xi3',Xi3
         Write(14,26)'Xl2',Xl2,'Xl3',Xl3,'Xl4',Xl4,'Xlamo',Xlamo,'Zmol',
     &                      Zmol,'Zmos',Zmos
         Write(14,23) 'Atime',Atime,'Xli',Xli,'Xni',Xni
  21    FORMAT( (A7,f15.9) )
  23    FORMAT( 3(A7,f15.9) )
  24    FORMAT( 4(A7,f15.9) )
  26    FORMAT( 6(A7,f15.9) )
  29    FORMAT( A7,I15,6(A7,f15.9) )
  27    FORMAT( A7,I15,A7,A15,4(A7,f15.9) )
  28    FORMAT( A7,I15,5(A7,f15.9) )
       ENDIF

