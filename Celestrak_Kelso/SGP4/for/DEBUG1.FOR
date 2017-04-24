      IF (Help .eq. 'Y') THEN
        Write(14,*)  ' ------------------After DPPER : -------------- '
        Write(14,*)  '    Inputs : '
        Write(14,26) 'e3',e3,'ee2',ee2,'peo',peo,'pgho',pgho,'pho',pho,
     &                      'pinco',pinco
        Write(14,26)  'plo',plo,'se2',se2,'se3',se3,'sgh2',sgh2,'sgh3',
     &                      sgh3,'sgh4',sgh4
        Write(14,26) 'sh2',sh2,'sh3',sh3,'si2',si2,'si3',si3,'sl2',sl2,
     &                      'sl3',sl3
        Write(14,26)'sl4',sl4,'T',T,'xgh2',xgh2,'xgh3',xgh3,'xgh4',xgh4,
     &                      'xh2',xh2
        Write(14,26)  'xh3',xh3,'xi2',xi2,'xi3',xi3,'xl2',xl2,'xl3',xl3,
     &                      'xl4',xl4
        Write(14,27)  'zmol',zmol,'zmos',zmos,'Init',Init
        Write(14,*)   '    In/Out : '
        Write(14,25)'EP',Eccp,'Inclp',Inclp,'nodep',nodep,'Argpp',
     &                      Argpp,'XMAP',Mp
  23    FORMAT( 3(A7,f15.9) )
  27    FORMAT( 2(A7,f15.9),A7,A15 )
  25    FORMAT( 5(A7,f15.9) )
  26    FORMAT( 6(A7,f15.9) )
       ENDIF

