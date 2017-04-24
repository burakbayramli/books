      IF (Help .eq. 'Y') THEN
        Write(14,*)  ' ------------------ After DSCOM :--------------- '
        Write(14,*)  '    Inputs : '
        Write(14,26)'EPOCH',EPOCH,'Ep',Eccp,'Argpp',Argpp,'Tc',Tc,
     &                'Inclp', Inclp,'nodep',nodep
        Write(14,21)  'Np  ',Np
        Write(14,*)  '    Outputs : '
        Write(14,26)  'SNODM',SNODM,'CNODM',CNODM,'SINIM',SINIM,'COSIM',
     &                      COSIM,'SINOMM',SINOMM,'COSOMM',COSOMM
        Write(14,26)  'DAY',DAY,'E3',E3,'Ee2',Ee2,'EM',EccM,'EMSQ',EmSQ,
     &                      'GAM',GAM
        Write(14,25)'Peo',Peo,'Pgho',Pgho,'Pho',Pho,'PInco',PInco,'Plo',
     &                       Plo
        Write(14,26)  'RTemSq',RTemSq,'Se2',Se2,'Se3',Se3,'Sgh2',Sgh2,
     &                      'Sgh3',Sgh3,'Sgh4',Sgh4
        Write(14,26)  'Sh2',Sh2,'Sh3',Sh3,'Si2',Si2,'Si3',Si3,'Sl2',Sl2,
     &                      'Sl3',Sl3
        Write(14,26)  'Sl4',Sl4,'S1',S1,'S2',S2,'S3',S3,'S4',S4,'S5',S5
        Write(14,26)'S6',S6,'S7',S7,'SS1',SS1,'SS2',SS2,'SS3',SS3,'SS4',
     &                      SS4
        Write(14,26)  'SS5',SS5,'SS6',SS6,'SS7',SS7,'SZ1',SZ1,'SZ2',SZ2,
     &                      'SZ3',SZ3
        Write(14,26)  'SZ11',SZ11,'SZ12',SZ12,'SZ13',SZ13,'SZ21',SZ21,
     &                      'SZ22',SZ22,'SZ23',SZ23
        Write(14,26)  'SZ31',SZ31,'SZ32',SZ32,'SZ33',SZ33,'Xgh2',Xgh2,
     &                      'Xgh3',Xgh3,'Xgh4',Xgh4
        Write(14,26)  'Xh2',Xh2,'Xh3',Xh3,'Xi2',Xi2,'Xi3',Xi3,'Xl2',Xl2,
     &                      'Xl3',Xl3
        Write(14,26) 'Xl4',Xl4,'Xn',Xn,'Z1',Z1,'Z2',Z2,'Z3',Z3,'Z11',Z11
        Write(14,26)  'Z12',Z12,'Z13',Z13,'Z21',Z21,'Z22',Z22,'Z23',Z23,
     &                      'Z31',Z31
        Write(14,24)  'Z32',Z32,'Z33',Z33,'Zmol',Zmol,'Zmos',Zmos
  21    FORMAT( (A7,f15.9) )
  24    FORMAT( 4(A7,f15.9) )
  25    FORMAT( 5(A7,f15.9) )
  26    FORMAT( 6(A7,f15.9) )

       ENDIF

