      IF (Help .eq. 'Y') THEN
         Write(14,*)  ' ------------------After INITL  :------------- '
         Write(14,*)   '    Inputs : '
         Write(14,27)  'Satn',Satn,'Yr',satn,'Ecco',Ecco,'Epoch',EPOCH,
     &                        'Inclo',Inclo
         Write(14,*)   '    In/Out : '
         Write(14,21)  'No',No
         Write(14,*)   '    Outputs : '
         Write(14,28)  'Method',Method,'Ainv',AINV,'Ao',AO,'Con41',
     &                       CON41,'Con42',CON42,'Cosio',COSIO
         Write(14,21)  'Cosio2',COSIO2
         Write(14,26)  'Einv',EINV,'Eccsq',EccSQ,'Omeosq',OMEOSQ,
     &                         'posq',POSQ,'rp',rp,'Rteosq',RTEOSQ
         Write(14,22)  'Sinio',SINIO,'GSTo',GSTo
  21    FORMAT( (A7,f15.9) )
  22    FORMAT( 2(A7,f15.9) )
  25    FORMAT( 5(A7,f15.9) )
  27    FORMAT( 2(A7,I15),3(A7,f15.9) )
  26    FORMAT( 6(A7,f15.9) )
  28    FORMAT( A7,A15,5(A7,f15.9) )
       ENDIF

