     if (help == 'y')
       //    if (dbgfile != NULL)
       {
       printf( "%85s\n",
                         " ------------------after initl  :---------------");
       printf( "    inputs : \n");
       printf( "%7s%15d%7s%15s%7s%15.9f%7s%15.9f%7s%15.9f\n",
         "satn", satn, "yr", " ", "ecco", ecco, "epoch", epoch, "inclo", inclo);
       printf( "    in/out : \n");
       printf( "%7s%15.9f\n", "no", no);
       printf( "    outputs : \n");
       printf(
              "%7s%15c%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
               "method", method, "ainv", ainv, "ao", ao, "con41", con41,
               "con42", con42, "cosio", cosio);
       printf( "%7s%15.9f\n", "cosio2", cosio2);
       printf( "%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f%7s%15.9f\n",
                         "einx", eccsq, "eccsq", eccsq, "omeosq", omeosq,
                         "posq", posq, "rp", rp, "rteosq", rteosq);
       printf( "%7s%15.9f%7s%15.9f\n", "sinio", sinio, "gsto", gsto);
       }

