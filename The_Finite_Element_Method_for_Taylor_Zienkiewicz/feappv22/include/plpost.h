
      integer         ibufsz
      parameter      (ibufsz = 80)

      integer         nxtchr
      character               buffer*1
      common /plpost/ nxtchr, buffer(ibufsz)
