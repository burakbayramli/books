
      integer          num_nps,       num_ups
      parameter       (num_nps = 400, num_ups = 200)

      integer          np
      common /pointer/ np(num_nps)

      integer           up
      common /upointer/ up(num_ups)
