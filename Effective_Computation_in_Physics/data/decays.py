import math
import datetime as dt

def decay(t):
  return 10*math.exp(-2.*t)


today = dt.datetime.utcnow()

print "Time,Decays"

for i in range(0,50):
  realt = today + dt.timedelta(0,i)
  t = realt.isoformat()
  if (i > 15 and i < 25) : 
    print str(t)+","+str(2)
  elif (i > 40 and i < 45) :
    print str(t)+",NaN"
  else : 
    print str(t)+","+str(decay(i))

    
