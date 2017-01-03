from numpy import random

roots = ["Oscorp", "acme_sats_", "Telecomm_", "MegaCorp", "zorbital",
         "siriuscybernetics", "atmos_sat"]
exts = [".DAT", ".data", ".dat", ".dat", ".dat", ".dat", ".dat"]

ids = range(1, 400)

names = dict(zip(roots, exts))

years = range(2000, 2010)

months = range(1, 12)

days = range(1, 28)

random.seed([1959])

for r, e in names.iteritems():
    unit = random.choice(["Gs", "T"])
    for i in ids:
        n = str(r)+"_"+str(i)+str(e)
        file = open(n, 'wa+')
        for y in years:
            for m in months:
                for d in days:
                    dat = random.uniform(10, 500)
                    content = str(y)+"-"+str(m)+"-"+str(d)+","+str(dat)+unit+"\n"
                    file.write(content)
        file.close()
