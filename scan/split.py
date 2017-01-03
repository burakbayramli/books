import os, re, glob, sys, string;
        
def two_digitize(i):
    if i < 10: return "0" + str(i)
    return str(i)

def run_command(command):
    result = []
    f = os.popen(command, "r")
    sys.stdout.flush()
    for l in f.xreadlines():
        result.append(l)
    return result

for a in range(450):
    print a
    i = a + 1
    file = str(two_digitize(i)) + ".tiff"
    cmd = "convert " + file + " -crop 100%x50% -rotate 270 +repage two/" + two_digitize(i) + "_%d.tiff"
    run_command(cmd)
        
