#!/usr/bin/env python
# automatic execution of HPC exercises

import shutil, sys, os, re, glob

counter = 0

def edit(from_to, filename='F77WAVE.fcp'):
    """
    substitute (from,to) in from_to list, e.g.
    edit((r'from',r'to'))
    """
    shutil.copy(filename + ".orig", filename)
    f = open(filename, 'r')
    filestr = f.read()  # read file into memory
    f.close()
    for (from_, to) in from_to:
        c = re.compile(from_, re.MULTILINE)
        filestr = c.sub(to, filestr)
    f = open(filename, 'w')
    f.write(filestr)
    f.close()

    global counter
    shutil.copy(filename, filename + str(counter)); counter += 1

def compile(compiler, options):
    # run C preprocessor on F77WAVE.fcp and create F77WAVE.f:
    os.system("./fcpp.py F77WAVE.fcp")
    # compile and link:
    cmd = "%s %s -pg -o app F77WAVE.f main.f" % (compiler, options)
    print cmd
    os.system(cmd)

def write_results(message, file):
    file.write("\n\n-----------------------------------------------\n")
    file.write(">>>> " +  message + "\n")
    # extract table from the output of gprof
    res = os.popen("gprof app")
    lines = res.readlines()
    res.close()
    # grab table:
    m = re.search(r"(\%   cumulative.*)\%\s+the percentage of the total",
                  ''.join(lines), re.DOTALL)
    if m:
        table = m.group(1)
        file.write(table)
    # write the current version F77WAVE.f:
    f = open('F77WAVE.f', 'r');  filestr = f.read();  f.close()
    file.write("\n*** current version of F77WAVE.f ***\n")
    file.write(filestr)
    file.write("************\n\n")
    # extract CPU time and return
    for line in lines:
        if re.search(r"MAIN__$", line):
            cpu_time = float(line.split()[1])
            break
    print message, cpu_time, "sec"
    return cpu_time

def run():
    #print "running app..."
    #os.system("app > /dev/null")
    os.system("app")
    
def test_IO(compiler, options):
    message = "with I/O (call dump)"
    edit(((r"^C(\s+)call dump", r" \1call dump"),), filename="main.f")
    compile(compiler, options)
    run()
    cpu = write_results(message, resultfile)
    compactresults.append((message, cpu))
    # back to normal files:
    shutil.copy('main.f.orig', 'main.f')
    shutil.copy('F77WAVE.fcp.orig', 'F77WAVE.fcp')
    # clean up the big files:
    tmpfiles = glob.glob('tmp_*')
    for file in tmpfiles: os.remove(file)

def test_Olevels(compiler, options):
    for i in range(0,4,1):
        message = "with optimization level -O%d" % i
        compile(compiler, options + " -O%d " % i)
        run()
        cpu = write_results(message, resultfile)
        compactresults.append((message, cpu))

def test_loop_unrolling(compiler, options):
    message = "loop unrolling (by the compiler)"
    compile(compiler, options + " -funroll-loops ")
    run()
    cpu = write_results(message, resultfile)
    compactresults.append((message, cpu))

def test_swap_loops(compiler, options):
    message = "traverse arrays column by column"
    edit(((r"DO 20 j = 2, ny-1", r"DO 20 i = 2, nx-1"),
          (r"DO 10 i = 2, nx-1", r"DO 10 j = 2, ny-1")))
    compile(compiler, options)
    run()
    cpu = write_results(message, resultfile)
    compactresults.append((message, cpu))

def test_callfunc1(compiler, options):
    message = "lambda(i,j) replaced by function call h(0,0)"
    edit(((r"lambda\(([^,]+),([^)]+)\)", r"h(0,0)"),
          (r"REAL\*8 a, b, c", r"REAL*8 a, b, c, h"),
          (r", h\(0,0\)", r"")))
    compile(compiler, options)
    run()
    cpu = write_results(message, resultfile)
    compactresults.append((message, cpu))

def test_callfunc2(compiler, options):
    message = "lambda(i,j) replaced by function call h((i-1)*delta,(j-1)*delta)"
    edit(((r"lambda\(([^,]+),([^)]+)\)", r"h((\1 -1)*delta,(\2 -1)*delta)"),
          (r"REAL\*8 a, b, c", r"REAL*8 a, b, c, h, delta"),
          (r"INTEGER i,j", r"INTEGER i,j\n      delta = 10.0/(nx-1)"),
          (r", h\(\(nx .*$", r"")))
    compile(compiler, options)
    run()
    cpu = write_results(message, resultfile)
    compactresults.append((message, cpu))
    

def test_iftests_in_loops(compiler, options):
    message = "if-tests inside loops"
    edit(((r"DO 20 j = 2, ny-1", r"DO 20 j = 1, ny"),
         (r"DO 10 i = 2, nx-1", r"DO 10 i = 1, nx\n            if (i .ge. 2 .and. i .le. nx-1 .and. j .ge. 2 \n     >          .and. j .le. ny-1) then"),
         (r"10      CONTINUE", r"        end if\n 10      CONTINUE")))
    compile(compiler, options)
    run()
    cpu = write_results(message, resultfile)
    compactresults.append((message, cpu))
    
#---------------------------------------------------------------------------
# run exercises:
resultfile = open('results', 'w')
compactresults = []

# make sure we start with the right files:
shutil.copy('main.f.orig', 'main.f')
shutil.copy('F77WAVE.fcp.orig', 'F77WAVE.fcp')

#print "test1:"
#test_iftests_in_loops('g77', '-O3')
#sys.exit(1)

tests = [test_IO, test_Olevels, test_loop_unrolling,
         test_swap_loops, test_callfunc1, test_callfunc2,
         test_iftests_in_loops]
#tests = [test_loop_unrolling,
#         test_callfunc1, test_callfunc2, test_iftests_in_loops]

for compiler in ['g77']:
    for options in ['-O3']:
        for test in tests:
            test(compiler, options)

resultfile.close()
# write a compact table with the main results:
print "\n\n\n"
for (case, cpu) in compactresults:
    print "%-65s %10.3f" % (case, cpu)

