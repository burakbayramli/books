import sys
sys_stdout_orig = sys.stdout
sys.stdout = open('tmp.out', 'w')
sys_stdin_orig = sys.stdin
sys.stdin = open('tmp.input', 'r')

# Make input
fo = open('tmp.input', 'w'); fo.write('\n\n  1432\m'); fo.close()
a = eval(raw_input('Give a:'))
print 'a was', a


