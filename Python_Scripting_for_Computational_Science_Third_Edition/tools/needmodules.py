#!/usr/bin/env python
"""
Find the modules required by a Python script.
Drop listing the built-in modules in Python
(method: only pay attention to site-packages
and modules not in the sys.prefix path).
"""
import sys, os, re

def extract_modules(python_v_output):
    modules = []
    # directory where Python libraries are installed:
    install_path = os.path.join(sys.prefix, 'lib',
                                'python'+sys.version[:3])
    for line in python_v_output:
        m = re.search(r'^import (.*?) # .*? (/.*)', line)
        if m:
            module = m.group(1)
            path = m.group(2)
            # is module not in standard Python?
            if 'site-packages' in path:
                modules.append((module,path)) # not in std Python
            elif install_path in path:
                modules.append((module,path)) # outside install_path
    return modules

if __name__ == '__main__':
    try:
        program = sys.argv[1]
    except:
        print 'Usage:', sys.argv[0], 'program [options]'
        sys.exit(1)
    if not os.path.isfile(program):
        print program, 'does not exist'
        sys.exit(1)
        
    os.system('python -v %s %s 2> tmp.1' % \
              (program, ' '.join(sys.argv[2:])))
    f = open('tmp.1', 'r')
    modules = extract_modules(f)
    f.close()
    print '------------\n', modules
    for module, path in modules:
        print '%s (in %s)' % (module, path)

        
