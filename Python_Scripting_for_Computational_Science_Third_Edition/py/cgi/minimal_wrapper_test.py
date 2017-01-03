# http://...../wrapper.sh.cgi?s=minimal_wrapper_test.py
print 'Content-type: text/html\n'
import sys; print 'running python in',sys.prefix
import cgi; cgi.test()
