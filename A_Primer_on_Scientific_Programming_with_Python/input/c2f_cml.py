import sys

def read_C():
    try:
        C = float(sys.argv[1])
    except IndexError:
        raise IndexError\
        ('Celsius degrees must be supplied on the command line')
    except ValueError:
        raise ValueError\
        ('Celsius degrees must be a pure number, '\
         'not "%s"' % sys.argv[1])
    # C is read correctly as a number, but can have wrong value:
    if C < -273.15:
        raise ValueError('C=%g is a non-physical value!' % C)
    return C

try:
    C = read_C()
except (IndexError, ValueError), e:
    print e
    sys.exit(1)
    
F = 9.0*C/5 + 32
print '%gC is %.1fF' % (C, F)
