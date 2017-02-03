i = 62
r = 189876545.7654675432

# Print out numbers with quotes "" such that we see the
# width of the field
print '"%d"' % i       # minimum field
print '"%5d"' % i      # field of width 5 characters
print '"%05d"' % i     # pad with zeros

print '"%g"' % r       # r is big number so this is scientific notation
print '"%G"' % r       # E in the exponent
print '"%e"' % r       # compact scientific notation
print '"%E"' % r       # compact scientific notation
print '"%20.2E"' % r   # 2 decimals, field of width 20
print '"%30g"' % r     # field of width 30 (right-adjusted)
print '"%-30g"' % r    # left-adjust number
print '"%-30.4g"' % r  # 3 decimals

print '%s' % i   # can convert i to string automatically
print '%s' % r

# Use %% to print the percentage sign
print '%g %% of %.2f Euro is %.2f Euro' % \
      (5.1, 346, 5.1/100*346)


