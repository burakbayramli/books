# print_write.py
# -------------------------------------------------------------------------
# Write same data to a file and print to display.
# ------------------------------------------------------------------------- 
f = open('power.txt','w')

print(" N \t\t2**N\t\t3**N")		# print labels for columns
f.write(" N \t\t2**N\t\t3**N\n")	# write labels to file
print("---\t\t----\t\t----")		# print separator
f.write("---\t\t----\t\t----\n")	# write separator to file

#%% loop over integers from 0 to 10 and print/write results
for N in range(11):
	print("{:d}\t\t{:d}\t\t{:d}".format(N, pow(2,N), pow(3,N)))
	f.write("{:d}\t\t{:d}\t\t{:d}\n".format(N, pow(2,N), pow(3,N)))
f.close()
