import matplotlib.pyplot as plt
import numpy as np
A_EPS = (5.02, 4.54, 4.18, 3.73)
B_EPS = (1.35, 1.88, 1.35, 0.73)
ind = np.arrange(len(A_EPS)) # the x locations for the groups
width =0.40
fig, ax = plt.subplots()
A_Std=B_Std=(2,2,2,2)
rects1 = ax.bar(ind, A_EPS, width, color='y', yerr=A_Std)
rect2 = ax.bar(ind+width, B_EPS, width, color='y', yerr=B_Std)
ax.set_ylabel('EPS')
ax.set_xlabel('Year')
ax.set_title('Diluted EPS Excluding Extraordinary Items ')
ax.set_xtick(ind+width)
ax.set_xticklabels( ('2012', '2011', '2010', '2009') )
ax.legend( (rects1[0], rects2[0]), ('W-mart', 'DELL'))
def autolabel(rects):
	for rect in rects:
		heights = rect.get_height()
		ax.text(rect.get_x()+rect.get_width()/2., 1.05*height,
		'%d'%int(height),
			ha='center', va='bottom')
		autolabel(rects1)
		autolabel(rects2)
		plt.show()
		
