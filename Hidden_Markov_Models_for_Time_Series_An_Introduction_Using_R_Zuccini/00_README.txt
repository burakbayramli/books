The files in this directory contain the data sets for the book:

Hidden Markov Models for Time Series: An Introduction Using R,

by Zucchini and MacDonald. All chapter and section references
refer to that book, as do the literature citations.


births.txt
==========

See Chapter 14, Section 14.1 in particular.
Births data from Edendale hospital. 

The rows represent monthly values starting February 1977 and ending 
January 1986. The columns are as follows:

tot:	number of mothers delivered
cae:	number of Caesarean sections performed
bb:	number of breech births
fd:	number of forceps deliveries
ve:	number of vacuum extraction deliveries

Prof. Linda Haines is thanked for making these data available.

caterpillars.txt
================

See Section 16.9. 
Caterpillar feeding data. There are 1132 binary observations on each
of eight caterpillars. "1" denotes feeding, "0" not feeding. 

Dr Lindsay Barton Browne is thanked for making these data available.

dax4.txt
========

See Section 13.2.
500 daily returns on closing prices for each of four DAX30 shares:
Allianz, Deutsche Bank, DaimlerChrysler, Siemens.

Dr Sasha Mergner is thanked for making these data available.

drosophilam.txt
===============

See Section 11.1.
381 consecutive observations, at intervals of one second, of speed and
change of direction for one mutant Drosophila Melanogaster larva.
Speed is in mm/s and change of direction in radians/s.

Dr Maximiliano Suster is thanked for making these data available.

drosophilaw.txt
===============

See Section 11.1.
180 consecutive observations, at intervals of one second, of speed and
change of direction for one wild Drosophila Melanogaster larva.
Speed is in mm/s and change of direction in radians/s.

Dr Maximiliano Suster is thanked for making these data available. 

earthquakes.txt
===============

See Section 1.1.
Annual counts of earthquakes magnitude 7 or greater, 1900-2006. 

Date of access: 25.07.2007

Source:
Earthquake Data Base System of the U.S. Geological Survey, National
Earthquake Information Center, Golden CO

Number of Magnitude 7.0 and Greater Earthquakes per Year, 1900-2005
http://neic.usgs.gov/neis/eqlists/7up.html

Earthquakes Magnitude 7.0 and Greater in 2006 
http://neic.usgs.gov/neis/eqlists/eqstats.html

ftse.txt
========

See Section 13.3.2.
Daily closing returns on the FTSE 100 index for 2 April 1986 to
6 May 1994 (2113 days, 2112 returns). This includes zero returns
imputed on public holidays, and matches the length of the series 
analysed by Shephard (1996, p. 1).

geyser.txt
==========

See Section 1.2.4 and Chapter 10.
Waiting times and durations of 299 eruptions of Old Faithful geyser.

Each of the 299 rows contains a waiting time (in minutes), an eruption
duration (in minutes) and a code (0, 1, 2, or 3). Without the code in
the last column, the data are the same as those given in the R "MASS"
library on CRAN (http://cran.r-project.org/).

The codes 1, 2, and 3 (respectively) indicate whether the durations 
were observed only as short, medium or long, and are based on Table 1
of Azzalini and Bowman (1990). 


homicides.txt
=============

See Chapter 15, section 15.1 in particular.
Cape Town homicides and suicides.

The file contains, for each of the 313 weeks making up the years
1986 - 1991, the numbers of deaths recorded at the Salt River state
mortuary, Cape Town, as falling in each of the following 5 categories
(in order): firearm homicide, nonfirearm homicide, firearm suicide,
nonfirearm suicide, legal intervention homicide.

Dr Leonard Lerer is thanked for making these data available. 

lamb.txt
========

See Section 4.3.2.
Counts of movements of a foetal lamb in 240 consecutive five-second
intervals.

Source: Leroux and Puterman (1992).  

seizures.txt
============

See Chapter 9, Section 9.2 in particular.

Counts of epileptic seizures in one patient on 204 counsecutive days.

Source: Le, Leroux and Puterman (1992), but see also p. 208 of
MacDonald and Zucchini (1997).

soap.txt
========

See Exercise 5 in Chapter 1.
Sales of a soap product in 242 consecutive weeks.

Source:  http://gsbwww.uchicago.edu/kilts/research/db/dominicks

thintrade.txt
=============

See Section 13.1.
Binary data representing the presence (1) or absence (0) of trading in
each of six shares listed on the Johannesburg Stock Exchange on each
of 910 consecutive trading days.

Dr David Bowie is thanked for making these data available.

topix.txt
=========

See Section 13.3.4.
1232 daily returns, based on opening prices on the 1233 trading days
30 December 1997 to 30 December 2002, of TOPIX, the Tokyo Stock Price
Index.

Source:  http://index.onvista.de  

wind1.txt
=========

See Sections 12.1 and 12.2.
35064 consecutive hourly observations of wind direction, categorized
into the 16 directions N,NNE, ... , NNW (coded 1 to 16).

The staff of the Koeberg weather station are thanked for making these
data available. 

wind2.txt
=========

See Sections 12.1 and 12.3.
35064 consecutive hourly observations of wind direction (in degrees)
and speed (in cm/s).

The staff of the Koeberg weather station are thanked for making these
data available. 
======================================================================