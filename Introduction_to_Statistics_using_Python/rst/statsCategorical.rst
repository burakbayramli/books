.. image:: ..\Images\title_categorical.png
    :height: 100 px

.. Tests on Categorical Data 
.. ==========================

In a sample of individuals the number falling into a particular group is
called the *frequency*, so the analysis of categorical data is the
analysis of frequencies. When two or more groups are compared the data
are often shown in the form of a *frequency table*, sometimes also
called *contingency table*.



+-------------+------------------+-----------------+-----------+
|             | *Right Handed*   | *Left Handed*   | *Total*   |
+=============+==================+=================+===========+
| *Males*     | 43               | 9               | 52        |
+-------------+------------------+-----------------+-----------+
| *Females*   | 44               | 4               | 48        |
+-------------+------------------+-----------------+-----------+
| *Total*     | 87               | 13              | 100       |
+-------------+------------------+-----------------+-----------+

The corresponding expected values are: 

+-------------+------------------+-----------------+-----------+
|             | *Right Handed*   | *Left Handed*   | *Total*   |
+=============+==================+=================+===========+
| *Males*     | 45.2             | 6.8             | 52        |
+-------------+------------------+-----------------+-----------+
| *Females*   | 41.8             | 6.2             | 48        |
+-------------+------------------+-----------------+-----------+
| *Total*     | 87               | 13              | 100       |
+-------------+------------------+-----------------+-----------+

If you have only one sample group of data, the analysis options are somewhat limited. In contrast, a number of statistical tests exist for the analysis of frequency tables.

Chi-square Test
    This is the most common type. It is a hypothesis test,
    which checks if the entries in the individual cells all come from the same
    distribution. In other words, it checks the null hypothesis *H_0* that the
    results are independent of the row or column in which they appear. The
    alternative hypothesis *H_a* does not specify the type of association, so
    close attention to the data is required to interpret the information
    provided by the test.


Fisher's Exact Test
    While the chi-square test is approximate, the *Fisher's Exact Test* is an exact test. As it is computationally much more expensive and intricate than the chi-square test, it was originally used only for small sample numbers. However, in general it is now the more advisable test to use.

McNemar's Test
    This is a matched pair test for 2x2 tables.

Cochran's Q Test
    Cochran's Q test is an extension to the McNemar's test for related samples that provides a method for testing for differences between three or more *matched/paired* sets of frequencies or proportions. For example, if you have exactly the same samples analyzed by 3 different laboratories, and you want to check if the results are statistically equivalent, you would use this test.

One Proportion 
---------------

If you have one sample group of data, you can check if your sample is
representative of the standard population. To do so, you have to know
the proportion :math:`p` of the characteristic in the standard
population.
The occurrence of a characteristic in a group of *n* people is described
by the binomial distribution, with :math:`mean = p*n`. The standard error
of samples with this characteristic is given by

.. math:: se(p) = \sqrt{p(1-p)/n}

and the corresponding 95% confidence interval is

.. math:: ci = mean \pm se * t_{n,0.95}

If your data lie outside this confidence interval, they are *not*
representative of the population.

Example
~~~~~~~

For example, let us look at incidence and mortality for breast cancer, and try to
answer the following two questions: among the FH-students, how many occurrences
of breast cancer should we expect per year? And how many of the female
FH-students will probably die from breast cancer at the end of their life?

We know that:

  - the FH OOe has about 5'000 students, about half of which are female.
  - breast cancer hits predominantly women.
  - the *incidence* of breast cancer in the age group 20-30 is about 10
  - 3.8\% of all women die of cancer.

From these points of information, we can obtain the following parameters for our
calculations

  - n = 2'500
  - :math:`p_{incidence} = 10 / 100'000`, as *incidence* is typically defined as
    the new occurrences of a disease per year per 100'000 people.
  - :math:`p_{mortality} = 3.8/100`.

The 95\% confidence interval for the incidence is -0.7 - 1.2, and for the number
of deaths 76 - 114. So we expect that every year most likely none or one of the
FH-students will be diagnosed with breast cancer; but between 76 and 114 of the
female students will eventually die from this disease.

Frequency Tables
----------------

If your data can be organized in a set of categories, and they are given as *frequencies*, i.e. the total number of samples in each category (not as percentages), the tests described in this section are appropriate for your data analysis.

Many of these tests analyze the *deviation from an expected value*. Since the chi-square distribution characterizes the variability of data (in other words, their deviation from a mean value), many of these tests refer to this distribution, and are accordingly termed *chi-square tests*.

Assume you have observed absolute frequencies :math:`o_i` and expected
absolute frequencies :math:`e_i`. Under the Null hypothesis all your data come from the same
population, and the test statistic

.. math:: V = \sum_i \frac{(o_i-e_i)^2}{e_i} \approx \chi^2_f

.

follows a chi square distribution with :math:`f` degrees of freedom. :math:`i` might denote a
simple index running from :math:`1,...,I` or even a multiindex
:math:`(i_1,...,i_p)` running from :math:`(1,...,1)` to
:math:`(I_1,...,I_p)`.


One-way Chi-square Test
~~~~~~~~~~~~~~~~~~~~~~~

For example, assume that you go hiking with your friends. Every evening, you draw lots who has to do the washing up.
But at the end of the trip, you seem to have done most of the work:

+--------+----------+-----------+----------+-----------+---------+
| *You*  | *Peter*  |  *Hans*   |  *Paul*  |  *Mary*   |  *Joe*  |
+========+==========+===========+==========+===========+=========+
|  *10*  |  *6*     |   *5*     |  *4*     |   *5*     |  *3*    |
+--------+----------+-----------+----------+-----------+---------+

You expect that there has been some foul play, and calculate how likely it is that this distribution came up by chance. The

.. math::   expectedFrequency = \frac{n_{total}}{n_{people}}

is *5.5*. The likelihood that this distribution came up by chance is

::

    V, p = stats.chisquare(data)
    print(p)
    >>> 0.373130385949

In other words, you doing a lot of the washing up really could have been by chance!

Chi-square Contingency Test
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you can arrange your data in rows and columns, you can check if the numbers in the individual columns are contingent on the row value. For this reason, this test is sometimes called *contingency test*.

The chi-square contingency test is based on a test statistic that measures the divergence of the observed data from the values that would be expected under the null hypothesis of no association. When *n* is the total number of observations included in the table, the expected value for each cell in a two-way table is


.. math::
    
    expected = \frac{row total*column total}{n}

Assumptions
^^^^^^^^^^^^

The test statistic :math:`V` is approximately :math:`\chi^2`
distributed, if

-  for all absolute expected frequencies :math:`e_i` holds
   :math:`e_i \geq 1` and

-  for at least 80% of the absolute expected frequencies :math:`e_i`
   holds :math:`e_i \geq 5`.

For small sample numbers, corrections should be made for some bias that
is caused by the use of the continuous chi-squared distribution. This
correction is referred to as *Yates correction*.

Degrees of Freedom
^^^^^^^^^^^^^^^^^^

The degrees of freedom (DOF) can be computed by the numbers of absolute observed
frequencies which can be chosen freely. For example, only one cell of a 2x2 table
with the sums at the side and bottom needs to be filled, and the others can be
found by subtraction. In general, an *r x c* table has *df=(r-1)x(c-1)*
degrees of freedom. We know that the sum of absolute expected frequencies is

.. math:: \sum_i o_i = n

which means that the maximum number of degrees of freedom is
:math:`I-1`. We might have to subtract from the number of degrees of
freedom the number of parameters we need to estimate from the sample,
since this implies further relationships between the observed
frequencies.

Example 1
^^^^^^^^^

The Python command *stats.chi2\_contingency* returns the following list: :math:`(\chi^2, p, dof, e_i)`.

::

    V, p, dof, expected = stats.chi2_contingency(data)
    print(p)
    >>> 0.300384770391

For the example data in the Table above, the results are :math:`\chi^2=1.1, p=0.3, df=1`). In other words, there is no indication that there is a difference in left-handed people vs right-handed people between males and females.

**Note:** These values assume the default setting, which uses the *Yates correction*. Without this correction, the results are :math:`\chi^2=1.8, p=0.18`.


Example 2
^^^^^^^^^

The :math:`\chi^2` test can be used to generate "quick and dirty" test,
e.g.

:math:`H_0:` The random variable :math:`X` is symmetrically distributed
versus

:math:`H_1:` the random variable :math:`X` is not symmetrically
distributed.

We know that in case of a symmetrical distribution the arithmetic mean
:math:`\bar{x}` and median should be nearly the same. So a simple way to
test this hypothesis would be to count how many observations are less
than the mean (:math:`n_-`)and how many observations are larger than the
arithmetic mean (:math:`n_+`). If mean and median are the same than 50%
of the observation should smaller than the mean and 50% should be larger
than the mean. It holds

.. math:: V = \frac{(n_- - n/2)^2}{n/2} + \frac{(n_+ - n/2)^2}{n/2} \approx \chi^2_1

.

Comments
^^^^^^^^

The Chi-square test is a pure hypothesis test. It tells you if your
observed frequency can be due to a random sample selection from a single
population. A number of different expressions have been used for
chi-square tests, which are due to the original derivation of the
formulas (from the time before computers were pervasive). Expression
such as *2x2 tables*, *r-c tables*, or *Chi-square test of contingency*
all refer to frequency tables and are typically analyzed with chi-square
tests.

Fisher's Exact Test
~~~~~~~~~~~~~~~~~~~

If the requirement that 80% of cells should have expected values of at least
5 is not fulfilled, *Fisher's exact test* should be used. This test is based
on the observed row and column totals. The method consists of evaluating the
probability associated with all possible 2x2 tables which have the same row
and column totals as the observed data, making the assumption that the null
hypothesis (i.e. that the row and column variables are unrelated) is true.
In most cases, Fisher's exact test is preferable to the chi-square test. But
until the advent of powerful computers, it was not practical. You should use
it up to approximately 10-15 cells in the frequency tables. It is called
"exact" because the significance of the deviation from a null hypothesis can
be calculated exactly, rather than relying on an approximation that becomes
exact in the limit as the sample size grows to infinity, as with many
statistical tests.

In using the test, you have to decide if you want to use a one-tailed test
or a two-tailed test. The former one looks for the probability to find a
distribution as extreme or more extreme as the observed one. The latter one
(which is the default in python) also considers tables as extreme in the
opposite direction.

**Note:** The python command *stats.fisher_exact* returns by default the
p-value for *finding a value as extreme or more extreme than the
observed one*. According to Altman, this is a reasonable approach, although
not all statisticians agree on that point.

Example: "A Lady Tasting Tea"
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

R.A. Fisher was one of the founding fathers of modern statistics. One of his early, and perhaps the most famous, experiments was to test an English lady's claim that she could tell whether milk was poured before tea or not. Here is an account of the seemingly trivial event that had the most profound impact on the history of modern statistics, and hence, arguably, modern quantitative science (J.F. Box : "R.A. Fisher: The Life of a Scientist". John Wiley and Sons, New York 1978).

    *Already, quite soon after he had come to Rothamstead, his presence had transformed one commonplace tea time to an historic event. It happened one afternoon when he drew a cup of tea from the urn and offered it to the lady beside him, Dr. B. Muriel Bristol, an algologist. She declined it, stating that she preferred a cup into which the milk had been poured first. "Nonsense," returned Fisher, smiling, "Surely it makes no difference." But she maintained, with emphasis, that of course it did. From just behind, a voice suggested, "Let's test her." It was William Roach who was not long afterward to marry Miss Bristol. Immediately, they embarked on the preliminaries of the experiment, Roach assisting with the cups and exulting that Miss Bristol divined correctly more than enough of those cups into which tea had been poured first to prove her case.*

    *Miss Bristol's personal triumph was never recorded, and perhaps Fisher was not satisfied at that moment with the extempore experimental procedure. One can be sure, however, that even as he conceived and carried out the experiment beside the trestle table, and the onlookers, no doubt, took sides as to its outcome, he was thinking through the questions it raised: How many cups should be used in the test? Should they be paired? In what order should the cups be presented? What should be done about chance variations in the temperature, sweetness, and so on? What conclusion could be drawn from a perfect score or from one with one or more errors?*

The real scientific significance of this experiment is in these questions. These are, allowing incidental particulars, the questions one has to consider before designing an experiment. We will look at these questions as pertaining to the "lady tasting tea", but you can imagine how these questions should be adapted to different situations.

  * *What should be done about chance variations in the temperature, sweetness, and so on?*
    Ideally, one would like to make all cups of tea identical except for the order of pouring milk first or tea first. But it is never possible to control all of the ways in which the cups of tea can differ from each other. If we cannot control these variations, then the best we can do - we do mean the "best" - is by randomization.

  * *How many cups should be used in the test? Should they be paired? In what order should the cups be presented?* The key idea here is that the number and ordering of the cups should allow a subject ample opportunity to prove his or her abilities and keep a fraud from easily succeeding at correctly discriminating the the order of pouring in all the cups of tea served.

  * *What conclusion could be drawn from a perfect score or from one with one or more errors?* If the lady is unable to discriminate between the different orders of pouring, then by guessing alone, it should be highly unlikely for that person to determine correctly which cups are which for all of the cups tested. Similarly, if she indeed possesses some skill at differentiating between the orders of pouring, then it may be unreasonable to require her to make no mistakes so as to distinguish her ability from a pure guesser.

An actual scenario described by Fisher and told by many others as the "lady tasting tea" experiment is as follows.

  * For each cup, we record the order of actual pouring and what the lady says the order is. We can summarize the result by a table like this:

+-------------------+-----------+------------+-----+
|                   | Tea first | Milk first |     |
|                   | poured    | poured     |     |
+===================+===========+============+=====+
| Lady  "Tea first" | a         | b          | a+b |
+-------------------+-----------+------------+-----+
| says "Milk first" | c         | d          | c+d |
+-------------------+-----------+------------+-----+
|                   | a+c       | b+d        | n   |
+-------------------+-----------+------------+-----+

Here *n* is the total number of cups of tea made. The number of cups where tea is poured first is *a+c* and the lady classifies *a+b* of them as tea first. Ideally, if she can taste the difference, the counts *b* and *c* should be small. On the other hand, if she can't really tell, we would expect *a* and *c* to be about the same.

  * Suppose now that to test the lady, 8 cups of tea are prepared, 4 tea first, 4 milk first, and she is informed of the design (that there are 4 cups milk first and 4 cups tea first). Suppose also that the cups are presented to her in random order. Her task then is to identify the 4 cups milk first and 4 cups tea first.

    This design fixes the row and column totals in the table above to be 4 each. That is,

    .. math::

      a + b = a + c = c + d = b + d =4.

    With these constraints, when any one of *a, b, c, d* is specified, the remaining three are uniquely determined:

    .. math::

      b =4 - a,\, c =4 - a, \textrm{ and } d = a

    In general, for this design, no matter how many cups (*n*) are served, the row total *a + b* will equal *a + c* because the subject knows how many of the cups are "tea first" (or one kind as supposed to the other). So once *a* is given, the other three counts are specified.

  * We can test the discriminating skill of the lady, if any, by randomizing the order of the cups served. If we take the position that she has no discriminating skill, then the randomization of the order makes the 4 cups chosen by her as tea first equally likely to be any 4 of the 8 cups served. There are :math:`\left( {\begin{array}{*{20}{c}} 8\\ 4 \end{array}} \right) = 70` (in Python, choose *scipy.misc.comb(8,4,exact=True)*) possible ways to classify 4 of the 8 cups as "tea first". If the subject has no ability to discriminate between two preparations, then by the randomization, each of these 70 ways is equally likely. Only one of 70 ways leads to a completely correct classification. So someone with no discriminating skill has 1/70 chance of making no errors.

  \item It turns out that, if we assume that she has no discriminating skill, the number of correct classifications of tea first ("a" in the table) has a "hypergeometric" probability distribution (\lstinline{hd=stats.hypergeom(8,4,4)} in Python). There are 5 possibilities: 0, 1, 2, 3, 4 for *a* and the corresponding probabilities (and Python commands for computing the probabilities) are tabulated below.

+---------------+------------+-------------+
| Number of     | Python     | Probability |
| correct calls | command    | 0           |
+===============+============+=============+
|     0         | hd.pmf(0)  | 1/70        |
+---------------+------------+-------------+
|     1         | hd.pmf(1)  | 16/70       |
+---------------+------------+-------------+
|     2         | hd.pmf(2)  | 36/70       |
+---------------+------------+-------------+
|     3         | hd.pmf(3)  | 16/70       |
+---------------+------------+-------------+
|     4         | hd.pmf(4)  | 1/70        |
+---------------+------------+-------------+

  * With these probabilities, we can compute the p-value for the test of the hypothesis that the lady cannot tell between the two preparations. Recall that the p-value is the probability of observing a result as extreme or more extreme than the observed result assuming the null hypothesis. If she makes all correct calls,the p-value is 1/70 and if she makes one error (3 correct calls) then the p-value is 1/70 + 16/70 ~ 0.24.

  	
The test described above is known as "Fisher's exact test."

McNemar's Test
~~~~~~~~~~~~~~

Although the McNemar test bears a superficial resemblance to a test of
categorical association, as might be performed by a 2x2 chi-square test or
a 2x2 Fisher exact probability test, it is doing something quite different.
The test of association examines the relationship that exists among the
cells of the table. The McNemar test examines the difference between the
proportions that derive from the marginal sums of the table (see Table below):
:math:`p_A=(a+b)/N` and :math:`p_B=(a+c)/N`. The question in the McNemar
test is: do these two proportions, :math:`p_A` and :math:`p_B`,
significantly differ? And the answer it receives must take into account the
fact that the two proportions are not independent. The correlation of
:math:`p_A` and :math:`p_B` is occasioned by the fact that both include the
quantity a in the upper left cell of the table.


+--------+-------+-------+-----------+
|        | B     | B     |           |
|        | 1     | 0     | *Totals*  |
+========+=======+=======+===========+
| A   1  | a     | b     | a+b       |
+--------+-------+-------+-----------+
| A   0  | c     | d     | c+d       |
+--------+-------+-------+-----------+
| Totals | a+c   | b+d   | a+b+c+d=N |
+--------+-------+-------+-----------+

*General Structure of 2x2 Frequency Tables*

McNemar's test can be used for example in studies in which patients serve as
their own control, or in studies with "before and after" design.

Example
^^^^^^^

In the following example, a researcher attempts to determine if a drug has an effect on a particular disease. Counts of individuals are given in the table, with the diagnosis (disease: present or absent) before treatment given in the rows, and the diagnosis after treatment in the columns. The test requires the same subjects to be included in the before-and-after measurements (matched pairs).

+-----------------+------------------+-----------------+-----------+
|                 | After: present   | After: absent   | Row total |
+=================+==================+=================+===========+
| Before: present | 101              | 121             | 222       |
+-----------------+------------------+-----------------+-----------+
| Before: absent  |  59              |  33             |  92       |
+-----------------+------------------+-----------------+-----------+
| Column total    | 160              | 154             | 314       |
+-----------------+------------------+-----------------+-----------+

*McNemar's Test: example*


In this example, the null hypothesis of "marginal homogeneity" would mean there
was no effect of the treatment. From the above data, the McNemar test statistic
with Yates's continuity correction is

The general solution for the McNemar's test is

.. math::    \chi^2 = {(|b-c|-correctionFactor)^2 \over b+c}.

For small number of sample numbers the \emph{correctionFactor} should be 0.5
(*Yates's correction*) or 1.0 (*Edward's correction*). (For :math:`b + c < 25`,
the binomial calculation should be performed, and indeed, most software
packages simply perform the binomial calculation in all cases, since the
result then is an exact test in all cases.) Using Yates's correction, we
get

.. math::     \chi^2 = {(|121 - 59| - 0.5)^2 \over {121 + 59}}

has the value 21.01, which is extremely unlikely from the distribution implied by
the null hypothesis. Thus the test provides strong evidence to reject the null
hypothesis of no treatment effect.


Cochran's Q Test
~~~~~~~~~~~~~~~~

Cochran's Q test is a hypothesis test where the response variable can take
only two possible outcomes (coded as 0 and 1). It is a non-parametric
statistical test to verify if k treatments have identical effects. Cochran's
Q test should not be confused with *Cochran's C test*, which is a variance
outlier test.

Example
^^^^^^^

12 subjects are asked to perform 3 tasks. The outcome of each task is
*success* or *failure*. The results are coded *0* for *failure* and *1* for
*success*. In the example, subject 1 was successful in task 2, but failed
tasks 1 and 3 (see Table).


+--------+--------+--------+--------+
| Subject| Task 1 | Task 2 | Task 3 |
+========+========+========+========+
| 1      | 0      | 1      | 0      |
+--------+--------+--------+--------+
| 2      | 1      | 1      | 0      |
+--------+--------+--------+--------+
| 3      | 1      | 1      | 1      |
+--------+--------+--------+--------+
| 4      | 0      | 0      | 0      |
+--------+--------+--------+--------+
| 5      | 1      | 0      | 0      |
+--------+--------+--------+--------+
| 6      | 0      | 1      | 1      |
+--------+--------+--------+--------+
| 7      | 0      | 0      | 0      |
+--------+--------+--------+--------+
| 8      | 1      | 1      | 0      |
+--------+--------+--------+--------+
| 9      | 0      | 1      | 0      |
+--------+--------+--------+--------+
| 10     | 0      | 1      | 0      |
+--------+--------+--------+--------+
| 11     | 0      | 1      | 0      |
+--------+--------+--------+--------+
| 12     | 0      | 1      | 0      |
+--------+--------+--------+--------+

*Cochran's Q Test: Success or failure for 12 subjects on 3 tasks*

The null hypothesis for the Cochran's Q test is that there are no
differences between the variables. If the calculated probability *p* is
below the selected significance level, the null-hypothesis is rejected, and
it can be concluded that the proportions in at least 2 of the variables are
significantly different from each other. For our example, the analysis of
the data provides *Cochran's Q = 8.6667* and a significance of *p = 0.013*.
In other words, at least one of the three Tasks is easier or harder than the
others.

Analysis Programs
-----------------

With computers, the computational steps are trivial

|ipynb| `70_compGroups.ipynb <http://nbviewer.ipython.org/url/raw.github.com/thomas-haslwanter/statsintro/master/ipynb/70_compGroups.ipynb>`_

|python| `compGroups.py <https://github.com/thomas-haslwanter/statsintro/blob/master/Code3/compGroups.py>`_


Exercises
---------

Fisher's Exact Test - The Tea Experiment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At a party, a lady claimed to be able to tell whether the tea or the
milk was added first to a cup. Fisher proposed to give her eight cups,
four of each variety, in random order. One could then ask what the
probability was for her getting the number she got correct, but just by
chance.

The experiment provided the Lady with 8 randomly ordered cups of tea - 4
prepared by first adding milk, 4 prepared by first adding the tea. She
was to select the 4 cups prepared by one method. (This offered the Lady
the advantage of judging cups by comparison.)

The null hypothesis was that the Lady had no such ability.

 * Calculate if the claim of the lady is supported if she gets three out of
    the four pairs correct. (Correct answer: No. If she gets three correct,
    that chance that a selection of "three or greater" was random is 0.243.
    She needs to get all four correct, if we set the rejection threshold at
    0.05)


Chi2 Contingency Test (1 DOF)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A test of the effect of a new drug on the heart rate has yielded the following results for
the heart rate (HR):

+---------------+------------------+---------------------+-----------+
|               | *HR increased*   | *HR not increased*  | *Total*   |
+===============+==================+=====================+===========+
| *treated*     | 36               | 14                  | 50        |
+---------------+------------------+---------------------+-----------+
| *not treated* | 30               | 25                  | 55        |
+---------------+------------------+---------------------+-----------+
| *Total*       | 66               | 39                  | 105       |
+---------------+------------------+---------------------+-----------+

  * Does the drug affect the heart rate?
    (Correct answer: no)
  * What would be the result if the response in one of the not-treated persons would have been different? Perform this test with and without the Yates-correction.
      (Correct anwer: without Yates correction: yes, p=0.042; with Yates correction: no, p=0.067)

+---------------+------------------+---------------------+-----------+
|               | *HR increased*   | *HR not increased*  | *Total*   |
+===============+==================+=====================+===========+
| *treated*     | 36               | 14                  | 50        |
+---------------+------------------+---------------------+-----------+
| *not treated* | 29               | 26                  | 55        |
+---------------+------------------+---------------------+-----------+
| *Total*       | 65               | 40                  | 105       |
+---------------+------------------+---------------------+-----------+

One way Chi2-Test (>1 DOF)
~~~~~~~~~~~~~~~~~~~~~~~~~~

The city of Linz wants to know if people want to build a long beach along the Danube. They interview local people, and decide to collect 20 responses from each of the five age groups: (<15, 15-30, 30-45, 45-60, >60)

The questionnaire states: *"A beachside development will benefit Linz."*

and the possible answers are

      * Strongly agree 
      * Agree 
      * Disagree 
      * Strongly Disagree 

The city council wants to find out if the age of people influenced feelings about the development, particularly of those who felt negatively (i.e. "disagreed" or "strongly disagreed") about the planned development.

    * <15:  4
    * 15-30:    6
    * 30-45:    14
    * 45-60:    10
    * >60:  16

The categories seem to show large differences of opinion between the groups.

  * Are these differences significant?
    (Correct answer: yes, p=0.034)
  * How many degrees of freedom does the resulting analysis have?
    (Correct answer: 4)


McNemar's Test
~~~~~~~~~~~~~~

In a lawsuit regarding a murder the defense uses a questionnaire to show that the defendant is insane. As a result of the questionnaire, the accused claims "not guilty by reason of insanity".

In return, the state attorney wants to show that the questionnaire does not work. He hires an experienced neurologist, and presents him with 40 patients, 20 of whom have completed the questionnaire with an "insane" result, and 20 with a "sane" result. When examined by the neurologist, the result is mixed: 19 of the "sane" people are found sane, but 6 of the 20 "insane" people are labelled as sane by the expert.

+-----------+------------------+---------------------+-----------+
|           | *sane by expert* | *insane by epxpert* | *Total*   |
+===========+==================+=====================+===========+
| *sane*    | 19               | 1                   | 20        |
+-----------+------------------+---------------------+-----------+
| *insane*  | 6                | 14                  | 20        |
+-----------+------------------+---------------------+-----------+
| *Total*   | 22               | 18                  | 40        |
+-----------+------------------+---------------------+-----------+

  * Is this result significantly different from the questionnaire? (Correct answer: no)
  *  Would the result be significantly different, if the expert had diagnosed all "sane" people as sane? (Correct answer: yes)

.. |ipynb| image:: ../Images/IPython.jpg
    :scale: 50 % 
.. |python| image:: ../Images/python.jpg
    :scale: 50 % 
