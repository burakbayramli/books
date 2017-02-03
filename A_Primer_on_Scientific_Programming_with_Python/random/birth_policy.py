import numpy as np
MALE = 1;  FEMALE = 2

def get_children(n, male_portion, fertility):
    if n == 0: return []
    n = int(fertility*n)  # not all n couples get a child
    r = np.random.random(n)
    children = np.zeros(n, int)
    children[r <  male_portion] = MALE
    children[r >= male_portion] = FEMALE
    return children
    
def advance_generation(parents, policy='one child',
                       male_portion=0.5, fertility=1.0,
                       law_breakers=0, wanted_children=4):
    """
    Given a generation of parents (random integers with
    values MALE or FEMALE), compute the next generation
    of children.
    Return: array of children (MALE and FEMALE values),
    and the maximum number of children found in a family.
    """
    males = len(parents[parents==MALE])
    females = len(parents) - males
    couples = min(males, females)

    if policy == 'one child':
        # Each couple gets one child.
        children = get_children(couples, male_portion, fertility)
        max_children = 1
    elif policy == 'one son':
        # Each couple can continue with a new child until 
        # they get a son.

        # First try.
        children = get_children(couples, male_portion, fertility)
        max_children = 1
        # Continue with getting a new child for each daughter.
        daughters = children[children == FEMALE]
        while len(daughters) > 0:
            new_children = get_children(len(daughters),
                                        male_portion, fertility)
            children = np.concatenate((children, new_children))
            daughters = new_children[new_children == FEMALE]
            max_children += 1
    # A portion law_breakers breaks the law and gets wanted_children.
    illegals = get_children(int(len(children)*law_breakers)*wanted_children,
                            male_portion, fertility=1.0)
    children = np.concatenate((children, illegals))
    return children, max_children

N = 1000000
male_portion = 0.51
fertility = 0.92
law_breakers = 0.06
wanted_children = 6

generations = 10
# Start with a "perfect" generation of parents.
start_parents = get_children(N, male_portion=0.5, fertility=1.0)
parents = start_parents.copy()
print 'one child policy, start: %d' % len(parents)
for i in range(generations):
    parents, mc = advance_generation(parents, 'one child',
                                     male_portion, fertility,
                                     law_breakers, wanted_children)
    print '%3d: %d' % (i+1, len(parents))

parents = start_parents.copy()
print 'one son policy, start: %d' % len(parents)
for i in range(generations):
    parents, mc = advance_generation(parents, 'one son',
                                     male_portion, fertility,
                                     law_breakers, wanted_children)
    print '%3d: %d (max children in a family: %d)' % \
          (i+1, len(parents), mc)

