class node:
    def __init__(self, nameValue, numOccur, parent):
        self.name = nameValue
        self.count = numOccur
        self.nodeLink = None
        self.parent = parent  #needs to be updated
        self.children = {} 
    
    def inc(self, numOccur):
        self.count += numOccur
        
    def disp(self, ind=1):
        print '  '*ind, self.name, ' ', self.count
        for child in self.children.values():
            child.disp(ind+1)

#create FP-tree from dataset but don't mine            
def create_tree(dataSet, minSup=1): 
    header_table = {}
    #go over dataSet twice
    for trans in dataSet:#first pass counts frequency of occurance
        for item in trans:
            header_table[item] = header_table.get(item, 0) + dataSet[trans]
    for k in header_table.keys():  #remove items not meeting minSup
        if header_table[k] < minSup: 
            del(header_table[k])
    freqItemSet = set(header_table.keys())
    #if no items meet min support -->get out    
    if len(freqItemSet) == 0: return None, None  
    for k in header_table:
        #reformat header_table to use Node link 
        header_table[k] = [header_table[k], None] 
    retTree = node('Null Set', 1, None) #create tree
    for tranSet, count in dataSet.items():  #go through dataset 2nd time
        localD = {}
        for item in tranSet:  #put transaction items in order
            if item in freqItemSet:
                localD[item] = header_table[item][0]
        if len(localD) > 0:
            orderedItems = [v[0] for v in sorted(localD.items(), key=lambda p: p[1], reverse=True)]
            #populate tree with ordered freq itemset
            update_tree(orderedItems, retTree, header_table, count)
    return retTree, header_table #return tree and header table

def update_tree(items, inTree, header_table, count):
    #check if orderedItems[0] in retTree.children
    if items[0] in inTree.children:
        inTree.children[items[0]].inc(count) #increment count
    else:   #add items[0] to inTree.children
        inTree.children[items[0]] = node(items[0], count, inTree)
        if header_table[items[0]][1] == None: #update header table 
            header_table[items[0]][1] = inTree.children[items[0]]
        else:
            update_header(header_table[items[0]][1], inTree.children[items[0]])
    #call update_tree() with remaining ordered items
    if len(items) > 1:
        update_tree(items[1::], inTree.children[items[0]], header_table, count)
        
def update_header(nodeToTest, targetNode):
    #This version does not use recursion
    #Do not use recursion to traverse a linked list!
    while (nodeToTest.nodeLink != None): 
        nodeToTest = nodeToTest.nodeLink
    nodeToTest.nodeLink = targetNode

def ascend_tree(leafNode, prefixPath): #ascends from leaf node to root
    if leafNode.parent != None:
        prefixPath.append(leafNode.name)
        ascend_tree(leafNode.parent, prefixPath)
    
def find_pre_path(basePat, node): #node comes from header table
    condPats = {}
    while node != None:
        prefixPath = []
        ascend_tree(node, prefixPath)
        if len(prefixPath) > 1: 
            condPats[frozenset(prefixPath[1:])] = node.count
        node = node.nodeLink
    return condPats

def mine_tree(inTree, header_table, minSup, preFix, freqItemList):
    print 'minSup',minSup
    #(sort header table)
    bigL = [v[0] for v in sorted(header_table.items(), key=lambda p: p[1])]
    for base_pattern in bigL:  #start from bottom of header table
        print 'base_pattern',base_pattern
        newFreqSet = preFix.copy()
        newFreqSet.add(base_pattern)
        freqItemList.append(newFreqSet)
        cond_pattern_bases = find_pre_path(base_pattern, header_table[base_pattern][1])
        print 'cond_pattern_bases',cond_pattern_bases
        #2. construct cond FP-tree from cond. pattern base
        myCondTree, myHead = create_tree(cond_pattern_bases, minSup)
        if myHead != None: #3. mine cond. FP-tree
            print 'conditional tree for: ',newFreqSet
            myCondTree.disp(1)            
            mine_tree(myCondTree, myHead, minSup, newFreqSet, freqItemList)
    
def load_ment():
    data = [
        ['outlook=sunny', 'temparature=hot', 'humidity=high', 'windy=false', 'play=no'],
        ['outlook=sunny', 'temparature=hot', 'humidity=high', 'windy=true', 'play=no'],
        ['outlook=overcast', 'temparature=hot', 'humidity=high', 'windy=false', 'play=yes'],
        ['outlook=rainy', 'temparature=mild', 'humidity=high', 'windy=false', 'play=yes'],
        ['outlook=rainy', 'temparature=cool', 'humidity=normal', 'windy=false', 'play=yes'],
        ['outlook=rainy', 'temparature=cool', 'humidity=normal', 'windy=true', 'play=no'],
        ['outlook=overcast', 'temparature=cool', 'humidity=normal', 'windy=true', 'play=yes'],
        ['outlook=sunny', 'temparature=mild', 'humidity=high', 'windy=false', 'play=no'],
        ['outlook=sunny', 'temparature=cool', 'humidity=normal', 'windy=false', 'play=yes'],
        ['outlook=rainy', 'temparature=mild', 'humidity=normal', 'windy=false', 'play=yes'],
        ['outlook=sunny', 'temparature=mild', 'humidity=normal', 'windy=true', 'play=yes'],
        ['outlook=overcast', 'temparature=mild', 'humidity=high', 'windy=true', 'play=yes'],
        ['outlook=overcast', 'temparature=hot', 'humidity=normal', 'windy=false', 'play=yes'],
        ['outlook=rainy', 'temparature=mild', 'humidity=high', 'windy=true', 'play=no']
        ]
    return data

def create_init_set(dataSet):
    retDict = {}
    for trans in dataSet:
        retDict[frozenset(trans)] = 1
    return retDict

def load_simp_dat():
    data = [
            ['z'],
            ['r', 'z', 'h', 'j', 'p'],
            ['z', 'y', 'x', 'w', 'v', 'u', 't', 's'],
            ['r', 'x', 'n', 'o', 's'],
            ['y', 'r', 'x', 'z', 'q', 't', 'p'],
            ['y', 'z', 'x', 'e', 'q', 's', 't', 'm']]
    return data

def create_init_set(dataSet):
    retDict = {}
    for trans in dataSet:
        retDict[frozenset(trans)] = 1
    return retDict

#data = load_simp_dat()
data = load_ment()

init_set = create_init_set(data)

tree, header_tab = create_tree(init_set, 6)
tree.disp()

items = []
mine_tree(tree, header_tab, 6, set([]), items)
for x in items:
    if len(x) > 1: print x
