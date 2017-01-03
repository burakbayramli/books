def mean(nums):
    bot = len(nums)
    print "Still Running at line 3"
    it=0
    top=0
    while it < len(nums):
        top += nums[it]
    return float(top)/float(bot)

a_list = [1,2,3,4,5,6,10,"muffin"]
mean(a_list)
