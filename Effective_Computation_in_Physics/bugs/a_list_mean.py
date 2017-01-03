import pdb
def mean(nums):
    top = sum(nums)
    bot = len(nums)
    return float(top)/float(bot)


if __name__ == "__main__":
    pdb.set_trace()
    a_list = [1,2,3,4,5,6,10,"muffin"]
    result=mean(a_list)
    print result
