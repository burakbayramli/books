@profile
def mean(nums):
    top = sum(nums)
    bot = len(nums)
    return float(top)/float(bot)

if __name__ == "__main__":
    a_list = [1, 2, 3, 4, 5, 6, 10, 100]
    result = mean(a_list)
    print result
