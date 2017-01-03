
def mean(num_list):
    if len(num_list) == 0 :
      raise Exception("The algebraic mean of an empty list is undefined. \
      Please provide a list of numbers")
    else :
      return sum(num_list)/len(num_list)
