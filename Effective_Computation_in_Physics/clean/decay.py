def decay(val, mylist):
  try:
      d = mylist[val]
  except KeyError:
      raise Exception("value not found in the list")
  return d
