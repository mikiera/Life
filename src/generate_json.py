# generate_json.py
# Generates pieces of a game json

def generate_map(begin, end):
  """Prints empty map squares from square with id begin to square with id end"""
  for i in range(begin, end + 1):
    print "{ \"squareid\": " + str(i) + ",\n  \"left\": ,\n  \"right\": \n},"
