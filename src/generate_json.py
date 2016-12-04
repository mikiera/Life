# generate_json.py
# Generates pieces of a game json

def generate_map(begin, end):
  """Prints empty map squares from square with id begin to square with id end"""
  for i in range(begin, end + 1):
    print "{ \"squareid\": " + str(i) + ",\n  \"left\": 0,\n  \"right\": \n},"

def generate_square(begin, end):
  """Prints empty map squares from square with id begin to square with id end"""
  for i in range(begin, end + 1):
    print ("{ \"squareid\": " + str(i) + ",\n  \"action\": \n  {\n  \"type\":" +
    " \"event\",\n  \"description\": \"\",\n  \"points\": 0,\n  \"karma\": 0,"
    + "\n  \"optdict\": []\n  }\n}")

