import csv

# changeable constants
interval = 1000
fileName = "howToWinFriendsAndInfluencePeople.txt"

# results
frequencies = []

# read the book and count the number of e's
with open(fileName) as f:
  charCounter = 0
  currentCount = 0
  while True:
    c = f.read(1)
    if c:
      if ('a' <= c <= 'z') or ('A' <= c <= 'Z'):
        charCounter += 1
        if c == 'e' or c == 'E':
          currentCount += 1
        if charCounter >= interval:
          frequencies.append(["%d-%d" % (len(frequencies) * interval, (len(frequencies) + 1) * interval - 1),
                              currentCount])  # store the count in this interval
          charCounter = 0  # reset counter once reached interval
          currentCount = 0  # then reset to zero as well
    else:
      print("File is done reading")
      break

# file is done reading, convert list to csv
frequencies.insert(0, ['Range', 'Frequency'])
with open('../lib/numberOfE.csv', 'w', newline='') as file:
  writer = csv.writer(file)
  writer.writerows(frequencies)
