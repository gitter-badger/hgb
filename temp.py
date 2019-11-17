lines = open("temp.txt").read().split('\n')
ret = []
for line in lines[:-1]:
    line = line.split(" ")
    ret.append((line[0], line[2]))
print(ret)
