import numpy as np
from save_csv import save_csv

file_name = "hurdat2-nepac-1949-2017-050418.txt"
hurricanes = list()
with open(file_name) as f:
    index = -1
    hurr_meta = list()
    for l in  f.readlines():
        cs = [c.strip() for c in l.split(',')]
        cs = list(filter(lambda x: True if x != '\n' else False, cs))
        if (cs[0][0].isalpha()):
            hurr_meta = cs[0:-1] # index, name, length
        else:
            hurricanes.append(hurr_meta)
            index += 1
            cs[4] = float(cs[4][0:-1]) if cs[4][-1] == 'N' else -float(cs[4][0:-1])
            cs[5] = float(cs[5][0:-1]) if cs[5][-1] == 'N' else -float(cs[5][0:-1])
            hurricanes[index] = hurricanes[index] + cs[0:-1]
            print("hurricane {0}, time {1}".format(hurricanes[index][1], index))
        pass
    pass

save_csv("hurricanes_nepac.csv", hurricanes)
