
# coding: utf-8

# In[49]:

# Rename the files

import os

def rename(x):
    new = x.split('_')
    two = new[1].split('.txt')[0]
    return two + '_' + new[0] + '.txt'

directory = os.listdir("C:\\Users\\nathan.a.miller\\OneDrive - Accenture Federal Services\\USCIS\\Text\\Plagiarism Sample Text\\o+p")

for i in directory:
#     print(rename(i))
    os.rename("C:\\Users\\nathan.a.miller\\OneDrive - Accenture Federal Services\\USCIS\\Text\\Plagiarism Sample Text\\o+p\\" + i, "C:\\Users\\nathan.a.miller\\OneDrive - Accenture Federal Services\\USCIS\\Text\\Plagiarism Sample Text\\o+p\\" + rename(i))



# In[7]:

# Pre-processing functions

import re
import string
import time

regex = re.compile('[%s]' % re.escape(string.punctuation))
# test = 'a bc defg hij k && lmno pqr stuv \t wxy z \n abcd nate is the  ;best  '  

def re_remove_punct(s):  # From Vinko's solution, with fix.
    return regex.sub('', s)

def remove_spaces(s, extra = False):
    s = s.lower()
    s = re_remove_punct(s)
    s = " ".join(s.split())
    if extra == True:
        s = s.replace(' ','')
    return s

def create_shingled_features(s, k, clean = False, remove_all_spaces= False, print_to_file = False, res = 'result ' + time.strftime("%Y%m%d-%H%M%S") + '.txt'):
    output = []
    if clean == True:
        s = remove_spaces(s, extra = remove_all_spaces)
    f = open(res, 'a')
    while True:
        result = (s[:k], s[1:])
        s = result[1]
        if result[0] == '':
            break
        if print_to_file == True:
            f.write(result[0] + '\n')
        else:
            output.append(result[0])
    f.close()
    if print_to_file == True:
        print("File printed to '" + res + "'")
    else:
        return output


# In[58]:

# Pre-process and Simhash all the files, place into a dictionary called "all_simhasht"

from simhash import Simhash, SimhashIndex
from collections import OrderedDict
### Create hamming distance matrix

#### import Shari's plagiarized samples

import glob
path="C:\\Users\\nathan.a.miller\\OneDrive - Accenture Federal Services\\USCIS\\Text\\Plagiarism Sample Text\\o+p\\*.txt"
chop = "C:\\Users\\nathan.a.miller\\OneDrive - Accenture Federal Services\\USCIS\\Text\\Plagiarism Sample Text\\o+p\\"

all_dict = {} #store Shari's sample plagiarism files here

for filename in glob.iglob(path):
    chopt = filename.replace(chop,'')
    with open(filename, encoding="ISO-8859-1") as f:
        all_dict[chopt] = f.read()
    
all_dict = OrderedDict(sorted(all_dict.items(), key=lambda t: t[0]))    

keys = list(all_dict.keys())

### Create the index of simhashed files

objs = [(str(k), Simhash(create_shingled_features(s, 3, clean=True))) for k, s in all_dict.items()]
index = SimhashIndex(objs, k=3)

all_simhasht = {}

for i in objs:
    all_simhasht[i[0]] = i[1]
    
all_simhasht = OrderedDict(sorted(all_simhasht.items(), key=lambda t: t[0]))    


# In[67]:

# Calculate Hamming Distances for each simhash'd file and input into a matrix

import numpy as np
import pandas as pd

# np.set_printoptions(threshold=np.nan)
distances = np.empty(len(all_simhasht))

for i in all_simhasht:
    m = []
    for j in all_simhasht:
        m.append(all_simhasht[j].distance(all_simhasht[i]))
    distances = np.vstack([distances, m])
    
print(all_simhasht.keys())    
distances = np.delete(distances, (0), axis=0) # for some reason, the first row is all 0s

df = pd.DataFrame(distances)
df.astype(int)


# In[63]:

# Write to an Excel File

writer = pd.ExcelWriter('plagiarism.xlsx')
df.to_excel(writer,'Sheet1')
writer.save()


# In[ ]:

import re
import string
import time
import numpy as np
import pandas as pd
from simhash import Simhash, SimhashIndex
from collections import OrderedDict
import glob

