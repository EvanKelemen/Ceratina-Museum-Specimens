# Variables
# Interger
x = 10 #interger
y = 5.5 #float (includes decimal)
z = 3j #complex number

print(x)
type(x)

# convert types
float(x)
int(x)
str(x)

#string does not always convert from interger
a = '5'
a = int(a)

# List
ls_1 = [5, 10, 20]
ls_1 = list([5, 10, 20])
ls_2 = [5, 5.5, 'abc']
ls_3 = [5, ls_1, [1,2,3], 'abc']

#indexing (in python starts a 0
ls_1[1]

# assign a new position
ls_1[1] = 12

#For loop
for item in ls_1:
    print(item)

colorlist = ['Red', 'Blue', 'Green']
for index, color in enumerate(colorlist):
    print('Color {} is in index {}' .format(index, color))

colorlist.index('Red')

colorlist.append('Purple')
colorlist

len()

# Dictionaries
adict = {"Red":3, "Blue":4, "Green":5}
adict['Red']
adict.keys()
adict.values()

addict2 = dict(Red=3, Blue=4, Green=5)
# Changing values
addict2['Red'] = 1
# can add
addict2['Purple'] = 16

for color in addict2:
    print(color)

for color in addict2:
    print(addict2[color])

# {} string formating .format passes the values to the backets could also pass directly by putting in , key, 'asdfa', value
for key, value in addict2.items():
    print('the key is', key, 'and the value', value)

# Tuples
# cannot modify Tuples
# inclosed in normal ()

red = ('Apple', "Pear", "Strawberry")
# have to modify to a list than list
aredlist = list(red)
aredlist[0] = 'Cherry'
red = tuple(aredlist)
red

#Boolean
# First letter has to be capitolized

a = 10 # 0 if statement would read it as false
if a:
    print(a)

# == equals; >= greater than or equal too != not equal to
# checks if both exist 'and' 'or'
if (a and b):
    print(b)

# can test for memebership
a = 'this is not a very good example'
b = 'example'
if b in a:
    print(a)

#if ...:
#else:
#    if:
#    elif:
#    else:

# Make a dictionary in tuples
fruits = ('Apple', 'Banana', 'Pear')
color = ('red', 'yellow', 'green')
dict(zip(fruits, color))

# Loops
range(10) # all intergers between 0 and 10 not including 10
range(3, 10) # not including 10
range(2, 10, 2) # third argument is steps

# while loop
current_number = 0
while current_number < 10:
    print("Current number is", current_number)
    current_number += 1 # += a way to add to a variable
    if current_number > 10:
        break # in cases where you need to to break

import time

rain = True
look = 0
while rain:
    print('It is raining, staying inside')
    time.sleep(0.5)
    if look > 10:
        break
    look +=1
print( 'It is not raining, going outside')

# Functions
def fahr_to_celsius(temp):
    return((temp - 32)* (5/9))

# Scope and Lifetime variables
def my_fucn()

#Base Problem
genes = ['COI', 'Cytb', '12S', '18S', 'IgA', 'A1BG', 'ZZZ3', 'GDF6', 'CNN1', 'MKNK1']
spx_counts = [1000, 1000, 5000, 500, 54, 35, 564, 6, 45, 68]
spy_counts = [1050, 1050, 3452, 315, 45, 45, 345, 5, 78, 15]
new_entry = ('ALPI', 789)
new_entry = list(new_entry)
new_entry[0] = 'ALPG'
new_entry = tuple(new_entry)
dx = {} # Empty dictionaries to start
dy = dict() # This is also a empty dictionary but with the constructor
for index, gene in enumerate(genes):
    # we could filter here ...
    dx[genes[index]] = spx_counts[index]
    dy[genes[index]] = spy_counts[index]
dx[new_entry[0]] = new_entry[1]
big_dict = {'x': dx, 'y': dy}
for sp, dictionary in big_dict.items():
    # another way to format strings works similar to {} for python 3, but for python 2. Note %s (string) %d integer
    print('The count of CNN1 in species %s is %d' % (sp, dictionary['CNN1']))

filtered = dict(x={}, y=dy)
for gene, count in big_dict['x'].items():
    if count >= 100:
        filtered['x'].update({'gene':count}) # .update same as filtered['x'][gene] = count

# Extended problem
for gene, count in big_dict['x'].items():
    if 'C' in gene and count >= 500 and big_dict['y'][gene]  >= 500:
        print(gene)
    # another way to format strings works similar to {} for python 3, but for python 2. Note %s (string) %d integer
    print('The count of CNN1 in species %s is %d' % (sp, dictionary['CNN1']))
# or
for gene, count in big_dict['x'].items():
    if 'C' in gene and count >= 500:
        print('Species x', gene)
    if 'C' in gene and big_dict['y'][gene]  >= 500:
        print('Species y', gene)

# read -

# load spicy-stack to load pandas in graham.csv
#pip install pandas
f = open("demofile2.txt", "a")
f.write("Now the file has more content!")
f.close()

nano base_problem.py
import sys
program = sys.argv[0]
threshold = sys.argv[1]
outfn = sys.argv[2]
outfn2 = sys.argv[3]

welcome_msg = 'Welcome to {}. Your threshold is {} and the output file is {} and the second output is {}'
print(welcome_msg.format(program, threshold, outfn, outfn2))

with open('frequent_terms.csv') as infile:
        text = [line.strip().split(,) for line in infile]

least_frequent = sorted([float(x[1])] for x in text[1:])[11]
with open(outfn, 'w') as outfile, out(outfn2, 'w') as outfile2:
    for index, entry in enumerate(text):
        line = '{}\n'.format(','.join(entry))
        if index == 0:
            outfile1.write(line)
            outfile2.write(line)
        elif float(entry[1]) >= threshold and (entry[0].startswith('e') or entry[0].endswith('n')):
            outfile1.write(line)
            line = '{}\n'.float(','.join(entry))
            outfile.write(line)
To load pandas into
module spider scipy-stack


#!/bin/bash
#SBATCH --account=def-training-wa
#SBATCH --mem-per-cpu=800M
#SBATCH --time=00:10:))
module load python/3.6
virtualenv --no-download $SLURM_TMPDIR/msprime
source $SLURM_TMDIR/msprime/bin/activate
pip install --no-index --upgrade pip
pip install --no-index msprime
msp simulate -L 1000000 1000000 test.tsf






