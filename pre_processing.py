import os
import sys
import csv
import re
import string
import nltk




data_arr=[]
Num_lines=0
i = 0
j = 0

sum_x=0
coloums=[]
file1 = open("reutersCSV.csv","r") # read the data file
for line in file1:                  # read the one line each time
	Num_lines+=1                   # count numbers of line
print	
print 'The line number: ',Num_lines     
file1.close


#######remove rows that all attributes equals to 0#####
row_del=0;
with open('reutersCSV.csv') as f:
    f_csv = csv.reader(f)
    headers = next(f_csv)
    for row in f_csv:
      for i in range(3,138):
        row_del=(int (row[i]))+ row_del
      #print row_del
      if (row_del != 0):
        #if row[2]!="not-used":
          data_arr.append(row)
      row_del=0

print "#######remove rows that all attributes equals to 0 #####" 
#print  len(data_arr)
#print len(data_arr[1])




#######remove coloums that all elements equals to 0#####
#print data_arr[1][0]
del_col =[]
for j in range (3,138):
  for i in range (0,len(data_arr)):
  
    sum_x = sum_x +(int(data_arr[i][j]))
    #print sum_x
  if sum_x == 0:
    #print j
    del_col.append(j)

  sum_x =0
    #data_arr.remove(i)

del_col.reverse()
for x in range(0,len(data_arr)):
  for y in del_col:
    del data_arr[x][y]
print "#######remove coloums that all elements equals to 0 #####" 


for y in del_col:
  del headers[y]
#print len(headers)
#print len(data_arr[1])

not_used =[]
for x in  range (1,len(data_arr)):
  if data_arr[x][2] =="not-used":
    not_used.append(x)
#print len(not_used)

not_used.reverse()
for q in not_used:
    del data_arr[q]
print "#######remove not-used #####" 

print "Final result: "
print "Number of rows = ",len(data_arr)
print "Number of coloums = ",len(data_arr[1])

df_file = file('cleaned_data.csv','wb')
writer = csv.writer(df_file)
writer.writerow(headers)
for item in data_arr:
    writer.writerow(item)
df_file.close()



