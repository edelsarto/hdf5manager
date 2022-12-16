# Matlab version
# fileID=fopen("./TF01_strip/stripf32_TF01.txt",'r');
# % data = textscan(fileID,'%s %*[^\n]');
# data = textscan(fileID,'%s','Delimiter','\n');
# fclose('all');
# stringData = string(data{:});
# for jjj=1:length(stringData)
#     splitted_string(jjj,:)=split(stringData(jjj));
#     if jjj>1
#         for kkk=1:length( splitted_string(jjj,:))
#             value(jjj-1,kkk)=str2num(splitted_string(jjj,kkk));
#         end
#     end
# end

import numpy as np

file = open("documents\SS_NOS_TES_TAS_v2_(0.76mg_s)_strip", "r")

#readFile = file.read()

data = np.loadtxt(file, delimiter='\n', dtype=np.dtype("str"))

file.close()

# print("---------------------------------")
# print(len(data))
# print("---------------------------------")

stringData = str(data)
val = []
for i in range(len(stringData)):
    splitted_string = stringData[i].split(" ")
    print(splitted_string)
    if i > 1:
        for j in range(len(splitted_string)):
            val.append(splitted_string[j])
            break

print("########################################")
#print(val)
print("########################################")
