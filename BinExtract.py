import numpy as np

#file = open("C:\Users\Andrea\Desktop\HDF5ManagerGithub\documents\SS_BT12_SS30.bin", "rb")
# file = np.fromfile("C:/Users/Andrea/Desktop/HDF5ManagerGithub/documents/TBM_Relief_0_5m3_e1_area_v1.rst")

# text = ""
# for i in file:
#     text += str(i)
#     print(i)

file = open("C:/Users/Andrea/Desktop/HDF5ManagerGithub/documents/TBM_Relief_0_5m3_e1_area_v1.rst", mode="rb")
binary_data = file.read()
print(binary_data)

for i in binary_data:
    print(i)
# text = ""
# for i in binary_data:
#     text += str(i)
#     print(i)

# file = open("C:/Users/Andrea/Desktop/HDF5ManagerGithub/documents/text.txt", "w") 
# file.write(text) 
# file.close()
