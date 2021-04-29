import numpy as np
mat = np.genfromtxt("before_inv.csv", delimiter = ",")
print(mat)

inv=np.linalg.inv(mat)
np.savetxt("after_inv.csv", inv, delimiter=",")

