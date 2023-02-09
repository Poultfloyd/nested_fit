import numpy as np
import matplotlib.pyplot as plt


a = np.loadtxt("convo.dat", skiprows=21)
b = np.loadtxt("to_interpolate_1.dat")

a[:, 1] = a[:, 1] * 1.01 * max(b[:, 1]) / max(a[:, 1])
a[:, 2] = a[:, 2] * 1.01 * max(b[:, 1]) / max(a[:, 2])
plt.plot(a[:, 0], a[:, 1])
plt.plot(b[:, 0], b[:, 1])
plt.plot(a[:, 0], a[:, 2])
plt.show()