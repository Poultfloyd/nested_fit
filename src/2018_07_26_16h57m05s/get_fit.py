import os
import glob
import numpy as np
import matplotlib.pyplot as plt


src = "/users/jussieu/lduval/DCS/b-like_sulfur/analyzed_no_cut/slice_sampling/5P/2018_07_27_16h54m56s"
fit_folder = src + ""
spectrum_num = ""
output = src
datatype = "/"
nb_peak = 1
mod = "max"
linestyle = {
    "markeredgewidth": 1,
    "elinewidth": 1,
    "capsize": 1,
    "markersize": 1,
    "linewidth": 0,
}


directories_peaks = glob.glob(fit_folder + datatype + "*")


energy_dir, filename = [], []


for i in energy_dir:
    print(i)
    for j in filename:
        path = i + "/" + j + "/nf_output_data_max.dat"
        if not os.path.isfile(path):
            continue
        data = np.loadtxt(path)
        x = data[:, 0]
        y = data[:, 1]
        yerr = data[:, 4]
        plt.errorbar(x, y, yerr=yerr, **linestyle, color="red")

        path = i + "/" + j + "/nf_output_fit_max.dat"
        print(j)
        if not os.path.isfile(path):
            continue

        fit = np.loadtxt(path, skiprows=5)
        x = fit[:, 0]
        y = fit[:, 1]
        bkg = fit[:, -1]
        plt.plot(x, y, color="blue")
        for peak_index in range(2, 5):

            y = fit[:, peak_index]
            y = np.add(y, bkg)
            plt.plot(x, y)

        plt.savefig(output + "base" + "_run_" + str(map[j]) + ".png")
        plt.savefig(output + "base" + "_run_" + str(map[j]) + ".pdf")
        plt.clf()

energy_dir, filename = [], []


for i in directories_peaks:
    energy_dir.append(os.path.split(i)[0])
    filename.append(os.path.split(i)[1])

energy_dir = set(energy_dir)
filename = set(filename)


path = "./nf_output_data_max" + spectrum_num + ".dat"

data = np.loadtxt(path)
x = data[:, 0]
y = data[:, 1]
yerr = data[:, 4]
plt.errorbar(x, y, yerr=yerr, **linestyle, color="red")

path = "./nf_output_fit_" + mod + spectrum_num + ".dat"
print(path)
fit = np.loadtxt(path, skiprows=5)
x = fit[:, 0]
y = fit[:, 1]
plt.plot(x, y, color="blue")

bkg = fit[:, -1]
plt.plot(x, bkg)
for peak_index in range(2, 6 + nb_peak):
    print("nb_peak=", nb_peak)
    y = fit[:, peak_index]
    y = np.add(y, bkg)
    plt.plot(x, y)
plt.show()
# plt.savefig(output + ".png")
# plt.savefig(output + ".pdf")
plt.clf()
