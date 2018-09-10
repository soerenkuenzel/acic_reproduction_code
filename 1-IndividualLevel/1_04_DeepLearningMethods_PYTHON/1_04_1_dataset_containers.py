import csv
import numpy as np
from pathlib import Path
import copy
import os
from tensorflow.examples.tutorials.mnist import input_data
import matplotlib.pyplot as plt
from skimage.transform import rotate

# ----------------------------------------------------------------------------------------------------------------------
# Multiple outcome social pressure

class MSPData:
    def __init__(self):
        self.evds = Path('../exploration_validation_splitting.csv')
        self.dfile = Path('../../data/synthetic_data.csv')
        self.read_csv()

    def read_csv(self):
        # Read in the exploration/validation indices -------------------------------------------------------------------
        with open(self.evds, 'r') as fp:
            reader = csv.reader(fp, delimiter=',', quotechar='"')
            next(reader, None)  # skip the headers
            idx = []
            exploration_bl = []
            validation_bl = []

            for row in reader:
                idx.append(int(row[0]))
                exploration_bl.append(row[1] == 'TRUE')
                validation_bl.append(row[2] == 'TRUE')

            self.idx = np.array(idx)
            self.exploration_bl = np.array(exploration_bl)
            self.validation_bl = np.array(validation_bl)

        # Read in the data file ----------------------------------------------------------------------------------------
        with open(self.dfile, 'r') as fd:
            reader2 = csv.reader(fd, delimiter=',', quotechar='"')
            next(reader2, None)  # skip the headers
            next(reader2, None)  # empty row after the header

            feat = []
            w = []
            y = []

            for row in reader2:
                w.append(int(row[1]))
                y.append(float(row[2]))

                # features
                schoolid = int(row[0])
                S3 = int(row[3])
                C1 = int(row[4])
                C2 = int(row[5])
                C3 = int(row[6])
                XC = int(row[7])
                X1 = float(row[8])
                X2 = float(row[9])
                X3 = float(row[10])
                X4 = float(row[11])
                X5 = float(row[12])
                next(reader2, None)  # file has an empty row after each full row so let us skip it

                feat.append(np.array([schoolid, S3, C1, C2, C3, XC, X1, X2, X3, X4, X5]))

            self.feat = np.array(feat)
            self.w = np.array(w)
            self.y = np.array(y)



    def get_data_for_experiment(self):
        # y, w, feat
        return [self.y[self.exploration_bl],
                self.w[self.exploration_bl],
                self.feat[self.exploration_bl]]

if __name__=='__main__':
    print(9)
    m = MSPData()
    y, w, feat = m.get_data_for_experiment()
    print(w)

