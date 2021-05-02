import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

if __name__ == '__main__':
    df = pd.read_csv("./data.csv")
    print(df)
    print(df.dtypes)

