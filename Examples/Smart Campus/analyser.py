import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

files = ["raw.csv"]

colors = {
    "mediate": "C1",
    "associate": "C0",
    "optimise": "C2",
    "total": "C3"
}

for scale in ["log", "linear"]:
    for file in files:
        raw = pd.read_csv('./'+file, names = ["departments", "mediate", "associate", "optimise", "total", "python-time"])

        attrs = ['min', 'max', 'mean', 'std']

        grouped = raw.groupby('departments').agg(attrs)

        for v in ["mediate", "associate", "optimise", "total"]:
            plt.plot(grouped[v].index, grouped[v]['mean'], label = v, color = colors[v])
        plt.yscale(scale)
        plt.xlabel('Departments')
        plt.ylabel('Time (s)')
        plt.legend()
        #plt.show()
        plt.savefig("./graphs/all-"+scale+"."+file+".png")
        plt.clf()

        for v in ["mediate", "associate", "optimise", "total"]:
            plt.plot(grouped[v]['mean'], label=v, color=colors[v])
            #plt.fill_between(grouped[v].index, grouped[v]['min'], grouped[v]['max'], alpha=0.2)
            plt.fill_between(grouped[v].index, grouped[v]['mean'] - grouped[v]['std'], grouped[v]['mean'] + grouped[v]['std'], alpha=0.4, color=colors[v])
            plt.yscale(scale)
            plt.xlabel('Departments')
            plt.ylabel('Time (s)')
            plt.legend()
            #plt.show()
            plt.savefig(."/graphs/"+v+"-"+scale+"."+file+".png")
            plt.clf()
