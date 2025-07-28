import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("data/olive_clustered.csv")

plt.figure(figsize=(8, 6))
plt.scatter(df.iloc[:, 0], df.iloc[:, 1], c=df['cluster'], cmap='viridis')
plt.title("Olive Oil Clusters")
plt.xlabel(df.columns[0])
plt.ylabel(df.columns[1])
plt.savefig("report.png")
