import pandas as pd
from sklearn.cluster import KMeans

df = pd.read_csv("data/olive_clean.csv")

kmeans = KMeans(n_clusters=3, random_state=42)
df['cluster'] = kmeans.fit_predict(df)

df.to_csv("data/olive_clustered.csv", index=False)
