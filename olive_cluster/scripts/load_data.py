import pandas as pd

df = pd.read_csv("data/olive.csv")
features = df.select_dtypes(include='number')
features.to_csv("data/olive_clean.csv", index=False)
