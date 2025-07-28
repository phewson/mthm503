import pandas as pd

def test_clean_data_columns():
    df = pd.read_csv("data/olive_clean.csv")
    assert 'Region' not in df.columns, "Region column should not be in clean data"
    assert df.shape[1] > 0
