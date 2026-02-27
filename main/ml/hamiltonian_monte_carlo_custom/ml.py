import pandas as pd
import numpy as np

us_data_fires_path = "~/MSISS/wildfires/main/data/1000_samples/us_1000_sample_fires.csv"
us_data_no_fires_path = "~/MSISS/wildfires/main/data/1000_samples/us_1000_sample_no_fires.csv"

df_fires = pd.read_csv(us_data_fires_path)
df_no_fires = pd.read_csv(us_data_no_fires_path)

# quick sanity check
print(df_fires.shape, df_no_fires.shape)

#######################
##  Get a clean df   ##
#######################

us_data = pd.concat([df_fires, df_no_fires], ignore_index=True)

# 2. Select relevant columns
us_data = us_data[
    [
        "wildfire",
        "temp_C",
        "latitude",
        "longitude",
        "computed_daynight_sza",
        "daynight",
        "humidity",
        "wind",
        "ndvi",
        "precipitation",
        "soil_moisture",
        "elevation",
        "cloud_cover",
        "brightness"
    ]
]

# 3. Fix day/night bug (coalesce equivalent)
us_data["daynight_combined"] = us_data["daynight"].fillna(
    us_data["computed_daynight_sza"]
)

# 4. Create nighttime indicator
us_data["nighttime"] = np.where(us_data["daynight_combined"] == "N", 1, 0)

# 5. Final clean dataset (select columns + drop NA)
data_clean = us_data[
    [
        "wildfire",
        "latitude",
        "longitude",
        "temp_C",
        "wind",
        "humidity",
        "nighttime",
        "ndvi",
        "precipitation",
        "soil_moisture",
        "cloud_cover",
        "elevation",
        # "brightness"
    ]
].dropna()

from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score


# Define features (X) and target (y)
X = data_clean.drop(columns=["wildfire"])
y = data_clean["wildfire"]

# Split into train and test sets (e.g., 80% train, 20% test)
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.4, random_state=42, stratify=y
)


from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn import svm
from sklearn.neural_network import MLPClassifier
# clf = DecisionTreeClassifier(random_state=321)
# clf = KNeighborsClassifier(n_neighbors = 10)
clf = svm.SVC()
# clf = MLPClassifier()

clf.fit(X_train, y_train)

predictions = clf.predict(X_test)
accuracy = accuracy_score(y_test, predictions)
print("Accuracy:", accuracy)

print(predictions)


