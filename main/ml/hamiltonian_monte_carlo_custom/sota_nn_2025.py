import pandas as pd
import numpy as np

us_data_fires_path = "~/MSISS/wildfires/main/data/1000_samples/au_1000_sample_fires.csv"
us_data_no_fires_path = "~/MSISS/wildfires/main/data/1000_samples/au_1000_sample_no_fires.csv"

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
        "elevation"
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


import torch
import torch.nn as nn
import torch.nn.functional as F

class Net(nn.Module):
    def __init__(self, input_size, output_size):
        super(Net, self).__init__()
        
        # Define hidden layers
        self.hidden_layers = nn.ModuleList([
            nn.Linear(input_size if i==0 else 16, 16) for i in range(2)
        ])
        
        # Dropout layer
        self.dropout = nn.Dropout(0.2)
        
        # Output layer
        self.output = nn.Linear(16, output_size)
        
    def forward(self, x):
        for layer in self.hidden_layers:
            x = F.relu(layer(x))
            x = self.dropout(x)
        x = self.output(x)
        return x

import torch

X_train_tensor = torch.tensor(X_train.values, dtype=torch.float32)
X_test_tensor  = torch.tensor(X_test.values, dtype=torch.float32)
y_train_tensor = torch.tensor(y_train.values, dtype=torch.float32)  # or .long() for classification
y_test_tensor  = torch.tensor(y_test.values, dtype=torch.float32)

from torch.utils.data import TensorDataset, DataLoader

train_dataset = TensorDataset(X_train_tensor, y_train_tensor)
test_dataset = TensorDataset(X_test_tensor, y_test_tensor)

train_loader = DataLoader(train_dataset, batch_size=32, shuffle=True)
test_loader = DataLoader(test_dataset, batch_size=32, shuffle=False)


input_size = X_train.shape[1]
output_size = 1  # adjust: 1 for binary regression or classification

model = Net(input_size=input_size, output_size=output_size)

# Binary classification
criterion = nn.BCEWithLogitsLoss()  # combines sigmoid + BCELoss
# Regression would use: nn.MSELoss()

optimizer = torch.optim.Adam(model.parameters(), lr=0.001)


num_epochs = 1000

for epoch in range(num_epochs):
    model.train()
    running_loss = 0.0
    
    for X_batch, y_batch in train_loader:
        optimizer.zero_grad()            # reset gradients
        outputs = model(X_batch).squeeze()  # forward pass
        
        loss = criterion(outputs, y_batch)
        loss.backward()                  # backpropagation
        optimizer.step()                 # update weights
        
        running_loss += loss.item() * X_batch.size(0)
    
    epoch_loss = running_loss / len(train_loader.dataset)
    print(f"Epoch {epoch+1}/{num_epochs}, Loss: {epoch_loss:.4f}")


model.eval()
with torch.no_grad():
    y_pred = model(X_test_tensor).squeeze()
    y_pred_class = (torch.sigmoid(y_pred) > 0.5).int()  # for binary classification

    accuracy = (y_pred_class == y_test_tensor.int()).float().mean()
    print(f"Test Accuracy: {accuracy:.4f}")






