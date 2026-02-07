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
        "brightness"
    ]
].dropna()


#######################
##  DF Analytics     ##
#######################

print(data_clean.shape)

print(data_clean)


########################################
###     Hamiltonian Monte Carlo      ###
########################################

print(us_data.columns.tolist())


X = data_clean[
    ["temp_C", "wind", "humidity", "ndvi",
     "precipitation", "cloud_cover",
     "soil_moisture", "elevation", "nighttime"]
].values

y = data_clean["wildfire"].values


def make_spatial_basis(lat, lon, centers, bandwidth=1.0):
    d2 = (lat[:, None] - centers[:, 0])**2 + (lon[:, None] - centers[:, 1])**2
    return np.exp(-d2 / (2 * bandwidth**2))

K = 55
idx = np.random.choice(len(data_clean), K, replace=False)
centers = data_clean[["latitude", "longitude"]].iloc[idx].values

Z = make_spatial_basis(
    data_clean["latitude"].values,
    data_clean["longitude"].values,
    centers
)


def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def kinetic_energy(p0, pb, pg):
    return 0.5 * (p0**2 + np.sum(pb**2) + np.sum(pg**2))


def logpost_and_grad(beta0, beta, gamma, X, Z, y):
    eta = beta0 + X @ beta + Z @ gamma
    p = sigmoid(eta)

    # log likelihood
    loglik = np.sum(y * np.log(p + 1e-9) + (1 - y) * np.log(1 - p + 1e-9))

    # Gaussian priors
    logprior = -0.5 * (beta0**2 + beta @ beta + gamma @ gamma)

    logpost = loglik + logprior

    # gradients
    error = y - p
    grad_beta0 = np.sum(error) - beta0
    grad_beta = X.T @ error - beta
    grad_gamma = Z.T @ error - gamma

    return logpost, grad_beta0, grad_beta, grad_gamma


def hmc_step(beta0, beta, gamma, X, Z, y,
             epsilon=0.00005, L=50):

    # sample momentum
    p0 = np.random.randn()
    pb = np.random.randn(len(beta))
    pg = np.random.randn(len(gamma))

    # save current state
    beta0_curr = beta0
    beta_curr = beta.copy()
    gamma_curr = gamma.copy()

    p0_curr = p0
    pb_curr = pb.copy()
    pg_curr = pg.copy()

    logp_curr, _, _, _ = logpost_and_grad(beta0, beta, gamma, X, Z, y)

    # ---- leapfrog ----
    _, g0, gb, gg = logpost_and_grad(beta0, beta, gamma, X, Z, y)

    # half-step momentum
    p0 += 0.5 * epsilon * g0
    pb += 0.5 * epsilon * gb
    pg += 0.5 * epsilon * gg

    for l in range(L):
        # full position step
        beta0 += epsilon * p0
        beta  += epsilon * pb
        gamma += epsilon * pg

        _, g0, gb, gg = logpost_and_grad(beta0, beta, gamma, X, Z, y)

        if l != L - 1:
            p0 += epsilon * g0
            pb += epsilon * gb
            pg += epsilon * gg

    # final half-step
    p0 += 0.5 * epsilon * g0
    pb += 0.5 * epsilon * gb
    pg += 0.5 * epsilon * gg

    # momentum flip (MANDATORY)
    p0 = -p0
    pb = -pb
    pg = -pg

    # ---- acceptance ----
    logp_new, _, _, _ = logpost_and_grad(beta0, beta, gamma, X, Z, y)

    H_curr = -logp_curr + kinetic_energy(p0_curr, pb_curr, pg_curr)
    H_new  = -logp_new  + kinetic_energy(p0, pb, pg)

    if np.log(np.random.rand()) < (H_curr - H_new):
        return beta0, beta, gamma, True
    else:
        return beta0_curr, beta_curr, gamma_curr, False


# TODO: Uncomment All - default HMC implementation
# beta0 = 0.0
# beta = np.zeros(X.shape[1])
# gamma = np.zeros(Z.shape[1])

# samples = []

# accepted = 0
# samples = []

# for i in range(10000):
#     beta0, beta, gamma, acc = hmc_step(beta0, beta, gamma, X, Z, y)
#     if acc:
#         accepted += 1
#     if i > 2000:  # burn-in
#         samples.append((beta0, beta.copy(), gamma.copy()))

# print("Acceptance rate:", accepted / 10000)



# def posterior_predict(samples, X, Z):
#     probs = []
#     for beta0, beta, gamma in samples:
#         eta = beta0 + X @ beta + Z @ gamma
#         probs.append(sigmoid(eta))
#     return np.array(probs)



# theta = posterior_predict(samples, X, Z)
# theta_mean = theta.mean(axis=0)
# y_pred = (theta_mean > 0.55).astype(int)
# accuracy = (y_pred == y).mean()
# print(accuracy)
# 0.8567787971457697 base accuracy





########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################



########################################
###     Hamiltonian Monte Carlo      ###
###           Experimental -           ###
########################################

X = data_clean[
    ["temp_C", "wind", "humidity", "ndvi",
     "precipitation", "cloud_cover",
     "soil_moisture", "elevation", "nighttime"]
].values

brightness = data_clean[["brightness"]].values

y = data_clean["wildfire"].values



def make_spatial_basis(lat, lon, centers, bandwidth=1.0):
    d2 = (lat[:, None] - centers[:, 0])**2 + (lon[:, None] - centers[:, 1])**2
    return np.exp(-d2 / (2 * bandwidth**2))

K = 20
idx = np.random.choice(len(data_clean), K, replace=False)
centers = data_clean[["latitude", "longitude"]].iloc[idx].values

Z = make_spatial_basis(
    data_clean["latitude"].values,
    data_clean["longitude"].values,
    centers
)


def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def kinetic_energy(p0, pb, pg):
    return 0.5 * (p0**2 + np.sum(pb**2) + np.sum(pg**2))


def logpost_and_grad(beta0, beta, gamma, X, Z, y, severity):
    eta = beta0 + X @ beta + Z @ gamma
    p = sigmoid(eta)

    # log likelihood
    loglik = np.sum(y * np.log(p + 1e-9) + (1 - y) * np.log(1 - p + 1e-9))

    # Gaussian priors
    logprior = -0.5 * (beta0**2 + beta @ beta + gamma @ gamma)

    logpost = loglik + logprior

    severity = severity.flatten()
    y = y.flatten()

    # error
    w = severity / np.max(severity)  # shape: (n_obs,)
    # ensure all entries are positive
    w = np.maximum(w, 0.4)  

    # weighted log-likelihood
    loglik = np.sum(w * (y * np.log(p + 1e-9) + (1 - y) * np.log(1 - p + 1e-9)))

    # weighted gradient
    error = w * (y - p)
    grad_beta0 = np.sum(error) - beta0
    grad_beta = X.T @ error - beta
    grad_gamma = Z.T @ error - gamma

    return logpost, grad_beta0, grad_beta, grad_gamma



def hmc_step(beta0, beta, gamma, X, Z, y, severity, epsilon=0.00005, L=100):

    # 1. Sample Momentum
    p0 = np.random.randn()
    pb = np.random.randn(len(beta))
    pg = np.random.randn(len(gamma))
    
    # save current state
    beta0_curr = beta0
    beta_curr = beta.copy()
    gamma_curr = gamma.copy()

    p0_curr = p0
    pb_curr = pb.copy()
    pg_curr = pg.copy()

    logp_curr, _, _, _ = logpost_and_grad(beta0, beta, gamma, X, Z, y, severity)

    # ---- leapfrog ----
    _, g0, gb, gg = logpost_and_grad(beta0, beta, gamma, X, Z, y, severity)

    # half-step momentum
    p0 += 0.5 * epsilon * g0
    pb += 0.5 * epsilon * gb
    pg += 0.5 * epsilon * gg

    for l in range(L):
        # full position step
        beta0 += epsilon * p0
        beta  += epsilon * pb
        gamma += epsilon * pg

        _, g0, gb, gg = logpost_and_grad(beta0, beta, gamma, X, Z, y, severity)

        if l != L - 1:
            p0 += epsilon * g0
            pb += epsilon * gb
            pg += epsilon * gg

    # final half-step
    p0 += 0.5 * epsilon * g0
    pb += 0.5 * epsilon * gb
    pg += 0.5 * epsilon * gg

    # momentum flip (MANDATORY)
    p0 = -p0
    pb = -pb
    pg = -pg

    # ---- acceptance ----
    logp_new, _, _, _ = logpost_and_grad(beta0, beta, gamma, X, Z, y, severity)

    H_curr = -logp_curr + kinetic_energy(p0_curr, pb_curr, pg_curr)
    H_new  = -logp_new  + kinetic_energy(p0, pb, pg)

    if np.log(np.random.rand()) < (H_curr - H_new):
        return beta0, beta, gamma, True
    else:
        return beta0_curr, beta_curr, gamma_curr, False


beta0 = 0.0
beta = np.zeros(X.shape[1])
gamma = np.zeros(Z.shape[1])

samples = []

accepted = 0
samples = []

np.random.seed(321)
for i in range(10000):
    beta0, beta, gamma, acc = hmc_step(beta0, beta, gamma, X, Z, y, brightness)
    if acc:
        accepted += 1
    if i > 2000:  # burn-in
        samples.append((beta0, beta.copy(), gamma.copy()))

print("Acceptance rate:", accepted / 10000)



def posterior_predict(samples, X, Z):
    probs = []
    for beta0, beta, gamma in samples:
        eta = beta0 + X @ beta + Z @ gamma
        probs.append(sigmoid(eta))
    return np.array(probs)



theta = posterior_predict(samples, X, Z)
theta_mean = theta.mean(axis=0)
y_pred = (theta_mean > 0.5).astype(int)
accuracy = (y_pred == y).mean()
print(accuracy) # 0.8547400611620795 - brightness factoring error.


# for coefficient in beta:
#     print(coefficient)
