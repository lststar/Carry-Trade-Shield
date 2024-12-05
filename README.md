# Carry Trade Shield

### A Risk Mitigation Product for Currency and Stock Volatility

## Introduction

The **Carry Trade Shield** is a financial product designed to mitigate the risks associated with carry trades, particularly when borrowing in Japanese Yen (JPY) to invest in American stocks. This product addresses volatility in stock returns and foreign exchange (FX) rates, offering compensation to investors during periods of negative net returns.

This is a course project of HKU FINA4354.

## Product Detail

Please read [our report](report.pdf) for more detail.

## Data

Please find our data at [data](data/).

## Code Usage

Clone the repository to your local machine.

```
git clone https://github.com/lststar/Carry-Trade-Shield.git
```

Install required environment.

Set working dictionary.

Use `code/estimating_parameters.R` to get the parameters.

Use `code/monte_carlo_simulation.R` to simulate product payoffs and deltas.

## Authors

We are Group 2 of FINA4354 2024 Fall at HKU.

- Lyu Xing (3035973828)
    - Designing the structure of the underlying and stochastic processes.
- Zheng Yiwen (3035844534)
    - Designing the payoff diagram and payoff features. 
- Tao Chenya (3036303044)
    - Modeling and Parameters Estimating.
- He Zhiyue (3036100973)
    - Calculating the parameters in the payoff function.
- Liu Sitong (3035844479)
    - Coding for Monte Carlo Simulation and Delta Hedging. Organizing codes, report and README.

## Support and Contact

If you have any questions or suggestions about this project, please raise an Issue or send an email to **lststar@connect.hku.hk**.