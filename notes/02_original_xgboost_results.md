# Original XGBoost baseline

## Model design

The model was trained on observations before January 01, 2024 and evaluated on later observations.

It used 300 trees, a tree depth of 5, a learning rate of 0.05, and a minimum node size of 10.

## Predictive performance

Out-of-sample RMSE was 9.35%, while MAE was 6.83%.

Prediction correlation was 0.0276, and directional accuracy was 52.5%.

Mean cross-sectional rank correlation was 0.0349.

## Investment performance

The model portfolio produced a total return of 174.3%, compared with 61.9% for the equal-weight universe and 56.0% for SPY.

Its annualized Sharpe ratio was 2.247, and maximum drawdown was -8.3%.

Estimated annualized CAPM alpha was 21.46%, and beta was 0.974.

## Baseline limitations

- Original momentum and reversal ratios are reversed.
- Forward target periods overlap.
- The original split does not purge targets crossing 2024.
- Backtest results exclude transaction costs and taxes.
- The final evaluable ranking is historical.
