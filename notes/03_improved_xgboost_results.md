# Improved XGBoost model

## Improvements

- Corrected momentum and short-term return direction.
- Removed raw price levels from the predictor set.
- Added relative momentum, volatility, beta, drawdown, and volume features.
- Used purged time splits to prevent target leakage.
- Selected hyperparameters using a separate validation period.
- Used equal-weight Top 10 portfolio construction.
- Applied estimated transaction costs of 10 basis points per unit of turnover.

## Selected parameters

The selected candidate was number 6, with 400 trees, tree depth 4, and learning rate 0.03.

## Predictive performance

Out-of-sample RMSE was 9.18%, prediction correlation was 0.1068, and mean rank correlation was 0.0434.

## Investment performance

After estimated trading costs, total return was 102.9%, annualized Sharpe ratio was 1.382, and maximum drawdown was -17.5%.
