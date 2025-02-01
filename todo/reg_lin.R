donnees <- readRDS("base_PC_Var_X_Var_Y_ENSAI_Respi_FINALE.RData", "rb")
var_a_expliquee <- donnees[c("PS_Eter_freq", "ENG_Eter_freq", "PS_Tx_freq", "ENG_Tx_freq")]
donnees2 <- var_a_expliquee
for (col in names(var_a_expliquee)) {
        donnees2[[paste0("log_", col)]] <- log(17* donnees2[[col]] + 1)
}
full_model <- lm(log_PS_Tx_freq ~ . - PS_Tx_freq, data = donnees2)
    
best_model <- step(full_model, direction = "both", k = 2, trace = 0)
summary(best_model)
pairs(donnees2)
best_model_log_trans <- function(a, b){
    donnees2 <- var_a_expliquee
    for (col in names(var_a_expliquee)) {
        donnees2[[paste0("log_", col)]] <- log(a * donnees2[[col]] + b)
    }
    
    full_model <- lm(log_PS_Tx_freq ~ . - PS_Tx_freq, data = donnees2)
    
    best_model <- step(full_model, direction = "both", k = 2, trace = 0)
    
    print(AIC(best_model))
    print(c(a, b))
    print(-summary(best_model)$r.squared)
    return(-summary(best_model)$r.squared)

}
mod <- best_model_log_trans(177.932779, 7)
optimization_result <- optim(
    par = c(a = 100, b = 7),  # Initial guesses for a and b (b should be greater than 0 to avoid log issues)
    fn = function(params) best_model_log_trans(a = params[1], b = params[2]), 
    method = "L-BFGS-B",       # L-BFGS-B supports bounds
    lower = c(0.001, 0.001),
    upper = c(Inf, Inf),  
    control = list(maxit = 10000, factr = 1e-6),  # Adjusting control settings for better convergence
    fnscale = 1
)
pairs()         


optimization_result

summary(best_model)
plot(best_model$residuals)

