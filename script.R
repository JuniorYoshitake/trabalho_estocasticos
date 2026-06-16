##### Seminário Estocásticos - MGB #####
library(quantmod)
library(ggplot2)


### ------- 1. ESCOLHA DO ATIVO ------- ### 

getSymbols("PETR4.SA", src = "yahoo", from = "2020-01-01", to = "2024-12-31")
getSymbols("VALE3.SA", src = "yahoo", from = "2020-01-01", to = "2024-12-31")
getSymbols("^BVSP",   src = "yahoo", from = "2020-01-01", to = "2024-12-31")
getSymbols("BTC-USD", src = "yahoo", from = "2020-01-01", to = "2024-12-31")

precos_petr <- Ad(PETR4.SA)
precos_vale <- Ad(VALE3.SA)
precos_ibov <- Ad(BVSP)
precos_btc  <- Ad(`BTC-USD`)

retornos_petr <- na.omit(diff(log(precos_petr)))
retornos_vale <- na.omit(diff(log(precos_vale)))
retornos_ibov <- na.omit(diff(log(precos_ibov)))
retornos_btc  <- na.omit(diff(log(precos_btc)))

### Paineis Comparativos: Gráfico de Preço Histórico 
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(precos_petr, main = "PETR4 — Preço histórico",
     ylab = "Preço (R$)", xlab = "", col = "steelblue")

plot(precos_vale, main = "VALE3 — Preço histórico",
     ylab = "Preço (R$)", xlab = "", col = "darkgreen")

plot(precos_ibov, main = "IBOVESPA — Preço histórico",
     ylab = "Pontos", xlab = "", col = "darkorange")

plot(precos_btc, main = "Bitcoin — Preço histórico",
     ylab = "USD", xlab = "", col = "goldenrod")

par(mfrow = c(1, 1))

### Paineis Comparativos: Gráfico de Retorno Diário
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(retornos_petr, main = "PETR4 — Retornos diários",
     ylab = "Retorno", xlab = "", col = "steelblue")

plot(retornos_vale, main = "VALE3 — Retornos diários",
     ylab = "Retorno", xlab = "", col = "darkgreen")

plot(retornos_ibov, main = "IBOVESPA — Retornos diários",
     ylab = "Retorno", xlab = "", col = "darkorange")

plot(retornos_btc, main = "Bitcoin — Retornos diários",
     ylab = "Retorno", xlab = "", col = "goldenrod")

par(mfrow = c(1, 1))

### Paineis Comparativos: Gráfico de Histograma de Retorno
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(retornos_petr, breaks = 60, probability = TRUE,
     main = "PETR4 — Histograma dos retornos",
     xlab = "Retorno diário", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(retornos_petr), sd = sd(retornos_petr)),
      add = TRUE, col = "red", lwd = 2)

hist(retornos_vale, breaks = 60, probability = TRUE,
     main = "VALE3 — Histograma dos retornos",
     xlab = "Retorno diário", col = "lightgreen", border = "white")
curve(dnorm(x, mean = mean(retornos_vale), sd = sd(retornos_vale)),
      add = TRUE, col = "red", lwd = 2)

hist(retornos_ibov, breaks = 60, probability = TRUE,
     main = "IBOVESPA — Histograma dos retornos",
     xlab = "Retorno diário", col = "#FFD9A0", border = "white")
curve(dnorm(x, mean = mean(retornos_ibov), sd = sd(retornos_ibov)),
      add = TRUE, col = "red", lwd = 2)

hist(retornos_btc, breaks = 60, probability = TRUE,
     main = "Bitcoin — Histograma dos retornos",
     xlab = "Retorno diário", col = "#FFF0A0", border = "white")
curve(dnorm(x, mean = mean(retornos_btc), sd = sd(retornos_btc)),
      add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))


### ------- 2. ESTIMAÇÃO DOS PARÂMETROS ------- ### 
# Ativo escolhido: VALE3

precos   <- precos_vale
retornos <- retornos_vale

### Estimação de µ e sigma 
dt        <- 1/252
sigma_hat <- sd(retornos) / sqrt(dt)
mu_hat    <- mean(retornos) / dt + (sigma_hat^2) / 2

cat("sigma (volatilidade anual):", round(sigma_hat * 100, 2), "%\n")
cat("mu    (drift anual):       ", round(mu_hat    * 100, 2), "%\n")
cat("Observações (dias úteis):  ", nrow(retornos), "\n")

### Gráficos da estimação
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

plot(precos, main = "VALE3 — Preço histórico (2020–2024)",
     ylab = "Preço (R$)", xlab = "", col = "darkgreen")

plot(retornos, main = "VALE3 — Retornos log-diários",
     ylab = "Retorno", xlab = "", col = "darkgreen")

hist(retornos, breaks = 60, probability = TRUE,
     main = "VALE3 — Histograma dos retornos",
     xlab = "Retorno diário", col = "lightgreen", border = "white")
curve(dnorm(x, mean = mean(retornos), sd = sd(retornos)),
      add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))
