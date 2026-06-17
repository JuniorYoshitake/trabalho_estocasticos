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


### ------- 3. SIMULAÇÃO COM OS PARÂMETROS ------- ###

set.seed(2028)

### Parâmetros estimados
mu    <- 0.1647
sigma <- 0.3728
dt    <- 1/252
n     <- 1243
S0    <- as.numeric(precos[1])

### Simular N trajetórias
N <- 200

trajetorias <- matrix(NA, nrow = n, ncol = N)

for (j in 1:N) {
    S <- numeric(n)
    S[1] <- S0
    for (i in 2:n) {
        S[i] <- S[i-1] * exp((mu - sigma^2/2) * dt + sigma * sqrt(dt) * rnorm(1))
    }
    trajetorias[, j] <- S}

### Gráfico: trajetórias simuladas vs preço real

datas <- index(precos)[1:n]

# Limites do gráfico [OBS: eixo Y fixado no dobro do preço real máximo 
# para evitar que trajetórias extremas distorçam a escala] 
y_max <- max(as.numeric(precos)) * 2
y_min <- 0

# Fundo: trajetórias simuladas em cinza
plot(datas, trajetorias[, 1], type = "l",
     col = adjustcolor("gray60", alpha.f = 0.3),
     ylim = c(y_min, y_max),
     xlab = "", ylab = "Preço (R$)",
     main = "VALE3 — Simulações MGB vs. Preço Real",
     lwd = 0.8)

for (j in 2:N) {
    lines(datas, trajetorias[, j],
          col = adjustcolor("gray60", alpha.f = 0.15), lwd = 0.8)
}

# Intervalo de confiança empírico (5% e 95%)
ic_low  <- apply(trajetorias, 1, quantile, probs = 0.05) 
ic_high <- apply(trajetorias, 1, quantile, probs = 0.95) 

polygon(c(datas, rev(datas)),
        c(ic_low, rev(ic_high)),
        col = adjustcolor("steelblue", alpha.f = 0.15),
        border = NA)

lines(datas, ic_low,  col = "steelblue", lwd = 1.2, lty = 2)
lines(datas, ic_high, col = "steelblue", lwd = 1.2, lty = 2)

# Mediana das simulações — trajetória central mais estável
mediana_sim <- apply(trajetorias, 1, median)
lines(datas, mediana_sim, col = "gray20", lwd = 2, lty = 2)

# Preço real por cima em destaque
lines(datas, as.numeric(precos[1:n]), col = "darkgreen", lwd = 2)

legend("topleft",
       legend = c("Preço real (VALE3)", "Mediana das simulações",
                  "Trajetórias simuladas", "IC 90%"),
       col    = c("darkgreen", "gray20", "gray60", "steelblue"),
       lwd    = c(2, 2, 1, 1.2),
       lty    = c(1, 2, 1, 2),
       bty    = "n")