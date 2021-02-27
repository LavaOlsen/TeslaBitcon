
plot(df$BTC,type = "l")

plot(df$TSLA,type = "l")


plot(log(df$BTC),type = "l")

plot(log(df$TSLA),type = "l")

trend <- seq(1,to = nrow(df))

lm.fit <- lm(BTC ~ TSLA + trend,data = log(df[,2:3]))

summary(lm.fit)

plot(lm.fit)

    