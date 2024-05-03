{library(readxl)
  library(dynlm)
  library(forecast)
  library(FinTS)
  library(fGarch)
  library(lmtest)}


####Индекс IMOEX 2019-2024####
IMOEX <- read_excel("Датасеты/Архив-Индекс IMOEX 19-24.xlsx")
View(IMOEX)
#Для работы с временными рядами используется функция ts ("time series" - времен-
#ной ряд). 
#В рассматриваемом датасете мы рассматриваем дневные данные, в календарном месяце 
#примерно 20 торговых дней, в году 250.
IMOEX$price <- ts(IMOEX$Close, start = c(2019, 1), frequency = 250)
#рассчитываем непрерывная доходность как разность логарифмов
returns<-log(IMOEX$price)
returns <- diff(returns, lag = 1)
returns
plot(returns)
#автоподбор ARIMA модели на основе информационных критерив
mod.mean <- auto.arima(returns)
summary(mod.mean)

# Шаг 1: оценим уравнение returns = alpha + beta*rt-1 + e
#авторегрессия первого порядка
AR <- arima(returns, order = c(1,0,0))
summary(AR)
coeftest(AR)
# Шаг 2: Извлекаем остатки из рассматриваемой модели и возводим их в квадрат
ehatsq <- ts(resid(AR)^2)
# Шаг 3: Построим регрессию квадратов остатков
mod.arch <- dynlm(ehatsq ~ L(ehatsq))
summary(mod.arch)
#проведем тест множителей Лагранжа для определения ARCH-эффектов
byd.archTest <- ArchTest(returns, lags = 1, demean = TRUE)
byd.archTest
#p-value = 1.661e-12, следовательно ARCH-эффект ЕСТЬ

#Оцениваем модель GARCH(1,1)
arch.fit <- garchFit(~arma(1,0)+garch(1,1), data = returns, trace = F)
summary(arch.fit)

#Определяем среднемесячные эпсилоны (в месяце около 20 рабочих дней на бирже)
eps<-arch.fit@residuals
rolling_mean <- rollapply(eps, width = 20, FUN = mean, by = 1, align = "right", fill = NA)
rolling_mean <-na.omit(rolling_mean)
# Вывод результатов
print(rolling_mean)

#Влияние шумовых инвесторов оценивается как разница между эпсилонами ежедневными
#и их среднемесячными значениеми
#Расчет значения дельты (влияние иррациональных трейдеров)
eps1<-eps[20:1319]
delta<-eps1-rolling_mean
plot(delta)
t.test(delta)
delta1 <- ts(delta, start = c(2019, 21), frequency = 250)
plot(delta1)
t.test(delta1)
print(mean(delta))


####Индекс MCXSM 2020-2022####

MCXSM <- read_excel("Датасеты/Архив-Индекс MCXSM 20-22.xlsx")
View(MCXSM)
MCXSM$price <- ts(MCXSM$Close, start = c(2020, 1), frequency = 250)
#рассчитываем непрерывную доходность как разность логарифмов
returns<-log(MCXSM$price)
returns <- diff(returns, lag = 1)
returns
plot(returns)
#автоподбор ARIMA модели
mod.mean <- auto.arima(returns)
summary(mod.mean)
coeftest(mod.mean)

# Шаг 1: оценим уравнение r = alpha + beta*rt-1 + error
#авторегрессия первого порядка
AR <- arima(returns, order = c(1,0,0))
summary(AR)
coeftest(AR)
# Шаг 2: Извлекаем остатки из рассматриваемой модели и возводим их в квадрат
ehatsq <- ts(resid(AR)^2)
# Шаг 3: Построим регрессию квадратов остатков
mod.arch <- dynlm(ehatsq ~ L(ehatsq))
summary(mod.arch)
#проведем тест множителей Лагранжа для определения ARCH-эффектов
byd.archTest <- ArchTest(returns, lags = 1, demean = TRUE)
byd.archTest
#p-value = 0.0001779, следовательно ARCH-эффект ЕСТЬ

#Мы можем оценить модель GARCH(1,1), используя функцию garchFit
arch.fit <- garchFit(~arma(1,0)+garch(1,1), data = returns, trace = F)
summary(arch.fit)

#Определяем среднемесячные эпсилоны (в месяце около 20 рабочих дней на бирже)
eps<-arch.fit@residuals
rolling_mean <- rollapply(eps, width = 20, FUN = mean, by = 1, align = "right", fill = NA)
rolling_mean <-na.omit(rolling_mean)

#Влияние шумовых инвесторов оценивается как разница между эпсилонами ежедневными
#и их среднемесячными значениеми
#выведем значение дельты (влияние иррациональных инвесторов)
eps1<-eps[20:713]
delta<-eps1-rolling_mean
plot(delta)
t.test(delta)
delta1 <- ts(delta, start = c(2020, 21), frequency = 250)
plot(delta1)
t.test(delta1)
print(mean(delta))

