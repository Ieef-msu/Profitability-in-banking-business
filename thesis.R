    ## 2.3 Описательные статистики, графический и корреляционный анализ

# Установка необходимых пакетов
if (!require("readxl")) install.packages("readxl")
if (!require("plm")) install.packages("plm")
if (!require("lmtest")) install.packages("lmtest")

# Загрузка библиотек
library(readxl)
library(plm)
library(lmtest)

# Загрузка данных из Excel файла
data <- read_excel("/Users/doroshenko.mary/Desktop/диплом/r_eu_banking_data.xlsx")

# Просмотр структуры данных
str(data)

# Создаем панельный dataframe
pdata <- pdata.frame(data, index = c("country", "date"))

# Проверяем балансировку панели
is.pbalanced(pdata)  # Если FALSE, панель несбалансированная

# Описательная статистика
summary(pdata)

# Проверка на мультиколлинеарность
cor(pdata[, c("roa", "roe", "innov_index", 
              "ln_ta", "npl", "car", "solv", "liq", "infl", "gdp")], use = "complete.obs")

model <- lm(roa ~ innov_index + ln_ta + npl + car + liq + infl + gdp, data = pdata)
car::vif(model)  # серьезной мультиколлинераности нет

    ## 2.4 Тесты на выбор спецификации модели

# Добавляю первые разности
pdata$roa_diff <- panel_diff(pdata$roa, n = 1, pdata$country_id)
pdata$npl_diff <- panel_diff(pdata$npl, n = 1, pdata$country_id)
pdata$roe_diff <- panel_diff(pdata$roe, n = 1, pdata$country_id)
pdata$non_int_income_diff <- panel_diff(pdata$non_int_income, n = 1, pdata$country_id)
pdata$ta_diff <- panel_diff(pdata$ta, n = 1, pdata$country_id)
pdata$ln_ta_diff <- panel_diff(pdata$ln_ta, n = 1, pdata$country_id)
pdata$solv_diff <- panel_diff(pdata$solv, n = 1, pdata$country_id)
pdata$car_diff <- panel_diff(pdata$car, n = 1, pdata$country_id)
pdata$liq_diff <- panel_diff(pdata$liq, n = 1, pdata$country_id)
pdata$int_income_diff <- panel_diff(pdata$int_income, n = 1, pdata$country_id)
pdata$innov_index_diff <- panel_diff(pdata$innov_index, n = 1, pdata$country_id)
pdata$infl_diff <- panel_diff(pdata$infl, n = 1, pdata$country_id)
pdata$gdp_diff <- panel_diff(pdata$gdp, n = 1, pdata$country_id)

# Удаляем NA значения, так как после взятия разностей появились пропуски
pdata_clean <- na.omit(pdata)

# Pooled OLS 
pooled <- plm(
  roa_diff ~ innov_index_diff + ln_ta + car_diff + npl + liq_diff + infl_diff + gdp_diff,
  data = pdata,
  model = "pooling"
)

# Fixed Effects (FE) — учитывает индивидуальные эффекты стран
fe <- plm(
  roa_diff ~ innov_index_diff + ln_ta + car_diff + npl + liq_diff + infl_diff + gdp_diff,
  data = pdata,
  model = "within"
)

# Random Effects (RE) — учитывает случайные индивидуальные эффекты
re <- plm(
  roa_diff ~ innov_index_diff + ln_ta + car_diff + npl + liq_diff + infl_diff + gdp_diff,
  data = pdata,
  model = "random"
)

summary(pooled)
summary(fe)
summary(re)

hausman_test <- phtest(fe, re)
print(hausman_test) # Выбираю RE
r.squared(fe, model = "within")  # Within R²

#Проверка нелинейности
re_nonlinear <- plm(roa_diff ~ innov_index_diff + ln_ta + npl + I(npl^2)
                    + car_diff + liq_diff +
                      + I(liq_diff^2) + infl_diff + gdp_diff, 
                    data = pdata, 
                    model = "random")
coeftest(re_nonlinear, vcov = function(x) vcovHC(x, method = "arellano", type = "HC1"))
summary(re_nonlinear) #добавляю квадрат npl, liq
waldtest(re, re_nonlinear) #Нелинейная модель лучше
pbgtest(re_nonlinear) #Тест на наличие автокоррелции
library(lmtest)
plmtest(re_nonlinear, type = "bp") #Тест на наличие гетероскедастичности

#Визуализация эффекта
library(ggplot2)
ggplot(as.data.frame(pdata_clean), 
       aes(x = innov_index_diff, y = roa_diff)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  labs(title = "Проверка квадратичной зависимости",
       subtitle = paste("Точка перегиба:", "-"),
       x = "NPL",
       y = "ROA") +
  theme_minimal()

#Добавляю lag roa
library(dynlm)
re_dynamic <- plm(roa_diff ~ lag(roa_diff,1) + innov_index_diff +
                    ln_ta + npl + I(npl^2) + liq_diff + I(liq_diff^2) + infl_diff +
                      car_diff + gdp_diff,
                  data = pdata,
                  model = "random")
summary(re_dynamic,robust = TRUE)
waldtest(re_nonlinear, re_dynamic) #Тест на улучшение спецификации модели
pbgtest(re_dynamic) #Тест на наличие автокоррелции
plmtest(re_dynamic, type = "bp") #Тест на наличие гетероскедастичности
summary(re_dynamic, vcov = vcovHC(fe_dynamic, type = "HC3"))


# System GMM оценка 
gmm_model <- pgmm(
  roa_diff ~ innov_index_diff + innov_index_diff*gdp_diff + innov_index_diff*infl_diff +
  innov_index_diff*ln_ta + 
    ln_ta + npl + I(npl^2) + liq_diff + I(liq_diff^2) +  car_diff + infl_diff + 
  + gdp_diff |
    lag(roa_diff, 2:3) + lag(innov_index_diff*ln_ta, 2), # Упрощённый набор инструментов
  data = pdata,
  effect = "individual",
  model = "onestep",
  transformation = "l",
  collapse = TRUE
)
# Робастные стандартные ошибки
summary(gmm_model, robust = TRUE)
summary(pdata$roa_diff)  # и аналогично для других переменных

print(gmm_model$model$n)  # Общее число наблюдений

# Проверка эндогенности
phtest(gmm_model, re_dynamic) #Используем GMM подход

    ## Разбивка данных на две временные группы
# Группа 1: 2015Q1 - 2019Q4
group1 <- pdata %>% 
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2019-12-31"))

# Группа 2: 2020Q1 - 2024Q3
group2 <- pdata %>% 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2024-10-01"))

# Проверка количества наблюдений в каждой группе
cat("Количество наблюдений в группе 1 (2015Q1-2019Q4):", nrow(group1), "\n")
cat("Количество наблюдений в группе 2 (2020Q1-2024Q3):", nrow(group2), "\n")

# Дополнительная проверка временного диапазона
cat("Диапазон дат в группе 1:", as.character(min(group1$date)), "-", as.character(max(group1$date)), "\n")
cat("Диапазон дат в группе 2:", as.character(min(group2$date)), "-", as.character(max(group2$date)), "\n")

# После фильтрации преобразуем обратно в pdata.frame
group1 <- pdata %>% 
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2019-12-31")) %>% 
  as.data.frame() %>%  # сначала в обычный data.frame
  pdata.frame(index = c("country", "date"))  # затем обратно в pdata.frame

group2 <- pdata %>% 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2024-10-01")) %>% 
  as.data.frame() %>% 
  pdata.frame(index = c("country", "date"))

# System GMM оценка для group1 (2015-2019 год)
gmm_model6 <- pgmm(
  roa_diff ~ innov_index_diff +  
    ln_ta + npl + I(npl^2) + liq_diff + I(liq_diff^2) +  car_diff + infl_diff + 
    + gdp_diff |
    lag(roa_diff, 2:3) + lag(innov_index_diff*ln_ta, 2), # Упрощённый набор инструментов
  data = group1,
  effect = "individual",
  model = "onestep",
  transformation = "l",
  collapse = TRUE
)
# Робастные стандартные ошибки
summary(gmm_model6, robust = TRUE)

# System GMM оценка для group2 (2020-2024 год)
gmm_model7 <- pgmm(
  roa_diff ~  innov_index_diff +  
    ln_ta + npl + I(npl^2) + liq_diff + I(liq_diff^2) +  car_diff + infl_diff + 
    + gdp_diff |
    lag(roa_diff, 2:3) + lag(innov_index_diff*ln_ta, 2), # Упрощённый набор инструментов
  data = group2,
  effect = "individual",
  model = "onestep",
  transformation = "l",
  collapse = TRUE
)
# Робастные стандартные ошибки
summary(gmm_model7, robust = TRUE)

      ## Разделение на подгруппы по уровню инновационности стран
# Определяем списки стран для каждой группы
less_innov <- c("Czech Republic", "Portugal", "Slovenia", "Lithuania", 
                "Hungary", "Bulgaria", "Poland", "Latvia", "Croatia", 
                "Greece", "Slovakia", "Romania")

more_innov <- c("Sweden", "Netherlands", "Finland", "Denmark", "Germany", 
                "Austria", "France", "Estonia", "Ireland", "Luxembourg", 
                "Belgium", "Italy", "Cyprus", "Spain", "Malta")

# Создаем подвыборки
df_group1 <- pdata[pdata$country %in% less_innov, ]
df_group2 <- pdata[pdata$country %in% more_innov, ]

# Проверяем размеры подвыборок
cat("Количество стран в группе 1:", length(unique(df_group1$country)), "\n")
cat("Количество наблюдений в группе 1:", nrow(df_group1), "\n\n")

cat("Количество стран в группе 2:", length(unique(df_group2$country)), "\n")
cat("Количество наблюдений в группе 2:", nrow(df_group2), "\n")

# System GMM оценка для df_group1 (less_innov)
gmm_model8 <- pgmm(
  roa_diff ~ innov_index_diff +  
    ln_ta + npl + I(npl^2) + liq_diff + I(liq_diff^2) +  car_diff + infl_diff + 
    + gdp_diff |
    lag(roa_diff, 2:3) + lag(innov_index_diff*ln_ta, 2), # Упрощённый набор инструментов
  data = df_group1,
  effect = "individual",
  model = "onestep",
  transformation = "l",
  collapse = TRUE
)
# Робастные стандартные ошибки
summary(gmm_model8, robust = TRUE)

# System GMM оценка для df_group2 (more_innov)
gmm_model9 <- pgmm(
  roa_diff ~  innov_index_diff +  
    ln_ta + npl + I(npl^2) + liq_diff + I(liq_diff^2) +  car_diff + infl_diff + 
    + gdp_diff |
    lag(roa_diff, 2:3) + lag(innov_index_diff*ln_ta, 2), # Упрощённый набор инструментов
  data = df_group2,
  effect = "individual",
  model = "onestep",
  transformation = "l",
  collapse = TRUE
)
# Робастные стандартные ошибки
summary(gmm_model9, robust = TRUE)