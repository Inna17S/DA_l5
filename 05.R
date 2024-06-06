
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(stargazer)
library(sandwich)
library(coda)
library(rjags)

# ==============================================


set.seed(123)  # Для відтворюваності результатів
sampled_data <- filtered_data_3 %>% sample_n(34914)

str(sampled_data )
# Перетворимо категоріальні змінні в числові
sampled_data$Accident_SeverityFatal <- as.numeric(sampled_data$Accident_Severity == "Fatal")
sampled_data$Light_ConditionsDarkness...lighting.unknown <- as.numeric(sampled_data$Light_Conditions == "Darkness - lighting unknown")
sampled_data$Light_ConditionsDarkness...lights.lit <- as.numeric(sampled_data$Light_Conditions == "Darkness - lights lit")
sampled_data$Light_ConditionsDarkness...lights.unlit <- as.numeric(sampled_data$Light_Conditions == "Darkness - lights unlit")
sampled_data$Light_ConditionsDarkness...no.lighting <- as.numeric(sampled_data$Light_Conditions == "Darkness - no lighting")
sampled_data$Weather_ConditionsFine...high.winds <- as.numeric(sampled_data$Weather_Conditions == "Fine + high winds")
sampled_data$Weather_ConditionsFog.or.mist <- as.numeric(sampled_data$Weather_Conditions == "Fog or mist")
sampled_data$Weather_ConditionsOther <- as.numeric(sampled_data$Weather_Conditions == "Other")
sampled_data$Weather_ConditionsRaining...high.winds <- as.numeric(sampled_data$Weather_Conditions == "Raining + high winds")
sampled_data$Weather_ConditionsRaining.no.high.winds <- as.numeric(sampled_data$Weather_Conditions == "Raining no high winds")
sampled_data$Weather_ConditionsSnowing...high.winds <- as.numeric(sampled_data$Weather_Conditions == "Snowing + high winds")
sampled_data$Weather_ConditionsSnowing.no.high.winds <- as.numeric(sampled_data$Weather_Conditions == "Snowing no high winds")
sampled_data$Road_Surface_ConditionsFlood.over.3cm..deep <- as.numeric(sampled_data$Road_Surface_Conditions == "Flood over 3cm deep")
sampled_data$Road_Surface_ConditionsFrost.or.ice <- as.numeric(sampled_data$Road_Surface_Conditions == "Frost or ice")
sampled_data$Road_Surface_ConditionsSnow <- as.numeric(sampled_data$Road_Surface_Conditions == "Snow")
sampled_data$Road_Surface_ConditionsWet.or.damp <- as.numeric(sampled_data$Road_Surface_Conditions == "Wet or damp")
sampled_data$Area_TypeRural <- as.numeric(sampled_data$Area_Type == "Rural")
sampled_data$Day_NightNight <- as.numeric(sampled_data$Day_Night == "Night")
sampled_data$Sex_of_Driver1Female <- as.numeric(sampled_data$Sex_of_Driver1 == "Female")

# Повторіть цей процес для всіх інших змінних, які ви хочете включити

# Створимо новий data_list
data_list <- list(
  N = nrow(sampled_data),
  Y = sampled_data$Accident_SeverityFatal,
  X1 = sampled_data$Light_ConditionsDarkness...lighting.unknown,
  X2 = sampled_data$Light_ConditionsDarkness...lights.lit,
  X3 = sampled_data$Light_ConditionsDarkness...lights.unlit,
  X4 = sampled_data$Light_ConditionsDarkness...no.lighting,
  X5 = sampled_data$Weather_ConditionsFine...high.winds,
  X6 = sampled_data$Weather_ConditionsFog.or.mist,
  X7 = sampled_data$Weather_ConditionsOther,
  X8 = sampled_data$Weather_ConditionsRaining...high.winds,
  X9 = sampled_data$Weather_ConditionsRaining.no.high.winds,
  X10 = sampled_data$Weather_ConditionsSnowing...high.winds,
  X11 = sampled_data$Weather_ConditionsSnowing.no.high.winds,
  X12 = sampled_data$Road_Surface_ConditionsFlood.over.3cm..deep,
  X13 = sampled_data$Road_Surface_ConditionsFrost.or.ice,
  X14 = sampled_data$Road_Surface_ConditionsSnow,
  X15 = sampled_data$Road_Surface_ConditionsWet.or.damp,
  X16 = sampled_data$Area_TypeRural,
  X17 = sampled_data$Day_NightNight,
  X18 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Changing lane to left"),
  X19 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Changing lane to right"),
  X20 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Going ahead left-hand bend"),
  X21 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Going ahead right-hand bend"),
  X22 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Moving off"),
  X23 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Overtaking - nearside"),
  X24 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Overtaking moving vehicle - offside"),
  X25 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Overtaking static vehicle - offside"),
  X26 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Parked"),
  X27 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Reversing"),
  X28 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Slowing or stopping"),
  X29 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Turning left"),
  X30 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Turning right"),
  X31 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "U turn"),
  X32 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Waiting to go - held up"),
  X33 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Waiting to turn left"),
  X34 = as.numeric(sampled_data$Vehicle_Manoeuvre1 == "Waiting to turn right"),
  X35 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Changing lane to left"),
  X36 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Changing lane to right"),
  X37 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Going ahead left-hand bend"),
  X38 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Going ahead right-hand bend"),
  X39 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Moving off"),
  X40 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Overtaking - nearside"),
  X41 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Overtaking moving vehicle - offside"),
  X42 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Overtaking static vehicle - offside"),
  X43 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Parked"),
  X44 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Reversing"),
  X45 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Slowing or stopping"),
  X46 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Turning left"),
  X47 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Turning right"),
  X48 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "U turn"),
  X49 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Waiting to go - held up"),
  X50 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Waiting to turn left"),
  X51 = as.numeric(sampled_data$Vehicle_Manoeuvre2 == "Waiting to turn right"),
  X52 = as.numeric(sampled_data$Road_Type == "Dual carriageway"),
  X53 = as.numeric(sampled_data$Road_Type == "One way street"),
  X54 = as.numeric(sampled_data$Road_Type == "Roundabout"),
  X55 = as.numeric(sampled_data$Road_Type == "Slip road"),
  X56 = as.numeric(sampled_data$Age_Band_of_Driver1 == "10 - 15"),
  X57 = as.numeric(sampled_data$Age_Band_of_Driver1 == "11 - 15"),
  X58 = as.numeric(sampled_data$Age_Band_of_Driver1 == "16 - 20"),
  X59 = as.numeric(sampled_data$Age_Band_of_Driver1 == "21 - 25"),
  X60 = as.numeric(sampled_data$Age_Band_of_Driver1 == "36 - 45"),
  X61 = as.numeric(sampled_data$Age_Band_of_Driver1 == "46 - 55"),
  X62 = as.numeric(sampled_data$Age_Band_of_Driver1 == "56 - 65"),
  X63 = as.numeric(sampled_data$Age_Band_of_Driver1 == "6 - 10"),
  X64 = as.numeric(sampled_data$Age_Band_of_Driver1 == "66 - 75"),
  X65 = as.numeric(sampled_data$Age_Band_of_Driver1 == "Over 75"),
  X66 = sampled_data$Sex_of_Driver1Female,
  X67 = as.numeric(sampled_data$Vehicle_Type1 == "Agricultural vehicle"),
  X68 = as.numeric(sampled_data$Vehicle_Type1 == "Bus or coach (17 or more pass. seats)"),
  X69 = as.numeric(sampled_data$Vehicle_Type1 == "Electric motorcycle"),
  X70 = as.numeric(sampled_data$Vehicle_Type1 == "Goods 7.5 tonnes mgw and over"),
  X71 = as.numeric(sampled_data$Vehicle_Type1 == "Goods over 3.5t. and under 7.5t"),
  X72 = as.numeric(sampled_data$Vehicle_Type1 == "Goods vehicle - unknown weight"),
  X73 = as.numeric(sampled_data$Vehicle_Type1 == "Minibus (8 - 16 passenger seats)"),
  X74 = as.numeric(sampled_data$Vehicle_Type1 == "Mobility scooter"),
  X75 = as.numeric(sampled_data$Vehicle_Type1 == "Motorcycle - unknown cc"),
  X76 = as.numeric(sampled_data$Vehicle_Type1 == "Motorcycle 125cc and under"),
  X77 = as.numeric(sampled_data$Vehicle_Type1 == "Motorcycle 50cc and under"),
  X78 = as.numeric(sampled_data$Vehicle_Type1 == "Motorcycle over 125cc and up to 500cc"),
  X79 = as.numeric(sampled_data$Vehicle_Type1 == "Motorcycle over 500cc"),
  X80 = as.numeric(sampled_data$Vehicle_Type1 == "Other vehicle"),
  X81 = as.numeric(sampled_data$Vehicle_Type1 == "Pedal cycle"),
  X82 = as.numeric(sampled_data$Vehicle_Type1 == "Ridden horse"),
  X83 = as.numeric(sampled_data$Vehicle_Type1 == "Taxi/Private hire car"),
  X84 = as.numeric(sampled_data$Vehicle_Type1 == "Van - Goods 3.5 tonnes mgw or under"),
  X85 = sampled_data$Age_of_Vehicle1
)

# Перевірка розмірностей змінних
lengths <- sapply(data_list, length)
lengths


# ================================================
# ==================================Бібліотеки================================

library(coda)
library(rjags)

# ==================================
# Модель з t-розподілом із 4 ступенями вільності
model_string_t4 <- "
model {
  for (i in 1:N) {
    Y[i] ~ dbern(p[i])
    p[i] <- ilogit(beta0 + beta1 * X1[i] + beta2 * X2[i] + beta3 * X3[i] + beta4 * X4[i] + beta5 * X5[i] + beta6 * X6[i] +
                   beta7 * X7[i] + beta8 * X8[i] + beta9 * X9[i] + beta10 * X10[i] + beta11 * X11[i] + beta12 * X12[i] +
                   beta13 * X13[i] + beta14 * X14[i] + beta15 * X15[i] + beta16 * X16[i] + beta17 * X17[i] +
                   beta18 * X18[i] + beta19 * X19[i] + beta20 * X20[i] + beta21 * X21[i] + beta22 * X22[i] +
                   beta23 * X23[i] + beta24 * X24[i] + beta25 * X25[i] + beta26 * X26[i] + beta27 * X27[i] +
                   beta28 * X28[i] + beta29 * X29[i] + beta30 * X30[i] + beta31 * X31[i] + beta32 * X32[i] +
                   beta33 * X33[i] + beta34 * X34[i] + beta35 * X35[i] + beta36 * X36[i] + beta37 * X37[i] +
                   beta38 * X38[i] + beta39 * X39[i] + beta40 * X40[i] + beta41 * X41[i] + beta42 * X42[i] +
                   beta43 * X43[i] + beta44 * X44[i] + beta45 * X45[i] + beta46 * X46[i] + beta47 * X47[i] +
                   beta48 * X48[i] + beta49 * X49[i] + beta50 * X50[i] + beta51 * X51[i] + beta52 * X52[i] +
                   beta53 * X53[i] + beta54 * X54[i] + beta55 * X55[i] + beta56 * X56[i] + beta57 * X57[i] +
                   beta58 * X58[i] + beta59 * X59[i] + beta60 * X60[i] + beta61 * X61[i] + beta62 * X62[i] +
                   beta63 * X63[i] + beta64 * X64[i] + beta65 * X65[i] + beta66 * X66[i] + beta67 * X67[i] +
                   beta68 * X68[i] + beta69 * X69[i] + beta70 * X70[i] + beta71 * X71[i] + beta72 * X72[i] +
                   beta73 * X73[i] + beta74 * X74[i] + beta75 * X75[i] + beta76 * X76[i] + beta77 * X77[i] +
                   beta78 * X78[i] + beta79 * X79[i] + beta80 * X80[i] + beta81 * X81[i] + beta82 * X82[i] +
                   beta83 * X83[i] + beta84 * X84[i] + beta85 * X85[i])
  }

  beta0 ~ dt(0, 0.01, 4)
  beta1 ~ dt(0, 0.01, 4)
  beta2 ~ dt(0, 0.01, 4)
  beta3 ~ dt(0, 0.01, 4)
  beta4 ~ dt(0, 0.01, 4)
  beta5 ~ dt(0, 0.01, 4)
  beta6 ~ dt(0, 0.01, 4)
  beta7 ~ dt(0, 0.01, 4)
  beta8 ~ dt(0, 0.01, 4)
  beta9 ~ dt(0, 0.01, 4)
  beta10 ~ dt(0, 0.01, 4)
  beta11 ~ dt(0, 0.01, 4)
  beta12 ~ dt(0, 0.01, 4)
  beta13 ~ dt(0, 0.01, 4)
  beta14 ~ dt(0, 0.01, 4)
  beta15 ~ dt(0, 0.01, 4)
  beta16 ~ dt(0, 0.01, 4)
  beta17 ~ dt(0, 0.01, 4)
  beta18 ~ dt(0, 0.01, 4)
  beta19 ~ dt(0, 0.01, 4)
  beta20 ~ dt(0, 0.01, 4)
  beta21 ~ dt(0, 0.01, 4)
  beta22 ~ dt(0, 0.01, 4)
  beta23 ~ dt(0, 0.01, 4)
  beta24 ~ dt(0, 0.01, 4)
  beta25 ~ dt(0, 0.01, 4)
  beta26 ~ dt(0, 0.01, 4)
  beta27 ~ dt(0, 0.01, 4)
  beta28 ~ dt(0, 0.01, 4)
  beta29 ~ dt(0, 0.01, 4)
  beta30 ~ dt(0, 0.01, 4)
  beta31 ~ dt(0, 0.01, 4)
  beta32 ~ dt(0, 0.01, 4)
  beta33 ~ dt(0, 0.01, 4)
  beta34 ~ dt(0, 0.01, 4)
  beta35 ~ dt(0, 0.01, 4)
  beta36 ~ dt(0, 0.01, 4)
  beta37 ~ dt(0, 0.01, 4)
  beta38 ~ dt(0, 0.01, 4)
  beta39 ~ dt(0, 0.01, 4)
  beta40 ~ dt(0, 0.01, 4)
  beta41 ~ dt(0, 0.01, 4)
  beta42 ~ dt(0, 0.01, 4)
  beta43 ~ dt(0, 0.01, 4)
  beta44 ~ dt(0, 0.01, 4)
  beta45 ~ dt(0, 0.01, 4)
  beta46 ~ dt(0, 0.01, 4)
  beta47 ~ dt(0, 0.01, 4)
  beta48 ~ dt(0, 0.01, 4)
  beta49 ~ dt(0, 0.01, 4)
  beta50 ~ dt(0, 0.01, 4)
  beta51 ~ dt(0, 0.01, 4)
  beta52 ~ dt(0, 0.01, 4)
  beta53 ~ dt(0, 0.01, 4)
  beta54 ~ dt(0, 0.01, 4)
  beta55 ~ dt(0, 0.01, 4)
  beta56 ~ dt(0, 0.01, 4)
  beta57 ~ dt(0, 0.01, 4)
  beta58 ~ dt(0, 0.01, 4)
  beta59 ~ dt(0, 0.01, 4)
  beta60 ~ dt(0, 0.01, 4)
  beta61 ~ dt(0, 0.01, 4)
  beta62 ~ dt(0, 0.01, 4)
  beta63 ~ dt(0, 0.01, 4)
  beta64 ~ dt(0, 0.01, 4)
  beta65 ~ dt(0, 0.01, 4)
  beta66 ~ dt(0, 0.01, 4)
  beta67 ~ dt(0, 0.01, 4)
  beta68 ~ dt(0, 0.01, 4)
  beta69 ~ dt(0, 0.01, 4)
  beta70 ~ dt(0, 0.01, 4)
  beta71 ~ dt(0, 0.01, 4)
  beta72 ~ dt(0, 0.01, 4)
  beta73 ~ dt(0, 0.01, 4)
  beta74 ~ dt(0, 0.01, 4)
  beta75 ~ dt(0, 0.01, 4)
  beta76 ~ dt(0, 0.01, 4)
  beta77 ~ dt(0, 0.01, 4)
  beta78 ~ dt(0, 0.01, 4)
  beta79 ~ dt(0, 0.01, 4)
  beta80 ~ dt(0, 0.01, 4)
  beta81 ~ dt(0, 0.01, 4)
  beta82 ~ dt(0, 0.01, 4)
  beta83 ~ dt(0, 0.01, 4)
  beta84 ~ dt(0, 0.01, 4)
  beta85 ~ dt(0, 0.01, 4)
}
"


# Збереження моделей у тимчасові файли
writeLines(model_string_t4, con = "logistic_regression_model_t4.bug")


# Параметри, які слід відстежувати
parameters <- c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "beta8", "beta9", "beta10",
                "beta11", "beta12", "beta13", "beta14", "beta15", "beta16", "beta17", "beta18", "beta19", "beta20",
                "beta21", "beta22", "beta23", "beta24", "beta25", "beta26", "beta27", "beta28", "beta29", "beta30",
                "beta31", "beta32", "beta33", "beta34", "beta35", "beta36", "beta37", "beta38", "beta39", "beta40",
                "beta41", "beta42", "beta43", "beta44", "beta45", "beta46", "beta47", "beta48", "beta49", "beta50",
                "beta51", "beta52", "beta53", "beta54", "beta55", "beta56", "beta57", "beta58", "beta59", "beta60",
                "beta61", "beta62", "beta63", "beta64", "beta65", "beta66", "beta67", "beta68", "beta69", "beta70",
                "beta71", "beta72", "beta73", "beta74", "beta75", "beta76", "beta77", "beta78", "beta79", "beta80",
                "beta81", "beta82", "beta83", "beta84", "beta85")

# Ініціалізація моделі з t-розподілом із 4 ступенями вільності
model_t4 <- jags.model("logistic_regression_model_t4.bug", data = data_list, n.chains = 3, n.adapt = 500)

# Пропуск фази згоряння
update(model_t4, n.iter = 1000)

# Отримання вибірок
samples_t4 <- coda.samples(model_t4, variable.names = parameters, n.iter = 1000)

# Перевірка результатів
summary(samples_t4)

# Візуалізація результатів
plot(samples_t4)


# Обчислення MSE для моделей
# Вибірка даних та їх середнє значення для передбачення
get_predicted_means <- function(samples, data_list) {
  sample_matrix <- as.matrix(samples)
  beta_values <- colMeans(sample_matrix)
  X <- model.matrix(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X30 + X31 + X32 + X33 + X34 + X35 + X36 + X37 + X38 + X39 + X40 + X41 + X42 + X43 + X44 + X45 + X46 + X47 + X48 + X49 + X50 + X51 + X52 + X53 + X54 + X55 + X56 + X57 + X58 + X59 + X60 + X61 + X62 + X63 + X64 + X65 + X66 + X67 + X68 + X69 + X70 + X71 + X72 + X73 + X74 + X75 + X76 + X77 + X78 + X79 + X80 + X81 + X82 + X83 + X84 + X85, data = data_list)
  p <- 1 / (1 + exp(-X %*% beta_values))
  return(p)
}

predicted_normal <- get_predicted_means(samples_normal, data_list)
predicted_t <- get_predicted_means(samples_t, data_list)

# Переконаємося, що довжини збігаються
predicted_normal <- predicted_normal[1:length(data_list$Y)]
predicted_t <- predicted_t[1:length(data_list$Y)]

# Обчислення MSE для моделей
mse_normal <- mean((data_list$Y - predicted_normal)^2, na.rm = TRUE)
mse_t <- mean((data_list$Y - predicted_t)^2, na.rm = TRUE)

# Виведення результатів MSE
print(paste("MSE для моделі з нормальним апріорним розподілом:", mse_normal))
print(paste("MSE для моделі з t-розподілом:", mse_t))



library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(stargazer)
library(sandwich)
library(ggplot2)
library(coda)
library(rjags)

# Завантаження даних
filtered_data_3 <- read.csv("C:/Users/User/Desktop/final_data.csv")

# data <- read.csv("C:/Users/User/Desktop/final_dataваня.csv")
#
# NEW_DATA <- read.csv("C:/Users/User/Desktop/NEW_DATA.csv")

# Перетворення семплів у формат mcmc.list
samples_normal_mcmc <- as.mcmc.list(samples_normal)
samples_t_mcmc <- as.mcmc.list(samples_t)

# Витягнення середніх значень коефіцієнтів
mean_coef_normal <- colMeans(as.matrix(samples_normal_mcmc))
mean_coef_t <- colMeans(as.matrix(samples_t_mcmc))


set.seed(123)

# Вибір рядків з датасету
filtered_data_3 <- filtered_data_3 %>% sample_n(10000)

# str(filtered_data_3)



# Стандартизація змінних, які не є бінарними
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Припустимо, що Age_of_Vehicle1 є неперервною змінною
filtered_data_3 <- filtered_data_3 %>%
  mutate(Age_of_Vehicle1 = standardize(Age_of_Vehicle1))

unique((filtered_data_3$Age_of_Vehicle1))

# Створення змінної групи для ієрархічної моделі
filtered_data_3 <- filtered_data_3 %>%
  mutate(group = factor(1 + (row_number() %% 10)))

# Створимо новий data_list для JAGS з урахуванням нових змінних
data_list_hierarchical <- list(
  N = nrow(filtered_data_3),
  Y = filtered_data_3$Accident_SeverityFatal,
  X1 = filtered_data_3$Light_ConditionsDarkness...lighting.unknown,
  X2 = filtered_data_3$Light_ConditionsDarkness...lights.lit,
  X3 = filtered_data_3$Light_ConditionsDarkness...lights.unlit,
  X4 = filtered_data_3$Light_ConditionsDarkness...no.lighting,
  X5 = filtered_data_3$Weather_ConditionsFine...high.winds,
  X6 = filtered_data_3$Weather_ConditionsFog.or.mist,
  X7 = filtered_data_3$Weather_ConditionsOther,
  X8 = filtered_data_3$Weather_ConditionsRaining...high.winds,
  X9 = filtered_data_3$Weather_ConditionsRaining.no.high.winds,
  X10 = filtered_data_3$Weather_ConditionsSnowing...high.winds,
  X11 = filtered_data_3$Weather_ConditionsSnowing.no.high.winds,
  X12 = filtered_data_3$Road_Surface_ConditionsFlood.over.3cm..deep,
  X13 = filtered_data_3$Road_Surface_ConditionsFrost.or.ice,
  X14 = filtered_data_3$Road_Surface_ConditionsSnow,
  X15 = filtered_data_3$Road_Surface_ConditionsWet.or.damp,
  X16 = filtered_data_3$Area_TypeRural,
  X17 = filtered_data_3$Day_NightNight,
  X18 = filtered_data_3$Vehicle_Manoeuvre1Changing.lane.to.left,
  X19 = filtered_data_3$Vehicle_Manoeuvre1Changing.lane.to.right,
  X20 = filtered_data_3$Vehicle_Manoeuvre1Going.ahead.left.hand.bend,
  X21 = filtered_data_3$Vehicle_Manoeuvre1Going.ahead.right.hand.bend,
  X22 = filtered_data_3$Vehicle_Manoeuvre1Moving.off,
  X23 = filtered_data_3$Vehicle_Manoeuvre1Overtaking...nearside,
  X24 = filtered_data_3$Vehicle_Manoeuvre1Overtaking.moving.vehicle...offside,
  X25 = filtered_data_3$Vehicle_Manoeuvre1Overtaking.static.vehicle...offside,
  X26 = filtered_data_3$Vehicle_Manoeuvre1Parked,
  X27 = filtered_data_3$Vehicle_Manoeuvre1Reversing,
  X28 = filtered_data_3$Vehicle_Manoeuvre1Slowing.or.stopping,
  X29 = filtered_data_3$Vehicle_Manoeuvre1Turning.left,
  X30 = filtered_data_3$Vehicle_Manoeuvre1Turning.right,
  X31 = filtered_data_3$Vehicle_Manoeuvre1U.turn,
  X32 = filtered_data_3$Vehicle_Manoeuvre1Waiting.to.go...held.up,
  X33 = filtered_data_3$Vehicle_Manoeuvre1Waiting.to.turn.left,
  X34 = filtered_data_3$Vehicle_Manoeuvre1Waiting.to.turn.right,
  X35 = filtered_data_3$Vehicle_Manoeuvre2Changing.lane.to.left,
  X36 = filtered_data_3$Vehicle_Manoeuvre2Changing.lane.to.right,
  X37 = filtered_data_3$Vehicle_Manoeuvre2Going.ahead.left.hand.bend,
  X38 = filtered_data_3$Vehicle_Manoeuvre2Going.ahead.right.hand.bend,
  X39 = filtered_data_3$Vehicle_Manoeuvre2Moving.off,
  X40 = filtered_data_3$Vehicle_Manoeuvre2Overtaking...nearside,
  X41 = filtered_data_3$Vehicle_Manoeuvre2Overtaking.moving.vehicle...offside,
  X42 = filtered_data_3$Vehicle_Manoeuvre2Overtaking.static.vehicle...offside,
  X43 = filtered_data_3$Vehicle_Manoeuvre2Parked,
  X44 = filtered_data_3$Vehicle_Manoeuvre2Reversing,
  X45 = filtered_data_3$Vehicle_Manoeuvre2Slowing.or.stopping,
  X46 = filtered_data_3$Vehicle_Manoeuvre2Turning.left,
  X47 = filtered_data_3$Vehicle_Manoeuvre2Turning.right,
  X48 = filtered_data_3$Vehicle_Manoeuvre2U.turn,
  X49 = filtered_data_3$Vehicle_Manoeuvre2Waiting.to.go...held.up,
  X50 = filtered_data_3$Vehicle_Manoeuvre2Waiting.to.turn.left,
  X51 = filtered_data_3$Vehicle_Manoeuvre2Waiting.to.turn.right,
  X52 = filtered_data_3$Road_TypeDual.carriageway,
  X53 = filtered_data_3$Road_TypeOne.way.street,
  X54 = filtered_data_3$Road_TypeRoundabout,
  X55 = filtered_data_3$Road_TypeSlip.road,
  X56 = filtered_data_3$Age_Band_of_Driver10...5,
  X57 = filtered_data_3$Age_Band_of_Driver111...15,
  X58 = filtered_data_3$Age_Band_of_Driver116...20,
  X59 = filtered_data_3$Age_Band_of_Driver121...25,
  X60 = filtered_data_3$Age_Band_of_Driver136...45,
  X61 = filtered_data_3$Age_Band_of_Driver146...55,
  X62 = filtered_data_3$Age_Band_of_Driver156...65,
  X63 = filtered_data_3$Age_Band_of_Driver16...10,
  X64 = filtered_data_3$Age_Band_of_Driver166...75,
  X65 = filtered_data_3$Age_Band_of_Driver1Over.75,
  X66 = filtered_data_3$Sex_of_Driver1Female,
  X67 = filtered_data_3$Vehicle_Type1Agricultural.vehicle,
  X68 = filtered_data_3$Vehicle_Type1Bus.or.coach..17.or.more.pass.seats.,
  X69 = filtered_data_3$Vehicle_Type1Electric.motorcycle,
  X70 = filtered_data_3$Vehicle_Type1Goods.7.5.tonnes.mgw.and.over,
  X71 = filtered_data_3$Vehicle_Type1Goods.over.3.5t..and.under.7.5t,
  X72 = filtered_data_3$Vehicle_Type1Goods.vehicle...unknown.weight,
  X73 = filtered_data_3$Vehicle_Type1Minibus..8...16.passenger.seats.,
  X74 = filtered_data_3$Vehicle_Type1Mobility.scooter,
  X75 = filtered_data_3$Vehicle_Type1Motorcycle...unknown.cc,
  X76 = filtered_data_3$Vehicle_Type1Motorcycle.125cc.and.under,
  X77 = filtered_data_3$Vehicle_Type1Motorcycle.50cc.and.under,
  X78 = filtered_data_3$Vehicle_Type1Motorcycle.over.125cc.and.up.to.500cc,
  X79 = filtered_data_3$Vehicle_Type1Motorcycle.over.500cc,
  X80 = filtered_data_3$Vehicle_Type1Other.vehicle,
  X81 = filtered_data_3$Vehicle_Type1Pedal.cycle,
  X82 = filtered_data_3$Vehicle_Type1Ridden.horse,
  X83 = filtered_data_3$Vehicle_Type1Taxi.Private.hire.car,
  X84 = filtered_data_3$Vehicle_Type1Van...Goods.3.5.tonnes.mgw.or.under,
  X85 = filtered_data_3$Age_of_Vehicle1,
  group = as.numeric(filtered_data_3$group),
  G = length(unique(filtered_data_3$group))
)


hierarchical_model_string <- "
model {
  for (i in 1:N) {
    Y[i] ~ dbern(p[i])
    p[i] <- ilogit(beta0[group[i]] + beta1[group[i]] * X1[i] + beta2[group[i]] * X2[i] +
                   beta3[group[i]] * X3[i] + beta4[group[i]] * X4[i] + beta5[group[i]] * X5[i] +
                   beta6[group[i]] * X6[i] + beta7[group[i]] * X7[i] + beta8[group[i]] * X8[i] +
                   beta9[group[i]] * X9[i] + beta10[group[i]] * X10[i] + beta11[group[i]] * X11[i] +
                   beta12[group[i]] * X12[i] + beta13[group[i]] * X13[i]
                   + beta14[group[i]] * X14[i]+ beta15[group[i]] * X15[i]   + beta16[group[i]] * X16[i]
                   + beta17[group[i]] * X17[i]
                   + beta18[group[i]] * X18[i]
                   + beta19[group[i]] * X19[i]
                   + beta20[group[i]] * X20[i]
                   + beta21[group[i]] * X21[i]
                   + beta22[group[i]] * X22[i]
                   + beta23[group[i]] * X23[i]
                   + beta24[group[i]] * X24[i]
                   + beta25[group[i]] * X25[i]
                   + beta26[group[i]] * X26[i]
                   + beta27[group[i]] * X27[i]
                   + beta28[group[i]] * X28[i]
                   + beta29[group[i]] * X29[i]
                   + beta30[group[i]] * X30[i]
                   + beta31[group[i]] * X31[i]
                   + beta32[group[i]] * X32[i]
                   + beta33[group[i]] * X33[i]
                   + beta34[group[i]] * X34[i]
                   + beta35[group[i]] * X35[i]
                   + beta36[group[i]] * X36[i]
                   + beta37[group[i]] * X37[i]
                   + beta38[group[i]] * X38[i]
                   + beta39[group[i]] * X39[i]
                   + beta40[group[i]] * X40[i]
                   + beta41[group[i]] * X41[i]
                   + beta42[group[i]] * X42[i]
                   + beta43[group[i]] * X43[i]
                   + beta44[group[i]] * X44[i]
                   + beta45[group[i]] * X45[i]
                   + beta46[group[i]] * X46[i]
                   + beta47[group[i]] * X47[i]
                   + beta48[group[i]] * X48[i]
                   + beta49[group[i]] * X49[i]
                   + beta50[group[i]] * X50[i]
                   + beta51[group[i]] * X51[i]
                   + beta52[group[i]] * X52[i]
                   + beta53[group[i]] * X53[i]
                   + beta54[group[i]] * X54[i]
                   + beta55[group[i]] * X55[i]
                   + beta56[group[i]] * X56[i]
                   + beta57[group[i]] * X57[i]
                   + beta58[group[i]] * X58[i]
                   + beta59[group[i]] * X59[i]
                   + beta60[group[i]] * X60[i]
                   + beta61[group[i]] * X61[i]
                   + beta62[group[i]] * X62[i]
                   + beta63[group[i]] * X63[i]
                   + beta64[group[i]] * X64[i]
                   + beta65[group[i]] * X65[i]
                   + beta66[group[i]] * X66[i]
                   + beta67[group[i]] * X67[i]
                   + beta68[group[i]] * X68[i]
                   + beta69[group[i]] * X69[i]
                   + beta70[group[i]] * X70[i]
                   + beta71[group[i]] * X71[i]
                   + beta72[group[i]] * X72[i]
                   + beta73[group[i]] * X73[i]
                   + beta74[group[i]] * X74[i]
                   + beta75[group[i]] * X75[i]
                   + beta76[group[i]] * X76[i]
                   + beta77[group[i]] * X77[i]
                   + beta78[group[i]] * X78[i]
                   + beta79[group[i]] * X79[i]
                   + beta80[group[i]] * X80[i]
                   + beta81[group[i]] * X81[i]
                   + beta82[group[i]] * X82[i]
                   + beta83[group[i]] * X83[i]
                   + beta84[group[i]] * X84[i]
                   + beta85[group[i]] * X85[i]

    )


  }

  # Апріорні розподіли для кожної групи
  for (j in 1:G) {
    beta0[j] ~ dnorm(0, 0.01)
    beta1[j] ~ dnorm(0, 0.01)
    beta2[j] ~ dnorm(0, 0.01)
    beta3[j] ~ dnorm(0, 0.01)
    beta4[j] ~ dnorm(0, 0.01)
    beta5[j] ~ dnorm(0, 0.01)
    beta6[j] ~ dnorm(0, 0.01)
    beta7[j] ~ dnorm(0, 0.01)
    beta8[j] ~ dnorm(0, 0.01)
    beta9[j] ~ dnorm(0, 0.01)
    beta10[j] ~ dnorm(0, 0.01)
    beta11[j] ~ dnorm(0, 0.01)
    beta12[j] ~ dnorm(0, 0.01)
    beta13[j] ~ dnorm(0, 0.01)
    beta14[j] ~ dnorm(0, 0.01)
    beta15[j] ~ dnorm(0, 0.01)
    beta16[j] ~ dnorm(0, 0.01)
    beta17[j] ~ dnorm(0, 0.01)
    beta18[j] ~ dnorm(0, 0.01)
    beta19[j] ~ dnorm(0, 0.01)
    beta20[j] ~ dnorm(0, 0.01)
    beta21[j] ~ dnorm(0, 0.01)
    beta22[j] ~ dnorm(0, 0.01)
    beta23[j] ~ dnorm(0, 0.01)
    beta24[j] ~ dnorm(0, 0.01)
    beta25[j] ~ dnorm(0, 0.01)
    beta26[j] ~ dnorm(0, 0.01)
    beta27[j] ~ dnorm(0, 0.01)
    beta28[j] ~ dnorm(0, 0.01)
    beta29[j] ~ dnorm(0, 0.01)
    beta30[j] ~ dnorm(0, 0.01)
    beta31[j] ~ dnorm(0, 0.01)
    beta32[j] ~ dnorm(0, 0.01)
    beta33[j] ~ dnorm(0, 0.01)
    beta34[j] ~ dnorm(0, 0.01)
    beta35[j] ~ dnorm(0, 0.01)
    beta36[j] ~ dnorm(0, 0.01)
    beta37[j] ~ dnorm(0, 0.01)
    beta38[j] ~ dnorm(0, 0.01)
    beta39[j] ~ dnorm(0, 0.01)
    beta40[j] ~ dnorm(0, 0.01)
    beta41[j] ~ dnorm(0, 0.01)
    beta42[j] ~ dnorm(0, 0.01)
    beta43[j] ~ dnorm(0, 0.01)
    beta44[j] ~ dnorm(0, 0.01)
    beta45[j] ~ dnorm(0, 0.01)
    beta46[j] ~ dnorm(0, 0.01)
    beta47[j] ~ dnorm(0, 0.01)
    beta48[j] ~ dnorm(0, 0.01)
    beta49[j] ~ dnorm(0, 0.01)
    beta50[j] ~ dnorm(0, 0.01)
    beta51[j] ~ dnorm(0, 0.01)
    beta52[j] ~ dnorm(0, 0.01)
    beta53[j] ~ dnorm(0, 0.01)
    beta54[j] ~ dnorm(0, 0.01)
    beta55[j] ~ dnorm(0, 0.01)
    beta56[j] ~ dnorm(0, 0.01)
    beta57[j] ~ dnorm(0, 0.01)
    beta58[j] ~ dnorm(0, 0.01)
    beta59[j] ~ dnorm(0, 0.01)
    beta60[j] ~ dnorm(0, 0.01)
    beta61[j] ~ dnorm(0, 0.01)
    beta62[j] ~ dnorm(0, 0.01)
    beta63[j] ~ dnorm(0, 0.01)
    beta64[j] ~ dnorm(0, 0.01)
    beta65[j] ~ dnorm(0, 0.01)
    beta66[j] ~ dnorm(0, 0.01)
    beta67[j] ~ dnorm(0, 0.01)
    beta68[j] ~ dnorm(0, 0.01)
    beta69[j] ~ dnorm(0, 0.01)
    beta70[j] ~ dnorm(0, 0.01)
    beta71[j] ~ dnorm(0, 0.01)
    beta72[j] ~ dnorm(0, 0.01)
    beta73[j] ~ dnorm(0, 0.01)
    beta74[j] ~ dnorm(0, 0.01)
    beta75[j] ~ dnorm(0, 0.01)
    beta76[j] ~ dnorm(0, 0.01)
    beta77[j] ~ dnorm(0, 0.01)
    beta78[j] ~ dnorm(0, 0.01)
    beta79[j] ~ dnorm(0, 0.01)
    beta80[j] ~ dnorm(0, 0.01)
    beta81[j] ~ dnorm(0, 0.01)
    beta82[j] ~ dnorm(0, 0.01)
    beta83[j] ~ dnorm(0, 0.01)
    beta84[j] ~ dnorm(0, 0.01)
    beta85[j] ~ dnorm(0, 0.01)
  }
}
"

writeLines(hierarchical_model_string, con = "hierarchical_modelнСТАНД12.bug")


# Ініціалізація ієрархічної моделі
hierarchical_model <- jags.model("hierarchical_modelнСТАНД12.bug", data = data_list_hierarchical, n.chains = 3, n.adapt = 500)
update(hierarchical_model, 500)

# Генерація вибірок з апостеріорного розподілу
hierarchical_samples <- coda.samples(hierarchical_model, variable.names = paste0("beta", 0:85), n.iter = 3000)

# Перевірка конвергенції
summary(hierarchical_samples)
traceplot(hierarchical_samples)

save(hierarchical_samples, file = "hierarchical_samplesн85СТАНД12.RData")

# Відкриття PDF файлу для збереження графіків
pdf("traceplots_hierarchical85СТАНД12.pdf")

# Створення графіків трас
traceplot(hierarchical_samples)

# Закриття PDF файлу
dev.off()



# Побудова графіків залишків для обох моделей
par(mfrow = c(2, 2))

# Графік залишків для нормального розподілу
plot(predicted_normal, residuals_normal,
     main = "Залишки vs Передбачення (Нормальний розподіл)",
     xlab = "Передбачені значення",
     ylab = "Залишки",
     pch = 20, col = "blue")
abline(h = 0, col = "red")

# Гістограма залишків для нормального розподілу
hist(residuals_normal, breaks = 30,
     main = "Гістограма залишків (Нормальний розподіл)",
     xlab = "Залишки",
     col = "lightblue", border = "black")

# Графік залишків для t-розподілу
plot(predicted_t, residuals_t,
     main = "Залишки vs Передбачення (t-розподіл)",
     xlab = "Передбачені значення",
     ylab = "Залишки",
     pch = 20, col = "blue")
abline(h = 0, col = "red")

# Гістограма залишків для t-розподілу
hist(residuals_t, breaks = 30,
     main = "Гістограма залишків (t-розподіл)",
     xlab = "Залишки",
     col = "lightblue", border = "black")




library(coda)

# Завантаження семплів (змінити шляхи до файлів відповідно до ваших даних)
samples <- readRDS(file.choose())
samples_t <- readRDS(file.choose())

# Перетворення семплів у формат mcmc.list
samples_mcmc <- as.mcmc.list(samples)
samples_t_mcmc <- as.mcmc.list(samples_t)

# Розрахунок статистики Гелмана-Рубіна для кожного набору семплів
gelman_diag_normal <- gelman.diag(samples_mcmc)
gelman_diag_t <- gelman.diag(samples_t_mcmc)

# Виведення результатів
cat("Статистика Гелмана-Рубіна для моделі з нормальним апріорним розподілом:\n")
print(gelman_diag_normal)
cat("Статистика Гелмана-Рубіна для моделі з t-розподілом:\n")
print(gelman_diag_t)


# Розрахунок помилок передбачень
errors_normal <- data_list$Y - predicted_normal
errors_t <- data_list$Y - predicted_t

# Побудова гістограм для розподілу помилок
par(mfrow = c(2, 1))  # Налаштування виводу двох графіків в одному вікні

hist(errors_normal, breaks = 30, main = "Розподіл помилок для нормального розподілу", xlab = "Помилки передбачень", col = "lightgray", border = "black")
hist(errors_t, breaks = 30, main = "Розподіл помилок для t-розподілу", xlab = "Помилки передбачень", col = "lightgray", border = "black")

# Виведення основних статистик
cat("Статистики помилок для нормального розподілу:\n")
summary(errors_normal)
cat("Статистики помилок для t-розподілу:\n")
summary(errors_t)

library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(stargazer)
library(sandwich)
library(ggplot2)
library(coda)
library(rjags)

# Вибір рядків з датасету
filtered_data_3 <- filtered_data_3 %>% sample_n(10000)

# Стандартизація змінних, які не є бінарними
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Припустимо, що Age_of_Vehicle1 є неперервною змінною
filtered_data_3 <- filtered_data_3 %>%
  mutate(Age_of_Vehicle1 = standardize(Age_of_Vehicle1))


# Створення змінної групи для ієрархічної моделі
filtered_data_3 <- filtered_data_3 %>%
  mutate(group = factor(1 + (row_number() %% 10)))

# Створимо новий data_list для JAGS з урахуванням нових змінних
data_list <- list(
  N = nrow(filtered_data_3),
  Y = filtered_data_3$Accident_SeverityFatal,
  X1 = filtered_data_3$Light_ConditionsDarkness...lighting.unknown,
  X2 = filtered_data_3$Light_ConditionsDarkness...lights.lit,
  X3 = filtered_data_3$Light_ConditionsDarkness...lights.unlit,
  X4 = filtered_data_3$Light_ConditionsDarkness...no.lighting,
  X5 = filtered_data_3$Weather_ConditionsFine...high.winds,
  X6 = filtered_data_3$Weather_ConditionsFog.or.mist,
  X7 = filtered_data_3$Weather_ConditionsOther,
  X8 = filtered_data_3$Weather_ConditionsRaining...high.winds,
  X9 = filtered_data_3$Weather_ConditionsRaining.no.high.winds,
  X10 = filtered_data_3$Weather_ConditionsSnowing...high.winds,
  X11 = filtered_data_3$Weather_ConditionsSnowing.no.high.winds,
  X12 = filtered_data_3$Road_Surface_ConditionsFlood.over.3cm..deep,
  X13 = filtered_data_3$Road_Surface_ConditionsFrost.or.ice,
  X14 = filtered_data_3$Road_Surface_ConditionsSnow,
  X15 = filtered_data_3$Road_Surface_ConditionsWet.or.damp,
  X16 = filtered_data_3$Area_TypeRural,
  X17 = filtered_data_3$Day_NightNight,
  X18 = filtered_data_3$Vehicle_Manoeuvre1Changing.lane.to.left,
  X19 = filtered_data_3$Vehicle_Manoeuvre1Changing.lane.to.right,
  X20 = filtered_data_3$Vehicle_Manoeuvre1Going.ahead.left.hand.bend,
  X21 = filtered_data_3$Vehicle_Manoeuvre1Going.ahead.right.hand.bend,
  X22 = filtered_data_3$Vehicle_Manoeuvre1Moving.off,
  X23 = filtered_data_3$Vehicle_Manoeuvre1Overtaking...nearside,
  X24 = filtered_data_3$Vehicle_Manoeuvre1Overtaking.moving.vehicle...offside,
  X25 = filtered_data_3$Vehicle_Manoeuvre1Overtaking.static.vehicle...offside,
  X26 = filtered_data_3$Vehicle_Manoeuvre1Parked,
  X27 = filtered_data_3$Vehicle_Manoeuvre1Reversing,
  X28 = filtered_data_3$Vehicle_Manoeuvre1Slowing.or.stopping,
  X29 = filtered_data_3$Vehicle_Manoeuvre1Turning.left,
  X30 = filtered_data_3$Vehicle_Manoeuvre1Turning.right,
  X31 = filtered_data_3$Vehicle_Manoeuvre1U.turn,
  X32 = filtered_data_3$Vehicle_Manoeuvre1Waiting.to.go...held.up,
  X33 = filtered_data_3$Vehicle_Manoeuvre1Waiting.to.turn.left,
  X34 = filtered_data_3$Vehicle_Manoeuvre1Waiting.to.turn.right,
  X35 = filtered_data_3$Vehicle_Manoeuvre2Changing.lane.to.left,
  X36 = filtered_data_3$Vehicle_Manoeuvre2Changing.lane.to.right,
  X37 = filtered_data_3$Vehicle_Manoeuvre2Going.ahead.left.hand.bend,
  X38 = filtered_data_3$Vehicle_Manoeuvre2Going.ahead.right.hand.bend,
  X39 = filtered_data_3$Vehicle_Manoeuvre2Moving.off,
  X40 = filtered_data_3$Vehicle_Manoeuvre2Overtaking...nearside,
  X41 = filtered_data_3$Vehicle_Manoeuvre2Overtaking.moving.vehicle...offside,
  X42 = filtered_data_3$Vehicle_Manoeuvre2Overtaking.static.vehicle...offside,
  X43 = filtered_data_3$Vehicle_Manoeuvre2Parked,
  X44 = filtered_data_3$Vehicle_Manoeuvre2Reversing,
  X45 = filtered_data_3$Vehicle_Manoeuvre2Slowing.or.stopping,
  X46 = filtered_data_3$Vehicle_Manoeuvre2Turning.left,
  X47 = filtered_data_3$Vehicle_Manoeuvre2Turning.right,
  X48 = filtered_data_3$Vehicle_Manoeuvre2U.turn,
  X49 = filtered_data_3$Vehicle_Manoeuvre2Waiting.to.go...held.up,
  X50 = filtered_data_3$Vehicle_Manoeuvre2Waiting.to.turn.left,
  X51 = filtered_data_3$Vehicle_Manoeuvre2Waiting.to.turn.right,
  X52 = filtered_data_3$Road_TypeDual.carriageway,
  X53 = filtered_data_3$Road_TypeOne.way.street,
  X54 = filtered_data_3$Road_TypeRoundabout,
  X55 = filtered_data_3$Road_TypeSlip.road,
  X56 = filtered_data_3$Age_Band_of_Driver10...5,
  X57 = filtered_data_3$Age_Band_of_Driver111...15,
  X58 = filtered_data_3$Age_Band_of_Driver116...20,
  X59 = filtered_data_3$Age_Band_of_Driver121...25,
  X60 = filtered_data_3$Age_Band_of_Driver136...45,
  X61 = filtered_data_3$Age_Band_of_Driver146...55,
  X62 = filtered_data_3$Age_Band_of_Driver156...65,
  X63 = filtered_data_3$Age_Band_of_Driver16...10,
  X64 = filtered_data_3$Age_Band_of_Driver166...75,
  X65 = filtered_data_3$Age_Band_of_Driver1Over.75,
  X66 = filtered_data_3$Sex_of_Driver1Female,
  X67 = filtered_data_3$Vehicle_Type1Agricultural.vehicle,
  X68 = filtered_data_3$Vehicle_Type1Bus.or.coach..17.or.more.pass.seats.,
  X69 = filtered_data_3$Vehicle_Type1Electric.motorcycle,
  X70 = filtered_data_3$Vehicle_Type1Goods.7.5.tonnes.mgw.and.over,
  X71 = filtered_data_3$Vehicle_Type1Goods.over.3.5t..and.under.7.5t,
  X72 = filtered_data_3$Vehicle_Type1Goods.vehicle...unknown.weight,
  X73 = filtered_data_3$Vehicle_Type1Minibus..8...16.passenger.seats.,
  X74 = filtered_data_3$Vehicle_Type1Mobility.scooter,
  X75 = filtered_data_3$Vehicle_Type1Motorcycle...unknown.cc,
  X76 = filtered_data_3$Vehicle_Type1Motorcycle.125cc.and.under,
  X77 = filtered_data_3$Vehicle_Type1Motorcycle.50cc.and.under,
  X78 = filtered_data_3$Vehicle_Type1Motorcycle.over.125cc.and.up.to.500cc,
  X79 = filtered_data_3$Vehicle_Type1Motorcycle.over.500cc,
  X80 = filtered_data_3$Vehicle_Type1Other.vehicle,
  X81 = filtered_data_3$Vehicle_Type1Pedal.cycle,
  X82 = filtered_data_3$Vehicle_Type1Ridden.horse,
  X83 = filtered_data_3$Vehicle_Type1Taxi.Private.hire.car,
  X84 = filtered_data_3$Vehicle_Type1Van...Goods.3.5.tonnes.mgw.or.under,
  X85 = filtered_data_3$Age_of_Vehicle1,
  group = as.numeric(filtered_data_3$group),
  G = length(unique(filtered_data_3$group))
)


prior_predictive_model_string <- "
model {
  for (i in 1:N) {
    Y[i] ~ dbern(p[i])
    p[i] <- ilogit(beta0[group[i]] + beta1[group[i]] * X1[i] + beta2[group[i]] * X2[i] +
                   beta3[group[i]] * X3[i] + beta4[group[i]] * X4[i] + beta5[group[i]] * X5[i] +
                   beta6[group[i]] * X6[i] + beta7[group[i]] * X7[i] + beta8[group[i]] * X8[i] +
                   beta9[group[i]] * X9[i] + beta10[group[i]] * X10[i] + beta11[group[i]] * X11[i] +
                   beta12[group[i]] * X12[i] + beta13[group[i]] * X13[i]
                   + beta14[group[i]] * X14[i]+ beta15[group[i]] * X15[i]   + beta16[group[i]] * X16[i]
                   + beta17[group[i]] * X17[i]
                   + beta18[group[i]] * X18[i]
                   + beta19[group[i]] * X19[i]
                   + beta20[group[i]] * X20[i]
                   + beta21[group[i]] * X21[i]
                   + beta22[group[i]] * X22[i]
                   + beta23[group[i]] * X23[i]
                   + beta24[group[i]] * X24[i]
                   + beta25[group[i]] * X25[i]
                   + beta26[group[i]] * X26[i]
                   + beta27[group[i]] * X27[i]
                   + beta28[group[i]] * X28[i]
                   + beta29[group[i]] * X29[i]
                   + beta30[group[i]] * X30[i]
                   + beta31[group[i]] * X31[i]
                   + beta32[group[i]] * X32[i]
                   + beta33[group[i]] * X33[i]
                   + beta34[group[i]] * X34[i]
                   + beta35[group[i]] * X35[i]
                   + beta36[group[i]] * X36[i]
                   + beta37[group[i]] * X37[i]
                   + beta38[group[i]] * X38[i]
                   + beta39[group[i]] * X39[i]
                   + beta40[group[i]] * X40[i]
                   + beta41[group[i]] * X41[i]
                   + beta42[group[i]] * X42[i]
                   + beta43[group[i]] * X43[i]
                   + beta44[group[i]] * X44[i]
                   + beta45[group[i]] * X45[i]
                   + beta46[group[i]] * X46[i]
                   + beta47[group[i]] * X47[i]
                   + beta48[group[i]] * X48[i]
                   + beta49[group[i]] * X49[i]
                   + beta50[group[i]] * X50[i]
                   + beta51[group[i]] * X51[i]
                   + beta52[group[i]] * X52[i]
                   + beta53[group[i]] * X53[i]
                   + beta54[group[i]] * X54[i]
                   + beta55[group[i]] * X55[i]
                   + beta56[group[i]] * X56[i]
                   + beta57[group[i]] * X57[i]
                   + beta58[group[i]] * X58[i]
                   + beta59[group[i]] * X59[i]
                   + beta60[group[i]] * X60[i]
                   + beta61[group[i]] * X61[i]
                   + beta62[group[i]] * X62[i]
                   + beta63[group[i]] * X63[i]
                   + beta64[group[i]] * X64[i]
                   + beta65[group[i]] * X65[i]
                   + beta66[group[i]] * X66[i]
                   + beta67[group[i]] * X67[i]
                   + beta68[group[i]] * X68[i]
                   + beta69[group[i]] * X69[i]
                   + beta70[group[i]] * X70[i]
                   + beta71[group[i]] * X71[i]
                   + beta72[group[i]] * X72[i]
                   + beta73[group[i]] * X73[i]
                   + beta74[group[i]] * X74[i]
                   + beta75[group[i]] * X75[i]
                   + beta76[group[i]] * X76[i]
                   + beta77[group[i]] * X77[i]
                   + beta78[group[i]] * X78[i]
                   + beta79[group[i]] * X79[i]
                   + beta80[group[i]] * X80[i]
                   + beta81[group[i]] * X81[i]
                   + beta82[group[i]] * X82[i]
                   + beta83[group[i]] * X83[i]
                   + beta84[group[i]] * X84[i]
                   + beta85[group[i]] * X85[i]

    )


  }

  # Апріорні розподіли для кожної групи
  for (j in 1:G) {
    beta0[j] ~ dnorm(0, 0.01)
    beta1[j] ~ dnorm(0, 0.01)
    beta2[j] ~ dnorm(0, 0.01)
    beta3[j] ~ dnorm(0, 0.01)
    beta4[j] ~ dnorm(0, 0.01)
    beta5[j] ~ dnorm(0, 0.01)
    beta6[j] ~ dnorm(0, 0.01)
    beta7[j] ~ dnorm(0, 0.01)
    beta8[j] ~ dnorm(0, 0.01)
    beta9[j] ~ dnorm(0, 0.01)
    beta10[j] ~ dnorm(0, 0.01)
    beta11[j] ~ dnorm(0, 0.01)
    beta12[j] ~ dnorm(0, 0.01)
    beta13[j] ~ dnorm(0, 0.01)
    beta14[j] ~ dnorm(0, 0.01)
    beta15[j] ~ dnorm(0, 0.01)
    beta16[j] ~ dnorm(0, 0.01)
    beta17[j] ~ dnorm(0, 0.01)
    beta18[j] ~ dnorm(0, 0.01)
    beta19[j] ~ dnorm(0, 0.01)
    beta20[j] ~ dnorm(0, 0.01)
    beta21[j] ~ dnorm(0, 0.01)
    beta22[j] ~ dnorm(0, 0.01)
    beta23[j] ~ dnorm(0, 0.01)
    beta24[j] ~ dnorm(0, 0.01)
    beta25[j] ~ dnorm(0, 0.01)
    beta26[j] ~ dnorm(0, 0.01)
    beta27[j] ~ dnorm(0, 0.01)
    beta28[j] ~ dnorm(0, 0.01)
    beta29[j] ~ dnorm(0, 0.01)
    beta30[j] ~ dnorm(0, 0.01)
    beta31[j] ~ dnorm(0, 0.01)
    beta32[j] ~ dnorm(0, 0.01)
    beta33[j] ~ dnorm(0, 0.01)
    beta34[j] ~ dnorm(0, 0.01)
    beta35[j] ~ dnorm(0, 0.01)
    beta36[j] ~ dnorm(0, 0.01)
    beta37[j] ~ dnorm(0, 0.01)
    beta38[j] ~ dnorm(0, 0.01)
    beta39[j] ~ dnorm(0, 0.01)
    beta40[j] ~ dnorm(0, 0.01)
    beta41[j] ~ dnorm(0, 0.01)
    beta42[j] ~ dnorm(0, 0.01)
    beta43[j] ~ dnorm(0, 0.01)
    beta44[j] ~ dnorm(0, 0.01)
    beta45[j] ~ dnorm(0, 0.01)
    beta46[j] ~ dnorm(0, 0.01)
    beta47[j] ~ dnorm(0, 0.01)
    beta48[j] ~ dnorm(0, 0.01)
    beta49[j] ~ dnorm(0, 0.01)
    beta50[j] ~ dnorm(0, 0.01)
    beta51[j] ~ dnorm(0, 0.01)
    beta52[j] ~ dnorm(0, 0.01)
    beta53[j] ~ dnorm(0, 0.01)
    beta54[j] ~ dnorm(0, 0.01)
    beta55[j] ~ dnorm(0, 0.01)
    beta56[j] ~ dnorm(0, 0.01)
    beta57[j] ~ dnorm(0, 0.01)
    beta58[j] ~ dnorm(0, 0.01)
    beta59[j] ~ dnorm(0, 0.01)
    beta60[j] ~ dnorm(0, 0.01)
    beta61[j] ~ dnorm(0, 0.01)
    beta62[j] ~ dnorm(0, 0.01)
    beta63[j] ~ dnorm(0, 0.01)
    beta64[j] ~ dnorm(0, 0.01)
    beta65[j] ~ dnorm(0, 0.01)
    beta66[j] ~ dnorm(0, 0.01)
    beta67[j] ~ dnorm(0, 0.01)
    beta68[j] ~ dnorm(0, 0.01)
    beta69[j] ~ dnorm(0, 0.01)
    beta70[j] ~ dnorm(0, 0.01)
    beta71[j] ~ dnorm(0, 0.01)
    beta72[j] ~ dnorm(0, 0.01)
    beta73[j] ~ dnorm(0, 0.01)
    beta74[j] ~ dnorm(0, 0.01)
    beta75[j] ~ dnorm(0, 0.01)
    beta76[j] ~ dnorm(0, 0.01)
    beta77[j] ~ dnorm(0, 0.01)
    beta78[j] ~ dnorm(0, 0.01)
    beta79[j] ~ dnorm(0, 0.01)
    beta80[j] ~ dnorm(0, 0.01)
    beta81[j] ~ dnorm(0, 0.01)
    beta82[j] ~ dnorm(0, 0.01)
    beta83[j] ~ dnorm(0, 0.01)
    beta84[j] ~ dnorm(0, 0.01)
    beta85[j] ~ dnorm(0, 0.01)
  }
}
"

writeLines(prior_predictive_model_string, con = "prior_predictive_model.bug")
# Ініціалізація моделі
prior_predictive_model <- jags.model("prior_predictive_model.bug", data = data_list, n.chains = 1, n.adapt = 100)
update(prior_predictive_model, 1000)

# Генерація апріорних предиктивних вибірок
prior_samples <- coda.samples(prior_predictive_model, variable.names = c("beta0", "beta1", "beta2", "beta3", "beta4",
                                                                         "beta5", "beta6", "beta7", "beta8", "beta9",
                                                                         "beta10", "beta11", "beta12", "beta13", "beta14",
                                                                         "beta15", "beta16", "beta17", "beta18", "beta19",
                                                                         "beta20", "beta21", "beta22", "beta23", "beta24",
                                                                         "beta25", "beta26", "beta27", "beta28", "beta29",
                                                                         "beta30", "beta31", "beta32", "beta33", "beta34",
                                                                         "beta35", "beta36", "beta37", "beta38", "beta39",
                                                                         "beta40", "beta41", "beta42", "beta43", "beta44",
                                                                         "beta45", "beta46", "beta47", "beta48", "beta49",
                                                                         "beta50", "beta51", "beta52", "beta53", "beta54",
                                                                         "beta55", "beta56", "beta57", "beta58", "beta59",
                                                                         "beta60", "beta61", "beta62", "beta63", "beta64",
                                                                         "beta65", "beta66", "beta67", "beta68", "beta69",
                                                                         "beta70", "beta71", "beta72", "beta73", "beta74",
                                                                         "beta75", "beta76", "beta77", "beta78", "beta79",
                                                                         "beta80", "beta81", "beta82", "beta83", "beta84",
                                                                         "beta85"), n.iter = 1000)


# Збереження апріорних вибірок у файл
saveRDS(prior_samples, file = "prior_samples.rds")

# Генерація апріорних предиктивних результатів
prior_predictive_y <- sapply(1:100, function(i) {
  beta <- as.vector(as.mcmc(prior_samples)[i, ])
  logit_p <- beta[1] + beta[2] * data_list$X1 + beta[3] * data_list$X2 + beta[4] * data_list$X3 +
    beta[5] * data_list$X4 + beta[6] * data_list$X5 + beta[7] * data_list$X6 + beta[8] * data_list$X7 +
    beta[9] * data_list$X8 + beta[10] * data_list$X9 + beta[11] * data_list$X10 + beta[12] * data_list$X11 +
    beta[13] * data_list$X12 + beta[14] * data_list$X13 + beta[15] * data_list$X14 + beta[16] * data_list$X15 + beta[17] * data_list$X16 + beta[18] * data_list$X17 +
    beta[19] * data_list$X18 + beta[20] * data_list$X19 + beta[21] * data_list$X20 + beta[22] * data_list$X21 +
    beta[23] * data_list$X22 + beta[24] * data_list$X23 + beta[25] * data_list$X24 + beta[26] * data_list$X25 + beta[27] * data_list$X26 + beta[28] * data_list$X27 +
    beta[29] * data_list$X28 + beta[30] * data_list$X29 + beta[31] * data_list$X30 + beta[32] * data_list$X31 +
    beta[33] * data_list$X32 + beta[34] * data_list$X33 + beta[35] * data_list$X34 + beta[36] * data_list$X35 + beta[37] * data_list$X36 + beta[38] * data_list$X37 +
    beta[39] * data_list$X38 + beta[40] * data_list$X39 + beta[41] * data_list$X40 + beta[42] * data_list$X41 +
    beta[43] * data_list$X42 + beta[44] * data_list$X43 + beta[45] * data_list$X44 + beta[46] * data_list$X45 + beta[47] * data_list$X46 + beta[48] * data_list$X47 +
    beta[49] * data_list$X48 + beta[50] * data_list$X49 + beta[51] * data_list$X50 + beta[52] * data_list$X51 +
    beta[53] * data_list$X52 + beta[54] * data_list$X53 + beta[55] * data_list$X54 + beta[56] * data_list$X55 + beta[57] * data_list$X56 + beta[58] * data_list$X57 +
    beta[59] * data_list$X58 + beta[60] * data_list$X59 + beta[61] * data_list$X60 + beta[62] * data_list$X61 +
    beta[63] * data_list$X62 + beta[64] * data_list$X63 + beta[65] * data_list$X64 + beta[66] * data_list$X65 + beta[67] * data_list$X66 + beta[68] * data_list$X67 +
    beta[69] * data_list$X68 + beta[70] * data_list$X69 + beta[71] * data_list$X70 + beta[72] * data_list$X71 +
    beta[73] * data_list$X72 + beta[74] * data_list$X73 + beta[75] * data_list$X74 + beta[76] * data_list$X75 + beta[77] * data_list$X76 + beta[78] * data_list$X77 +
    beta[79] * data_list$X78 + beta[80] * data_list$X79 + beta[81] * data_list$X80 + beta[82] * data_list$X81 +
    beta[83] * data_list$X82 + beta[84] * data_list$X83 + beta[85] * data_list$X84 + beta[86] * data_list$X85
  p <- exp(logit_p) / (1 + exp(logit_p))
  rbinom(length(p), 1, p)
  print(logit_p)
})



# Збереження апріорних предиктивних результатів у файл
write.csv(prior_predictive_y, file = "prior_predictive_y.csv", row.names = FALSE)

# Порівняння апріорних предиктивних результатів з реальними даними
prior_predictive_means <- rowMeans(prior_predictive_y)
ggplot(data.frame(real = data_list$Y, predicted = prior_predictive_means), aes(x = real, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Апріорні предиктивні перевірки", x = "Реальні дані", y = "Апріорні предиктивні значення") +
  theme_minimal()

# Збереження графіка
ggsave("prior_predictive_checks.png")