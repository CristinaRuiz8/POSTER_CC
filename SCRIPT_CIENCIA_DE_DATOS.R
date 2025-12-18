# =========================================================
# 0. Paquetes
# =========================================================
install.packages(c("dplyr", "ggplot2", "skimr"))

library(dplyr)
library(ggplot2)
library(skimr)
df <- read.csv("heart_statlog_cleveland_hungary_final.csv")
# =========================================================
# 1. Estructura general del dataset
# =========================================================
dim(df)          # nº filas y columnas
names(df)        # nombres de variables
str(df)          # tipo de cada variable
skim(df)         # resumen rápido (distribuciones, NA, etc.)

# =========================================================
# 2. Valores perdidos, duplicados y resumen básico
# =========================================================

# NAs por variable
na_por_variable <- colSums(is.na(df))
na_por_variable

# Porcentaje de NAs por variable
na_pct <- colMeans(is.na(df)) * 100
na_pct

# Filas completamente duplicadas
n_duplicadas <- sum(duplicated(df))
n_duplicadas
names(df) <- gsub("\\.", "_", names(df))
names(df)
# Estadísticos descriptivos de variables numéricas
df %>%
  select(age, resting_ecg, cholesterol, fasting_blood_sugar,
         max_heart_rate, oldpeak) %>%
  summary()

# Tabla de frecuencias de variables categóricas
table(df$sex)
table(df$chest_pain_type)
table(df$resting_ecg)
table(df$exercise_angina)
table(df$ST_slope)
table(df$target)

# =========================================================
# 3. Distribución de variables (histogramas)
# =========================================================

num_vars <- c("age", "resting_bp_s", "cholesterol",
              "fasting_blood_sugar", "max_heart_rate", "oldpeak")

for (v in num_vars) {
  print(
    ggplot(df, aes_string(x = v)) +
      geom_histogram(bins = 30) +
      labs(title = paste("Histograma de", v), x = v, y = "Frecuencia")
  )
}


ggplot(df, aes(x = target, fill = target)) +
  geom_bar() +
  scale_fill_manual(
    values = c("0" = "#FF9999",   # rojo claro
               "1" = "#E41A1C"),  # rojo fuerte
    labels = c("0 = No enfermedad", "1 = Enfermedad")
  ) +
  labs(
    title = "Distribución de la variable target",
    x = "Clase",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

df$target <- factor(df$target, levels = c("0", "1"))

for (v in num_vars) {
  print(
    ggplot(df, aes(x = target, y = .data[[v]], fill = target)) +
      geom_boxplot() +
      scale_fill_manual(
        values = c("0" = "#FF9999",  # rojo claro
                   "1" = "#E41A1C")  # rojo fuerte
      ) +
      labs(
        title = paste("Boxplot de", v, "según target"),
        x = "Enfermedad (0 = no, 1 = sí)",
        y = v
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  )
}

# =========================================================
# 4. Detección de outliers (regla IQR)
# =========================================================

detectar_outliers <- function(x) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  which(x < lower | x > upper)
}

out_age   <- detectar_outliers(df$age)
out_bp    <- detectar_outliers(df$resting_bp_s)
out_chol  <- detectar_outliers(df$cholesterol)
out_fbs   <- detectar_outliers(df$fasting_blood_sugar)
out_hr    <- detectar_outliers(df$max_heart_rate)
out_op    <- detectar_outliers(df$oldpeak)

length(out_age);   length(out_bp);  length(out_chol)
length(out_fbs);   length(out_hr);  length(out_op)

# Ver algunas filas con outliers (ejemplo age y oldpeak)
df[out_age[1:10], c("age", "target")]
df[out_op[1:10],  c("oldpeak", "target")]

# =========================================================
# 5. Correlaciones entre variables numéricas
# =========================================================
mat_cor <- df %>%
  select(age, resting_bp_s, cholesterol,
         fasting_blood_sugar, max_heart_rate, oldpeak) %>%
  cor(use = "complete.obs")

mat_cor

# Instalar si hace falta
install.packages("reshape2")
install.packages("ggplot2")

library(reshape2)
library(ggplot2)

# Seleccionar variables numéricas
num_vars <- c("age", "resting_bp_s", "cholesterol", 
              "fasting_blood_sugar", "max_heart_rate", "oldpeak")

# Matriz de correlación
mat_cor <- cor(df[, num_vars], use = "complete.obs")

# Convertimos a formato largo
cor_long <- melt(mat_cor)

# Mapa de color con tus colores
ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#FF9999",      # rojo claro
    high = "#E41A1C",     # rojo fuerte
    mid = "white",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlación"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  labs(
    title = "Mapa de calor de correlaciones",
    x = "",
    y = ""
  )

ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#FF9999", color = "black", alpha = 0.8) +
  geom_density(color = "#E41A1C", size = 1.2) +
  labs(title = "Distribución de la edad",
       x = "Edad (años)", y = "Frecuencia") +
  theme_minimal(base_size = 14)
ggplot(df, aes(x = target, y = oldpeak, fill = target)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#FF9999", "1" = "#E41A1C")) +
  labs(title = "Boxplot de oldpeak según presencia de enfermedad",
       x = "Enfermedad (0 = no, 1 = sí)", y = "oldpeak") +
  theme_minimal(base_size = 14)
ggplot(df, aes(x = as.factor(sex))) +
  geom_bar(fill = "#E41A1C", alpha = 0.85) +
  labs(title = "Distribución del sexo",
       x = "Sexo (0 = Mujer, 1 = Hombre)", y = "Frecuencia") +
  theme_minimal(base_size = 14)
ggplot(df, aes(x = cholesterol, fill = target)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("0" = "#FF9999", "1" = "#E41A1C")) +
  labs(title = "Densidad de colesterol según enfermedad",
       x = "Colesterol (mg/dL)", y = "Densidad") +
  theme_minimal(base_size = 14)

library(reshape2)

num_vars <- df[, sapply(df, is.numeric)]
cor_mat <- round(cor(num_vars, use = "complete.obs"), 2)

melt_cor <- melt(cor_mat)

ggplot(melt_cor, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#FF9999", mid = "white", high = "#E41A1C",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Mapa de calor de correlaciones",
       x = "", y = "", fill = "Correlación") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, aes(x = age, y = max_heart_rate, color = target)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("0" = "#FF9999", "1" = "#E41A1C")) +
  labs(title = "Edad vs Frecuencia cardíaca máxima",
       x = "Edad (años)", y = "Frecuencia máxima") +
  theme_minimal(base_size = 14)

ggplot(df, aes(x = as.factor(chest_pain_type), fill = as.factor(chest_pain_type))) +
  geom_bar() +
  scale_fill_manual(values = c("#FF9999", "#E41A1C", "#FF9999", "#E41A1C")) +
  labs(title = "Tipos de dolor torácico",
       x = "Tipo de dolor torácico", y = "Frecuencia") +
  theme_minimal(base_size = 14)

library(ggplot2)

df <- heart_statlog_cleveland_hungary_final

ggplot(df, aes(x = age, fill = as.factor(target))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("#FF9999", "#E41A1C"),
                    name = "Enfermedad cardíaca",
                    labels = c("No", "Sí")) +
  theme_minimal(base_size = 14) +
  labs(title = "Distribución de la edad según presencia de enfermedad cardíaca",
       x = "Edad (años)",
       y = "Densidad")
ggplot(df, aes(x = age, y = oldpeak, color = as.factor(target))) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("#FF9999", "#E41A1C")) +
  theme_minimal(base_size = 14) +
  labs(title = "Relación entre edad y depresión del ST",
       x = "Edad",
       y = "Oldpeak",
       color = "Enfermedad")

#boxplot 

prop_chest <- df %>%
  count(chest_pain_type, target) %>%
  group_by(chest_pain_type) %>%
  mutate(prop = n / sum(n))

ggplot(prop_chest,
       aes(x = as.factor(chest_pain_type),
           y = prop,
           fill = as.factor(target))) +
  geom_col() +
  scale_fill_manual(values = c("#FF9999", "#E41A1C"),
                    labels = c("No", "Sí"),
                    name = "Enfermedad cardíaca") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proporción de enfermedad según tipo de dolor torácico",
       x = "Tipo de dolor torácico (chest_pain_type)",
       y = "Porcentaje dentro de cada tipo") +
  theme_minimal(base_size = 14)

 $#############################################################
# PRACTICA: REGRESIÓN LOGÍSTICA
# Objetivo: reproducir la estructura clásica del modelo lineal,
#           pero ahora con un modelo logístico binario.
#############################################################

# ==========================
# 1. Librerías
# ==========================
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
instal.packages("ResourceSelection")
library(ResourceSelection)
library(pROC)



vars_trad <- c(
  "age",                   # edad
  "sex",                   # sexo
  "resting_bp_s",          # presión arterial sistólica
  "cholesterol",           # colesterol sérico
  "fasting_blood_sugar"    # glucosa en ayunas
)

# -----------------------------
# B) Variables clínicas (pruebas diagnósticas)
# -----------------------------
vars_clin <- c(
  "chest_pain_type",       # tipo de dolor torácico
  "resting_ecg",           # ECG en reposo
  "max_heart_rate",        # frecuencia cardíaca máxima
  "exercise_angina",       # angina inducida por ejercicio
  "oldpeak",               # depresión del ST
  "st_slope"               # pendiente del ST
)
#Generar pseudo-aleatoriedad
set.seed(169)

set.seed(123)
index <- sample(1:nrow(df), 0.7 * nrow(df))

train <- df[index, ]
test  <- df[-index, ]


# Outcome aleatorio con prevalencia del 30%
#df$risk <- rbinom(n, size = 1, prob = 0.30)
names(data)
# Renombrar columnas con espacios para que R no se líe
df <- data |>
  rename(
    chest_pain_type     = chest.pain.type,
    resting_bp_s        = resting.bp.s,
    fasting_blood_sugar = fasting.blood.sugar,
    resting_ecg         = resting.ecg,
    max_heart_rate      = max.heart.rate,
    exercise_angina     = exercise.angina,
    st_slope            = ST.slope
  )

# Aseguramos que la variable outcome es factor binaria
df$target <- as.factor(df$target)

# Número de observaciones
n <- nrow(df)

# ==========================
# 3. Inspección inicial
# ==========================
data <- read.csv("heart_statlog_cleveland_hungary_final.csv")
summary(data)
table(data$risk)
prop.table(table(data$risk))


# ==========================
# 4. Partición Train/Test (70/30)
# ==========================
set.seed(321)
idx <- sample(1:n, size = 0.7*n)

train <- df[idx, ]
test  <- df[-idx, ]

# =========================================================
# 5. Modelos Logísticos
#    M1: solo factores tradicionales
#    M2: tradicionales + clínicas (tu hipótesis)
# =========================================================

# --- Modelo 1: solo tradicionales
form_m1 <- as.formula(
  paste("target ~", paste(vars_trad, collapse = " + "))
)

modelo_trad <- glm(
  form_m1,
  data   = train,
  family = binomial
)

summary(modelo_trad)

# --- Modelo 2: tradicionales + clínicas
form_m2 <- as.formula(
  paste("target ~", paste(c(vars_trad, vars_clin), collapse = " + "))
)

modelo_completo <- glm(
  form_m2,
  data   = train,
  family = binomial
)

summary(modelo_completo)

# -------- Odds Ratios (Modelo completo) ----------
OR      <- exp(coef(modelo_completo))
IC_OR   <- exp(confint(modelo_completo))

OR
IC_OR

# =========================================================
# 6. Diagnóstico del modelo (usamos el modelo completo)
# =========================================================

## --- 6.1. Multicolinealidad
vif(modelo_completo)   # >5–10 podría ser problema

## --- 6.2. Residuos (Pearson + Deviance)
resid_pearson  <- residuals(modelo_completo, type = "pearson")
resid_deviance <- residuals(modelo_completo, type = "deviance")

## --- 6.3. Autocorrelación (Durbin-Watson aproximado)
dwtest(resid_pearson ~ train$age)

acf(resid_pearson, main = "ACF residuos Pearson (modelo completo)")

## --- 6.4. Bondad de ajuste (Hosmer-Lemeshow)
hl <- hoslem.test(
  x = as.numeric(train$target) - 1,
  y = fitted(modelo_completo),
  g = 10
)
hl
# p > 0.05 → no hay evidencia de mal ajuste

# =========================================================
# 7. Predicción y métricas en TEST (comparar M1 vs M2)
# =========================================================

# ---- Modelo 1 (tradicional) ----
pred_prob_m1   <- predict(modelo_trad, newdata = test, type = "response")
pred_class_m1  <- ifelse(pred_prob_m1 > 0.5, 1, 0)

tab_m1 <- table(Predicho = pred_class_m1, Real = test$target)
tab_m1

accuracy_m1 <- mean(pred_class_m1 == as.numeric(test$target) - 1)
accuracy_m1

# ---- Modelo 2 (completo) ----
pred_prob_m2   <- predict(modelo_completo, newdata = test, type = "response")
pred_class_m2  <- ifelse(pred_prob_m2 > 0.5, 1, 0)

tab_m2 <- table(Predicho = pred_class_m2, Real = test$target)
tab_m2

accuracy_m2 <- mean(pred_class_m2 == as.numeric(test$target) - 1)
accuracy_m2

# =========================================================
# 8. Curvas ROC y AUC (M1 vs M2)
# =========================================================

# Outcome numérica para pROC
y_test <- as.numeric(test$target) - 1

roc_m1 <- roc(y_test, pred_prob_m1)
roc_m2 <- roc(y_test, pred_prob_m2)

plot(roc_m1, col = "black", main = "Curvas ROC — Tradicional vs Completo")
lines(roc_m2, col = "red")
legend("bottomright",
       legend = c(paste0("Tradicional (AUC=", round(auc(roc_m1), 3), ")"),
                  paste0("Completo (AUC=", round(auc(roc_m2), 3), ")")),
       col = c("black", "red"),
       lwd = 2)

auc(roc_m1)
auc(roc_m2)

# =========================================================
# 9. Comparación formal de modelos (AIC y test de razón de verosimilitud)
# =========================================================
AIC(modelo_trad, modelo_completo)

anova(modelo_trad, modelo_completo, test = "Chisq")
# Si p < 0.05 → el modelo completo mejora significativamente al tradicional
anova(modelo_trad, modelo_completo, test = "LRT")
library(ggplot2)
library(dplyr)

## PREDICCIONES
pred_prob_m1 <- predict(modelo_trad, newdata = test, type = "response")
pred_class_m1 <- ifelse(pred_prob_m1 > 0.5, 1, 0)

pred_prob_m2 <- predict(modelo_completo, newdata = test, type = "response")
pred_class_m2 <- ifelse(pred_prob_m2 > 0.5, 1, 0)

## MATRICES DE CONFUSIÓN
cm1 <- table(Predicho = pred_class_m1, Real = as.numeric(test$target) - 1)
cm2 <- table(Predicho = pred_class_m2, Real = as.numeric(test$target) - 1)

## FUNCIÓN PARA CONVERTIR A DATAFRAME
cm_to_df <- function(cm){
  as.data.frame(cm) %>%
    rename(Predicho = Predicho, Real = Real, Freq = Freq)
}

df_cm1 <- cm_to_df(cm1)
df_cm2 <- cm_to_df(cm2)

## GRAFICO HEATMAP 1 (TRADICIONAL)
plt_cm1 <- ggplot(df_cm1, aes(x = Real, y = Predicho, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "#FF9999", high = "#E41A1C") +
  labs(title = "Matriz de confusión - Modelo Tradicional",
       x = "Real", y = "Predicho") +
  theme_minimal(base_size = 14)

## GRAFICO HEATMAP 2 (CLINICO)
plt_cm2 <- ggplot(df_cm2, aes(x = Real, y = Predicho, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "#FF9999", high = "#E41A1C") +
  labs(title = "Matriz de confusión - Modelo Clínico",
       x = "Real", y = "Predicho") +
  theme_minimal(base_size = 14)

plt_cm1
plt_cm2
library(broom)
library(ggplot2)
library(dplyr)

# Extraer OR + IC95%
 or_table <- data.frame(
  term = rownames(coef(summary(modelo_completo))),
  estimate = exp(coef(modelo_completo)),
  conf.low = exp(confint(modelo_completo)[,1]),
  conf.high = exp(confint(modelo_completo)[,2])
)


# FOREST PLOT (ODDS RATIOS)
ggplot(or_table, aes(x = term, y = estimate)) +
  geom_point(color = "#E41A1C", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, color = "#FF9999", linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(title = "Variables predictivas del modelo clínico (Odds Ratios)",
       x = "Variable",
       y = "OR (IC95%)") +
  theme_minimal(base_size = 14)


library(dplyr)
library(ggplot2)

calib_data <- data.frame(
  obs       = as.numeric(as.character(test$target)),  # 👈 FIX
  pred_m1   = pred_prob_m1,
  pred_m2   = pred_prob_m2
)

calib_m1 <- calib_data %>%
  mutate(bin = ntile(pred_m1, 10)) %>%
  group_by(bin) %>%
  summarise(
    pred_mean = mean(pred_m1),
    obs_rate  = mean(obs),
    n         = n()
  )
calib_m2 <- calib_data %>%
  mutate(bin = ntile(pred_m2, 10)) %>%
  group_by(bin) %>%
  summarise(
    pred_mean = mean(pred_m2),
    obs_rate  = mean(obs),
    n         = n()
  )
ggplot(calib_m2, aes(x = pred_mean, y = obs_rate)) +
  geom_point(size = 3, color = "#E41A1C") +
  geom_line(color = "#E41A1C") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Curva de calibración del modelo clínico",
    x = "Probabilidad predicha (media)",
    y = "Proporción observada de enfermedad"
  ) +
  theme_minimal(base_size = 14)
calib_m1$model <- "Tradicional"
calib_m2$model <- "Clínico"

calib_both <- bind_rows(
  calib_m1 %>% select(pred_mean, obs_rate, model),
  calib_m2 %>% select(pred_mean, obs_rate, model)
)


ggplot(calib_both, aes(x = pred_mean, y = obs_rate, color = model)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Curva de calibración: modelo tradicional vs clínico",
    x = "Probabilidad predicha (media)",
    y = "Proporción observada",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 14)
ggplot(calib_both,
       aes(x = pred_mean, y = obs_rate, color = model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "black", linewidth = 1) +
  scale_color_manual(values = c(
    "Clínico" = "#E41A1C",     # rojo fuerte
    "Tradicional" = "#FF9999"  # rosa claro
  )) +
  labs(
    title = "Curva de calibración: modelo tradicional vs clínico",
    x = "Probabilidad predicha (media)",
    y = "Proporción observada",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 15)
