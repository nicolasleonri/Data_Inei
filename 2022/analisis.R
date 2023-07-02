##### CONFIGURATIONS ####
library(dplyr)
setwd("C:/Users/mehr als lernen/Documents/Data 2022")
getwd()



##### FUNCTIONS #####
add_id_to_df <- function(input_df) {
  input_df$ID <- paste(input_df$ANO, input_df$MES, input_df$CONGLOME, input_df$VIVIENDA, input_df$HOGAR, input_df$CODPERSO, input_df$UBIGEO, input_df$DOMINIO, sep = "")
  input_df$ID <- as.numeric(input_df$ID)
  return(input_df$ID)
}

move_and_remove_df <- function(input_df, x) {
  input_df <- input_df[,c(ncol(input_df),1:(ncol(input_df)-1))]
  input_df <- input_df %>%
    select(1, (length(input_df)-x):length(input_df))
  return(input_df)
}

tipo_de_seguro_y_contribucion <- function(input_df, col1, col2, colname1) {
  input_df[[colname1]] <- ifelse(input_df[[col1]] == 1, "Si", ifelse(input_df[[col1]] == 2, "No", NA))
  
  colname2 = paste(colname1, "CONTRIBUCION", sep = "_")
  
  switch_values <- c("1" = "Centro de trabajo",
                     "2" = "Mismo",
                     "3" = "Jubilado",
                     "4" = "Familiar",
                     "5" = "No paga")
  
  input_df[[colname2]] <- ifelse(!is.na(input_df[[col2]]),
                                 switch_values[match(input_df[[col2]], names(switch_values))],
                                 NA)
  return(input_df)
}



######### EDAD, SEXO ########
df <- read.csv("Enaho01-2022-200.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE, fileEncoding = "UTF-8", encoding = "UTF-8")

df$SEXO <- ifelse(df$P207 == 1, "Hombre", ifelse(df$P207 == 2, "Mujer", NA))
df$EDAD <- replace(df$P208A, df$P208A == 0 & !is.na(df$P208A) & df$P208A != "", NA)
df <- df %>% 
  mutate(ESTADO_CIVIL = case_when(P209 == 1 ~ "Conviviente",
                                  P209 == 2 ~ "Casado",
                                  P209 == 3 ~ "Soltero",
                                  P209 == 4 ~ "Divorciado",
                                  P209 == 5 ~ "Viudo",
                                  P209 == 6 ~ "Separado",
                                  TRUE ~ NA_character_))
df$ID <- add_id_to_df(df)
df <- move_and_remove_df(df, 2)

output <- df



######### EDUCACION ####
df <- read.csv("Enaho01a-2022-300.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE, fileEncoding = "UTF-8", encoding = "UTF-8")
df <- df %>% 
  mutate(LENGUA_NATIVA = case_when(P300A == 1 ~ "Quechua",
                                  P300A == 2 ~ "Aimara",
                                  P300A == 10 ~ "Ashaninka",
                                  P300A == 11 ~ "Awajun",
                                  P300A == 12 ~ "Shipibo",
                                  P300A == 13 ~ "Shawi",
                                  P300A == 14 ~ "Matsigenka",
                                  P300A == 15 ~ "Achuar",
                                  P300A == 3 ~ "Otra nativa",
                                  P300A == 4 ~ "Castellano",
                                  P300A == 6 ~ "Portugues",
                                  P300A == 7 ~ "Otra extranjera",
                                  P300A == 8 ~ "No",
                                  P300A == 9 ~ "Senas",
                                  TRUE ~ NA_character_))
df <- df %>% 
  mutate(EDUCACION = case_when(P301A == 1 ~ "Sin nivel",
                               P301A == 2 ~ "Inicial",
                               P301A == 3 ~ "Primaria incompleta",
                               P301A == 4 ~ "Primaria completa",
                               P301A == 5 ~ "Secundaria incompleta",
                               P301A == 6 ~ "Secundaria completa",
                               P301A == 7 ~ "Sup no universitaria incompleta",
                               P301A == 8 ~ "Sup no universitaria completa",
                               P301A == 9 ~ "Sup universitaria incompleta",
                               P301A == 10 ~ "Sup universitaria completa",
                               P301A == 11 ~ "Maestria o Doctorado",
                               P301A == 12 ~ "Basica especial",
                               TRUE ~ NA_character_))
df$ID <- add_id_to_df(df)
df <- move_and_remove_df(df, 1)

output <- merge(output, df, by = "ID", all = FALSE)

summary(output)

######## SALUD ######
df <- read.csv("Enaho01a-2022-400.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE, fileEncoding = "UTF-8", encoding = "UTF-8")

df <- tipo_de_seguro_y_contribucion(df, "P4191", "P419A1", "ESSALUD")
df <- tipo_de_seguro_y_contribucion(df, "P4192", "P419A2", "SEGURO_PRIVADO")
df <- tipo_de_seguro_y_contribucion(df, "P4193", "P419A3", "ENTIDAD_PRESTADORA")
df <- tipo_de_seguro_y_contribucion(df, "P4194", "P419A4", "SEGURO_FFAA_PNP")
df <- tipo_de_seguro_y_contribucion(df, "P4195", "P419A5", "SIS")
df <- tipo_de_seguro_y_contribucion(df, "P4196", "P419A6", "SEGURO_UNIVERSITARIO")
df <- tipo_de_seguro_y_contribucion(df, "P4197", "P419A7", "SEGURO_ESCOLAR_PRIVADO")
df <- tipo_de_seguro_y_contribucion(df, "P4198", "P419A8", "OTRO_SEGURO")

df$ID <- add_id_to_df(df)
df <- move_and_remove_df(df, 15)

output <- merge(output, df, by = "ID", all = FALSE)
output <- subset(output, !duplicated(ID))


summary(output)


