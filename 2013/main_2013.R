##### CONFIGURATIONS ####
library(haven)
library(dplyr)
setwd("C:/Users/mehr als lernen/Downloads/Data_Inei-main/Data_Inei-main/2013/input/")
getwd()

##### FUNCTIONS #####
find_repeated_values <- function(data, column_name) {
  # Check if the specified column exists in the data frame
  if (!column_name %in% colnames(data)) {
    stop(paste("Column", column_name, "does not exist in the data frame."))
  }
  
  # Extract the specified column as a vector
  column_vector <- data[[column_name]]
  
  # Find the repeated values in the column
  duplicated_values <- column_vector[duplicated(column_vector)]
  
  # Return the vector of duplicated values
  return(duplicated_values)
}

add_id_to_df <- function(input_df) {
  input_df$ID <- paste(input_df$AÑO, input_df$MES, input_df$CONGLOME, input_df$VIVIENDA, input_df$HOGAR, input_df$CODPERSO, input_df$UBIGEO, input_df$DOMINIO, sep = "")
  #input_df$ID <- as.numeric(input_df$ID)
  
  print("Repeated values:")
  print(length(find_repeated_values(input_df, "ID")))
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

check_duplicated_values <- function(data, column_name) {
  # Check if the specified column exists in the data frame
  if (!column_name %in% colnames(data)) {
    stop(paste("Column", column_name, "does not exist in the data frame."))
  }
  
  # Extract the specified column as a vector
  column_vector <- data[[column_name]]
  
  # Check for duplicated values in the column
  has_duplicates <- any(duplicated(column_vector))
  
  # Return TRUE if there are duplicated values, otherwise FALSE
  return(has_duplicates)
}

remove_rows_with_mostly_NAs <- function(data_frame, threshold = 3) {
  # Calculate the number of NAs in each row
  na_counts <- rowSums(is.na(data_frame))
  
  # Keep rows where the number of NAs is less than the threshold
  filtered_data <- data_frame[na_counts < threshold, ]
  
  return(filtered_data)
}

column_summary <- function(data, column_name) {
  # Get the unique values and their counts
  unique_counts <- table(data[[column_name]])
  
  # Calculate the total number of observations in the column
  total_obs <- length(data[[column_name]])
  
  # Calculate the percentages
  percentages <- (unique_counts / total_obs) * 100
  
  # Create a data frame to store the results
  summary_df <- data.frame(Value = names(unique_counts),
                           Count = as.vector(unique_counts),
                           Percentage = percentages)
  
  return(summary_df)
}

summary_table <- function(data, group_column, count_column) {
  summary_data <- data %>%
    group_by({{ group_column }}, {{ count_column }}) %>%
    count(name = "Count_Unique_Values")
  
  summary_data_with_percentage <- summary_data %>%
    group_by({{ group_column }}) %>%
    mutate(Percentage = Count_Unique_Values / sum(Count_Unique_Values) * 100)
  
  return(summary_data_with_percentage)
}

calculate_correlation <- function(data, column1, column2) {
  # Convert "No" and "Si" to 0 and 1 in the specified columns
  data[[column1]] <- ifelse(data[[column1]] == "No", 0, 1)
  data[[column2]] <- ifelse(data[[column2]] == "No", 0, 1)
  
  # Calculate the Pearson correlation coefficient, standard deviation, and p-value
  correlation_test <- cor.test(data[[column1]], data[[column2]])
  
  # Extract the correlation coefficient, standard deviation, and p-value
  correlation_coefficient <- correlation_test$estimate
  standard_deviation <- sqrt(correlation_test$statistic)
  
  # Print the results
  print(paste("Correlation coefficient:", correlation_coefficient))
  print(paste("Standard deviation:", standard_deviation))
  
  # Return the results as a list
  return(list(correlation_coefficient = correlation_coefficient,
              standard_deviation = standard_deviation))
}

two_tailed_t_test <- function(data, group_column, variable_0, numeric_column) {
  #Used to check whether two populations are different from one another
  df <- data
  
  # Convert the specified group_column to binary
  df[[group_column]] <- ifelse(df[[group_column]] == variable_0, 0, 1)
  
  # Convert the specified numeric_column to binary
  df[[numeric_column]] <- ifelse(df[[numeric_column]] == "No", 0, 1)
  
  # Perform the t-test
  t_test_result <- t.test(df[[numeric_column]] ~ df[[group_column]])
  
  return(t_test_result)
}



######### EDAD, SEXO, ESTADO CIVIL, URBANO, REGION ########
df <- read_sav("Enaho01-2013-200.sav")

df$EDAD <- replace(df$P208A, df$P208A == 0 & !is.na(df$P208A) & df$P208A != "", NA)
df$SEXO <- ifelse(df$P207 == 1, "Hombre", ifelse(df$P207 == 2, "Mujer", NA))
df <- df %>% 
  mutate(ESTADO_CIVIL = case_when(P209 == 1 ~ "Conviviente",
                                  P209 == 2 ~ "Casado",
                                  P209 == 3 ~ "Viudo",
                                  P209 == 4 ~ "Divorciado",
                                  P209 == 5 ~ "Separado",
                                  P209 == 6 ~ "Soltero",
                                  TRUE ~ NA_character_))
df$URBANO <- ifelse(df$ESTRATO >= 6, "No", ifelse(df$ESTRATO < 6, "Si", NA))
df <- df %>% 
  mutate(REGION = case_when(DOMINIO == 1 ~ "Costa Norte",
                            DOMINIO == 2 ~ "Costa Centro",
                            DOMINIO == 3 ~ "Costa Sur",
                            DOMINIO == 4 ~ "Sierra Norte",
                            DOMINIO == 5 ~ "Sierra Centro",
                            DOMINIO == 6 ~ "Sierra Sur",
                            DOMINIO == 7 ~ "Selva",
                            DOMINIO == 8 ~ "Lima Metropolitana",
                            TRUE ~ NA_character_))
df$ID <- add_id_to_df(df)
df <- move_and_remove_df(df, 4)

output <- df


######### LENGUA MATERNA, EDUCACION ####
df <- read_sav("Enaho01a-2013-300.sav")
df <- df %>% 
  mutate(LENGUA_MATERNA = case_when(P300A == 1 ~ "Quechua",
                                    P300A == 2 ~ "Aimara",
                                    P300A == 3 ~ "Otra lengua nativa",
                                    P300A == 4 ~ "Castellano",
                                    P300A == 5 ~ "Ingles",
                                    P300A == 6 ~ "Portugues",
                                    P300A == 7 ~ "Otra lengua extranjera",
                                    P300A == 8 ~ "Sordomudo",
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
                               P301A == 11 ~ "Posgrado",
                               TRUE ~ NA_character_))
df$ID <- add_id_to_df(df)
df <- move_and_remove_df(df, 1)
output <- merge(output, df, by = "ID", all = FALSE)


######### ETNIA ####
df <- read_sav("Enaho01a-2013-500.sav")
df <- df %>% 
  mutate(ETNIA = case_when(P558C == 1 ~ "Quechua",
                           P558C == 2 ~ "Aimara",
                           P558C == 3 ~ "Amazonico",
                           P558C == 4 ~ "Afrodescendiente",
                           P558C == 5 ~ "Blanco",
                           P558C == 6 ~ "Mestizo",
                           P558C == 7 ~ "Otro",
                           P558C == 8 ~ "NS/NR",
                           TRUE ~ NA_character_))
df$ID <- add_id_to_df(df)
df <- move_and_remove_df(df, 0)
output <- merge(output, df, by = "ID", all = FALSE) #A partir de aquí desde 14 a más años (reducción de obs.)



######## SEGURO Y CONTRIBUCION ######
df <- read_sav("Enaho01A-2013-400.sav")

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


######### EMPLEO, CONTRATO, PENSION, TAMAÑO, REGISTRO E IMPUESTOS LUGAR TRABAJO ########
df <- read_sav("Enaho01a-2013-500.sav")

df$EMPLEO <- ifelse(df$P501 == 1 | df$P502 == 1 | df$P503 == 1, "Si", "No")
df <- df %>% 
  mutate(TIPO_EMPLEO = case_when(P507 == 1 ~ "Empleador",
                                 P507 == 2 ~ "Independiente",
                                 P507 == 3 ~ "Empleado (sector privado)",
                                 P507 == 4 ~ "Obrero",
                                 P507 == 5 ~ "Familiar no remunerado",
                                 P507 == 6 ~ "Trabajador del hogar",
                                 P507 == 7 ~ "Otro",
                                 TRUE ~ NA_character_))
df$TIPO_EMPLEO <- ifelse((df$P507 == 2 | df$P507 == 7) & df$P509 == 1, "Independiente (empleador)", df$TIPO_EMPLEO)
df$TIPO_EMPLEO <- ifelse(df$P507 == 3 & df$P510 < 4, "Empleado (sector público)", df$TIPO_EMPLEO)
df$TIPO_EMPLEO <- replace(df$TIPO_EMPLEO, is.na(df$TIPO_EMPLEO), "Otro")

df$CONTRATO <- ifelse((df$P511A < 7 | df$P511A == 8), "Si", "No")
df$SEGURO_PENSION <- ifelse(df$P558A5 == 5, "No", "Si") 
df$REGISTRO_IMPUESTOS_LUG_TRABAJO <- ifelse(df$P510A1 < 3 | df$P510B == 1, "Si", "No")
df$TAMANO_LUG_TRABAJO <- df$P512B

df$ID <- add_id_to_df(df)
df <- move_and_remove_df(df, 5)

output <- merge(output, df, by = "ID", all = FALSE)


######### SEGURO SOCIAL ########
output$SEGURO_SALUD <- ifelse((output$ESSALUD == "Si" & (output$ESSALUD_CONTRIBUCION == "Centro de trabajo" | output$ESSALUD_CONTRIBUCION == "Mismo")) | 
                                (output$SEGURO_PRIVADO == "Si" & (output$SEGURO_PRIVADO_CONTRIBUCION == "Centro de trabajo" | output$SEGURO_PRIVADO_CONTRIBUCION == "Mismo")) |
                                (output$ENTIDAD_PRESTADORA == "Si" & (output$ENTIDAD_PRESTADORA_CONTRIBUCION == "Centro de trabajo" | output$ENTIDAD_PRESTADORA_CONTRIBUCION == "Mismo")) |
                                (output$SEGURO_FFAA_PNP_CONTRIBUCION == "Si" & (output$SEGURO_FFAA_PNP_CONTRIBUCION == "Centro de trabajo" | output$SEGURO_FFAA_PNP_CONTRIBUCION == "Mismo")) |
                                (output$SEGURO_UNIVERSITARIO == "Si" & output$SEGURO_UNIVERSITARIO_CONTRIBUCION == "Mismo") | 
                                (output$OTRO_SEGURO == "Si" & (output$OTRO_SEGURO_CONTRIBUCION == "Centro de trabajo" | output$OTRO_SEGURO_CONTRIBUCION == "Mismo")), 
                              "Si", 
                              "No")
output$SEGURO_SOCIAL <- ifelse(output$SEGURO_PENSION == "Si" | output$SEGURO_SALUD == "Si", "Si", "No")
######### SAVE CSV ########
write.csv(output, "results_2013.csv", row.names = FALSE, fileEncoding = "UTF-8")
output <- read.csv("results_2013.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE, fileEncoding = "UTF-8", encoding = "UTF-8")

######### CODE ########
#backup <- output

output <- output[complete.cases(output$EMPLEO), ]
column_summary(output, "EMPLEO")

df <- output[output$EMPLEO == "Si", ]
df$CONTRATO <- replace(df$CONTRATO, is.na(df$CONTRATO), "No")
column_summary(df, "CONTRATO")

df$SEGURO_SOCIAL <- replace(df$SEGURO_SOCIAL, is.na(df$SEGURO_SOCIAL), "No")
column_summary(df, "SEGURO_SOCIAL")

df$REGISTRO_IMPUESTOS_LUG_TRABAJO <- replace(df$REGISTRO_IMPUESTOS_LUG_TRABAJO, is.na(df$REGISTRO_IMPUESTOS_LUG_TRABAJO), "No")
column_summary(df, "REGISTRO_IMPUESTOS_LUG_TRABAJO")

df$TAMANO_LUG_TRABAJO <- replace(df$TAMANO_LUG_TRABAJO, is.na(df$TAMANO_LUG_TRABAJO), 0)
df$SECTOR_FORMAL <- ifelse(df$TAMANO_LUG_TRABAJO > 5, "Si", "No")
column_summary(df, "SECTOR_FORMAL")

calculate_correlation(df, "CONTRATO", "SEGURO_SOCIAL")

summary_table(df, EDUCACION, CONTRATO)

two_tailed_t_test(df, "SEXO", "Hombre", "SECTOR_FORMAL")