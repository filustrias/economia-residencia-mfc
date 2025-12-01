# ==============================================================================
# 00_carregar_dados.R
# Script auxiliar para carregar dados do estudo
# ==============================================================================

# Proporções do estudo original
estudo <- list(
  populacao = 600000,
  prop_mfc = 0.336,
  prop_gen = 0.664
)

# Função de conversão para 0% MFC
converter_para_0mfc <- function(valor_obs, rr, 
                                 prop_mfc = 0.336, 
                                 prop_gen = 0.664,
                                 pop_original = 600000,
                                 pop_final = 500000) {
  taxa_generalista <- valor_obs / (prop_mfc * rr + prop_gen)
  valor_500k <- taxa_generalista * (pop_final / pop_original)
  list(valor_600k = taxa_generalista, valor_500k = valor_500k)
}

adicionar_colunas_0mfc <- function(df, col_rr = "rr", col_eventos = "eventos_ano") {
  df$eventos_0mfc_600k <- NA
  df$eventos_0mfc_500k <- NA
  for (i in 1:nrow(df)) {
    conv <- converter_para_0mfc(df[[col_eventos]][i], df[[col_rr]][i])
    df$eventos_0mfc_600k[i] <- conv$valor_600k
    df$eventos_0mfc_500k[i] <- conv$valor_500k
  }
  df
}

# Carregar dados
encaminhamentos_ambulatoriais <- data.frame(
  especialidade = c("Cardiologia", "Neurologia", "Psiquiatria", "Dermatologia",
                    "Pneumologia", "Doenças infecciosas", "Urologia", "Alergologia",
                    "Nefrologia", "Endocrinologia", "Gastroenterologia", "Angiologia",
                    "Reumatologia", "Fisioterapia", "Reabilitação", "Oftalmologia",
                    "ORL", "Ortopedia", "Ginecologia", "Pré-natal alto risco"),
  rr = c(0.40, 0.50, 0.45, 0.49, 0.54, 0.74, 0.57, 0.54, 0.63, 0.42, 
         0.38, 0.35, 0.47, 1.17, 1.68, 1.09, 0.71, 0.52, 0.86, 0.66),
  eventos_ano = c(2008, 1835, 897, 4144, 903, 290, 1762, 480, 603, 1021, 
                  867, 1030, 830, 2390, 1173, 8713, 2269, 4934, 738, 991),
  stringsAsFactors = FALSE
)
encaminhamentos_ambulatoriais <- adicionar_colunas_0mfc(encaminhamentos_ambulatoriais)

encaminhamentos_cirurgicos <- data.frame(
  especialidade = c("Cirurgia oftalmológica", "Cirurgia ginecológica", "Cirurgia ortopédica",
                    "Cirurgia geral", "Cirurgia plástica", "Cirurgia vascular"),
  rr = c(1.21, 0.87, 1.22, 0.91, 1.19, 0.87),
  eventos_ano = c(4458, 989, 147, 1968, 859, 359),
  stringsAsFactors = FALSE
)
encaminhamentos_cirurgicos <- adicionar_colunas_0mfc(encaminhamentos_cirurgicos)

exames_diagnosticos <- data.frame(
  exame = c("Ecocardiograma", "Espirometria", "Colonoscopia", "EDA", "Teste ergométrico", "Mamografia"),
  rr = c(0.66, 0.96, 0.76, 0.49, 0.72, 0.98),
  eventos_ano = c(1201, 385, 510, 1090, 431, 2424),
  custo_sigtap = c(39.94, 13.20, 86.00, 48.16, 30.00, 45.00),
  stringsAsFactors = FALSE
)
exames_diagnosticos <- adicionar_colunas_0mfc(exames_diagnosticos)

exames_laboratoriais <- data.frame(
  exame = c("Hemograma", "Creatinina", "Ureia", "Sódio", "Potássio", "Glicose",
            "Hemoglobina glicada", "Colesterol total", "HDL", "LDL", "Triglicerídeos",
            "Ácido úrico", "TSH", "T3", "T4", "T4 livre", "Bilirrubina", "AST", "ALT",
            "Fosfatase alcalina", "Gama-GT", "VHS", "Ova e parasitas", "Urinálise",
            "Cálcio", "LH", "FSH", "Rubéola IgG", "Rubéola IgM", "PSA"),
  rr = c(0.53, 0.85, 0.29, 0.42, 0.73, 0.46, 0.64, 0.81, 0.94, 0.41, 0.82,
         0.23, 0.71, 0.08, 0.13, 0.56, 0.78, 0.48, 0.47, 0.69, 0.64, 0.74,
         0.09, 0.67, 0.44, 0.56, 0.60, 0.48, 0.47, 0.36),
  eventos_ano = c(51757, 43746, 22577, 10517, 18265, 9072, 25720, 34673, 31127,
                  22163, 33828, 15603, 11679, 791, 886, 5432, 2304, 9181, 9363,
                  3505, 4386, 1957, 604, 41333, 1866, 674, 863, 210, 216, 4365),
  custo_sigtap = c(4.11, 1.85, 1.85, 1.85, 1.85, 1.85, 7.86, 1.85, 3.51, 3.51, 3.51,
                   1.85, 8.96, 8.71, 8.71, 11.60, 2.01, 2.01, 2.01, 2.01, 3.51, 2.63, 
                   1.65, 3.70, 1.85, 10.15, 10.15, 18.55, 18.55, 16.42),
  stringsAsFactors = FALSE
)
exames_laboratoriais <- adicionar_colunas_0mfc(exames_laboratoriais)

internacoes_icsap <- data.frame(
  condicao = c("Hipertensão", "Diabetes mellitus", "AVC", "Angina pectoris",
               "Insuficiência cardíaca", "Epilepsia", "Asma", "Gravidez",
               "Gastroenterite", "Pneumonia crianças", "Pneumonia adultos",
               "Infecção pele", "ORL", "DIP"),
  rr = c(0.83, 0.76, 0.74, 0.62, 0.52, 0.79, 0.35, 0.78, 0.45, 0.57, 0.36, 0.82, 1.06, 0.59),
  eventos_ano = c(128, 160, 244, 271, 230, 86, 54, 136, 45, 114, 116, 331, 28, 41),
  custo_aih = c(761.35, 1178.17, 1756.62, 4774.49, 2553.91, 957.47, 853.21, 646.69,
                527.15, 1969.07, 1969.07, 679.82, 502.60, 612.67),
  stringsAsFactors = FALSE
)
internacoes_icsap <- adicionar_colunas_0mfc(internacoes_icsap)

# Confirmar carregamento
message("Dados carregados: encaminhamentos_ambulatoriais, encaminhamentos_cirurgicos, ",
        "exames_diagnosticos, exames_laboratoriais, internacoes_icsap")
