# ==============================================================================
# 01_funcoes.R
# Funções para simulação e cálculos econômicos
# ==============================================================================

# ------------------------------------------------------------------------------
# Funções de cálculo de eventos
# ------------------------------------------------------------------------------

calcular_eventos_por_cobertura <- function(eventos_0mfc, rr, prop_mfc) {
  #' Calcula eventos esperados para um dado nível de cobertura MFC
  if (prop_mfc < 0 | prop_mfc > 1) stop("prop_mfc deve estar entre 0 e 1")
  prop_gen <- 1 - prop_mfc
  fator_ajuste <- (prop_mfc * rr) + prop_gen
  eventos <- eventos_0mfc * fator_ajuste
  return(eventos)
}

calcular_eventos_evitados <- function(eventos_0mfc, rr, prop_mfc_inicial, prop_mfc_final) {
  #' Calcula eventos evitados ao expandir cobertura MFC
  eventos_inicial <- calcular_eventos_por_cobertura(eventos_0mfc, rr, prop_mfc_inicial)
  eventos_final <- calcular_eventos_por_cobertura(eventos_0mfc, rr, prop_mfc_final)
  eventos_evitados <- eventos_inicial - eventos_final
  list(
    eventos_inicial = eventos_inicial,
    eventos_final = eventos_final,
    eventos_evitados = eventos_evitados,
    reducao_pct = (eventos_evitados / eventos_inicial) * 100
  )
}

calcular_beneficio_categoria <- function(df, 
                                          col_eventos_0mfc = "eventos_0mfc_500k",
                                          col_rr = "rr",
                                          col_custo = "custo_sigtap",
                                          prop_mfc_inicial = 0,
                                          prop_mfc_final = 0.60,
                                          fator_correcao = 1.0) {
  #' Calcula benefício econômico total para uma categoria de eventos
  df$eventos_inicial <- sapply(1:nrow(df), function(i) {
    calcular_eventos_por_cobertura(df[[col_eventos_0mfc]][i], df[[col_rr]][i], prop_mfc_inicial)
  })
  df$eventos_final <- sapply(1:nrow(df), function(i) {
    calcular_eventos_por_cobertura(df[[col_eventos_0mfc]][i], df[[col_rr]][i], prop_mfc_final)
  })
  df$eventos_evitados <- df$eventos_inicial - df$eventos_final
  df$custo_evitado <- df$eventos_evitados * df[[col_custo]] * fator_correcao
  totais <- list(
    eventos_inicial = sum(df$eventos_inicial),
    eventos_final = sum(df$eventos_final),
    eventos_evitados = sum(df$eventos_evitados),
    custo_evitado_total = sum(df$custo_evitado)
  )
  list(detalhamento = df, totais = totais)
}

# ------------------------------------------------------------------------------
# Funções econômicas
# ------------------------------------------------------------------------------

calcular_valor_presente <- function(valor, ano, taxa_desconto) {
  #' Calcula o valor presente de um valor futuro
  valor / ((1 + taxa_desconto) ^ ano)
}

calcular_vpl <- function(fluxos, taxa_desconto) {
  #' Calcula o Valor Presente Líquido
  anos <- seq_along(fluxos) - 1
  valores_presentes <- mapply(function(v, a) calcular_valor_presente(v, a, taxa_desconto), 
                               fluxos, anos)
  sum(valores_presentes)
}

calcular_tir <- function(fluxos, precisao = 0.0001) {
  #' Calcula a Taxa Interna de Retorno
  vpl_func <- function(taxa) {
    anos <- seq_along(fluxos) - 1
    sum(fluxos / ((1 + taxa) ^ anos))
  }
  tryCatch({
    uniroot(vpl_func, c(-0.99, 10), tol = precisao)$root
  }, error = function(e) NA)
}

calcular_payback <- function(custos, beneficios, taxa_desconto = NULL) {
  #' Calcula período de payback (simples ou descontado)
  anos <- seq_along(custos) - 1
  if (is.null(taxa_desconto)) {
    custo_acum <- cumsum(custos)
    benef_acum <- cumsum(beneficios)
  } else {
    custo_vp <- sapply(seq_along(custos), function(i) calcular_valor_presente(custos[i], anos[i], taxa_desconto))
    benef_vp <- sapply(seq_along(beneficios), function(i) calcular_valor_presente(beneficios[i], anos[i], taxa_desconto))
    custo_acum <- cumsum(custo_vp)
    benef_acum <- cumsum(benef_vp)
  }
  saldo_acum <- benef_acum - custo_acum
  payback_idx <- which(saldo_acum >= 0)[1]
  if (is.na(payback_idx)) NA else anos[payback_idx]
}

calcular_roi <- function(custo_total, beneficio_total) {
  #' Calcula ROI em percentual
  ((beneficio_total - custo_total) / custo_total) * 100
}

calcular_rcb <- function(custo_total, beneficio_total) {
  #' Calcula Razão Custo-Benefício
  beneficio_total / custo_total
}

message("Funções carregadas: calcular_eventos_por_cobertura, calcular_beneficio_categoria, ",
        "calcular_vpl, calcular_tir, calcular_payback, calcular_roi, calcular_rcb")
