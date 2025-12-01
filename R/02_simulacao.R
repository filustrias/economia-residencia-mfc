# ==============================================================================
# 02_simulacao.R
# Funções para simulação do programa de residência
# ==============================================================================

simular_expansao_programa <- function(
    n_medicos_total = 100,
    prop_mfc_inicial = 0,
    vagas_ano = 10,
    duracao_residencia = 2,
    taxa_retencao = 0.60,
    horizonte_anos = 10,
    n_preceptores_inicial = 3
) {
  #' Simula expansão do programa de residência ao longo dos anos
  
  anos <- 0:horizonte_anos
  n_anos <- length(anos)
  
  n_mfc <- numeric(n_anos)
  n_residentes_r1 <- numeric(n_anos)
  n_residentes_r2 <- numeric(n_anos)
  n_formados_ano <- numeric(n_anos)
  n_preceptores <- numeric(n_anos)
  
  n_mfc[1] <- round(n_medicos_total * prop_mfc_inicial)
  
  for (i in 2:n_anos) {
    ano <- anos[i]
    if (ano == 1) {
      n_preceptores[i] <- n_preceptores_inicial
      n_residentes_r1[i] <- vagas_ano
      n_residentes_r2[i] <- 0
      n_formados_ano[i] <- 0
      n_mfc[i] <- n_mfc[i-1] + n_preceptores_inicial
    } else if (ano == 2) {
      n_preceptores[i] <- n_preceptores[i-1]
      n_residentes_r2[i] <- n_residentes_r1[i-1]
      n_residentes_r1[i] <- vagas_ano
      n_formados_ano[i] <- 0
      n_mfc[i] <- n_mfc[i-1]
    } else {
      n_formados_ano[i] <- round(n_residentes_r2[i-1] * taxa_retencao)
      n_residentes_r2[i] <- n_residentes_r1[i-1]
      n_residentes_r1[i] <- vagas_ano
      n_preceptores[i] <- n_preceptores[i-1]
      n_mfc[i] <- n_mfc[i-1] + n_formados_ano[i]
    }
  }
  
  prop_mfc <- pmin(n_mfc / n_medicos_total, 1)
  
  data.frame(
    ano = anos,
    n_mfc = n_mfc,
    n_residentes_r1 = n_residentes_r1,
    n_residentes_r2 = n_residentes_r2,
    n_residentes_total = n_residentes_r1 + n_residentes_r2,
    n_formados_ano = n_formados_ano,
    n_formados_acum = cumsum(n_formados_ano),
    n_preceptores = n_preceptores,
    prop_mfc = prop_mfc
  )
}

calcular_custos_programa <- function(
    expansao_df,
    bolsa_mensal = 8106.90,
    custo_preceptor_mensal = 4000.00,
    incentivo_equipe_mensal = 4500.00,
    custos_operacionais_ano = 50000
) {
  #' Calcula custos anuais do programa de residência
  
  custo_bolsas <- expansao_df$n_residentes_total * bolsa_mensal * 12
  custo_preceptoria <- expansao_df$n_preceptores * custo_preceptor_mensal * 12
  receita_incentivo <- expansao_df$n_residentes_total * incentivo_equipe_mensal * 12
  custo_operacional <- ifelse(expansao_df$ano > 0, custos_operacionais_ano, 0)
  
  custo_bruto <- custo_bolsas + custo_preceptoria + custo_operacional
  custo_liquido <- pmax(custo_bruto - receita_incentivo, 0)
  
  data.frame(
    ano = expansao_df$ano,
    custo_bolsas = custo_bolsas,
    custo_preceptoria = custo_preceptoria,
    custo_operacional = custo_operacional,
    custo_bruto = custo_bruto,
    receita_incentivo = receita_incentivo,
    custo_liquido = custo_liquido
  )
}

executar_simulacao_completa <- function(
    n_medicos = 100,
    vagas_ano = 10,
    taxa_retencao = 0.60,
    custo_consulta = 50,
    fator_aih = 1.5,
    taxa_desconto = 0.05,
    horizonte = 10
) {
  #' Executa simulação completa e retorna indicadores
  
  # Simulação simplificada para uso em análise de sensibilidade
  custo_anual <- vagas_ano * 2 * 8107 * 12 + 50000
  custo_anual <- custo_anual - vagas_ano * 2 * 4500 * 12
  
  cobertura_final <- min((3 + horizonte * vagas_ano * taxa_retencao * 0.5) / n_medicos, 1)
  beneficio_ano_10 <- cobertura_final * 3000000 * fator_aih
  
  custos <- c(0, rep(custo_anual, horizonte))
  beneficios <- seq(0, beneficio_ano_10, length.out = horizonte + 1)
  
  custo_total <- sum(custos)
  beneficio_total <- sum(beneficios)
  saldo <- beneficio_total - custo_total
  
  anos <- 0:horizonte
  saldos <- beneficios - custos
  vpl <- sum(saldos / ((1 + taxa_desconto) ^ anos))
  
  roi <- (beneficio_total - custo_total) / custo_total * 100
  rcb <- beneficio_total / custo_total
  
  saldo_acum <- cumsum(saldos)
  payback <- which(saldo_acum >= 0)[1] - 1
  if (is.na(payback)) payback <- Inf
  
  list(
    custo_total = custo_total,
    beneficio_total = beneficio_total,
    saldo = saldo,
    vpl = vpl,
    roi = roi,
    rcb = rcb,
    payback = payback,
    cobertura_final = cobertura_final
  )
}

message("Funções de simulação carregadas: simular_expansao_programa, ",
        "calcular_custos_programa, executar_simulacao_completa")
