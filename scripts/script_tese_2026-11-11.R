# ==============================================================================
# 0. PREPARAÇÃO
# ==============================================================================

pkgs <- c("dplyr", "tidyr", "openxlsx", "readxl", "lubridate", "pscl", "sandwich",
          "lmtest", "nnet", "ggplot2", "MatchIt", "estimatr", "PNADcIBGE")
novos <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(novos) > 0) install.packages(novos)
invisible(lapply(pkgs, library, character.only = TRUE))
options(dplyr.summarise.inform = FALSE, warn = 1)

# Pasta raiz do repositório (rode a partir dela) ou defina a variável de ambiente TESE_DIR
base_dir  <- Sys.getenv("TESE_DIR", ".")
dados_dir <- file.path(base_dir, "Dados")
cache_dir <- file.path(dados_dir, "pnadc_cache")
saida_dir <- file.path(base_dir, "Resultados_2026-11-11")

# Recorte temporal: matrículas com ingresso entre 2022 e 2024 e questionário até dez./2024
ANO_INI <- 2022
ANO_FIM <- 2024
graf_dir  <- file.path(saida_dir, "graficos")
dir.create(graf_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

saidas <- list()
guardar <- function(nome, df) {
  nome <- substr(nome, 1, 31)
  saidas[[nome]] <<- as.data.frame(df)
  invisible(df)
}

blocos <- list(
  "Institucionais"  = c("tipo_turma", "modalidade", "turno"),
  "Socioeconômicas" = c("renda", "RS", "ocupado", "ocupacao2", "inativo", "horas",
                        "tempo2", "psocial_beneficiario", "trabalho_mudanca",
                        "formal_mudanca"),
  "Demográficas"    = c("mulher", "idade", "cor", "escolaridade", "moradia"),
  "Ano de ingresso" = c("ano_f")
)
vars_ordem <- unlist(blocos, use.names = FALSE)

bloco_de <- function(term) {
  for (b in names(blocos)) for (v in blocos[[b]]) if (startsWith(term, v)) return(b)
  NA_character_
}
ordem_de <- function(term) {
  pos <- sapply(vars_ordem, function(v) startsWith(term, v))
  if (any(pos)) min(which(pos)) else 999
}

rotulo <- function(term) {
  r <- term
  r <- sub("^tipo_turma[01]-", "Tipo de curso: ", r)
  r <- sub("^tipo_turma", "Tipo de curso: ", r)
  r <- sub("^modalidade", "Modalidade: ", r)
  r <- sub("^turno", "Turno: ", r)
  r <- sub("^cor", "Cor/raça: ", r)
  r <- sub("^moradia", "Moradia: ", r)
  r <- sub("^ocupacao2", "Ocupação no ingresso: ", r)
  r <- sub("^RS", "RS: ", r)
  r <- sub("^ano_f", "Ano de ingresso: ", r)
  r <- sub("^horas_f", "Horas de trabalho: ", r)
  r <- sub("^escolaridade_f", "Escolaridade: ", r)
  r[r == "mulher"] <- "Mulher (ref.: homem)"
  r[r == "idade"] <- "Idade"
  r[r == "renda"] <- "Renda"
  r[r == "ocupado"] <- "Ocupado"
  r[r == "inativo"] <- "Inativo no ingresso (ref.: desocupado)"
  r[r == "horas"] <- "Horas de trabalho"
  r[r == "tempo2"] <- "Tempo de deslocamento"
  r[r == "psocial_beneficiario"] <- "Benefício social"
  r[r == "escolaridade"] <- "Escolaridade"
  r[r == "trabalho_mudanca"] <- "Transição para a ocupação"
  r[r == "formal_mudanca"] <- "Transição para a formalização"
  r[r == "renda:ocupado"] <- "Renda × Ocupado"
  r
}

estrelas <- function(p) cut(p, c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                            labels = c("***", "**", "*", ".", ""))

vc_hc1 <- function(m) sandwich::vcovHC(m, type = "HC1")
vc_cl  <- function(m, cl) sandwich::vcovCL(m, cluster = cl, type = "HC1")

tab_glm <- function(m, V, nome) {
  b  <- coef(m)
  se <- sqrt(diag(V))[names(b)]
  z  <- b / se
  p  <- 2 * pnorm(-abs(z))
  df <- data.frame(Modelo = nome, termo = names(b), b = b, ep = se, p = p,
                   stringsAsFactors = FALSE)
  df <- df[df$termo != "(Intercept)", ]
  df$Bloco    <- sapply(df$termo, bloco_de)
  df$ordem    <- sapply(df$termo, ordem_de)
  df$Variavel <- rotulo(df$termo)
  df$OR       <- exp(df$b)
  df$IC_inf   <- exp(df$b - 1.96 * df$ep)
  df$IC_sup   <- exp(df$b + 1.96 * df$ep)
  df$Sig      <- estrelas(df$p)
  df <- df[order(df$ordem, df$termo), ]
  df[, c("Modelo", "Bloco", "Variavel", "OR", "IC_inf", "IC_sup", "p", "Sig")]
}

ajuste <- function(m, nome) {
  invisible(capture.output(r <- suppressMessages(pscl::pR2(m))))
  data.frame(Modelo = nome, McFadden = r[["McFadden"]], Cox_Snell = r[["r2ML"]],
             Nagelkerke = r[["r2CU"]], AIC = AIC(m), N = nobs(m))
}

cc <- function(d, vars) d[stats::complete.cases(d[, vars, drop = FALSE]), , drop = FALSE]

ajusta_logit <- function(formula, d, w = NULL) {
  if (is.null(w)) glm(formula, data = d, family = binomial("logit"))
  else {
    d$peso_ipw <- w
    glm(formula, data = d, family = quasibinomial("logit"), weights = peso_ipw)
  }
}

wald_bloco <- function(m, V, vars, nome, bloco) {
  idx <- which(sapply(names(coef(m)), function(t) any(startsWith(t, vars))))
  b <- coef(m)[idx]
  W <- as.numeric(t(b) %*% solve(V[idx, idx, drop = FALSE]) %*% b)
  data.frame(Modelo = nome, Bloco = bloco, Wald = W, gl = length(idx),
             p = pchisq(W, length(idx), lower.tail = FALSE))
}

pred_media <- function(m, beta, nd) {
  X <- model.matrix(delete.response(terms(m)), nd, xlev = m$xlevels)
  X <- X[, names(beta), drop = FALSE]
  mean(plogis(X %*% beta))
}

ame <- function(m, d, V, nome, fixar = NULL) {
  vars <- intersect(all.vars(formula(m))[-1], vars_ordem)
  vars <- c(vars, setdiff(all.vars(formula(m))[-1], c(vars, "ano_f")))
  beta <- coef(m)
  beta <- beta[!is.na(beta)]
  V <- V[names(beta), names(beta)]
  if (!is.null(fixar)) for (f in names(fixar)) d[[f]] <- fixar[[f]]
  contrastes <- list()
  for (v in vars) {
    if (!is.null(fixar) && v %in% names(fixar)) next
    x <- d[[v]]
    if (is.factor(x)) {
      lv <- levels(droplevels(x))
      for (l in lv[-1]) {
        d0 <- d; d1 <- d
        d0[[v]] <- factor(lv[1], levels = levels(x))
        d1[[v]] <- factor(l, levels = levels(x))
        contrastes[[paste0(v, l)]] <- list(d0 = d0, d1 = d1, termo = paste0(v, l))
      }
    } else if (all(na.omit(unique(x)) %in% c(0, 1))) {
      d0 <- d; d1 <- d
      d0[[v]] <- 0; d1[[v]] <- 1
      contrastes[[v]] <- list(d0 = d0, d1 = d1, termo = v)
    } else {
      d0 <- d; d1 <- d
      d1[[v]] <- d1[[v]] + 1
      contrastes[[v]] <- list(d0 = d0, d1 = d1, termo = v)
    }
  }
  h <- 1e-5
  res <- lapply(contrastes, function(cn) {
    f <- function(bb) pred_media(m, bb, cn$d1) - pred_media(m, bb, cn$d0)
    est <- f(beta)
    g <- sapply(seq_along(beta), function(j) {
      e <- rep(0, length(beta)); e[j] <- h
      (f(beta + e) - f(beta - e)) / (2 * h)
    })
    se <- sqrt(as.numeric(t(g) %*% V %*% g))
    data.frame(Modelo = nome, termo = cn$termo, AME = est, ep = se)
  })
  out <- do.call(rbind, res)
  out$IC_inf <- out$AME - 1.96 * out$ep
  out$IC_sup <- out$AME + 1.96 * out$ep
  out$p      <- 2 * pnorm(-abs(out$AME / out$ep))
  out$Bloco  <- sapply(out$termo, bloco_de)
  out$ordem  <- sapply(out$termo, ordem_de)
  out$Variavel <- rotulo(out$termo)
  out <- out[order(out$ordem, out$termo), ]
  rownames(out) <- NULL
  out[, c("Modelo", "Bloco", "Variavel", "AME", "IC_inf", "IC_sup", "p")]
}

grafico_ame <- function(tab, arquivo, titulo) {
  tab <- tab[tab$Bloco != "Ano de ingresso", ]
  tab$Variavel <- factor(tab$Variavel, levels = rev(unique(tab$Variavel)))
  tab$sig <- ifelse(tab$p < 0.05, "p < 0,05", "n.s.")
  g <- ggplot(tab, aes(x = 100 * AME, y = Variavel, colour = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = 100 * IC_inf, xmax = 100 * IC_sup), height = 0.25) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("p < 0,05" = "#1D6FA3", "n.s." = "#9A9A9A"), name = NULL) +
    labs(x = "Efeito marginal médio (pontos percentuais)", y = NULL, title = titulo) +
    theme_minimal(base_size = 12, base_family = "serif") +
    theme(legend.position = "bottom")
  ggsave(file.path(graf_dir, arquivo), g, width = 8, height = 0.35 * nrow(tab) + 2,
         dpi = 300, bg = "white")
}

smd <- function(d, grupo, vars) {
  out <- list()
  for (v in vars) {
    x <- d[[v]]
    if (is.numeric(x) && length(unique(na.omit(x))) > 2) {
      a <- x[d[[grupo]] == 1]; b <- x[d[[grupo]] == 0]
      s <- sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)
      out[[v]] <- data.frame(Variavel = rotulo(v), Categoria = "média",
                             Grupo1 = mean(a, na.rm = TRUE), Grupo0 = mean(b, na.rm = TRUE),
                             SMD = (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) / s)
    } else {
      x <- as.character(x)
      for (l in sort(unique(na.omit(x)))) {
        a <- mean(x[d[[grupo]] == 1] == l, na.rm = TRUE)
        b <- mean(x[d[[grupo]] == 0] == l, na.rm = TRUE)
        s <- sqrt((a * (1 - a) + b * (1 - b)) / 2)
        out[[paste(v, l)]] <- data.frame(Variavel = rotulo(v), Categoria = l,
                                         Grupo1 = a, Grupo0 = b, SMD = (a - b) / s)
      }
    }
  }
  do.call(rbind, out)
}

# --- Complemento para os gráficos em Python ---
# Lista que reúne as tabelas exportadas para complemento_graficos_2026-11.xlsx
# e funções auxiliares usadas nas seções 2 a 5.

comp <- list()

prob_prevista <- function(m, d, V, var, vals, nome) {
  beta <- coef(m)
  beta <- beta[!is.na(beta)]
  V <- V[names(beta), names(beta)]
  h <- 1e-5
  do.call(rbind, lapply(vals, function(v) {
    nd <- d
    nd[[var]] <- v
    f <- function(bb) pred_media(m, bb, nd)
    est <- f(beta)
    g <- sapply(seq_along(beta), function(j) {
      e <- rep(0, length(beta)); e[j] <- h
      (f(beta + e) - f(beta - e)) / (2 * h)
    })
    se <- sqrt(as.numeric(t(g) %*% V %*% g))
    data.frame(Modelo = nome, Variavel = var, Valor = v, Prob = est,
               IC_inf = est - 1.96 * se, IC_sup = est + 1.96 * se, N = nrow(d))
  }))
}

esc_rot <- c("Sem instrução", "Fundamental incompleto", "Fundamental completo",
             "Médio incompleto", "Médio completo", "Superior incompleto", "Superior completo")
renda_rot <- c("Sem renda", "Até 1 SM", "Mais de 1 a 2 SM", "Mais de 2 a 5 SM",
               "Mais de 5 a 7 SM", "Mais de 7 a 10 SM", "Mais de 10 SM")
faixa_idade <- function(x) as.character(cut(x, c(15, 24, 29, 39, 49, 59, Inf),
                                            labels = c("Até 24 anos", "25 a 29 anos", "30 a 39 anos", "40 a 49 anos",
                                                       "50 a 59 anos", "60 anos ou mais")))

categorias <- function(d) {
  data.frame(
    Sexo = ifelse(d$mulher == 1, "Mulher", ifelse(d$mulher == 0, "Homem", NA)),
    `Faixa etária` = faixa_idade(d$idade),
    `Cor/raça` = as.character(d$cor),
    Escolaridade = esc_rot[d$escolaridade + 1],
    Moradia = as.character(d$moradia),
    `Renda no ingresso` = renda_rot[d$renda + 1],
    `Situação ocupacional no ingresso` = recode(as.character(d$ocupacao2),
                                                "Desocupação" = "Desocupado"),
    `Tipo de curso` = recode(as.character(d$tipo_turma), "0-Capacitação" = "Capacitação",
                             "1-Qualificação" = "Qualificação", "Tecnico/Superior" = "Técnico/Superior"),
    check.names = FALSE, stringsAsFactors = FALSE)
}

dist_cat <- function(cat_df, grupo, w = rep(1, nrow(cat_df))) {
  do.call(rbind, lapply(names(cat_df), function(v) {
    x <- cat_df[[v]]
    ok <- !is.na(x)
    t <- tapply(w[ok], x[ok], sum)
    data.frame(Grupo = grupo, Variavel = v, Categoria = names(t),
               N = as.numeric(tapply(rep(1, sum(ok)), x[ok], sum)[names(t)]),
               Proporcao = as.numeric(t) / sum(w[ok]))
  }))
}

faixa5 <- function(x) c("Sem renda", "Até 1 SM", "Mais de 1 a 2 SM", "Mais de 2 a 5 SM",
                        "Mais de 5 SM", "Mais de 5 SM", "Mais de 5 SM")[x + 1]

# ==============================================================================
# 1. LEITURA E RECODIFICAÇÃO
# ==============================================================================

se_raw <- read.csv(file.path(dados_dir, "dadosSE.csv"), encoding = "UTF-8",
                   stringsAsFactors = FALSE)
eg_raw <- read_excel(file.path(dados_dir, "dadosEG.xlsx"))
arq_eg <- file.path(dados_dir, "EG.xlsx")
col_eg <- names(read_excel(arq_eg, n_max = 0))
eg_uni <- read_excel(arq_eg, col_types = ifelse(col_eg %in% c("id_alunos", "id_escolas"), "guess", "skip")) %>%
  distinct(id_alunos, .keep_all = TRUE)

recod_comum <- function(d) {
  d %>%
    mutate(
      mulher        = ifelse(sexo %in% c(0, 1), 1 - sexo, NA_real_),
      ano_ingresso  = year(as.Date(data_matricula)),
      ano_f         = factor(ano_ingresso),
      ano_quest     = year(as.Date(data)),
      turno         = relevel(factor(turno), ref = "MATUTINO"),
      tipo_turma    = relevel(factor(tipo_turma), ref = "1-Qualificação"),
      moradia       = relevel(factor(moradia), ref = "Urbana"),
      escolaridade_f = factor(escolaridade)
    )
}

se <- recod_comum(se_raw) %>%
  mutate(
    exito      = ifelse(classe_calc == "4-Êxito", 1, 0),
    ocupado    = ifelse(ocupacao2 == "Ocupado", 1, 0),
    modalidade = case_when(modalidade == "EAD" ~ "EaD",
                           modalidade == "Hibrida" ~ "Híbrida",
                           TRUE ~ modalidade),
    modalidade = relevel(factor(modalidade), ref = "Presencial"),
    horas_f    = factor(horas),
    renda_cat  = cut(renda, c(-Inf, 2, 5, Inf), labels = c("Baixa", "Média", "Alta")),
    RS         = interaction(ocupacao2, renda_cat, sep = " + ", drop = TRUE)
  )

fluxo1 <- data.frame(Etapa = "Ingressantes", Passo = "Base integrada", N = nrow(se))
se1 <- se %>% filter(ano_quest <= 2024)
fluxo1 <- rbind(fluxo1, data.frame(Etapa = "Ingressantes", Passo = "Questionário até dez./2024", N = nrow(se1)))
se1 <- se1 %>% filter(ano_ingresso >= ANO_INI, ano_ingresso <= ANO_FIM) %>% mutate(ano_f = droplevels(ano_f))
fluxo1 <- rbind(fluxo1, data.frame(Etapa = "Ingressantes", Passo = "Ingresso entre 2022 e 2024", N = nrow(se1)))
se1 <- se1 %>% filter(cor != "Não informado")
fluxo1 <- rbind(fluxo1, data.frame(Etapa = "Ingressantes", Passo = "Cor/raça informada", N = nrow(se1)))
se1 <- se1 %>% filter(!is.na(renda))
fluxo1 <- rbind(fluxo1, data.frame(Etapa = "Ingressantes", Passo = "Renda informada", N = nrow(se1)))
se1 <- se1 %>% filter(ocupacao2 != "Não informado")
fluxo1 <- rbind(fluxo1, data.frame(Etapa = "Ingressantes", Passo = "Situação ocupacional informada", N = nrow(se1)))
se1 <- se1 %>% filter(!is.na(classe_calc))
fluxo1 <- rbind(fluxo1, data.frame(Etapa = "Ingressantes", Passo = "Situação da matrícula informada", N = nrow(se1)))

se1 <- se1 %>%
  mutate(cor = relevel(factor(cor), ref = "Não branca"),
         RS  = relevel(droplevels(RS), ref = "Desocupação + Baixa"),
         ocupacao2 = relevel(factor(ocupacao2), ref = "Ocupado"),
         classe_calc = relevel(factor(classe_calc), ref = "4-Êxito"))

rendaE_num <- function(x) case_when(
  x == "Nenhum" ~ 0, x == "Até 1 salário mínimo" ~ 1,
  x == "Mais de 1 até 2 salários mínimos" ~ 2, x == "Mais de 2 até 5 salários mínimos" ~ 3,
  x == "Mais de 5 até 7 salários mínimos" ~ 4, x == "Mais de 7 até 10 salários mínimos" ~ 5,
  x == "Mais do que 10 salários mínimos" ~ 6, TRUE ~ NA_real_)

eg <- recod_comum(eg_raw) %>%
  left_join(eg_uni, by = "id_alunos") %>%
  mutate(
    ocupacao2  = case_when(ocupacao %in% c("Empregado St.Público", "Empregado St.Privado",
                                           "Empresário/Empreendedor/MEI",
                                           "Estagiário/Jovem Aprendiz") ~ "Ocupado",
                           ocupacao == "Desocupação" ~ "Desocupação",
                           TRUE ~ ocupacao),
    ocupacao2E = ifelse(ocupacao2E == "Desocupado", "Desocupação", ocupacao2E),
    modalidade = ifelse(modalidade == "Presencial", "Presencial", "Não presencial"),
    modalidade = relevel(factor(modalidade), ref = "Presencial"),
    cor        = relevel(factor(cor), ref = "Não branca"),
    rendaE_n   = rendaE_num(rendaE)
  )

fluxo2 <- data.frame(Etapa = "Egressos", Passo = "Pesquisa com egressos", N = nrow(eg))
eg2 <- eg %>% filter(idade >= 16, idade <= 100, sexo %in% c(0, 1))
fluxo2 <- rbind(fluxo2, data.frame(Etapa = "Egressos", Passo = "Idade de 16 a 100 anos e sexo informado", N = nrow(eg2)))
eg2 <- eg2 %>% filter(ano_quest <= 2024)
fluxo2 <- rbind(fluxo2, data.frame(Etapa = "Egressos", Passo = "Questionário até dez./2024", N = nrow(eg2)))
eg2 <- eg2 %>% filter(ano_ingresso >= ANO_INI, ano_ingresso <= ANO_FIM) %>% mutate(ano_f = droplevels(ano_f))
fluxo2 <- rbind(fluxo2, data.frame(Etapa = "Egressos", Passo = "Ingresso entre 2022 e 2024", N = nrow(eg2)))
eg2 <- eg2 %>% filter(!is.na(renda), !is.na(rendaE), rendaE != "Não sei/Prefiro não responder")
fluxo2 <- rbind(fluxo2, data.frame(Etapa = "Egressos", Passo = "Renda informada nos dois momentos", N = nrow(eg2)))
eg2 <- eg2 %>% filter(!is.na(ocupacao2), ocupacao2 != "Não informado", !is.na(ocupacao2E))
fluxo2 <- rbind(fluxo2, data.frame(Etapa = "Egressos", Passo = "Situação ocupacional informada nos dois momentos", N = nrow(eg2)))
eg2 <- eg2 %>% filter(!is.na(escolaridade))
fluxo2 <- rbind(fluxo2, data.frame(Etapa = "Egressos", Passo = "Escolaridade informada", N = nrow(eg2)))

eg2 <- eg2 %>%
  mutate(
    ocupacao2        = relevel(factor(ocupacao2), ref = "Desocupação"),
    inativo          = ifelse(ocupacao2 == "Inativo", 1, 0),
    renda_melhora    = ifelse(rendaE_n > renda, 1, 0),
    trabalho_mudanca = ifelse(ocupacao2 != "Ocupado" & ocupacao2E == "Ocupado", 1, 0),
    formal_entrada   = ifelse(ocupacao %in% c("Empregado St.Público", "Empregado St.Privado"), 1, 0),
    formal_saida     = ifelse(is.na(ocupacaoE), NA_real_,
                              ifelse(ocupacaoE %in% c("Empregado(a) com carteira",
                                                      "Servidor(a) público(a)"), 1, 0)),
    formal_mudanca   = ifelse(is.na(formal_saida), NA_real_,
                              ifelse(formal_entrada == 0 & formal_saida == 1, 1, 0)),
    mudanca_geral    = ifelse(renda_melhora == 1 | trabalho_mudanca == 1 |
                                (!is.na(formal_mudanca) & formal_mudanca == 1), 1, 0)
  )

guardar("T1_fluxo_amostra", rbind(fluxo1, fluxo2))

# ==============================================================================
# 2. ETAPA 1: DETERMINANTES DO ÊXITO
# ==============================================================================

guardar("E1_tipo_turma", as.data.frame(table(Tipo_de_curso = se1$tipo_turma, useNA = "ifany")))

taxa <- mean(se1$exito)
n1 <- nrow(se1)
ic_w <- prop.test(sum(se1$exito), n1, correct = FALSE)$conf.int
guardar("E1_H1a_taxa_exito", data.frame(
  N = n1, Exitos = sum(se1$exito), Taxa = taxa, IC_inf = ic_w[1], IC_sup = ic_w[2],
  Benchmark_Pronatec_FIC = 0.43,
  p_unilateral = binom.test(sum(se1$exito), n1, p = 0.43, alternative = "greater")$p.value,
  H1a = ifelse(ic_w[1] > 0.43, "Confirmada", "Não confirmada")))

guardar("E1_descritivo_por_ano", se1 %>%
          group_by(Ano_ingresso = ano_ingresso) %>%
          summarise(Matriculas = n(),
                    Part_capacitacao = mean(tipo_turma == "0-Capacitação"),
                    Taxa_exito = mean(exito),
                    Exito_capacitacao = mean(exito[tipo_turma == "0-Capacitação"]),
                    Exito_qualificacao = mean(exito[tipo_turma == "1-Qualificação"]),
                    Exito_tecnico_superior = mean(exito[tipo_turma == "Tecnico/Superior"])))

inst <- "tipo_turma + modalidade + turno"
f1 <- list(
  CM1  = paste("exito ~", inst, "+ renda + ocupado + tempo2 + psocial_beneficiario + mulher + idade + cor + escolaridade + ano_f"),
  CM2  = "exito ~ renda + ocupado + ano_f",
  CM3  = paste("exito ~", inst, "+ renda + mulher + idade + cor + escolaridade + moradia + ano_f"),
  CM4  = paste("exito ~", inst, "+ renda + horas + mulher + idade + cor + escolaridade + moradia + ano_f"),
  CM5  = paste("exito ~", inst, "+ RS + mulher + idade + cor + escolaridade + moradia + ano_f"),
  CM5b = "exito ~ RS + ano_f",
  CM5c = "exito ~ RS + mulher + idade + cor + escolaridade + moradia + ano_f",
  CM6  = paste("exito ~", inst, "+ ano_f")
)
amostra1 <- list(CM3 = function(d) d[d$ocupacao2 == "Desocupação", ],
                 CM4 = function(d) d[d$ocupacao2 == "Ocupado", ])

mods1 <- list(); dados1 <- list(); V1 <- list()
for (nm in names(f1)) {
  fo <- as.formula(f1[[nm]])
  d  <- if (nm %in% names(amostra1)) amostra1[[nm]](se1) else se1
  d  <- droplevels(cc(d, all.vars(fo)))
  mods1[[nm]] <- ajusta_logit(fo, d)
  dados1[[nm]] <- d
  V1[[nm]] <- vc_hc1(mods1[[nm]])
}
guardar("E1_modelos_binarios", do.call(rbind, lapply(names(mods1), function(nm) tab_glm(mods1[[nm]], V1[[nm]], nm))))
guardar("E1_ajuste", do.call(rbind, lapply(names(mods1), function(nm) ajuste(mods1[[nm]], nm))))

rob1 <- list(
  CM1_escol_cat = sub("escolaridade", "escolaridade_f", f1$CM1),
  CM4_horas_escol_cat = sub("horas", "horas_f", sub("escolaridade", "escolaridade_f", f1$CM4)),
  CM5_escol_cat = sub("escolaridade", "escolaridade_f", f1$CM5)
)
mods_rob1 <- list()
for (nm in names(rob1)) {
  fo <- as.formula(rob1[[nm]])
  base_nm <- sub("_.*", "", nm)
  d <- if (base_nm %in% names(amostra1)) amostra1[[base_nm]](se1) else se1
  d <- droplevels(cc(d, all.vars(fo)))
  mods_rob1[[nm]] <- ajusta_logit(fo, d)
}
guardar("E1_robustez_categoricas", do.call(rbind, lapply(names(mods_rob1), function(nm)
  tab_glm(mods_rob1[[nm]], vc_hc1(mods_rob1[[nm]]), nm))))

fo_ha3 <- as.formula(paste(f1$CM1, "+ renda:ocupado"))
m_ha3 <- ajusta_logit(fo_ha3, dados1$CM1)
V_ha3 <- vc_hc1(m_ha3)
guardar("E1_Ha3_interacao", tab_glm(m_ha3, V_ha3, "CM1 + renda × ocupado"))
ame_ha3 <- rbind(
  ame(m_ha3, dados1$CM1, V_ha3, "Ocupados", fixar = list(ocupado = 1)),
  ame(m_ha3, dados1$CM1, V_ha3, "Não ocupados", fixar = list(ocupado = 0)))
guardar("E1_Ha3_efeito_renda", ame_ha3[ame_ha3$Variavel == "Renda", ])

ame_cm1 <- ame(mods1$CM1, dados1$CM1, V1$CM1, "CM1")
ame_cm5 <- ame(mods1$CM5, dados1$CM5, V1$CM5, "CM5")
guardar("E1_AME_CM1_CM5", rbind(ame_cm1, ame_cm5))
grafico_ame(ame_cm1, "E1_AME_CM1.png", "CM1: efeitos marginais médios sobre o êxito")
grafico_ame(ame_cm5, "E1_AME_CM5.png", "CM5: efeitos marginais médios sobre o êxito")

v_inst  <- blocos[["Institucionais"]]
v_socio <- c("renda", "ocupado", "tempo2", "psocial_beneficiario")
v_demo  <- c("mulher", "idade", "cor", "escolaridade")
d_h1 <- dados1$CM1
blocos_h1 <- list(Institucionais = v_inst, Socioeconomicas = v_socio, Demograficas = v_demo)
mcf <- function(vs) {
  fo <- as.formula(paste("exito ~", paste(c(vs, "ano_f"), collapse = " + ")))
  m <- glm(fo, data = d_h1, family = binomial)
  list(m = m, r2 = 1 - as.numeric(logLik(m)) / as.numeric(logLik(m0)))
}
m0 <- glm(exito ~ 1, data = d_h1, family = binomial)
m_ano <- glm(exito ~ ano_f, data = d_h1, family = binomial)
r2_ano <- 1 - as.numeric(logLik(m_ano)) / as.numeric(logLik(m0))
subconj <- unlist(lapply(0:3, function(k) combn(names(blocos_h1), k, simplify = FALSE)), recursive = FALSE)
r2_sub <- sapply(subconj, function(s) if (length(s) == 0) r2_ano else mcf(unlist(blocos_h1[s]))$r2)
chave <- function(s) if (length(s) == 0) "vazio" else paste(sort(s), collapse = "+")
names(r2_sub) <- sapply(subconj, chave)
shap <- sapply(names(blocos_h1), function(b) {
  outros <- setdiff(names(blocos_h1), b)
  tot <- 0
  for (k in 0:length(outros)) for (s in combn(outros, k, simplify = FALSE)) {
    w <- factorial(length(s)) * factorial(length(blocos_h1) - length(s) - 1) / factorial(length(blocos_h1))
    tot <- tot + w * (r2_sub[[chave(c(s, b))]] - r2_sub[[chave(s)]])
  }
  tot
})
guardar("E1_H1b_Shapley_pseudoR2", data.frame(
  Bloco = names(blocos_h1), Contribuicao_McFadden = as.numeric(shap),
  Participacao = as.numeric(shap) / sum(shap),
  McFadden_ano = r2_ano, McFadden_completo = r2_sub[["Demograficas+Institucionais+Socioeconomicas"]],
  N = nrow(d_h1)))
m_full <- mcf(unlist(blocos_h1))$m
lr <- function(sem, rot) {
  m_r <- mcf(unlist(blocos_h1[setdiff(names(blocos_h1), sem)]))$m
  t <- lmtest::lrtest(m_r, m_full)
  data.frame(Teste = rot, LR = t$Chisq[2], gl = t$Df[2], p = t$`Pr(>Chisq)`[2])
}
guardar("E1_H1b_razao_verossimilhanca", rbind(
  lr("Institucionais", "Completo × sem institucionais"),
  lr(c("Socioeconomicas", "Demograficas"), "Completo × sem socioeconômicas e demográficas"),
  lr("Socioeconomicas", "Completo × sem socioeconômicas"),
  lr("Demograficas", "Completo × sem demográficas")))
guardar("E1_H1b_modelos_blocos", rbind(
  ajuste(mcf(v_inst)$m, "Só institucionais"),
  ajuste(mcf(c(v_socio, v_demo))$m, "Só socioeconômicas e demográficas"),
  ajuste(m_full, "Completo")))

multi <- function(fo_bin, d, nome) {
  fo <- update(as.formula(fo_bin), classe_calc ~ .)
  m <- nnet::multinom(fo, data = d, trace = FALSE, maxit = 1000, Hess = TRUE)
  m_nulo <- nnet::multinom(classe_calc ~ 1, data = d, trace = FALSE)
  s <- summary(m)
  out <- do.call(rbind, lapply(rownames(s$coefficients), function(y) {
    b <- s$coefficients[y, ]; e <- s$standard.errors[y, ]
    data.frame(Modelo = nome, Desfecho = y, termo = names(b), b = b, ep = e)
  }))
  out <- out[out$termo != "(Intercept)", ]
  out$p <- 2 * pnorm(-abs(out$b / out$ep))
  out$RRR <- exp(out$b); out$IC_inf <- exp(out$b - 1.96 * out$ep); out$IC_sup <- exp(out$b + 1.96 * out$ep)
  out$Sig <- estrelas(out$p)
  out$Bloco <- sapply(out$termo, bloco_de); out$ordem <- sapply(out$termo, ordem_de)
  out$Variavel <- rotulo(out$termo)
  out <- out[order(out$Desfecho, out$ordem, out$termo), ]
  list(tab = out[, c("Modelo", "Desfecho", "Bloco", "Variavel", "RRR", "IC_inf", "IC_sup", "p", "Sig")],
       aj = data.frame(Modelo = nome,
                       McFadden = 1 - as.numeric(logLik(m)) / as.numeric(logLik(m_nulo)),
                       N = nrow(d)))
}
mm1 <- multi(f1$CM1, dados1$CM1, "CM1 multinomial")
mm5 <- multi(f1$CM5, dados1$CM5, "CM5 multinomial")
guardar("E1_multinomiais", rbind(mm1$tab, mm5$tab))
guardar("E1_multinomiais_ajuste", rbind(mm1$aj, mm5$aj))

# --- Complemento para os gráficos em Python ---
# Taxa de êxito por categoria dos ingressantes, com IC de 95%.

cat_ing <- categorias(se1)

comp[["Exito_por_categoria"]] <- do.call(rbind, lapply(names(cat_ing), function(v) {
  x <- cat_ing[[v]]
  ok <- !is.na(x)
  do.call(rbind, lapply(sort(unique(x[ok])), function(l) {
    y <- se1$exito[ok & x == l]
    ic <- prop.test(sum(y), length(y), correct = FALSE)$conf.int
    data.frame(Variavel = v, Categoria = l, N = length(y), Taxa_exito = mean(y),
               IC_inf = ic[1], IC_sup = ic[2])
  }))
}))

# ==============================================================================
# 3. ETAPA 2: EMPREGABILIDADE E RENDA
# ==============================================================================

eleg <- se %>%
  filter(ano_quest <= 2024, ano_ingresso >= ANO_INI, ano_ingresso <= ANO_FIM,
         classe_calc == "4-Êxito" | id_alunos %in% eg$id_alunos) %>%
  mutate(ano_f = droplevels(ano_f)) %>%
  mutate(respondeu = ifelse(id_alunos %in% eg$id_alunos, 1, 0),
         na_amostra = ifelse(id_alunos %in% eg2$id_alunos, 1, 0))
guardar("E2_taxa_resposta", data.frame(
  Elegiveis = nrow(eleg), Respondentes = sum(eleg$respondeu),
  Taxa_resposta = mean(eleg$respondeu), Na_amostra_analitica = sum(eleg$na_amostra)))
vars_sel <- c("mulher", "idade", "cor", "escolaridade", "renda", "ocupacao2", "tipo_turma",
              "modalidade", "ano_f")
tab_sel <- smd(eleg, "respondeu", vars_sel)
names(tab_sel)[3:4] <- c("Respondentes", "Nao_respondentes")
guardar("E2_respondentes_x_nao", tab_sel)
max_smd <- max(abs(tab_sel$SMD), na.rm = TRUE)

d_sel <- eleg %>% filter(ocupacao2 != "Não informado", cor != "Não informado") %>%
  mutate(ocupacao2 = factor(ocupacao2), cor = factor(cor), modalidade = factor(modalidade))
d_sel <- droplevels(cc(d_sel, c("respondeu", vars_sel)))
m_resp <- glm(as.formula(paste("respondeu ~", paste(vars_sel, collapse = " + "))),
              data = d_sel, family = binomial)
d_sel$p_resp <- fitted(m_resp)
eg2 <- eg2 %>% left_join(d_sel[, c("id_alunos", "p_resp")], by = "id_alunos") %>%
  mutate(w_ipw = ifelse(is.na(p_resp), NA_real_, 1 / p_resp),
         w_ipw = w_ipw / mean(w_ipw, na.rm = TRUE))

ctrl2 <- "psocial_beneficiario + moradia + mulher + idade + cor + escolaridade + ano_f"
f2 <- list(
  EM1  = paste("renda_melhora ~", inst, "+ ocupacao2 +", ctrl2),
  EM1_renda_inicial = paste("renda_melhora ~", inst, "+ ocupacao2 + renda +", ctrl2),
  EM1a = paste("renda_melhora ~ trabalho_mudanca +", inst, "+", ctrl2),
  EM1b = paste("renda_melhora ~ formal_mudanca +", inst, "+ ocupacao2 +", ctrl2),
  EM1ab = paste("renda_melhora ~ trabalho_mudanca + formal_mudanca +", inst, "+", ctrl2),
  EM2  = paste("trabalho_mudanca ~", inst, "+ renda +", ctrl2),
  EM2_inativo = paste("trabalho_mudanca ~", inst, "+ renda + inativo +", ctrl2),
  EM2_desocupados = paste("trabalho_mudanca ~", inst, "+ renda +", ctrl2),
  EM3  = paste("formal_saida ~", inst, "+ renda +", ctrl2),
  EM4  = paste("mudanca_geral ~", inst, "+", ctrl2)
)
amostra2 <- list(
  EM1a = function(d) d[!is.na(d$ocupacaoE), ],
  EM1b = function(d) d[!is.na(d$ocupacaoE), ],
  EM1ab = function(d) d[!is.na(d$ocupacaoE), ],
  EM2 = function(d) d[d$ocupacao2 != "Ocupado", ],
  EM2_inativo = function(d) d[d$ocupacao2 != "Ocupado", ],
  EM2_desocupados = function(d) d[d$ocupacao2 == "Desocupação", ],
  EM3 = function(d) d[!is.na(d$ocupacaoE) & d$formal_entrada == 0, ]
)
mods2 <- list(); dados2 <- list(); V2 <- list()
for (nm in names(f2)) {
  fo <- as.formula(f2[[nm]])
  d <- if (nm %in% names(amostra2)) amostra2[[nm]](eg2) else eg2
  d <- droplevels(cc(d, c(all.vars(fo), "id_escolas")))
  mods2[[nm]] <- ajusta_logit(fo, d)
  dados2[[nm]] <- d
  V2[[nm]] <- vc_cl(mods2[[nm]], d$id_escolas)
}
guardar("E2_modelos", do.call(rbind, lapply(names(mods2), function(nm) tab_glm(mods2[[nm]], V2[[nm]], nm))))
guardar("E2_ajuste", do.call(rbind, lapply(names(mods2), function(nm) ajuste(mods2[[nm]], nm))))
guardar("E2_H2a_Wald_institucional", do.call(rbind, lapply(c("EM1", "EM2", "EM3", "EM4"), function(nm)
  wald_bloco(mods2[[nm]], V2[[nm]], blocos[["Institucionais"]], nm, "Institucionais"))))

ame2 <- do.call(rbind, lapply(c("EM1", "EM1a", "EM1b", "EM2", "EM3", "EM4"), function(nm)
  ame(mods2[[nm]], dados2[[nm]], V2[[nm]], nm)))
guardar("E2_AME", ame2)
for (nm in c("EM1", "EM2", "EM3", "EM4")) grafico_ame(ame2[ame2$Modelo == nm, ],
                                                      paste0("E2_AME_", nm, ".png"),
                                                      paste0(nm, ": efeitos marginais médios"))

mods_ipw <- list()
for (nm in c("EM1", "EM2", "EM3", "EM4")) {
  d <- cc(dados2[[nm]], "w_ipw")
  mods_ipw[[nm]] <- ajusta_logit(as.formula(f2[[nm]]), d, w = d$w_ipw)
  mods_ipw[[nm]]$cl <- d$id_escolas
}
guardar("E2_IPW", cbind(
  do.call(rbind, lapply(names(mods_ipw), function(nm)
    tab_glm(mods_ipw[[nm]], vc_cl(mods_ipw[[nm]], mods_ipw[[nm]]$cl), paste(nm, "IPW")))),
  SMD_max_selecao = max_smd))

# --- Complemento para os gráficos em Python ---
# Probabilidades previstas por sexo e por transição (EM1a e EM1b).

comp[["P_sexo_EM"]] <- do.call(rbind, lapply(c("EM1", "EM2", "EM3", "EM4"), function(nm)
  prob_prevista(mods2[[nm]], dados2[[nm]], V2[[nm]], "mulher", c(1, 0), nm)))
comp[["P_EM1a_EM1b"]] <- rbind(
  prob_prevista(mods2$EM1a, dados2$EM1a, V2$EM1a, "trabalho_mudanca", c(0, 1), "EM1a"),
  prob_prevista(mods2$EM1b, dados2$EM1b, V2$EM1b, "formal_mudanca", c(0, 1), "EM1b"))

cat_eg  <- categorias(eg2)

# ==============================================================================
# 4. ETAPA 3: COMPARAÇÃO COM A PNAD CONTÍNUA
# ==============================================================================

trat <- eg2 %>%
  transmute(
    tratado = 1L, id_registro = as.character(id_alunos),
    sexo_h = ifelse(sexo == 1, "Homem", "Mulher"),
    cor_h = as.character(cor), moradia_h = as.character(moradia),
    escolaridade_h = c("Sem instrucao", "Fundamental incompleto", "Fundamental completo",
                       "Medio incompleto", "Medio completo", "Superior incompleto",
                       "Superior completo")[escolaridade + 1],
    idade_h = idade, ano_h = as.character(ano_quest), tipo_turma_h = as.character(tipo_turma),
    uf_h = "Goiás", v1028 = NA_real_,
    forca_trabalho = ocupacao2E != "Inativo",
    ocupado_h = case_when(ocupacao2E == "Ocupado" ~ 1L, ocupacao2E == "Desocupação" ~ 0L,
                          TRUE ~ NA_integer_),
    formal_h = case_when(
      ocupacaoE %in% c("Empregado(a) com carteira", "Servidor(a) público(a)") ~ 1L,
      ocupacaoE %in% c("Empregado(a) sem carteira", "Freelancer/Trabalho eventual",
                       "Trabalhador PJ", "Estagiário/Jovem aprendiz") ~ 0L,
      TRUE ~ NA_integer_),
    renda_faixa_h = rendaE_n - 1,
    renda_faixa_h = ifelse(renda_faixa_h < 0, NA_real_, renda_faixa_h)
  )

pnad_ano <- function(ano) {
  vars <- c("UF", "V1028", "V2007", "V2009", "V2010", "V1022", "VD3004", "VD4001", "VD4002",
            "VD4009", "VD4020", "V3019A", "V3021A", "V3026", "V3029")
  p <- get_pnadc(year = as.integer(ano), topic = 2, design = FALSE, labels = TRUE,
                 reload = FALSE, savedir = cache_dir, vars = vars)
  p <- p %>%
    filter(V2009 >= 16, V2009 <= 100) %>%
    mutate(
      fez_tecnico = (!is.na(V3019A) & grepl("^Sim", V3019A)) | (!is.na(V3021A) & grepl("^Sim", V3021A)),
      fez_qualif  = (!is.na(V3026) & grepl("^Sim", V3026)) | (!is.na(V3029) & grepl("^Sim", V3029)))
  excl <- data.frame(Ano = ano, Pessoas_16_100 = nrow(p), Exc_tecnico = sum(p$fez_tecnico),
                     Exc_qualificacao = sum(p$fez_qualif & !p$fez_tecnico),
                     Controles = sum(!p$fez_tecnico & !p$fez_qualif),
                     Controles_GO = sum(!p$fez_tecnico & !p$fez_qualif & as.character(p$UF) == "Goiás"))
  p <- p %>%
    filter(!fez_tecnico, !fez_qualif) %>%
    transmute(
      tratado = 0L, id_registro = paste0("pnadc_", ano, "_", row_number()),
      sexo_h = as.character(V2007),
      cor_h = case_when(V2010 == "Branca" ~ "Branca",
                        V2010 %in% c("Preta", "Amarela", "Parda", "Indígena") ~ "Não branca",
                        TRUE ~ "Não informado"),
      moradia_h = as.character(V1022),
      escolaridade_h = case_when(
        grepl("Sem instru", VD3004) ~ "Sem instrucao",
        grepl("Fundamental incompleto", VD3004) ~ "Fundamental incompleto",
        grepl("Fundamental completo", VD3004) ~ "Fundamental completo",
        grepl("dio incompleto", VD3004) ~ "Medio incompleto",
        grepl("dio completo", VD3004) ~ "Medio completo",
        grepl("Superior incompleto", VD3004) ~ "Superior incompleto",
        grepl("Superior completo", VD3004) ~ "Superior completo", TRUE ~ NA_character_),
      idade_h = V2009, ano_h = as.character(ano), tipo_turma_h = NA_character_,
      uf_h = as.character(UF), v1028 = as.numeric(V1028),
      forca_trabalho = VD4001 == "Pessoas na força de trabalho",
      ocupado_h = case_when(VD4002 == "Pessoas desocupadas" ~ 0L,
                            VD4002 == "Pessoas ocupadas" ~ 1L, TRUE ~ NA_integer_),
      formal_h = case_when(
        grepl("com carteira|estatut|militar", VD4009, ignore.case = TRUE) ~ 1L,
        grepl("sem carteira|conta.pr|conta pr", VD4009, ignore.case = TRUE) ~ 0L,
        TRUE ~ NA_integer_),
      renda_h = as.numeric(VD4020))
  list(p = p, excl = excl)
}

anos <- intersect(sort(unique(trat$ano_h)), c("2022", "2023", "2024"))
pn <- lapply(anos, pnad_ano)
guardar("E3_exclusoes_PNAD", do.call(rbind, lapply(pn, `[[`, "excl")))
sm <- c(`2022` = 1212, `2023` = 1320, `2024` = 1412)
ctrl <- bind_rows(lapply(pn, `[[`, "p")) %>%
  mutate(s = sm[ano_h],
         renda_faixa_h = case_when(is.na(renda_h) ~ NA_real_, renda_h <= s ~ 0, renda_h <= 2 * s ~ 1,
                                   renda_h <= 5 * s ~ 2, renda_h <= 7 * s ~ 3, renda_h <= 10 * s ~ 4,
                                   TRUE ~ 5)) %>%
  select(-s, -renda_h)
rm(pn); invisible(gc())

covs <- c("sexo_h", "cor_h", "moradia_h", "escolaridade_h", "idade_h", "ano_h")
base3 <- bind_rows(trat, ctrl) %>% filter(if_all(all_of(covs), ~ !is.na(.)))
regioes <- list(GO = "Goiás",
                CO = c("Goiás", "Distrito Federal", "Mato Grosso", "Mato Grosso do Sul"),
                BR = NULL)
cortes_idade <- c(16, 18, 24, 30, 40, 50, 65, 100)
pm <- c(0.5, 1.5, 3.5, 6, 8.5, 12)
pm15 <- c(0.5, 1.5, 3.5, 6, 8.5, 15)

l1 <- function(d, w, vars) {
  cel <- interaction(d[, vars], drop = TRUE)
  ft <- tapply(w[d$tratado == 1], cel[d$tratado == 1], sum)
  fc <- tapply(w[d$tratado == 0], cel[d$tratado == 0], sum)
  ft[is.na(ft)] <- 0; fc[is.na(fc)] <- 0
  0.5 * sum(abs(ft / sum(ft) - fc / sum(fc)))
}

media_aparada <- function(y, w, p, ponta) {
  o <- order(y, decreasing = (ponta == "topo"))
  y <- y[o]; w <- w[o]
  corte <- p * sum(w)
  acum <- cumsum(w)
  w2 <- pmax(0, pmin(w, acum - corte))
  sum(y * w2) / sum(w2)
}

lee <- function(par, y, obs) {
  t1 <- par$tratado == 1
  q_t <- weighted.mean(obs[t1], par$weights[t1])
  q_c <- weighted.mean(obs[!t1], par$weights[!t1])
  yt <- y[t1 & obs]; wt <- par$weights[t1 & obs]
  yc <- y[!t1 & obs]; wc <- par$weights[!t1 & obs]
  if (q_c >= q_t) {
    p <- (q_c - q_t) / q_c
    mt <- weighted.mean(yt, wt)
    inf <- mt - media_aparada(yc, wc, p, "base")
    sup <- mt - media_aparada(yc, wc, p, "topo")
  } else {
    p <- (q_t - q_c) / q_t
    mc <- weighted.mean(yc, wc)
    inf <- media_aparada(yt, wt, p, "topo") - mc
    sup <- media_aparada(yt, wt, p, "base") - mc
  }
  data.frame(Selecao_tratados = q_t, Selecao_controles = q_c, Proporcao_aparada = p,
             Limite_inferior = min(inf, sup), Limite_superior = max(inf, sup))
}

rodar_cem <- function(base, com_cor, pesos_pnad, rotulo_spec) {
  f <- if (com_cor) tratado ~ sexo_h + cor_h + moradia_h + escolaridade_h + idade_h + ano_h
  else tratado ~ sexo_h + moradia_h + escolaridade_h + idade_h + ano_h
  m <- matchit(f, data = base, method = "cem", cutpoints = list(idade_h = cortes_idade))
  par <- match.data(m)
  if (pesos_pnad) {
    fator <- ifelse(par$tratado == 1, 1, par$v1028 / mean(par$v1028[par$tratado == 0], na.rm = TRUE))
    par$weights <- par$weights * fator
  }
  vl1 <- c("sexo_h", "moradia_h", "escolaridade_h", "idade_h", "ano_h", if (com_cor) "cor_h")
  L1_antes  <- l1(base, rep(1, nrow(base)), vl1)
  L1_depois <- l1(par, par$weights, vl1)
  nn <- summary(m)$nn
  n_estratos <- length(unique(par$subclass))
  par$renda_sm   <- pm[par$renda_faixa_h + 1]
  par$renda_sm15 <- pm15[par$renda_faixa_h + 1]
  des <- list(
    "Ocupação"         = list(d = par %>% filter(forca_trabalho, !is.na(ocupado_h)), y = "ocupado_h"),
    "Formalização"     = list(d = par %>% filter(ocupado_h == 1, !is.na(formal_h)), y = "formal_h"),
    "Renda (faixa)"    = list(d = par %>% filter(!is.na(renda_faixa_h)), y = "renda_faixa_h"),
    "Renda (faixa, ocupados)" = list(d = par %>% filter(ocupado_h %in% 1, !is.na(renda_faixa_h)), y = "renda_faixa_h"),
    "Renda (SM)"       = list(d = par %>% filter(!is.na(renda_faixa_h)), y = "renda_sm"),
    "Renda (SM, teto 15)" = list(d = par %>% filter(!is.na(renda_faixa_h)), y = "renda_sm15"))
  res <- bind_rows(lapply(names(des), function(k) {
    dd <- des[[k]]$d; y <- des[[k]]$y
    mod <- lm_robust(as.formula(paste(y, "~ tratado")), data = dd, weights = weights,
                     clusters = subclass)
    s <- summary(mod)$coefficients
    data.frame(Especificacao = rotulo_spec, Desfecho = k,
               ATT = s["tratado", "Estimate"], EP = s["tratado", "Std. Error"],
               IC_inf = s["tratado", "CI Lower"], IC_sup = s["tratado", "CI Upper"],
               p = s["tratado", "Pr(>|t|)"], N_modelo = nobs(mod),
               N_tratados_desfecho = sum(dd$tratado == 1),
               Media_tratados = weighted.mean(dd[[y]][dd$tratado == 1], dd$weights[dd$tratado == 1]),
               Media_controles = weighted.mean(dd[[y]][dd$tratado == 0], dd$weights[dd$tratado == 0]))
  }))
  res$Tratados_total <- nn["All", "Treated"]
  res$Tratados_pareados <- nn["Matched", "Treated"]
  res$Tratados_sem_par <- nn["Unmatched", "Treated"]
  res$Perc_sem_par <- nn["Unmatched", "Treated"] / nn["All", "Treated"]
  res$Controles_pareados <- nn["Matched", "Control"]
  res$N_estratos <- n_estratos
  res$L1_antes <- L1_antes
  res$L1_depois <- L1_depois
  sig <- res$p < 0.05
  res$NNT_NNH <- ifelse(sig, 1 / abs(res$ATT), NA_real_)
  res$Tipo <- ifelse(!sig, "n.s.", ifelse(res$ATT > 0, "NNT", "NNH"))
  res$NNT_IC_a <- ifelse(sig & sign(res$IC_inf) == sign(res$IC_sup), 1 / abs(res$IC_sup), NA_real_)
  res$NNT_IC_b <- ifelse(sig & sign(res$IC_inf) == sign(res$IC_sup), 1 / abs(res$IC_inf), NA_real_)
  res$NNT_IC_inf <- pmin(res$NNT_IC_a, res$NNT_IC_b); res$NNT_IC_sup <- pmax(res$NNT_IC_a, res$NNT_IC_b)
  res$NNT_IC_a <- NULL; res$NNT_IC_b <- NULL
  res[grepl("^Renda \\(SM", res$Desfecho), c("NNT_NNH", "NNT_IC_inf", "NNT_IC_sup")] <- NA
  res[grepl("^Renda", res$Desfecho), "Tipo"] <- "não se aplica"
  lb <- rbind(
    cbind(Especificacao = rotulo_spec, Desfecho = "Formalização",
          lee(par, par$formal_h, par$ocupado_h %in% 1 & !is.na(par$formal_h))),
    cbind(Especificacao = rotulo_spec, Desfecho = "Renda (faixa, ocupados)",
          lee(par, par$renda_faixa_h, par$ocupado_h %in% 1 & !is.na(par$renda_faixa_h))))
  list(res = res, lee = lb, par = par)
}

specs <- list(
  list(nome = "GO sem cor (principal)", reg = "GO", cor = FALSE, pesos = FALSE),
  list(nome = "GO com cor",            reg = "GO", cor = TRUE,  pesos = FALSE),
  list(nome = "GO pesos V1028",        reg = "GO", cor = FALSE, pesos = TRUE),
  list(nome = "CO sem cor",            reg = "CO", cor = FALSE, pesos = FALSE),
  list(nome = "BR sem cor",            reg = "BR", cor = FALSE, pesos = FALSE))
tipos <- list(Geral = NULL, "Capacitação" = "0-Capacitação", "Qualificação" = "1-Qualificação")

res3 <- list(); lee3 <- list(); par_principal <- NULL
for (sp in specs) for (tp in names(tipos)) {
  b <- base3
  if (!is.null(regioes[[sp$reg]])) b <- b %>% filter(tratado == 1 | uf_h %in% regioes[[sp$reg]])
  if (!is.null(tipos[[tp]])) b <- b %>% filter(tratado == 0 | tipo_turma_h == tipos[[tp]])
  r <- rodar_cem(b, sp$cor, sp$pesos, paste(sp$nome, "|", tp))
  r$res$Regiao <- sp$reg; r$res$Base <- tp
  res3[[length(res3) + 1]] <- r$res
  lee3[[length(lee3) + 1]] <- r$lee
  if (sp$nome == "GO sem cor (principal)" && tp == "Geral") par_principal <- r$par
  cat("CEM:", sp$nome, "|", tp, "concluído\n")
}
res3 <- bind_rows(res3)
guardar("E3_ATT_CEM", res3)
guardar("E3_Lee", bind_rows(lee3))
perda_go <- res3$Perc_sem_par[res3$Especificacao == "GO sem cor (principal) | Geral"][1]
guardar("E3_decisao_recorte", data.frame(
  Perc_sem_par_GO = perda_go,
  Recomendacao = ifelse(perda_go > 0.10, "Usar Centro-Oeste como principal", "Manter Goiás como principal")))

ipm <- function(d, rot) {
  comp <- c("MS (qualquer melhora)" = "mudanca_geral", "Transição para a ocupação" = "trabalho_mudanca",
            "Transição para a formalização" = "formal_mudanca", "Aumento de renda" = "renda_melhora")
  do.call(rbind, lapply(names(comp), function(k) {
    p <- mean(coalesce(d[[comp[k]]], 0))
    data.frame(Base = rot, Indicador = k, N = nrow(d), Proporcao = p, IPM = 1 / p)
  }))
}
guardar("E3_IPM", rbind(ipm(eg2, "Geral"),
                        ipm(eg2[eg2$tipo_turma == "0-Capacitação", ], "Capacitação"),
                        ipm(eg2[eg2$tipo_turma == "1-Qualificação", ], "Qualificação")))

# --- Complemento para os gráficos em Python ---
# Proporção de cor/raça não informada por ano da pesquisa com egressos.

comp[["Cor_nao_informada_ano"]] <- eg2 %>%
  group_by(Ano = ano_quest) %>%
  summarise(N = n(), Nao_informada = mean(cor == "Não informado")) %>%
  bind_rows(data.frame(Ano = NA, N = nrow(eg2), Nao_informada = mean(eg2$cor == "Não informado")))

# ==============================================================================
# 5. ESTATÍSTICAS DESCRITIVAS
# ==============================================================================

desc_col <- function(d, w, rot) {
  w <- w / sum(w)
  wm <- function(x) sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)])
  esc <- d$escolaridade_h
  data.frame(
    Variavel = c("Mulher", "Idade média", "Cor branca", "Cor não branca", "Cor não informada",
                 "Moradia urbana", paste("Escolaridade:", c("Sem instrucao", "Fundamental incompleto",
                                                            "Fundamental completo", "Medio incompleto", "Medio completo", "Superior incompleto",
                                                            "Superior completo")), "Ocupado no momento de referência", "Renda até 1 SM",
                 "Renda de 1 a 2 SM", "Renda acima de 2 SM"),
    Valor = c(wm(d$sexo_h == "Mulher"), wm(d$idade_h), wm(d$cor_h == "Branca"),
              wm(d$cor_h == "Não branca"), wm(d$cor_h == "Não informado"),
              wm(d$moradia_h == "Urbana"),
              sapply(c("Sem instrucao", "Fundamental incompleto", "Fundamental completo",
                       "Medio incompleto", "Medio completo", "Superior incompleto",
                       "Superior completo"), function(l) wm(esc == l)),
              wm(d$ocupado_h), wm(d$renda_faixa_h == 0), wm(d$renda_faixa_h == 1),
              wm(d$renda_faixa_h >= 2)),
    Grupo = rot)
}
ing <- se1 %>% transmute(
  sexo_h = ifelse(mulher == 1, "Mulher", "Homem"), idade_h = idade, cor_h = as.character(cor),
  moradia_h = as.character(moradia),
  escolaridade_h = c("Sem instrucao", "Fundamental incompleto", "Fundamental completo",
                     "Medio incompleto", "Medio completo", "Superior incompleto",
                     "Superior completo")[escolaridade + 1],
  ocupado_h = ocupado, renda_faixa_h = pmax(renda - 1, 0))
egd <- trat %>% mutate(ocupado_h = ifelse(ocupado_h %in% 1, 1, 0))
ctr <- par_principal[par_principal$tratado == 0, ] %>% mutate(ocupado_h = ifelse(ocupado_h %in% 1, 1, 0))
desc <- rbind(desc_col(ing, rep(1, nrow(ing)), "Ingressantes"),
              desc_col(egd, rep(1, nrow(egd)), "Egressos"),
              desc_col(ctr, ctr$weights, "Controles pareados (PNAD, GO)"))
guardar("T7_descritivos", tidyr::pivot_wider(desc, names_from = Grupo, values_from = Valor))

g1 <- se1 %>%
  mutate(Tipo = recode(as.character(tipo_turma), "0-Capacitação" = "Capacitação",
                       "1-Qualificação" = "Qualificação", "Tecnico/Superior" = "Técnico/Superior")) %>%
  group_by(Tipo) %>% summarise(Taxa = mean(exito), N = n())
p1 <- ggplot(g1, aes(x = Tipo, y = 100 * Taxa)) +
  geom_col(fill = "#1D6FA3", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * Taxa)), vjust = -0.4, family = "serif") +
  labs(x = NULL, y = "Taxa de êxito (%)") + ylim(0, 100) +
  theme_minimal(base_size = 12, base_family = "serif")
ggsave(file.path(graf_dir, "T7_exito_por_tipo.png"), p1, width = 7, height = 4.5, dpi = 300, bg = "white")

g2 <- bind_rows(
  data.frame(Momento = "Ingresso", Situacao = as.character(eg2$ocupacao2)),
  data.frame(Momento = "Pesquisa com egressos", Situacao = as.character(eg2$ocupacao2E))) %>%
  mutate(Situacao = recode(Situacao, "Desocupação" = "Desocupado"),
         Momento = factor(Momento, levels = c("Ingresso", "Pesquisa com egressos"))) %>%
  count(Momento, Situacao) %>% group_by(Momento) %>% mutate(p = n / sum(n))
p2 <- ggplot(g2, aes(x = Situacao, y = 100 * p, fill = Momento)) +
  geom_col(position = position_dodge(0.7), width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * p)), position = position_dodge(0.7),
            vjust = -0.4, size = 3.4, family = "serif") +
  scale_fill_manual(values = c("#9EC5E0", "#1D6FA3"), name = NULL) +
  labs(x = NULL, y = "Egressos (%)") +
  theme_minimal(base_size = 12, base_family = "serif") + theme(legend.position = "bottom")
ggsave(file.path(graf_dir, "T7_situacao_trabalho.png"), p2, width = 7, height = 4.5, dpi = 300, bg = "white")

fx <- c("Nenhuma", "Até 1 SM", "1 a 2 SM", "2 a 5 SM", "5 a 7 SM", "7 a 10 SM", "Mais de 10 SM")
g3 <- bind_rows(
  data.frame(Momento = "Ingresso", Faixa = fx[eg2$renda + 1]),
  data.frame(Momento = "Pesquisa com egressos", Faixa = fx[eg2$rendaE_n + 1])) %>%
  filter(!is.na(Faixa)) %>%
  mutate(Faixa = factor(Faixa, levels = fx),
         Momento = factor(Momento, levels = c("Ingresso", "Pesquisa com egressos"))) %>%
  count(Momento, Faixa) %>% group_by(Momento) %>% mutate(p = n / sum(n))
p3 <- ggplot(g3, aes(x = Faixa, y = 100 * p, fill = Momento)) +
  geom_col(position = position_dodge(0.7), width = 0.65) +
  scale_fill_manual(values = c("#9EC5E0", "#1D6FA3"), name = NULL) +
  labs(x = NULL, y = "Egressos (%)") +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(graf_dir, "T7_faixas_renda.png"), p3, width = 7, height = 4.5, dpi = 300, bg = "white")

# --- Complemento para os gráficos em Python ---
# Distribuição por categoria (ingressantes, egressos e controles pareados)
# e matrizes de transição para os gráficos aluviais.

ctr_pp  <- par_principal[par_principal$tratado == 0, ]
cat_ctr <- data.frame(
  Sexo = ctr_pp$sexo_h,
  `Faixa etária` = faixa_idade(ctr_pp$idade_h),
  `Cor/raça` = ctr_pp$cor_h,
  Escolaridade = recode(ctr_pp$escolaridade_h, "Sem instrucao" = "Sem instrução",
                        "Medio incompleto" = "Médio incompleto", "Medio completo" = "Médio completo"),
  Moradia = ctr_pp$moradia_h,
  check.names = FALSE, stringsAsFactors = FALSE)

comp[["Dist_categorias"]] <- rbind(
  dist_cat(cat_ing, "Ingressantes"),
  dist_cat(cat_eg, "Egressos"),
  dist_cat(cat_ctr, "Controles pareados (PNAD, GO)", ctr_pp$weights))

ocu_in <- recode(as.character(eg2$ocupacao2), "Desocupação" = "Desocupado")
ocu_eg <- recode(as.character(eg2$ocupacao2E), "Desocupação" = "Desocupado")
comp[["Aluvial_ocupacao"]] <- as.data.frame(table(Ingresso = ocu_in, Egresso = ocu_eg),
                                            stringsAsFactors = FALSE)
comp[["Aluvial_renda"]] <- as.data.frame(table(Ingresso = faixa5(eg2$renda),
                                               Egresso = faixa5(eg2$rendaE_n)),
                                         stringsAsFactors = FALSE)

# ==============================================================================
# 6. EXPORTAÇÃO
# ==============================================================================

wb <- createWorkbook()
for (nm in names(saidas)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, saidas[[nm]])
}
arq <- file.path(saida_dir, "resultados_tese_2026-11.xlsx")
saveWorkbook(wb, arq, overwrite = TRUE)
writeLines(capture.output(sessionInfo()), file.path(saida_dir, "sessionInfo.txt"))
cat("\nResultados salvos em:", arq, "\nGráficos em:", graf_dir, "\n")

wb2 <- createWorkbook()
for (nm in c("P_sexo_EM", "P_EM1a_EM1b", "Dist_categorias", "Exito_por_categoria",
            "Cor_nao_informada_ano", "Aluvial_ocupacao", "Aluvial_renda")) {
  addWorksheet(wb2, nm)
  writeData(wb2, nm, comp[[nm]])
}
arq2 <- file.path(saida_dir, "complemento_graficos_2026-11.xlsx")
saveWorkbook(wb2, arq2, overwrite = TRUE)
cat("\nComplemento salvo em:", arq2, "\n")
