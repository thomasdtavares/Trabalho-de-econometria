# Escolaridade do eleitorado e voto presidencial por seção eleitoral (Brasil, 2022)
# Dados: TSE. Chave da seção: SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO

# =============================================================================
# 0. PACOTES, CAMINHOS E FUNÇÕES
# =============================================================================
# install.packages(c("data.table", "lmtest", "sandwich", "car", "ggplot2", "patchwork", "stargazer"))
library(data.table)
library(lmtest)
library(sandwich)
library(car)
library(ggplot2)
library(patchwork)
library(stargazer)

options(timeout = 3600)

dir_raw <- "dados/brutos"
dir_out <- "dados"
dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)

url_base <- "https://cdn.tse.jus.br/estatistica/sead/odsele"
zip_vot  <- file.path(dir_raw, "votacao_secao_2022_BR.zip")
zip_loc  <- file.path(dir_raw, "eleitorado_local_votacao_2022.zip")

chave <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")

# Baixa só se o arquivo ainda não existir
baixar <- function(url, destino) {
  if (!file.exists(destino)) {
    message("Baixando: ", basename(destino))
    download.file(url, destino, mode = "wb")
  } else {
    message("Já existe: ", basename(destino))
  }
  invisible(destino)
}

# Lê o csv de um zip do TSE e confere se as colunas do select existem
ler_zip_tse <- function(zip, padrao_csv = "\\.csv$", select = NULL) {
  arquivos <- unzip(zip, list = TRUE)$Name
  csv <- grep(padrao_csv, arquivos, value = TRUE, ignore.case = TRUE)
  if (length(csv) != 1) {
    stop("Esperava 1 csv em ", basename(zip), ", encontrei: ",
         paste(csv, collapse = ", "))
  }
  tmp <- tempfile()
  unzip(zip, files = csv, exdir = tmp)
  arq <- file.path(tmp, csv)
  
  if (!is.null(select)) {
    cabecalho <- names(fread(arq, sep = ";", encoding = "Latin-1", nrows = 0))
    faltando  <- setdiff(select, cabecalho)
    if (length(faltando) > 0) {
      stop("Colunas não encontradas em ", basename(zip), ": ",
           paste(faltando, collapse = ", "),
           "\nColunas disponíveis: ", paste(cabecalho, collapse = ", "))
    }
  }
  
  dt <- fread(arq, sep = ";", encoding = "Latin-1",
              select = select, showProgress = FALSE)
  unlink(tmp, recursive = TRUE)
  dt
}

checar_nao_vazia <- function(dt, nome) {
  if (nrow(dt) == 0) stop("A tabela '", nome, "' ficou vazia após o filtro.")
  invisible(dt)
}

# Erro-padrão robusto (HC1)
ep <- function(m) sqrt(diag(vcovHC(m, type = "HC1")))

# Variáveis de escolaridade (referência omitida: médio completo)
esc <- c("pct_analfabeto", "pct_le_escreve", "pct_fund_inc", "pct_fund_comp",
         "pct_medio_inc", "pct_sup_inc", "pct_sup_comp")

# =============================================================================
# 1. IMPORTAÇÃO - VOTAÇÃO POR SEÇÃO (presidente, 1º e 2º turnos)
# =============================================================================
baixar(file.path(url_base, "votacao_secao", "votacao_secao_2022_BR.zip"), zip_vot)

vot <- ler_zip_tse(
  zip_vot,
  select = c("NR_TURNO", "CD_CARGO", "SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO",
             "NR_ZONA", "NR_SECAO", "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS")
)

vot <- vot[CD_CARGO == 1]
checar_nao_vazia(vot, "votação (presidente)")

print(vot[, .N, by = NR_TURNO][order(NR_TURNO)])
print(unique(vot[, .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)])[order(NR_TURNO, NR_VOTAVEL)])

municipios <- unique(vot[, .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)])

# Uma linha por seção, colunas t<turno>_votos_<NR_VOTAVEL>
votos_turno <- function(t) {
  vt <- vot[NR_TURNO == t]
  checar_nao_vazia(vt, paste0("votação, turno ", t))
  w <- dcast(vt, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~
               paste0("t", t, "_votos_", NR_VOTAVEL),
             value.var = "QT_VOTOS", fun.aggregate = sum, fill = 0)
  w[, paste0("na_votacao_t", t) := TRUE]
  w
}

# Seção ausente em um turno fica NA nas colunas desse turno
vot_w <- merge(votos_turno(1), votos_turno(2), by = chave, all = TRUE)
rm(vot); invisible(gc())

# =============================================================================
# 2. IMPORTAÇÃO - PERFIL DO ELEITORADO POR SEÇÃO (um zip por UF; ZZ = exterior)
# =============================================================================
ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA",
         "PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO","ZZ")

cols_perfil <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO",
                 "CD_GENERO", "DS_GENERO",
                 "CD_FAIXA_ETARIA", "DS_FAIXA_ETARIA",
                 "CD_GRAU_ESCOLARIDADE", "DS_GRAU_ESCOLARIDADE",
                 "QT_ELEITORES_PERFIL")

perfil <- rbindlist(lapply(ufs, function(uf) {
  arq <- sprintf("perfil_eleitor_secao_2022_%s.zip", uf)
  z <- file.path(dir_raw, arq)
  baixar(file.path(url_base, "perfil_eleitor_secao", arq), z)
  ler_zip_tse(z, select = cols_perfil)
}))

# Dicionários código -> descrição
dic_escolaridade <- unique(perfil[, .(CD_GRAU_ESCOLARIDADE, DS_GRAU_ESCOLARIDADE)])[order(CD_GRAU_ESCOLARIDADE)]
dic_genero       <- unique(perfil[, .(CD_GENERO, DS_GENERO)])[order(CD_GENERO)]
dic_faixa        <- unique(perfil[, .(CD_FAIXA_ETARIA, DS_FAIXA_ETARIA)])[order(CD_FAIXA_ETARIA)]
print(dic_escolaridade); print(dic_genero); print(dic_faixa)

# Contagens por seção: esc_<cód>, gen_<cód>, idade_<cód>
perfil_esc <- dcast(perfil, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~
                      paste0("esc_", CD_GRAU_ESCOLARIDADE),
                    value.var = "QT_ELEITORES_PERFIL", fun.aggregate = sum, fill = 0)
perfil_gen <- dcast(perfil, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~
                      paste0("gen_", CD_GENERO),
                    value.var = "QT_ELEITORES_PERFIL", fun.aggregate = sum, fill = 0)
perfil_ida <- dcast(perfil, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~
                      paste0("idade_", CD_FAIXA_ETARIA),
                    value.var = "QT_ELEITORES_PERFIL", fun.aggregate = sum, fill = 0)

perfil_tot <- perfil[, .(qt_eleitores_perfil = sum(QT_ELEITORES_PERFIL)), by = chave]

perfil_w <- Reduce(function(a, b) merge(a, b, by = chave),
                   list(perfil_tot, perfil_esc, perfil_gen, perfil_ida))
rm(perfil, perfil_esc, perfil_gen, perfil_ida, perfil_tot); invisible(gc())

# =============================================================================
# 3. IMPORTAÇÃO - ELEITORADO POR LOCAL DE VOTAÇÃO (latitude / longitude)
# =============================================================================
baixar(file.path(url_base, "eleitorado_locais_votacao",
                 "eleitorado_local_votacao_2022.zip"), zip_loc)

loc <- ler_zip_tse(zip_loc)
print(names(loc))

faltando_chave <- setdiff(chave, names(loc))
if (length(faltando_chave) > 0) {
  stop("A base de locais não tem as colunas da chave: ",
       paste(faltando_chave, collapse = ", "))
}

# Mantém o 2º turno
if ("NR_TURNO" %in% names(loc)) {
  print(loc[, .N, by = NR_TURNO][order(NR_TURNO)])
  loc <- loc[NR_TURNO == 2]
  checar_nao_vazia(loc, "locais (2º turno)")
}

cols_loc <- intersect(c(chave, "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO",
                        "DS_TIPO_LOCAL", "NM_BAIRRO", "NR_CEP",
                        "NR_LATITUDE", "NR_LONGITUDE",
                        "NR_SECAO_PRINCIPAL", "DS_TIPO_SECAO_AGREGADA",
                        "QT_ELEITOR_SECAO"),
                      names(loc))
loc <- loc[, ..cols_loc]

# Coordenadas como número
for (v in intersect(c("NR_LATITUDE", "NR_LONGITUDE"), names(loc))) {
  if (is.character(loc[[v]])) set(loc, j = v, value = as.numeric(gsub(",", ".", loc[[v]])))
}

# Para se houver seções repetidas
dup <- loc[, .N, by = chave][N > 1]
if (nrow(dup) > 0) {
  print(loc[dup, on = chave][order(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO)])
  stop(nrow(dup), " seções aparecem mais de uma vez na base de locais.")
}

# =============================================================================
# 4. IMPORTAÇÃO - JUNÇÃO DAS TRÊS BASES
# =============================================================================
# Junção completa; as colunas na_/no_ indicam a base de origem
perfil_w[, no_perfil := TRUE]
loc[,      no_local  := TRUE]

base <- merge(vot_w, perfil_w, by = chave, all = TRUE)
base <- merge(base,  loc,      by = chave, all = TRUE)

for (v in c("na_votacao_t1", "na_votacao_t2", "no_perfil", "no_local")) {
  set(base, i = which(is.na(base[[v]])), j = v, value = FALSE)
}

base <- merge(municipios, base, by = c("SG_UF", "CD_MUNICIPIO"), all.y = TRUE)
setcolorder(base, chave)

print(base[, .N, by = .(na_votacao_t1, na_votacao_t2, no_perfil, no_local)][order(-N)])

# =============================================================================
# 5. IMPORTAÇÃO - COMPARAÇÃO COM O RESULTADO OFICIAL
# =============================================================================
# Totais oficiais (Brasil + exterior)
oficial <- data.table(
  turno     = c(1, 1, 2, 2),
  candidato = c("13", "22", "13", "22"),
  oficial   = c(57259504, 51072345, 60345999, 58206354)
)
oficial[, coluna := paste0("t", turno, "_votos_", candidato)]
oficial[, base := sapply(coluna, function(cl)
  if (cl %in% names(base)) sum(base[[cl]], na.rm = TRUE) else NA_real_)]
oficial[, diferenca := base - oficial]
print(oficial)

# =============================================================================
# 5B. IMPORTAÇÃO - RENDA DO RESPONSÁVEL POR SETOR CENSITÁRIO (IBGE, CENSO 2022)
# Cada seção recebe a renda do setor censitário em que fica o seu local de votação
# (latitude/longitude do TSE dentro do polígono do setor na malha do IBGE).
# Fica antes da seção 6 para que a base salva já traga a renda.
# =============================================================================
# install.packages("sf")
library(sf)
sf_use_s2(FALSE)   # ponto em polígono no plano; evita erros com polígonos imperfeitos da malha

url_ibge <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022"

# Renda por setor: V06004 = rendimento médio e V06006 = rendimento mediano mensal
# dos responsáveis com rendimento (R$); "X" = valor omitido pelo IBGE
z <- file.path(dir_raw, "Agregados_por_setores_renda_responsavel_BR_20260508_csv.zip")
baixar(file.path(url_ibge, "Agregados_por_Setores_Censitarios_Rendimento_do_Responsavel", basename(z)), z)
tmp <- tempfile()
unzip(z, exdir = tmp)
renda <- fread(list.files(tmp, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE),
               select = c("CD_SETOR", "V06004", "V06006"),
               colClasses = "character", na.strings = c("", "X"))
unlink(tmp, recursive = TRUE)
setnames(renda, c("V06004", "V06006"), c("renda_media", "renda_mediana"))
for (v in c("renda_media", "renda_mediana")) set(renda, j = v, value = as.numeric(gsub(",", ".", renda[[v]])))
stopifnot(!anyDuplicated(renda$CD_SETOR))

# Coordenadas válidas dos locais (mesmo critério da seção 7): -1 ou fora do Brasil é inválida e fica sem renda
pts <- unique(base[SG_UF != "ZZ" & NR_LATITUDE != -1 & NR_LONGITUDE != -1 &
                     NR_LATITUDE >= -34 & NR_LATITUDE <= 6 & NR_LONGITUDE >= -74 & NR_LONGITUDE <= -32,
                   .(SG_UF, NR_LATITUDE, NR_LONGITUDE)])
# Setor de cada coordenada válida (malha do IBGE, um arquivo por UF)
pts <- rbindlist(lapply(setdiff(ufs, "ZZ"), function(uf) {
  g <- file.path(dir_raw, paste0(uf, "_setores_CD2022.gpkg"))
  baixar(file.path(url_ibge, "Agregados_por_Setores_Censitarios/malha_com_atributos/setores/gpkg/UF",
                   uf, basename(g)), g)
  malha <- st_read(g, quiet = TRUE)
  p <- pts[SG_UF == uf]
  dentro <- st_intersects(st_as_sf(p, coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = st_crs(malha)), malha)
  p[, CD_SETOR := as.character(malha$CD_SETOR[sapply(dentro, function(i) i[1])])]
}))

# Junta à base só as colunas de renda (coordenada inválida, sem setor ou sem renda = NA)
pts <- merge(pts, renda, by = "CD_SETOR", all.x = TRUE)[, CD_SETOR := NULL]
base <- merge(base, pts, by = c("SG_UF", "NR_LATITUDE", "NR_LONGITUDE"), all.x = TRUE)
setorderv(base, chave)
setcolorder(base, chave)

print(base[SG_UF != "ZZ", .(secoes = .N, com_renda = sum(!is.na(renda_media)))])
rm(url_ibge, z, tmp, renda, pts); invisible(gc())


# =============================================================================
# 6. IMPORTAÇÃO - SALVA
# =============================================================================
fwrite(base, file.path(dir_out, "base_secao_2022.csv.gz"))
saveRDS(base, file.path(dir_out, "base_secao_2022.rds"))
fwrite(dic_escolaridade, file.path(dir_out, "dic_escolaridade.csv"))
fwrite(dic_genero,       file.path(dir_out, "dic_genero.csv"))
fwrite(dic_faixa,        file.path(dir_out, "dic_faixa_etaria.csv"))

message("Base final: ", nrow(base), " linhas x ", ncol(base), " colunas")

# =============================================================================
# 7. AMOSTRA E VARIÁVEIS
# (para não importar de novo: rodar a seção 0 e começar daqui)
# =============================================================================
base <- readRDS(file.path(dir_out, "base_secao_2022.rds"))
base <- as.data.frame(base)

base <- subset(base, SG_UF != "ZZ" & na_votacao_t2 & no_perfil)

# Y: % de votos válidos em Lula (Lula + Bolsonaro); 1º turno com a mesma definição
base$pct_lula_t2 <- 100 * base$t2_votos_13 / (base$t2_votos_13 + base$t2_votos_22)
base$pct_lula_t1 <- 100 * base$t1_votos_13 / (base$t1_votos_13 + base$t1_votos_22)

# Escolaridade
base$pct_esc_nao_inf <- 100 * base$esc_0 / base$qt_eleitores_perfil
base$pct_analfabeto  <- 100 * base$esc_1 / base$qt_eleitores_perfil
base$pct_le_escreve  <- 100 * base$esc_2 / base$qt_eleitores_perfil
base$pct_fund_inc    <- 100 * base$esc_3 / base$qt_eleitores_perfil
base$pct_fund_comp   <- 100 * base$esc_4 / base$qt_eleitores_perfil
base$pct_medio_inc   <- 100 * base$esc_5 / base$qt_eleitores_perfil
base$pct_medio_comp  <- 100 * base$esc_6 / base$qt_eleitores_perfil
base$pct_sup_inc     <- 100 * base$esc_7 / base$qt_eleitores_perfil
base$pct_sup_comp    <- 100 * base$esc_8 / base$qt_eleitores_perfil

# Gênero e idade
base$pct_mulheres <- 100 * base$gen_4 / base$qt_eleitores_perfil

base$pct_16_24 <- 100 * (base$idade_1600 + base$idade_1700 + base$idade_1800 +
                           base$idade_1900 + base$idade_2000 + base$idade_2124) / base$qt_eleitores_perfil
base$pct_25_34 <- 100 * (base$idade_2529 + base$idade_3034) / base$qt_eleitores_perfil
base$pct_35_44 <- 100 * (base$idade_3539 + base$idade_4044) / base$qt_eleitores_perfil
base$pct_45_59 <- 100 * (base$idade_4549 + base$idade_5054 + base$idade_5559) / base$qt_eleitores_perfil
base$pct_60_mais <- 100 * (base$idade_6064 + base$idade_6569 + base$idade_7074 +
                             base$idade_7579 + base$idade_8084 + base$idade_8589 +
                             base$idade_9094 + base$idade_9599 + base$idade_9999) / base$qt_eleitores_perfil

# Coordenadas: -1 ou fora do Brasil vira NA
base$latitude  <- base$NR_LATITUDE
base$longitude <- base$NR_LONGITUDE
base$latitude[which(base$latitude == -1 | base$latitude > 6 | base$latitude < -34)] <- NA
base$longitude[which(base$longitude == -1 | base$longitude > -32 | base$longitude < -74)] <- NA
invalida <- is.na(base$latitude) | is.na(base$longitude)
base$latitude[invalida]  <- NA
base$longitude[invalida] <- NA

# Conferências
nrow(base)
summary(base$pct_lula_t2)
sum(is.na(base$pct_lula_t2)); sum(is.na(base$pct_lula_t1))
sum(invalida)
summary(base$pct_16_24 + base$pct_25_34 + base$pct_35_44 + base$pct_45_59 + base$pct_60_mais)
table(base$DS_TIPO_SECAO_AGREGADA, useNA = "ifany")

# Renda local e tamanho da seção
base$log_renda     <- log(base$renda_media)
base$renda_mil     <- base$renda_media / 1000   # só para comparar nível x log (teste C da seção 9)
base$log_eleitores <- log(base$qt_eleitores_perfil)

# Região e chaves do município e do local de votação
base$nordeste  <- as.integer(base$SG_UF %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE"))
base$regiao    <- ifelse(base$nordeste == 1, "Nordeste", "Demais regiões")
base$municipio <- paste(base$SG_UF, base$CD_MUNICIPIO, sep = "_")
base$local     <- paste(base$SG_UF, base$CD_MUNICIPIO, base$NR_ZONA, base$NR_LOCAL_VOTACAO, sep = "_")

# Seções que receberam eleitores de seções agregadas no 2º turno
# (os votos delas incluem eleitores que não estão no seu perfil)
locais <- as.data.frame(ler_zip_tse(zip_loc, select = c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA",
                                                        "NR_SECAO", "NR_SECAO_PRINCIPAL")))
locais <- subset(locais, NR_TURNO == 2 & !is.na(NR_SECAO_PRINCIPAL) & NR_SECAO_PRINCIPAL > 0 &
                   NR_SECAO_PRINCIPAL != NR_SECAO)
lista_principais_t2 <- unique(paste(locais$SG_UF, locais$CD_MUNICIPIO, locais$NR_ZONA,
                                    locais$NR_SECAO_PRINCIPAL, sep = "_"))
base$recebeu_agregada_t2 <- paste(base$SG_UF, base$CD_MUNICIPIO, base$NR_ZONA, base$NR_SECAO, sep = "_") %in%
  lista_principais_t2
rm(locais)

# Critérios de amostra
# Elegível: tem local de votação, não recebeu agregadas, tem eleitores e votos válidos.
# Amostra final: elegível e com renda do setor.
base$elegivel   <- base$no_local & !base$recebeu_agregada_t2 & base$qt_eleitores_perfil > 0 & !is.na(base$pct_lula_t2)
base$na_amostra <- base$elegivel & is.finite(base$log_renda)

# Perdas de observações em cada etapa e por região
perdas <- data.frame(
  etapa  = c("Com votos no 2º turno e perfil (Brasil)", "Com local de votação",
             "Elegíveis (sem seções que receberam agregadas)", "Amostra final (com renda do setor)"),
  secoes = c(nrow(base), sum(base$no_local), sum(base$elegivel), sum(base$na_amostra)))
perdas$pct <- round(100 * perdas$secoes / nrow(base), 1)
perdas
aggregate(cbind(elegivel, na_amostra) ~ regiao, data = base, FUN = sum)

# As seções elegíveis que ficam são parecidas com as que saem por falta de renda?
aggregate(cbind(pct_lula_t2, pct_sup_comp, pct_analfabeto, nordeste) ~ na_amostra,
          data = subset(base, elegivel), FUN = mean)

# Daqui em diante a base fica só com a amostra final (usada em todas as regressões)
base <- subset(base, na_amostra)
base$SG_UF <- relevel(factor(base$SG_UF), ref = "SP")
nrow(base); length(unique(base$municipio)); length(unique(base$local))

# Cada local deve ter um só nome e uma só coordenada (tudo 1)
conf_nome <- aggregate(NM_LOCAL_VOTACAO ~ local, data = base, FUN = function(v) length(unique(v)))
conf_lat  <- aggregate(latitude ~ local,         data = base, FUN = function(v) length(unique(v)))
conf_lon  <- aggregate(longitude ~ local,        data = base, FUN = function(v) length(unique(v)))
table(conf_nome$NM_LOCAL_VOTACAO)
table(conf_lat$latitude)
table(conf_lon$longitude)

# Interações escolaridade x renda (renda centrada na média), usadas só no teste B
base$sup_x_renda   <- base$pct_sup_comp   * (base$log_renda - mean(base$log_renda))
base$analf_x_renda <- base$pct_analfabeto * (base$log_renda - mean(base$log_renda))

saveRDS(base, file.path(dir_out, "base_analise.rds"))

# Efeitos fixos de município (ou de local) = subtrair de cada variável a média do grupo.
# A base não é copiada: centrar() devolve a versão centralizada na hora, dentro do lm().
# As colunas mantêm o mesmo nome, para as tabelas alinharem os modelos.
vars_c <- c("pct_lula_t2", esc, "pct_esc_nao_inf", "pct_mulheres", "pct_25_34", "pct_35_44",
            "pct_45_59", "pct_60_mais", "log_eleitores", "log_renda", "renda_mil",
            "sup_x_renda", "analf_x_renda", "latitude", "longitude")
centrar <- function(d, grupo) {
  for (v in vars_c) d[[v]] <- d[[v]] - ave(d[[v]], d[[grupo]])
  # Interações com o Nordeste (constante no município, então o produto já sai centralizado)
  d$sup_x_ne   <- d$pct_sup_comp   * d$nordeste
  d$analf_x_ne <- d$pct_analfabeto * d$nordeste
  d
}

# =============================================================================
# 8. DESCRITIVA
# =============================================================================
vars <- c("pct_lula_t2", "pct_analfabeto", "pct_le_escreve", "pct_fund_inc",
          "pct_fund_comp", "pct_medio_inc", "pct_medio_comp", "pct_sup_inc",
          "pct_sup_comp", "pct_esc_nao_inf", "pct_mulheres", "pct_16_24",
          "pct_25_34", "pct_35_44", "pct_45_59", "pct_60_mais",
          "qt_eleitores_perfil", "renda_media")

tabela_desc <- data.frame(
  Media   = sapply(base[vars], mean),
  DP      = sapply(base[vars], sd),
  Mediana = sapply(base[vars], median),
  Min     = sapply(base[vars], min),
  Max     = sapply(base[vars], max)
)
colSums(is.na(base[vars]))
tabela_desc <- round(tabela_desc, 2)

rownames(tabela_desc) <- c(
  "% votos em Lula (2º turno)", "% analfabetos", "% lê e escreve",
  "% fundamental incompleto", "% fundamental completo", "% médio incompleto",
  "% médio completo", "% superior incompleto", "% superior completo",
  "% escolaridade não informada", "% mulheres", "% 16 a 24 anos",
  "% 25 a 34 anos", "% 35 a 44 anos", "% 45 a 59 anos", "% 60 anos ou mais",
  "Nº de eleitores da seção", "Renda média do responsável no setor (R$)"
)
tabela_desc
write.csv2(tabela_desc, "tabela_descritiva.csv")

# Média simples x ponderada pelo número de eleitores
weighted.mean(base$pct_lula_t2, base$qt_eleitores_perfil)
weighted.mean(base$pct_sup_comp, base$qt_eleitores_perfil)

# Matriz de correlação
round(cor(base[vars]), 2)

# Histograma
g_hist <- ggplot(base, aes(x = pct_lula_t2)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(x = "% de votos válidos em Lula (2º turno)", y = "Número de seções") +
  theme_minimal()
g_hist
ggsave("fig_histograma.png", g_hist, width = 7, height = 4, dpi = 300, bg = "white")

# Binned scatter: faixas de 5% calculadas dentro de cada região
base$pct_baixa <- base$pct_analfabeto + base$pct_le_escreve   # só para o gráfico

faixa_regiao <- function(x) {
  as.integer(cut(x, breaks = unique(quantile(x, probs = seq(0, 1, by = 0.05))), include.lowest = TRUE))
}
base$faixa_sup   <- ave(base$pct_sup_comp, base$regiao, FUN = faixa_regiao)
base$faixa_baixa <- ave(base$pct_baixa,    base$regiao, FUN = faixa_regiao)

tab_sup   <- aggregate(cbind(pct_sup_comp, pct_lula_t2) ~ faixa_sup + regiao, data = base, FUN = mean)
tab_baixa <- aggregate(cbind(pct_baixa, pct_lula_t2) ~ faixa_baixa + regiao, data = base, FUN = mean)

g_bin1 <- ggplot(tab_sup, aes(x = pct_sup_comp, y = pct_lula_t2, color = regiao)) +
  geom_point() +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(x = "% superior completo", y = "% de votos válidos em Lula (2º turno)", color = NULL,
       title = "A) Superior completo") +
  ylim(0, 100) + theme_minimal()

g_bin2 <- ggplot(tab_baixa, aes(x = pct_baixa, y = pct_lula_t2, color = regiao)) +
  geom_point() +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(x = "% baixíssima escolaridade", y = "% de votos válidos em Lula (2º turno)", color = NULL,
       title = "B) Baixíssima escolaridade") +
  ylim(0, 100) + theme_minimal()

g_bin <- g_bin1 + g_bin2 + plot_layout(guides = "collect") &
  theme(legend.position = "bottom", plot.title = element_text(size = 11))
g_bin
ggsave("fig_binscatter.png", g_bin, width = 10, height = 4.5, dpi = 300, bg = "white")

# Renda x escolaridade
base$faixa_renda <- ave(base$log_renda, base$regiao, FUN = faixa_regiao)
tab_renda <- aggregate(cbind(log_renda, pct_sup_comp) ~ faixa_renda + regiao, data = base, FUN = mean)

g_renda <- ggplot(tab_renda, aes(x = log_renda, y = pct_sup_comp, color = regiao)) +
  geom_point() +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  labs(x = "log(renda local)", y = "% superior completo", color = NULL,
       title = "Renda x superior completo") +
  theme_minimal()
g_renda
ggsave("fig_renda_esc.png", g_renda, width = 7, height = 4.5, dpi = 300, bg = "white")

# Mapas (amostra de 100 mil seções para o gráfico)
set.seed(123)
amostra_mapa <- sample(nrow(base), 100000)

g_mapa1 <- ggplot(base[amostra_mapa, ], aes(x = longitude, y = latitude, color = pct_lula_t2)) +
  geom_point(size = 0.3) +
  scale_color_viridis_c(option = "plasma") +
  coord_quickmap() +
  labs(color = "% Lula", title = "A) % de votos em Lula") +
  theme_void()

g_mapa2 <- ggplot(base[amostra_mapa, ], aes(x = longitude, y = latitude, color = pct_sup_comp)) +
  geom_point(size = 0.3) +
  scale_color_viridis_c(option = "viridis") +
  coord_quickmap() +
  labs(color = "% superior", title = "B) % com superior completo") +
  theme_void()

g_mapa3 <- ggplot(base[amostra_mapa, ], aes(x = longitude, y = latitude, color = log_renda)) +
  geom_point(size = 0.3) +
  scale_color_viridis_c(option = "mako") +
  coord_quickmap() +
  labs(color = "log(renda)", title = "C) Log da renda média") +
  theme_void()

g_mapas <- g_mapa1 + g_mapa2 + g_mapa3 +
  plot_annotation(caption = "Amostra aleatória de 100 mil seções.")
g_mapas
ggsave("fig_mapas.png", g_mapas, width = 15, height = 5, dpi = 300, bg = "white")

# =============================================================================
# 9. REGRESSÕES E TESTES (M1 a M10)
# Variável dependente: % dos votos válidos em Lula no 2º turno.
# Escolaridade em % de eleitores de cada nível (referência: médio completo); idade com
# referência 16 a 24 anos. M1 a M8 e M10 usam a mesma amostra; M9 usa a mesma amostra sem o Nordeste.
#
# Inferência: erro-padrão agrupado por município (vcovCL, tipo HC1), pois seções do mesmo
# município (e do mesmo local) dividem características não observadas e a mesma renda do setor.
# Ajuste de graus de liberdade do vcovCL: G/(G-1) x (n-1)/(n-k), com G = nº de municípios e
# k = nº de coeficientes estimados. Nos modelos centralizados (M6 a M10), os efeitos fixos não
# entram em k; como municípios e locais estão contidos nos grupos do agrupamento (município),
# esse é o ajuste usual (o mesmo do fixest). Conferência com o fixest logo após o M7.
# Nos modelos centralizados, o summary() dá coeficientes corretos e R² within, mas seus
# erros-padrão, t e F ignoram os efeitos fixos absorvidos: a inferência vem do coeftest/linearHypothesis.
# =============================================================================
hip_esc   <- paste(esc, "= 0")                                           # H0: todos os níveis de escolaridade = 0
hip_idade <- c("pct_25_34 = 0", "pct_35_44 = 0", "pct_45_59 = 0", "pct_60_mais = 0")   # H0: todas as faixas etárias = 0

# M1: só escolaridade (associação bruta, sem controles)
m1 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf,
         data = base)
summary(m1)
# Erros-padrão agrupados por município
V1 <- vcovCL(m1, cluster = ~municipio)
coeftest(m1, vcov = V1)
# Breusch-Pagan (H0: variância do erro constante, não relacionada aos regressores)
bptest(m1)
# VIF: quanto cada variável é explicada pelas outras (acima de 10 indica colinearidade forte)
vif(m1)
# Teste F (H0: escolaridade conjuntamente igual a zero)
linearHypothesis(m1, hip_esc, vcov. = V1)

# M2: + gênero, faixas etárias e tamanho da seção (composição demográfica)
m2 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores,
         data = base)
summary(m2)
V2 <- vcovCL(m2, cluster = ~municipio)
coeftest(m2, vcov = V2)
bptest(m2)
vif(m2)
# Teste F (H0: faixas etárias conjuntamente iguais a zero)
linearHypothesis(m2, hip_idade, vcov. = V2)

# M3: + log da renda local (renda média do responsável no setor do local de votação)
# Log: a renda é assimétrica e a associação esperada é proporcional;
# coeficiente / 100 = variação em p.p. de % Lula associada a +1% de renda
m3 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           log_renda,
         data = base)
summary(m3)
V3 <- vcovCL(m3, cluster = ~municipio)
coeftest(m3, vcov = V3)
bptest(m3)
# VIF: a renda é muito correlacionada com a escolaridade?
vif(m3)

# M4: + latitude e longitude (tendência geográfica linear no país)
m4 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           log_renda + latitude + longitude,
         data = base)
summary(m4)
V4 <- vcovCL(m4, cluster = ~municipio)
coeftest(m4, vcov = V4)
bptest(m4)
vif(m4)
# Teste F (H0: latitude e longitude conjuntamente iguais a zero)
linearHypothesis(m4, c("latitude = 0", "longitude = 0"), vcov. = V4)

# M5: + efeitos fixos de UF (dummies; referência SP): diferenças entre estados
m5 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           log_renda + latitude + longitude + SG_UF,
         data = base)
summary(m5)
V5 <- vcovCL(m5, cluster = ~municipio)
coeftest(m5, vcov = V5)
bptest(m5)
# GVIF: para o bloco de UF, comparar GVIF^(1/(2*gl)) ao quadrado com o VIF das demais variáveis;
# latitude e longitude devem ter GVIF alto aqui, pois as UFs já explicam boa parte da posição
vif(m5)
# Teste F (H0: dummies de UF conjuntamente iguais a zero)
linearHypothesis(m5, matchCoefs(m5, "SG_UF"), vcov. = V5)
# Teste F (H0: escolaridade conjuntamente igual a zero)
linearHypothesis(m5, hip_esc, vcov. = V5)

# M6: efeitos fixos de município, sem renda (compara seções do mesmo município).
# Dados centralizados no município, sem intercepto. UF, latitude e longitude entre municípios
# ficam absorvidas.
m6 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores - 1,
         data = centrar(base, "municipio"))
summary(m6)   # coeficientes e R² within corretos; inferência: usar a linha abaixo
V6 <- vcovCL(m6, cluster = ~municipio)
coeftest(m6, vcov = V6)
# VIF dentro do município: com variáveis centralizadas (média zero), o VIF mede a colinearidade
# que sobra dentro dos municípios; o aviso "No intercept" pode ser ignorado
vif(m6)
# Teste F (H0: escolaridade conjuntamente igual a zero)
linearHypothesis(m6, hip_esc, vcov. = V6)

# M7: MODELO PRINCIPAL - efeitos fixos de município + renda local.
# Escolaridade, gênero, idade e tamanho da seção, comparando seções do mesmo município que
# diferem na renda do entorno do local de votação. Coordenadas: ver teste A.
m7 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           log_renda - 1,
         data = centrar(base, "municipio"))
summary(m7)
V7 <- vcovCL(m7, cluster = ~municipio)
coeftest(m7, vcov = V7)
# VIF dentro do município: a renda local fica muito colinear com a escolaridade?
vif(m7)
# Teste F (H0: escolaridade conjuntamente igual a zero)
linearHypothesis(m7, hip_esc, vcov. = V7)
# Teste F (H0: faixas etárias conjuntamente iguais a zero)
linearHypothesis(m7, hip_idade, vcov. = V7)
# Breusch-Pagan adaptado ao modelo centralizado: os resíduos do M7 são regredidos nos regressores
# COM intercepto (o bptest(m7) direto usaria uma regressão auxiliar sem intercepto, o que não é válido).
# H0: variância constante. Com ~470 mil seções, o teste tende a rejeitar mesmo com
# heterocedasticidade pequena; serve só como diagnóstico, pois a inferência já é agrupada (robusta).
bptest(m7, varformula = ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
         pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
         pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores + log_renda,
       data = centrar(base, "municipio"))

# Conferência dos graus de liberdade: o mesmo M7 estimado pelo fixest::feols, que absorve os
# efeitos fixos internamente. Coeficientes devem ser idênticos e erros-padrão praticamente iguais;
# se forem, a estimação centralizada com vcovCL está correta e não há ganho em trocar de método.
# install.packages("fixest")
m7_fixest <- fixest::feols(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
                             pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
                             pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
                             log_renda | municipio,
                           data = base, cluster = ~municipio)
round(cbind(coef_lm = coef(m7)[esc], coef_fixest = coef(m7_fixest)[esc],
            ep_lm = sqrt(diag(V7))[esc], ep_fixest = fixest::se(m7_fixest)[esc]), 4)
rm(m7_fixest)

# RESET não é aplicado: com efeitos fixos absorvidos, as potências dos valores ajustados teriam
# de ser recentralizadas no município, e a versão usual do teste não é robusta ao agrupamento.
# A forma funcional é avaliada de modo mais direto pelos testes B (interações) e C (nível x log).

# Testes de especificação do modelo principal ----------------------------------
# Teste A: latitude e longitude centralizadas no município (tendência espacial dentro da cidade).
# H0: as duas = 0. A decisão de incluir não deve depender só do p-valor: comparar também quanto
# os coeficientes de escolaridade mudam e se uma tendência linear norte-sul/leste-oeste, igual
# para todos os municípios, tem interpretação no trabalho.
tA <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           log_renda + latitude + longitude - 1,
         data = centrar(base, "municipio"))
VA <- vcovCL(tA, cluster = ~municipio)
linearHypothesis(tA, c("latitude = 0", "longitude = 0"), vcov. = VA)
round(cbind(M7 = coef(m7)[esc], M7_com_coord = coef(tA)[esc], dif = coef(tA)[esc] - coef(m7)[esc]), 3)

# Teste B: interações renda x superior completo e renda x analfabetismo.
# Pergunta: a associação dos extremos de escolaridade com o voto muda conforme a renda do entorno?
# H0 (F): as duas interações = 0. Os coeficientes dos níveis passam a valer para a renda média.
tB <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           log_renda + sup_x_renda + analf_x_renda - 1,
         data = centrar(base, "municipio"))
VB <- vcovCL(tB, cluster = ~municipio)
coeftest(tB, vcov = VB)[c("sup_x_renda", "analf_x_renda"), ]
linearHypothesis(tB, c("sup_x_renda = 0", "analf_x_renda = 0"), vcov. = VB)

# Teste C: renda em nível (R$ mil) x em log, na mesma amostra e com os mesmos controles.
# Critério: maior R² within (os dois modelos têm o mesmo número de regressores)
tC <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           renda_mil - 1,
         data = centrar(base, "municipio"))
c(R2_within_log = summary(m7)$r.squared, R2_within_nivel = summary(tC)$r.squared)
rm(tA, tB, tC, VA, VB); invisible(gc())

# M8: M7 + interações escolaridade x Nordeste (superior completo e analfabeto).
# A dummy do Nordeste é absorvida pelos efeitos fixos de município; as interações não.
# Coeficiente do nível = associação nas demais regiões; nível + interação = associação no Nordeste.
m8 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
           pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
           pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
           log_renda + sup_x_ne + analf_x_ne - 1,
         data = centrar(base, "municipio"))
summary(m8)
V8 <- vcovCL(m8, cluster = ~municipio)
coeftest(m8, vcov = V8)
# Teste F (H0: as duas interações = 0, isto é, mesma associação no Nordeste e nas demais regiões)
linearHypothesis(m8, c("sup_x_ne = 0", "analf_x_ne = 0"), vcov. = V8)
# Testes das somas (H0: a associação no Nordeste é zero)
linearHypothesis(m8, "pct_analfabeto + analf_x_ne = 0", vcov. = V8)
linearHypothesis(m8, "pct_sup_comp + sup_x_ne = 0",     vcov. = V8)
# Associação no Nordeste e seu erro-padrão: Var(a + b) = Var(a) + Var(b) + 2 Cov(a, b)
ne_analf    <- coef(m8)[["pct_analfabeto"]] + coef(m8)[["analf_x_ne"]]
ne_analf_ep <- sqrt(V8["pct_analfabeto", "pct_analfabeto"] + V8["analf_x_ne", "analf_x_ne"] + 2 * V8["pct_analfabeto", "analf_x_ne"])
ne_sup      <- coef(m8)[["pct_sup_comp"]] + coef(m8)[["sup_x_ne"]]
ne_sup_ep   <- sqrt(V8["pct_sup_comp", "pct_sup_comp"] + V8["sup_x_ne", "sup_x_ne"] + 2 * V8["pct_sup_comp", "sup_x_ne"])
round(c(analf_NE = ne_analf, ep = ne_analf_ep, sup_NE = ne_sup, ep = ne_sup_ep), 3)
# Sem VIF no M8: interações são colineares com os termos principais por construção,
# então o VIF fica alto sem indicar problema

# M9: Brasil sem o Nordeste, efeitos fixos de município, sem renda (M9a) e com renda (M9b)
# (mesma amostra nos dois; complementa o M8)
m9a <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
            pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
            pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores - 1,
          data = subset(centrar(base, "municipio"), nordeste == 0))
summary(m9a)
V9a <- vcovCL(m9a, cluster = ~municipio)
coeftest(m9a, vcov = V9a)
# Teste F (H0: escolaridade conjuntamente igual a zero)
linearHypothesis(m9a, hip_esc, vcov. = V9a)

m9b <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
            pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
            pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores +
            log_renda - 1,
          data = subset(centrar(base, "municipio"), nordeste == 0))
summary(m9b)
V9b <- vcovCL(m9b, cluster = ~municipio)
coeftest(m9b, vcov = V9b)
linearHypothesis(m9b, hip_esc, vcov. = V9b)

# M10: efeitos fixos de local de votação (compara seções da mesma escola).
# A renda do setor e as coordenadas são iguais para todas as seções do local: ficam absorvidas.
m10 <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
            pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
            pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores - 1,
          data = centrar(base, "local"))
summary(m10)   # R² within do local
V10 <- vcovCL(m10, cluster = ~municipio)
coeftest(m10, vcov = V10)
# VIF dentro do local: quanto de variação de escolaridade sobra entre seções da mesma escola
vif(m10)
linearHypothesis(m10, hip_esc, vcov. = V10)

# Verificação: mesma amostra em M1 a M8 e M10; M9 = amostra sem o Nordeste
sapply(list(M1 = m1, M2 = m2, M3 = m3, M4 = m4, M5 = m5, M6 = m6, M7 = m7, M8 = m8,
            M9a = m9a, M9b = m9b, M10 = m10), nobs)

# Coeficientes de escolaridade lado a lado
round(cbind(M1 = coef(m1)[esc], M2 = coef(m2)[esc], M3 = coef(m3)[esc], M5 = coef(m5)[esc],
            M6 = coef(m6)[esc], M7 = coef(m7)[esc], M9b = coef(m9b)[esc], M10 = coef(m10)[esc]), 3)

# Robustez: em vez de excluir as seções principais que receberam eleitores de seções agregadas,
# soma ao perfil delas o perfil das seções agregadas (que votaram na seção principal).
# M6 com todas as seções e M7 com as que têm renda; comparar com m6 e m7 acima.
rob <- as.data.frame(readRDS(file.path(dir_out, "base_secao_2022.rds")))
rob <- subset(rob, SG_UF != "ZZ" & no_perfil)
agreg <- as.data.frame(ler_zip_tse(zip_loc, select = c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA",
                                                       "NR_SECAO", "NR_SECAO_PRINCIPAL")))
agreg <- subset(agreg, NR_TURNO == 2 & !is.na(NR_SECAO_PRINCIPAL) & NR_SECAO_PRINCIPAL > 0 &
                  NR_SECAO_PRINCIPAL != NR_SECAO)
agreg <- unique(agreg[, c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "NR_SECAO_PRINCIPAL")])
cont <- grep("^(esc_|gen_|idade_)|^qt_eleitores_perfil$", names(rob), value = TRUE)
agreg <- merge(agreg, rob[, c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", cont)])
soma <- aggregate(agreg[, cont], by = list(SG_UF = agreg$SG_UF, CD_MUNICIPIO = agreg$CD_MUNICIPIO,
                                           NR_ZONA = agreg$NR_ZONA, NR_SECAO = agreg$NR_SECAO_PRINCIPAL), FUN = sum)
names(soma)[-(1:4)] <- paste0("ag_", cont)
rob <- merge(rob, soma, all.x = TRUE)
for (v in cont) rob[[v]] <- rob[[v]] + ifelse(is.na(rob[[paste0("ag_", v)]]), 0, rob[[paste0("ag_", v)]])

rob <- subset(rob, na_votacao_t2 & no_local & qt_eleitores_perfil > 0)
rob$pct_lula_t2 <- 100 * rob$t2_votos_13 / (rob$t2_votos_13 + rob$t2_votos_22)
rob <- subset(rob, !is.na(pct_lula_t2))
rob$pct_esc_nao_inf <- 100 * rob$esc_0 / rob$qt_eleitores_perfil
rob$pct_analfabeto  <- 100 * rob$esc_1 / rob$qt_eleitores_perfil
rob$pct_le_escreve  <- 100 * rob$esc_2 / rob$qt_eleitores_perfil
rob$pct_fund_inc    <- 100 * rob$esc_3 / rob$qt_eleitores_perfil
rob$pct_fund_comp   <- 100 * rob$esc_4 / rob$qt_eleitores_perfil
rob$pct_medio_inc   <- 100 * rob$esc_5 / rob$qt_eleitores_perfil
rob$pct_sup_inc     <- 100 * rob$esc_7 / rob$qt_eleitores_perfil
rob$pct_sup_comp    <- 100 * rob$esc_8 / rob$qt_eleitores_perfil
rob$pct_mulheres <- 100 * rob$gen_4 / rob$qt_eleitores_perfil
rob$pct_25_34 <- 100 * (rob$idade_2529 + rob$idade_3034) / rob$qt_eleitores_perfil
rob$pct_35_44 <- 100 * (rob$idade_3539 + rob$idade_4044) / rob$qt_eleitores_perfil
rob$pct_45_59 <- 100 * (rob$idade_4549 + rob$idade_5054 + rob$idade_5559) / rob$qt_eleitores_perfil
rob$pct_60_mais <- 100 * (rob$idade_6064 + rob$idade_6569 + rob$idade_7074 + rob$idade_7579 + rob$idade_8084 +
                            rob$idade_8589 + rob$idade_9094 + rob$idade_9599 + rob$idade_9999) / rob$qt_eleitores_perfil
rob$log_eleitores <- log(rob$qt_eleitores_perfil)
rob$log_renda <- log(rob$renda_media)
rob$municipio <- paste(rob$SG_UF, rob$CD_MUNICIPIO, sep = "_")

vars_r <- c("pct_lula_t2", esc, "pct_esc_nao_inf", "pct_mulheres", "pct_25_34", "pct_35_44",
            "pct_45_59", "pct_60_mais", "log_eleitores", "log_renda")
rob_m6 <- rob
rob_m7 <- subset(rob, is.finite(log_renda))
for (v in vars_r) {
  rob_m6[[v]] <- rob_m6[[v]] - ave(rob_m6[[v]], rob_m6$municipio)
  rob_m7[[v]] <- rob_m7[[v]] - ave(rob_m7[[v]], rob_m7$municipio)
}
m6_rob <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
               pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
               pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores - 1,
             data = rob_m6)
m7_rob <- lm(pct_lula_t2 ~ pct_analfabeto + pct_le_escreve + pct_fund_inc + pct_fund_comp +
               pct_medio_inc + pct_sup_inc + pct_sup_comp + pct_esc_nao_inf +
               pct_mulheres + pct_25_34 + pct_35_44 + pct_45_59 + pct_60_mais + log_eleitores + log_renda - 1,
             data = rob_m7)
coeftest(m6_rob, vcov = vcovCL(m6_rob, cluster = ~municipio))
coeftest(m7_rob, vcov = vcovCL(m7_rob, cluster = ~municipio))
sapply(list(M6_rob = m6_rob, M7_rob = m7_rob), nobs)
round(cbind(M6 = coef(m6)[esc], M6_rob = coef(m6_rob)[esc], M7 = coef(m7)[esc], M7_rob = coef(m7_rob)[esc]), 3)


# =============================================================================
# 10. TABELAS E GRÁFICO DE COEFICIENTES
# Coeficientes, erros-padrão e estrelas vêm todos da inferência agrupada por município
# (se = erros-padrão do vcovCL; p = p-valores do coeftest com o mesmo vcovCL).
# =============================================================================
# Só escolaridade e renda aparecem nas tabelas; os demais controles são indicados nas linhas de baixo
manter   <- paste0("^", c(esc, "log_renda", "sup_x_ne", "analf_x_ne"), "$")
rot_esc  <- c("% analfabeto", "% lê e escreve", "% fundamental incompleto", "% fundamental completo",
              "% médio incompleto", "% superior incompleto", "% superior completo")

# Tabela 2: sequência de modelos M1 a M7
se_t2 <- list(sqrt(diag(V1)), sqrt(diag(V2)), sqrt(diag(V3)), sqrt(diag(V4)), sqrt(diag(V5)), sqrt(diag(V6)), sqrt(diag(V7)))
p_t2  <- list(coeftest(m1, vcov = V1)[, 4], coeftest(m2, vcov = V2)[, 4], coeftest(m3, vcov = V3)[, 4],
              coeftest(m4, vcov = V4)[, 4], coeftest(m5, vcov = V5)[, 4], coeftest(m6, vcov = V6)[, 4],
              coeftest(m7, vcov = V7)[, 4])
r2_t2 <- formatC(c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared, summary(m4)$r.squared,
                   summary(m5)$r.squared, summary(m6)$r.squared, summary(m7)$r.squared), format = "f", digits = 3)
stargazer(m1, m2, m3, m4, m5, m6, m7, se = se_t2, p = p_t2, t.auto = FALSE, p.auto = FALSE,
          keep = manter, covariate.labels = c(rot_esc, "log(renda local)"),
          column.labels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7 (principal)"), model.numbers = FALSE,
          dep.var.labels = "% de votos válidos em Lula (2º turno)",
          add.lines = list(c("Controles demográficos", "Não", "Sim", "Sim", "Sim", "Sim", "Sim", "Sim"),
                           c("Latitude e longitude",   "Não", "Não", "Não", "Sim", "Sim", "–", "–"),
                           c("Efeitos fixos",          "–", "–", "–", "–", "UF", "Município", "Município"),
                           c("R² (MQO)",               r2_t2[1:5], "–", "–"),
                           c("R² within (município)",  "–", "–", "–", "–", "–", r2_t2[6:7])),
          keep.stat = "n", digits = 3,
          notes = c("Erros-padrão agrupados por município entre parênteses.",
                    "Referência: médio completo. Controles demográficos: % mulheres, faixas etárias",
                    "(ref. 16 a 24 anos), log(nº de eleitores) e % escolaridade não informada."),
          notes.append = TRUE, type = "text")
stargazer(m1, m2, m3, m4, m5, m6, m7, se = se_t2, p = p_t2, t.auto = FALSE, p.auto = FALSE,
          keep = manter, covariate.labels = c(rot_esc, "log(renda local)"),
          column.labels = c("M1", "M2", "M3", "M4", "M5", "M6", "M7 (principal)"), model.numbers = FALSE,
          dep.var.labels = "% de votos válidos em Lula (2º turno)",
          add.lines = list(c("Controles demográficos", "Não", "Sim", "Sim", "Sim", "Sim", "Sim", "Sim"),
                           c("Latitude e longitude",   "Não", "Não", "Não", "Sim", "Sim", "–", "–"),
                           c("Efeitos fixos",          "–", "–", "–", "–", "UF", "Município", "Município"),
                           c("R² (MQO)",               r2_t2[1:5], "–", "–"),
                           c("R² within (município)",  "–", "–", "–", "–", "–", r2_t2[6:7])),
          keep.stat = "n", digits = 3,
          notes = c("Erros-padrão agrupados por município entre parênteses.",
                    "Referência: médio completo. Controles demográficos: % mulheres, faixas etárias",
                    "(ref. 16 a 24 anos), log(nº de eleitores) e % escolaridade não informada."),
          notes.append = TRUE, type = "html", out = "tabela_2_modelos.html")

# Tabela 3: Nordeste e efeitos fixos de local (M7, M8, M9a, M9b, M10)
se_t3 <- list(sqrt(diag(V7)), sqrt(diag(V8)), sqrt(diag(V9a)), sqrt(diag(V9b)), sqrt(diag(V10)))
p_t3  <- list(coeftest(m7, vcov = V7)[, 4], coeftest(m8, vcov = V8)[, 4], coeftest(m9a, vcov = V9a)[, 4],
              coeftest(m9b, vcov = V9b)[, 4], coeftest(m10, vcov = V10)[, 4])
r2_t3 <- formatC(c(summary(m7)$r.squared, summary(m8)$r.squared, summary(m9a)$r.squared,
                   summary(m9b)$r.squared, summary(m10)$r.squared), format = "f", digits = 3)
soma_ne <- c(sprintf("%.3f (%.3f)", ne_analf, ne_analf_ep), sprintf("%.3f (%.3f)", ne_sup, ne_sup_ep))
stargazer(m7, m8, m9a, m9b, m10, se = se_t3, p = p_t3, t.auto = FALSE, p.auto = FALSE,
          keep = manter,
          covariate.labels = c(rot_esc, "log(renda local)", "% superior completo × Nordeste", "% analfabeto × Nordeste"),
          column.labels = c("M7 (principal)", "M8", "M9a", "M9b", "M10"), model.numbers = FALSE,
          dep.var.labels = "% de votos válidos em Lula (2º turno)",
          add.lines = list(c("Analfabeto no Nordeste (nível + interação)", "", soma_ne[1], "", "", ""),
                           c("Superior no Nordeste (nível + interação)",   "", soma_ne[2], "", "", ""),
                           c("Controles demográficos", "Sim", "Sim", "Sim", "Sim", "Sim"),
                           c("Efeitos fixos", "Município", "Município", "Município", "Município", "Local"),
                           c("Amostra", "Brasil", "Brasil", "Sem Nordeste", "Sem Nordeste", "Brasil"),
                           c("R² within", r2_t3)),
          keep.stat = "n", digits = 3,
          notes = c("Erros-padrão agrupados por município entre parênteses.",
                    "R² within do município (M7 a M9) ou do local de votação (M10)."),
          notes.append = TRUE, type = "text")
stargazer(m7, m8, m9a, m9b, m10, se = se_t3, p = p_t3, t.auto = FALSE, p.auto = FALSE,
          keep = manter,
          covariate.labels = c(rot_esc, "log(renda local)", "% superior completo × Nordeste", "% analfabeto × Nordeste"),
          column.labels = c("M7 (principal)", "M8", "M9a", "M9b", "M10"), model.numbers = FALSE,
          dep.var.labels = "% de votos válidos em Lula (2º turno)",
          add.lines = list(c("Analfabeto no Nordeste (nível + interação)", "", soma_ne[1], "", "", ""),
                           c("Superior no Nordeste (nível + interação)",   "", soma_ne[2], "", "", ""),
                           c("Controles demográficos", "Sim", "Sim", "Sim", "Sim", "Sim"),
                           c("Efeitos fixos", "Município", "Município", "Município", "Município", "Local"),
                           c("Amostra", "Brasil", "Brasil", "Sem Nordeste", "Sem Nordeste", "Brasil"),
                           c("R² within", r2_t3)),
          keep.stat = "n", digits = 3,
          notes = c("Erros-padrão agrupados por município entre parênteses.",
                    "R² within do município (M7 a M9) ou do local de votação (M10)."),
          notes.append = TRUE, type = "html", out = "tabela_3_nordeste_local.html")


# Gráfico de coeficientes: m1 x m3 x m5 x m7, intervalo de 95% com erro-padrão agrupado por município (igual às tabelas)
nomes <- c("Analfabeto", "Lê e escreve", "Fund. incompleto", "Fund. completo",
           "Médio incompleto", "Superior incompleto", "Superior completo")
rot_mod <- c("Sem controles (m1)", "+ Demog. e renda (m3)", "+ Geografia e UF (m5)", "Dentro do município (m7)")

r1 <- coeftest(m1, vcov = V1)
r3 <- coeftest(m3, vcov = V3)
r5 <- coeftest(m5, vcov = V5)
r7 <- coeftest(m7, vcov. = V7)

coefs <- data.frame(
  nivel  = factor(rep(nomes, 4), levels = nomes),
  modelo = factor(rep(rot_mod, each = 7), levels = rot_mod),
  est    = c(r1[esc, 1], r3[esc, 1], r5[esc, 1], r7[esc, 1]),
  ep     = c(r1[esc, 2], r3[esc, 2], r5[esc, 2], r7[esc, 2])
)
coefs$inf <- coefs$est - 1.96 * coefs$ep
coefs$sup <- coefs$est + 1.96 * coefs$ep

g_coef <- ggplot(coefs, aes(x = nivel, y = est, color = modelo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_pointrange(aes(ymin = inf, ymax = sup), size = 0.3,
                  position = position_dodge(width = 0.6)) +
  scale_color_manual(values = c("darkorange", "purple", "steelblue", "darkgreen")) +
  labs(x = NULL, y = "Coeficiente (ref.: médio completo)", color = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top")
g_coef
ggsave("fig_coeficientes.png", g_coef, width = 7, height = 4.5, dpi = 300, bg = "white")