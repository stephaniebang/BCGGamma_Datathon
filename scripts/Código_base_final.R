# Esse codigo tem como funcao preparar as bases fornecidas pelo BCGGamma para
# realizar a analise dos dados sobre os municipios do Brasil e suas escolas.
# As bases usadas nesse script estao originalmente localizadas no mesmo
# diretorio do script, com excecao das bases de docencia, que estao no
# diretorio ./docencia
# Dentre as bases usadas no script, a base escolas20170101.csv deve ser
# baixada do repositorio, nao deve ser usada a base fornecidade pelo BCGGamma,
# e tambem deve-se utilizar a base Base_Munic_Resumo.xlsx do repositorio



## Pacotes utilizados -----------------------------------------------------

library(readr)
library(readxl)
library(dplyr)



## Leitura das bases ------------------------------------------------------

path <- "./"

# Base do Ideb de municipio
ideb_munic <- read.csv(paste0(path, "ideb_municipios_anosfinais2005_2017.csv"),
                       na = "-")

# Base do Ideb de UF
ideb_uf <- read.csv(paste0(path, "ideb_uf_regioes_anosfinais2005_2017.csv"),
                    na = "-")

# Base de escola de 2017
escolas <- read.csv(paste0(path, "escolas20170101.csv"),
                    #na = "-",
                    na.strings=c(""),
                    encoding = "UTF-8")

# Lista com os nomes das bases de docencia de 2017
bases_docencia <- dir(paste0(path, "docencia/"), full.names = T)



## Preparacao das bases do Ideb -------------------------------------------

# Função que padroniza nome das UF
muda_uf <- function(x){
  if (x[1] == "Acre")             x[1] = "AC"
  if (x[1] == "Alagoas")          x[1] = "AL"
  if (x[1] == "Amapá")            x[1] = "AP"
  if (x[1] == "Amazonas")         x[1] = "AM"
  if (x[1] == "Bahia")            x[1] = "BA"
  if (x[1] == "Ceará")            x[1] = "CE"
  if (x[1] == "Distrito Federal") x[1] = "DF"
  if (x[1] == "Espírito Santo")   x[1] = "ES"
  if (x[1] == "Goiás")            x[1] = "GO"
  if (x[1] == "Maranhão")         x[1] = "MA"
  if (x[1] == "M. G. do Sul")     x[1] = "MS"
  if (x[1] == "Mato Grosso")      x[1] = "MT"
  if (x[1] == "Minas Gerais")     x[1] = "MG"
  if (x[1] == "Pará")             x[1] = "PA"
  if (x[1] == "Paraíba")          x[1] = "PB"
  if (x[1] == "Paraná")           x[1] = "PR"
  if (x[1] == "Pernambuco")       x[1] = "PE"
  if (x[1] == "Piauí")            x[1] = "PI"
  if (x[1] == "R. G. do Norte")   x[1] = "RN"
  if (x[1] == "R. G. do Sul")     x[1] = "RS"
  if (x[1] == "Rio de Janeiro")   x[1] = "RJ"
  if (x[1] == "Rondônia")         x[1] = "RO"
  if (x[1] == "Roraima")          x[1] = "RR"
  if (x[1] == "São Paulo")        x[1] = "SP"
  if (x[1] == "Santa Catarina")   x[1] = "SC"
  if (x[1] == "Sergipe")          x[1] = "SE"
  if (x[1] == "Tocantins")        x[1] = "TO"
  return(x[1])
}

ideb_uf$UF_REG <- apply(ideb_uf, 1, muda_uf)

# Funcao que busca a nota generica do estado caso falte do municipio
resposta_geral <- function(x) {
  x <- x %>% as.matrix()
  
  resp <- ideb_munic %>%
    filter(Rede == x[4], Cod_Municipio_Completo == x[2]) %>%
    select(Ideb2017) %>%
    as.numeric()
  
  if (resp %>% is.na() == T) {
    resp <- ideb_munic %>%
      filter(Rede == x[4], Cod_Municipio_Completo == x[2]) %>% 
      select(ProjecaoIdeb2017) %>%
      as.numeric()
  }
  
  if (resp %>% is.na() == T) {
    resp <- ideb_uf %>%
      filter(Rede == "Pública", UF_REG == x[1]) %>% 
      select(Ideb2017) %>%
      as.numeric()
  }
  
  return(resp)
}

ideb_munic$Nota <- apply(ideb_munic, 1, resposta_geral)

ideb <- ideb_munic %>% 
  group_by(Nome_Municipio, Cod_Municipio_Completo) %>% 
  summarise(Nota = mean(Nota))

# Funcao que categoriza a resposta para 1 se maior que a meta e 0 se nao
resposta_cat <- function(x) {
  if (x[3] >= 5) return(1)
  
  return(0)
}

ideb$Resposta <- apply(ideb, 1, resposta_cat)

rm(ideb_munic)
rm(ideb_uf)

# Descomentar para salvar a base com os dados extraidos das bases do Ideb
# saveRDS(ideb, "Base_Ideb.rds")



## Preparacao da base de escola -------------------------------------------

# Manipula as variaveis
escolas$in_laboratorio <- pmax(escolas$in_laboratorio_informatica,
                               escolas$in_laboratorio_ciencias,
                               na.rm = T)

escolas$in_patio <- pmax(escolas$in_patio_coberto,
                         escolas$in_patio_descoberto,
                         na.rm = T)

escolas$in_equip <- apply(escolas %>% select(in_equip_copiadora, in_equip_videocassete,
                                             in_equip_dvd, in_equip_tv,
                                             in_equip_parabolica, in_equip_retroprojetor,
                                             in_equip_impressora, in_equip_impressora_mult,
                                             in_equip_som, in_equip_multimidia,
                                             in_equip_fax, in_equip_foto),
                          1,
                          function(x) sum(x, na.rm = T))

escolas$num_equip <- apply(escolas %>% select(num_equip_copiadora, num_equip_videocassete,
                                              num_equip_dvd, num_equip_tv,
                                              num_equip_parabolica, num_equip_retroprojetor,
                                              num_equip_impressora, num_equip_impressora_mult,
                                              num_equip_som, num_equip_multimidia,
                                              num_equip_fax, num_equip_foto),
                           1,
                           function(x) sum(x, na.rm = T))

# Separa as variaveis de interesse
filtro <- escolas %>%
  filter(rede_publica_desc == "pública") %>% 
  group_by(cod_municipio) %>% 
  summarise(perc_laboratorio             = mean(in_laboratorio,             na.rm = T),
            perc_patio                   = mean(in_patio,                   na.rm = T),
            num_matriculas               = mean(num_matriculas,             na.rm = T),
            num_estudantes               = mean(num_estudantes,             na.rm = T),
            perc_agua_filtrada           = mean(in_agua_filtrada,           na.rm = T),
            perc_agua_inexistente        = mean(in_agua_inexistente,        na.rm = T), 
            perc_esgoto_inexistente      = mean(in_esgoto_inexistente,      na.rm = T),
            perc_energia_inexistente     = mean(in_energia_inexistente,     na.rm = T), 
            perc_sala_diretoria          = mean(in_sala_diretoria,          na.rm = T), 
            perc_sala_professor          = mean(in_sala_professor,          na.rm = T),
            perc_quadra_esportes         = mean(in_quadra_esportes,         na.rm = T),
            perc_cozinha                 = mean(in_quadra_esportes,         na.rm = T), 
            perc_biblioteca_sala_leitura = mean(in_biblioteca_sala_leitura, na.rm = T), 
            perc_parque_infantil         = mean(in_parque_infantil,         na.rm = T), 
            perc_bercario                = mean(in_bercario,                na.rm = T),
            perc_banheiro_dentro_predio  = mean(in_banheiro_dentro_predio,  na.rm = T), 
            perc_refeitorio              = mean(in_refeitorio,              na.rm = T), 
            perc_auditorio               = mean(in_auditorio,               na.rm = T),
            perc_alojam_aluno            = mean(in_alojam_aluno,            na.rm = T), 
            perc_area_verde              = mean(in_area_verde,              na.rm = T), 
            num_salas_existentes         = mean(num_salas_existentes,       na.rm = T),
            num_salas_utilizadas         = mean(num_salas_utilizadas,       na.rm = T), 
            perc_equip                   = mean(in_equip,                   na.rm = T), 
            num_equip                    = mean(num_equip,                  na.rm = T), 
            num_comp_aluno               = mean(num_comp_aluno,             na.rm = T),
            num_funcionarios             = mean(num_funcionarios,           na.rm = T), 
            perc_alimentacao             = mean(in_alimentacao,             na.rm = T), 
            num_professores              = mean(num_professores,            na.rm = T),
            perc_final_semana            = mean(in_final_semana,            na.rm = T), 
            tp_integral                  = mean(tp_integral,                na.rm = T)
           )

# Junta as bases
base_aux <- merge(ideb,
                  filtro,
                  by.x = "Cod_Municipio_Completo",
                  by.y = "cod_municipio")

munic <- read_excel(paste0(path, "Base_Munic_Resumo.xlsx"))
base_ideb_esc <- merge(base_aux,
                   munic,
                   by.x = "Cod_Municipio_Completo",
                   by.y = "CodMun")

rm(base_aux)
rm(munic)
rm(escolas)
rm(filtro)
rm(ideb)

# Descomentar para salvar a base com os dados extraidos das bases do Ideb e da base de escola
# saveRDS(base_ideb_esc, "Base_Ideb.rds")



## Preparacao das bases de docencia ---------------------------------------

municipios = base_ideb_esc %>% select(Cod_Municipio_Completo, Nome_Municipio)

# Colunas a serem usadas
colunas <- c("cod_municipio", "tp_cor_raca", "tem_ensino_superior", "etapa_agg_fund_ai",
             "etapa_agg_fund_af", "tp_etapa_ensino", "num_disciplinas_ensina", "num_formacoes",
             "num_disc_form_adequada", "ensina_disc_lingua_portuguesa",
             "ensina_disc_lingua_estrangeira", "ensina_disc_artes", "ensina_disc_educacao_fisica",
             "ensina_disc_matematica", "ensina_disc_ciencias", "ensina_disc_quimica",
             "ensina_disc_fisica", "ensina_disc_biologia", "ensina_disc_estudos_sociais",
             "ensina_disc_historia", "ensina_disc_geografia", "ensina_disc_sociologia",
             "ensina_disc_filosofia", "ensina_disc_ensino_religioso",
             "tem_formacao_lingua_portuguesa", "tem_formacao_lingua_estrangeira",
             "tem_formacao_artes", "tem_formacao_educacao_fisica", "tem_formacao_matematica",
             "tem_formacao_ciencias", "tem_formacao_quimica", "tem_formacao_fisica",
             "tem_formacao_biologia", "tem_formacao_estudos_sociais", "tem_formacao_historia",
             "tem_formacao_geografia", "tem_formacao_sociologia", "tem_formacao_filosofia",
             "tem_formacao_ensino_religioso", "form_adequada_lingua_portuguesa",
             "form_adequada_lingua_estrangeira", "form_adequada_artes",
             "form_adequada_educacao_fisica", "form_adequada_matematica", "form_adequada_ciencias",
             "form_adequada_quimica", "form_adequada_fisica", "form_adequada_biologia",
             "form_adequada_estudos_sociais", "form_adequada_historia", "form_adequada_geografia",
             "form_adequada_sociologia", "form_adequada_filosofia", "form_adequada_ensino_religioso"
            )

# Pega a primeira base com apenas os docentes em regencia de escolas publicas e em ativiade e que dao
# aula para o ensino fundamental
base <- read.csv(bases_docencia[1]) %>%
  filter(tp_rede_publica == "true" & escolas_em_atividade == "true" & docente_em_regencia == "true" &
           (etapa_agg_fund_ai == "true" | etapa_agg_fund_af == "true")) %>%
  select(colunas) %>%
  mutate_if(is.factor, as.character)

# Adiciona as outras bases a primeira base lida
for (i in (2:8)) {
  df <- read.csv(bases_docencia[i]) %>%
    filter(tp_rede_publica == "true" & escolas_em_atividade == "true" & docente_em_regencia == "true" &
             (etapa_agg_fund_ai == "true" | etapa_agg_fund_af == "true")) %>%
    select(colunas) %>%
    mutate_if(is.factor, as.character)
  
  docencia <- rbind(base, df)
}

# Preparacao da base para visao municipio
docencias <- docencia %>%
  group_by(cod_municipio) %>%
  summarise(num_disciplinas_por_professor = mean(num_disciplinas_ensina),
            num_formacoes_por_professor   = mean(num_formacoes),
            num_tipos_disc_forma_adeq     = mean(num_disc_form_adequada),
            disc_ling_portuguesa          = sum(ensina_disc_lingua_portuguesa=="true")/sum(ensina_disc_lingua_portuguesa!=""),
            disc_ling_estrangeira         = sum(ensina_disc_lingua_estrangeira=="true")/sum(ensina_disc_lingua_estrangeira!=""),
            disc_artes                    = sum(ensina_disc_artes=="true")/sum(ensina_disc_artes!=""),
            disc_educacao_fisica          = sum(ensina_disc_educacao_fisica=="true")/sum(ensina_disc_educacao_fisica!=""),
            disc_matematica               = sum(ensina_disc_matematica=="true")/sum(ensina_disc_matematica!=""),
            disc_ciencias                 = sum(ensina_disc_ciencias=="true" | ensina_disc_quimica=="true" |
                                                  ensina_disc_fisica=="true" | ensina_disc_biologia=="true")/sum(
                                                    ensina_disc_ciencias!="" | ensina_disc_quimica!="" |
                                                      ensina_disc_fisica!="" | ensina_disc_biologia!=""),
            disc_ciencias_humanas         = sum(ensina_disc_estudos_sociais=="true" | ensina_disc_sociologia=="true" |
                                                  ensina_disc_filosofia=="true")/sum(ensina_disc_estudos_sociais!="" |
                                                                                       ensina_disc_sociologia!="" |
                                                                                       ensina_disc_filosofia!=""),
            disc_hist_geo                 = sum(ensina_disc_historia=="true" | ensina_disc_geografia=="true")/sum(ensina_disc_historia
                                                                                                                  !="" | ensina_disc_geografia!=""),
            disc_ensino_religioso         = sum(ensina_disc_ensino_religioso=="true")/sum(ensina_disc_ensino_religioso!="")
  ) %>% 
  right_join(
    municipios %>% rename(cod_municipio = Cod_Municipio_Completo), by = 'cod_municipio')

rm(docencia)
rm(municipios)
rm(df)
rm(base)



## Montagem da base final --------------------------------------------------

base_final <- left_join(base_ideb_esc %>% rename(cod_municipio = Cod_Municipio_Completo),
                        docencias,
                        by = 'cod_municipio')

# Descomentar para salvar a base final
# saveRDS(base_final, 'base_final.rds')
