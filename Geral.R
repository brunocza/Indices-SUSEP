#__________________________________________________________________________________________________

# Índices para Análise Econômico-Financeira das Supervisionadas pela SUSEP 

# Autor: Bruno Czarnescki
# Email: brunocza1@gmail.com

# --/07/2020
#__________________________________________________________________________________________________

# Escolha a data inicial:

dt_ini <- 201501

#   IMPORTAÇÃO DE PACOTES 
{
  # Check if the packages that we need are installed
  want = c("readxl", "ggplot2","data.table","dplyr","lubridate","readr")
  have = want %in% rownames(installed.packages())
  
  # Install the packages that we miss
  if ( any(!have) ) { install.packages( want[!have] ) }
  
  # Load the packages
  junk <- lapply(want, library, character.only = T)
  
  # Remove the objects we created
  rm(have, want, junk) 
} 

# Blocos de cálculos (importação online)
{

  block_1 <- read_csv("https://raw.githubusercontent.com/brunocza/Indices-SUSEP/master/block_1.csv", 
                      col_types = cols(alocation_mode = col_character(), 
                                       cmpid = col_double(), `function` = col_character(), 
                                       name = col_character()))
  block_2 <- read_csv("https://raw.githubusercontent.com/brunocza/Indices-SUSEP/master/block_2.csv", 
                      col_types = cols(alocation_mode = col_character(), 
                                       cmpid = col_double(), `function` = col_character(), 
                                       name = col_character()))
  block_3 <- read_csv("https://raw.githubusercontent.com/brunocza/Indices-SUSEP/master/block_3.csv", 
                      col_types = cols(alocation_mode = col_character(), 
                                       cmpid = col_double(), `function` = col_character(), 
                                       name = col_character()))
  
}





#   Base de dados principal  ----------------------------------------------------------------------
{
  # codigo pra a impotação local : 
  
  # SES_BALANCO <- as.data.frame(read_delim("C:/ ... /SES_Balanco.csv",
  #                                         ";", escape_double = FALSE, col_types = cols(coenti = col_number(),
  #                                                                                      cmpid = col_double(),
  #                                                                                      damesano = col_date(format = "%Y%m"),
  #                                                                                      quadro = col_skip(), seq = col_skip()),
  #                                         locale = locale(decimal_mark = ","),
  #                                         trim_ws = TRUE))
  # 
  
  # Código para dowload da base SUSEP, extrção .zip e importação SES_Balanco.csv (apos o dowload, o arquivo zip e a extração do mesmo será excluida automaticamente)
  # Não é preciso configurar um diretório, pois tudo sera salvo na pasta TEMP do sistema operacional e apos importação.
  

  destfile<- tempdir()
  download.file(url="https://www2.susep.gov.br/download/estatisticas/BaseCompleta.zip",
                destfile = paste0(destfile,"/BaseCompleta.zip"), mode="wb")


  lista<-unzip(paste0(destfile,"/BaseCompleta.zip"),
               list = TRUE)[34,1]

  files<-unzip(paste0(destfile,"/BaseCompleta.zip"), files= "SES_Balanco.csv",
               exdir= destfile)


  SES_BALANCO <-as.data.frame(read_delim(files,
                                      ";", escape_double = FALSE, col_types = cols(coenti = col_number(),
                                                                                   cmpid = col_double(),
                                                                                   damesano = col_date(format = "%Y%m"),
                                                                                   quadro = col_skip(), seq = col_skip()),
                                      locale = locale(decimal_mark = ","),
                                      trim_ws = TRUE))
  file.remove(files)

  tt= paste0(destfile,"/BaseCompleta.zip")

  file.remove(tt)
  remove(tt,destfile, lista)

}# SES_Balanco.CSV  




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                 Seleção de empresas    
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{
  LISTAEMPRESAS <- read_delim("https://www2.susep.gov.br/menuestatistica/SES/download/LISTAEMPRESAS.csv", 
                              ";", escape_double = FALSE, col_types = cols(CNPJ = col_character(), 
                                                                           CodigoFIP = col_double(), NomeEntidade = col_character()), 
                              locale = locale(encoding = "ISO-8859-1"), 
                              trim_ws = TRUE) ; colnames(LISTAEMPRESAS)[colnames(LISTAEMPRESAS) == 'CodigoFIP'] <- 'coenti'

  
} # LISTAEMPRESAS

{
only_usual_conti <- read_csv("https://raw.githubusercontent.com/brunocza/Indices-SUSEP/master/only_usual_conti.csv", 
                             col_types = cols(CNPJ = col_character(), 
                                              NomeEntidade = col_character(), coenti = col_double(), 
                                              ramo = col_character()), locale = locale(encoding = "ISO-8859-1"))

} # only_usual_conti





# coenti_seguradoras ----
coenti_seguradoras = only_usual_conti %>% filter(., ramo == "seguradora")

# coenti_prev_comp ------
coenti_prev_comp = only_usual_conti %>% filter(., ramo == "previdencia complementar")

# coenti_cap ------------
coenti_cap = only_usual_conti %>% filter(., ramo == "captalizacao")



# df_all_1 ,df_all_1 E df_all_1 ----

{
  df_all_1 <- 
    SES_BALANCO %>% filter(.,
                           damesano >= as.Date(paste0(dt_ini, '01'), format = "%Y%m%d"),
                           coenti %in% coenti_seguradoras$coenti ) %>% 
                           arrange(damesano,
                                   coenti,
                                   cmpid)
                        
  
  
  df_all_2 <- 
    SES_BALANCO %>% filter(.,
                           damesano >= as.Date(paste0(dt_ini, '01'), format = "%Y%m%d"),
                           coenti %in% coenti_prev_comp$coenti ) %>% 
                           arrange(damesano,
                                    coenti,
                                    cmpid)
                
  
  
  df_all_3 <- 
    SES_BALANCO %>% filter(.,
                           damesano >= as.Date(paste0(dt_ini, '01'), format = "%Y%m%d"),
                           coenti %in% coenti_cap$coenti ) %>% 
                          arrange(damesano,
                                  coenti,
                                  cmpid)
  
  remove(SES_BALANCO)
  
} # df_all_1 ,df_all_1 E df_all_1

# função acu(), tranforma incremental em acumulado ------------------------------------------------
acu <- function(entrada, nome)  {
  
  df <- df_all[df_all$cmpid == entrada, ]
  df$quadro <- NULL
  df$seq <- NULL
  df$cmpid <- NULL
  
  df$ano = year(df$damesano)
  
  df = df %>%  arrange(coenti,damesano) %>%  group_by( . ,coenti, damesano)  %>% arrange(coenti,damesano)
  
  setDT(df)[, lag_1  := shift(valor, fill=0), by = c("coenti", "ano") ]
  
  
  df = df %>% mutate(., valdiff = valor - lag_1)
  
  colnames(df)[colnames(df)      # Rename two variable names
               %in% "valdiff"] <- nome
  df$ano = NULL
  df$lag_1 = NULL
  df$valor <- NULL
  
  df = df %>% ungroup()
  return(as.data.frame(df))
}

# Função para dados ja incremental -------------------------------------------

incremental <- function(entrada, nome) {
  
  df <- df_all[df_all$cmpid == entrada, ]
  df$quadro <- NULL
  df$seq <- NULL
  df$cmpid <- NULL
  
  
  # Order data with Base R
  
  df <- df[order(df$coenti,  df$damesano), ]
  
  
  
  
  colnames(df)[colnames(df)      # Rename two variable names
               %in% "valor"] <- nome
  
  return(df)
  
}


#   Função inf()rmaçoes de um data frame  ---------------

info <- function(x) {
  cat(paste("linhas:",
            nrow(x),
            " ",
            "Colunas:",
            ncol(x),
            " ",
            "" , sep = "\n"))
  return(head(x))
}

# # Feunção "ver ()" dataframe em grafico  --------------------
# 
# ver <- function(x) {
#   df = df_all[df_all$cmpid == x , ]
#   return(plot(df$damesano, df$valor  , "p"))
#   
# }


# Feunção plotar indice da empresa  ---------------------------------------------------------------

indice_grafico  <- function(df, coenti) {
  
  
  
  g <-  ggplot(df[df$coenti== coenti,]) +
    aes(x = damesano, y = indice) +
    geom_line(size = 1L, colour = "#0c4c8a") +
    labs(y = "ÍNDICE", title = "Retorno de indice_grafic() ") +
    theme_minimal() +
    facet_wrap(vars(coenti), scales = "free_y")
  return( g )
  
}



###################################################################################################
#                              SOCIEDADES SEGURADORAS
###################################################################################################

df_all <- df_all_1  

for (i in 1:nrow(block_1)) {
  class(eval(parse(text=block_1[i,1])))
}



#      Índices para Análise Econômico-Financeira das Supervisionadas     --------------------------
# Necessaria as funções acu(), ver(), info(), indice_grafico()


#  1.1.1 SOCIEDADES   SEGURADORAS(Inclusive as que operam com Previdência Complementar) -----------
#1.1.1ÍNDICE DE RETENÇÃO  -------------------------------------------------------------------------


# irets    ----------------------------------------------------------------------------------------


irets_1 <- merge(prem_ced_resseg_bruto,prem_emit_liq, by=c("coenti","damesano"),all = TRUE)
irets_1$indice <- 1 - (-1 * irets_1$prem_ced_resseg_bruto ) / (irets_1$prem_emit_liq)


#   1.1.2ÍNDICES DE CUSTOS   ----------------------------------------------------------------------

# ISR  --------------------------------------------------------------------------------------------

isr_1 <- merge(dsp_ben,sin_oco, by=c("coenti","damesano"),all = TRUE)
isr_1 <- merge(isr_1 ,prem_ganho, by=c("coenti","damesano"),all = TRUE)
isr_1 <- merge(isr_1 ,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
isr_1 <- merge(isr_1 ,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
isr_1 <- merge(isr_1 ,var_opt_prev, by=c("coenti","damesano"),all = TRUE)

isr_1$indice <-
  -1 * (isr_1$sin_oco +   isr_1$dsp_ben) / (isr_1$prem_ganho +
                                              isr_1$rec_contr_prev +
                                              isr_1$resdas_tx_gestao +
                                              isr_1$var_opt_prev)


# IDC   -------------------------------------------------------------------------------------------

idc_1 <- merge(custo_aq_seg,custo_aq_prev, by=c("coenti","damesano"),all = TRUE)
idc_1 <- merge(idc_1 ,prem_ganho, by=c("coenti","damesano"),all = TRUE)
idc_1 <- merge(idc_1 ,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
idc_1 <- merge(idc_1 ,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
idc_1 <- merge(idc_1 ,var_opt_prev, by=c("coenti","damesano"),all = TRUE)


idc_1$indice <-
  -1 * (idc_1$custo_aq_seg +   idc_1$custo_aq_prev) / (idc_1$prem_ganho +
                                                         idc_1$rec_contr_prev +
                                                         idc_1$resdas_tx_gestao +
                                                         idc_1$var_opt_prev)


# IORDO  ------------------------------------------------------------------------------------------

iordo_1 <- merge(out_rec_dsp_seg,rec_emissao_apolice_dpvat, by=c("coenti","damesano"),all = TRUE)
iordo_1 <- merge(iordo_1 ,out_rec_dsp_prev, by=c("coenti","damesano"),all = TRUE)
iordo_1 <- merge(iordo_1 ,prem_ganho, by=c("coenti","damesano"),all = TRUE)
iordo_1 <- merge(iordo_1 ,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
iordo_1 <- merge(iordo_1 ,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
iordo_1 <- merge(iordo_1 ,var_opt_prev, by=c("coenti","damesano"),all = TRUE)



iordo_1 $indice <- -1 * ( iordo_1 $out_rec_dsp_seg +
                            iordo_1 $rec_emissao_apolice_dpvat +
                            iordo_1 $out_rec_dsp_prev  ) / (
                              iordo_1 $prem_ganho + iordo_1 $rec_contr_prev +  iordo_1 $resdas_tx_gestao +
                                iordo_1 $var_opt_prev )

# IRRES  ------------------------------------------------------------------------------------------

irres_1 <- merge(res_resseg_seg,  res_resseg_prev, by=c("coenti","damesano"),all = TRUE)
irres_1 <- merge(irres_1,  prem_ganho, by=c("coenti","damesano"),all = TRUE)
irres_1 <- merge(irres_1,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
irres_1 <- merge(irres_1,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
irres_1 <- merge(irres_1,var_opt_prev, by=c("coenti","damesano"),all = TRUE)


irres_1 $indice <- -1*(irres_1 $res_resseg_seg +  
                         irres_1 $res_resseg_prev) / (  irres_1 $prem_ganho + 
                                                          irres_1 $rec_contr_prev +
                                                          irres_1 $resdas_tx_gestao +
                                                          irres_1 $var_opt_prev  )


# IDA  --------------------------------------------------------------------------------------------

ida_1 <- merge(dsp_adm,  dsp_trib, by=c("coenti","damesano"),all = TRUE)
ida_1 <- merge(ida_1,  prem_ganho, by=c("coenti","damesano"),all = TRUE)
ida_1 <- merge(ida_1,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
ida_1 <- merge(ida_1,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
ida_1 <- merge(ida_1,var_opt_prev, by=c("coenti","damesano"),all = TRUE)


ida_1 $indice <- -1*( ida_1$dep_adm +ida_1 $dsp_trib) / (  ida_1 $prem_ganho +
                                                             ida_1 $rec_contr_prev +
                                                             ida_1 $resdas_tx_gestao +
                                                             ida_1 $var_opt_prev  )


# IC  ---------------------------------------------------------------------------------------------

sin_oco <- acu(11232, "sin_oco")
dsp_ben <- acu(11248, "dsp_ben")
custo_aq_seg <- acu(11237, "custo_aq_seg")
custo_aq_prev <- acu(11249, "custo_aq_prev")
out_rec_dsp_seg <- acu(6202, "out_rec_dsp_seg")
rec_emissao_apolice_dpvat <- acu(11231, "rec_emissao_apolice_dpvat")
out_rec_dsp_prev <- acu(6261, "out_rec_dsp_prev")
res_resseg_seg <- acu(11238, "res_resseg_seg")
res_resseg_prev <- acu(11250, "res_resseg_prev")
dsp_adm <- acu(4069, "dsp_adm")
dsp_trib <- acu(4070, "dsp_trib")

prem_ganho <- acu(4027,"prem_ganho")
rec_contr_prev <- acu(7186,"rec_contr_prev")
resdas_tx_gestao <- acu(6238,"resdas_tx_gestao")
var_opt_prev <- acu(6256,"var_opt_prev")

ic_1 <- merge(sin_oco,  dsp_ben, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,  custo_aq_seg, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,custo_aq_prev, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,out_rec_dsp_seg, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,rec_emissao_apolice_dpvat, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,  out_rec_dsp_prev, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,  res_resseg_seg, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,  res_resseg_prev, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,  dsp_adm, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,  dsp_trib, by=c("coenti","damesano"),all = TRUE)

ic_1 <- merge(ic_1,  prem_ganho, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
ic_1 <- merge(ic_1,var_opt_prev, by=c("coenti","damesano"),all = TRUE)


ic_1 $indice <- -1*(ic_1$sin_oco  + 
                      ic_1$dsp_ben  + 
                      ic_1$custo_aq_seg  + 
                      ic_1$custo_aq_prev  + 
                      ic_1$out_rec_dsp_seg  + 
                      ic_1$rec_emissao_apolice_dpvat  + 
                      ic_1$out_rec_dsp_prev  + 
                      ic_1$res_resseg_seg  + 
                      ic_1$res_resseg_prev  + 
                      ic_1$dsp_adm  + 
                      ic_1$dsp_trib ) / (ic_1$prem_ganho  + 
                                           ic_1$rec_contr_prev  + 
                                           ic_1$resdas_tx_gestao  + 
                                           ic_1$var_opt_prev        )


# ICA  -------------------------------------------------------------------------------------------

ica_1 <- merge(sin_oco,  dsp_ben, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,  custo_aq_seg, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,custo_aq_prev, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,out_rec_dsp_seg, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,rec_emissao_apolice_dpvat, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,  out_rec_dsp_prev, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,  res_resseg_seg, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,  res_resseg_prev, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,  dsp_adm, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,  dsp_trib, by=c("coenti","damesano"),all = TRUE)

ica_1 <- merge(ica_1,  prem_ganho, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,var_opt_prev, by=c("coenti","damesano"),all = TRUE)
ica_1 <- merge(ica_1,res_fin, by=c("coenti","damesano"),all = TRUE)

ica_1 $indice <- -1*(ica_1$sin_oco  + 
                       ica_1$dsp_ben  + 
                       ica_1$custo_aq_seg  + 
                       ica_1$custo_aq_prev  + 
                       ica_1$out_rec_dsp_seg  + 
                       ica_1$rec_emissao_apolice_dpvat  + 
                       ica_1$out_rec_dsp_prev  + 
                       ica_1$res_resseg_seg  + 
                       ica_1$res_resseg_prev  + 
                       ica_1$dsp_adm  + 
                       ica_1$dsp_trib ) / (ica_1$prem_ganho  + 
                                             ica_1$rec_contr_prev  + 
                                             ica_1$resdas_tx_gestao  + 
                                             ica_1$var_opt_prev    +
                                             ica_1$res_fin  )


#   1.1.3ÍNDICES DE LIQUIDEZ  ---------------------------------------------------------------------

#   ILC -------------------------------------------------------------------------------------------

#todos incrementais

ilc_1 <- merge(ativo_circ,  custo_aq_dif_cp, by=c("coenti","damesano"),all = TRUE)
ilc_1 <- merge(ilc_1,  dsp_ant_cp, by=c("coenti","damesano"),all = TRUE)
ilc_1 <- merge(ilc_1,  pasivo_circ, by=c("coenti","damesano"),all = TRUE)


ilc_1$indice <- (ilc_1$ativo_circ - ilc_1$custo_aq_dif_cp - ilc_1$dsp_ant_cp)/ ilc_1$pasivo_circ


#   ILT -------------------------------------------------------------------------------------------

ilt_1 <- merge(ativo_circ,  custo_aq_dif_cp, by=c("coenti","damesano"),all = TRUE)
ilt_1 <- merge(ilt_1,  dsp_ant_cp, by=c("coenti","damesano"),all = TRUE)
ilt_1 <- merge(ilt_1,  ativo_rlp, by=c("coenti","damesano"),all = TRUE)
ilt_1 <- merge(ilt_1,  custo_aq_diflp, by=c("coenti","damesano"),all = TRUE)
ilt_1 <- merge(ilt_1,  dsp_ant_lp, by=c("coenti","damesano"),all = TRUE)
ilt_1 <- merge(ilt_1,  pasivo_circ, by=c("coenti","damesano"),all = TRUE)
ilt_1 <- merge(ilt_1,  passivo_n_circ, by=c("coenti","damesano"),all = TRUE)


ilt_1$indice <-  ( ilt_1$ativo_circ 
                   - ilt_1$custo_aq_dif_cp 
                   - ilt_1$dsp_ant_cp 
                   + ilt_1$ativo_rlp 
                   - ilt_1$custo_aq_diflp 
                   - ilt_1$dsp_ant_lp  )  /  ( ilt_1$pasivo_circ 
                                               + ilt_1$passivo_n_circ )



#   1.1.4 ÍNDICES DE IMOBILIZAÇÃO   --------------------------------------------------------------

#    IATIM   --------------------------------------------------------------------------------------

iatim_1 <- merge(ativo_imob,  im_urb_renda, by=c("coenti","damesano"),all = TRUE)
iatim_1 <- merge(iatim_1,  im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_1 <- merge(iatim_1,  red_val_rec_im_urb, by=c("coenti","damesano"),all = TRUE)
iatim_1 <- merge(iatim_1,  red_val_rec_im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_1 <- merge(iatim_1,  deprec_im_urb, by=c("coenti","damesano"),all = TRUE)
iatim_1 <- merge(iatim_1,  deprec_im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_1 <- merge(iatim_1,  ativo_total, by=c("coenti","damesano"),all = TRUE)

iatim_1$indice <- (iatim_1$ativo_imob 
                   + iatim_1$im_urb_renda 
                   + iatim_1$im_rural 
                   - iatim_1$red_val_rec_im_urb 
                   - iatim_1$red_val_rec_im_rural 
                   - iatim_1$deprec_im_urb 
                   - iatim_1$deprec_im_rural )  / iatim_1$ativo_total 



#    IIMOB    -------------------------------------------------------------------------------------

iimob_1 <- merge(ativo_imob,  im_urb_renda, by=c("coenti","damesano"),all = TRUE)
iimob_1 <- merge(iimob_1,  im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_1 <- merge(iimob_1,  red_val_rec_im_urb, by=c("coenti","damesano"),all = TRUE)
iimob_1 <- merge(iimob_1,  red_val_rec_im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_1 <- merge(iimob_1,  deprec_im_urb, by=c("coenti","damesano"),all = TRUE)
iimob_1 <- merge(iimob_1,  deprec_im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_1 <- merge(iimob_1,  patr_liq, by=c("coenti","damesano"),all = TRUE)


iimob_1$indice <-  ( iimob_1$ativo_imob 
                     + iimob_1$im_urb_renda 
                     + iimob_1$im_rural 
                     - iimob_1$red_val_rec_im_urb 
                     - iimob_1$red_val_rec_im_rural 
                     - iimob_1$deprec_im_urb 
                     - iimob_1$deprec_im_rural )  / iimob_1$patr_liq 



#   1.1.5ÍNDICES DE PARTICIPAÇÕES SOCIETÁRIAS   ---------------------------------------------------


#   IPAS      -------------------------------------------------------------------------------------

ipas_1 <- merge(part_soc_fin,  part_soc_n_fin, by=c("coenti","damesano"),all = TRUE)
ipas_1 <- merge(ipas_1,  part_soc_fin_ext, by=c("coenti","damesano"),all = TRUE)
ipas_1 <- merge(ipas_1,  part_soc_n_fin_ext, by=c("coenti","damesano"),all = TRUE)
ipas_1 <- merge(ipas_1,  red_val_rec, by=c("coenti","damesano"),all = TRUE)
ipas_1 <- merge(ipas_1,  patr_liq, by=c("coenti","damesano"),all = TRUE)

ipas_1$indice <- (  ipas_1$part_soc_fin 
                    + ipas_1$part_soc_n_fin 
                    + ipas_1$part_soc_fin_ext 
                    + ipas_1$part_soc_n_fin_ext 
                    - ipas_1$red_val_rec )       /     ipas_1$patr_liq



#    ILPL   ---------------------------------------------------------------------------------------

{
  
  patr_liq_2 <- incremental(3333,"patr_liq_2")
  
  patr_liq_2$ ano <- as.numeric(strftime(patr_liq_2$damesano, "%Y"))
  
  for (i in 1:nrow(patr_liq_2)) {
    
    if ( as.numeric(strftime(patr_liq_2$damesano[i], "%m")) == 12) {
      
      patr_liq_2$patr_liq_dez[i] <-patr_liq_2$patr_liq_2[i]
      
    }else{
      
      patr_liq_2$patr_liq_dez[i] <- NA
      
    }
    
  }
  
  patri_liq_aux <- subset(patr_liq_2, subset = as.numeric(strftime(damesano, "%m")) == 12)
  
  
  colnames(patri_liq_aux)[colnames(patri_liq_aux)      # Rename two variable names
                          %in% "patr_liq_2"] <- "patr_liq_dez_anterior"
  
  # patri_liq_aux$patr_liq_2 <- NULL
  
  patri_liq_aux$ano <- patri_liq_aux$ano + 1
  head(patri_liq_aux)
  patri_liq_aux$patr_liq_dez <- NULL
  patri_liq_aux$damesano <- NULL
  
  
  patr_liq_2 <- merge(patr_liq_2,  patri_liq_aux, by=c("coenti","ano"),all = TRUE)
  patr_liq_2 <- patr_liq_2[order(patr_liq_2$coenti,  patr_liq_2$damesano), ]
  patr_liq_2$ano <- NULL
  
}##xxxxxx ESPECIAL CASO xxxxxxxxx

ilpl_1 <- merge(lucro_liq,  patr_liq_2, by=c("coenti","damesano"),all = TRUE)

ilpl_1$indice <- ilpl_1$lucro_liq /  (  ( ilpl_1$patr_liq_2 + ilpl_1$patr_liq_dez_anterior) / 2)

remove(patr_liq_2)
remove(patri_liq_aux)
#    IREPLL ---------------------------------------------------------------------------------------

irepll_1 <- merge( rec_aj_inv_ctrl_col,  dsp_aj_inv_ctrl_col, by=c("coenti","damesano"),all = TRUE)
irepll_1 <- merge( irepll_1 ,  lucro_liq , by=c("coenti","damesano"),all = TRUE)

irepll_1$ indice <- (irepll_1$rec_aj_inv_ctrl_col - irepll_1$dsp_aj_inv_ctrl_col ) / irepll_1$lucro_liq


#   IGDF     --------------------------------------------------------------------------------------

igdf_1 <- merge( res_fin , lucro_liq , by=c("coenti","damesano"),all = TRUE)

igdf_1$indice <- igdf_1$res_fin / igdf_1$lucro_liq


#  LIMPA TUDO    ----------------------------------------------------------------------------------

remove(list = block_1$name)


###################################################################################################
#                         ENTIDADES DE PREVIDÊNCIA COMPLEMENTAR
###################################################################################################




df_all <- df_all_2

for (i in 1:nrow(block_2)) {
  class(eval(parse(text=block_2[i,1])))
}


#      Índices para Análise Econômico-Financeira das Supervisionadas     --------------------------

# Necessaria as funções acu(), ver(), info(), indice_grafico()


#  1.2 ENTIDADES DE PREVIDÊNCIA COMPLEMENTAR  -----------------------------------------------------
#   1.2.1 ÍNDICES DE CUSTOS  ----------------------------------------------------------------------

# ISR  --------------------------------------------------------------------------------------------

isr_2 <- merge(dsp_ben,sin_oco, by=c("coenti","damesano"),all = TRUE)
isr_2 <- merge(isr_2 ,prem_ganho, by=c("coenti","damesano"),all = TRUE)
isr_2 <- merge(isr_2 ,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
isr_2 <- merge(isr_2 ,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
isr_2 <- merge(isr_2 ,var_opt_prev, by=c("coenti","damesano"),all = TRUE)
isr_2 <- merge(isr_2 ,res_fin, by=c("coenti","damesano"),all = TRUE)


isr_2$indice <-
  -1 * (isr_2$sin_oco +   isr_2$dsp_ben) / (isr_2$prem_ganho +
                                              isr_2$rec_contr_prev +
                                              isr_2$resdas_tx_gestao +
                                              isr_2$var_opt_prev+
                                              isr_2$res_fin)


# IDC   -------------------------------------------------------------------------------------------

idc_2 <- merge(custo_aq_seg,custo_aq_prev, by=c("coenti","damesano"),all = TRUE)
idc_2 <- merge(idc_2 ,prem_ganho, by=c("coenti","damesano"),all = TRUE)
idc_2 <- merge(idc_2 ,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
idc_2 <- merge(idc_2 ,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
idc_2 <- merge(idc_2 ,var_opt_prev, by=c("coenti","damesano"),all = TRUE)
idc_2 <- merge(idc_2 ,res_fin, by=c("coenti","damesano"),all = TRUE)


idc_2$indice <-
  -1 * (idc_2$custo_aq_seg +   idc_2$custo_aq_prev) / (idc_2$prem_ganho +
                                                         idc_2$rec_contr_prev +
                                                         idc_2$resdas_tx_gestao +
                                                         idc_2$var_opt_prev+
                                                         idc_2$res_fin)

# IORDO  ------------------------------------------------------------------------------------------

iordo_2 <- merge(out_rec_dsp_seg,rec_emissao_apolice_dpvat, by=c("coenti","damesano"),all = TRUE)
iordo_2 <- merge(iordo_2 ,out_rec_dsp_prev, by=c("coenti","damesano"),all = TRUE)
iordo_2 <- merge(iordo_2 ,prem_ganho, by=c("coenti","damesano"),all = TRUE)
iordo_2 <- merge(iordo_2 ,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
iordo_2 <- merge(iordo_2 ,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
iordo_2 <- merge(iordo_2 ,var_opt_prev, by=c("coenti","damesano"),all = TRUE)
iordo_2 <- merge(iordo_2 ,res_fin, by=c("coenti","damesano"),all = TRUE)


iordo_2$indice <- -1 * (iordo_2$out_rec_dsp_seg +
                          iordo_2$rec_emissao_apolice_dpvat +
                          iordo_2$out_rec_dsp_prev) / ( iordo_2$prem_ganho +
                                                          iordo_2$rec_contr_prev +
                                                          iordo_2$resdas_tx_gestao +
                                                          iordo_2$var_opt_prev + 
                                                          iordo_2$res_fin )



# IRRES  ------------------------------------------------------------------------------------------

irres_2 <- merge(res_resseg_seg,  res_resseg_prev, by=c("coenti","damesano"),all = TRUE)
irres_2 <- merge(irres_2,  prem_ganho, by=c("coenti","damesano"),all = TRUE)
irres_2 <- merge(irres_2,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
irres_2 <- merge(irres_2,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
irres_2 <- merge(irres_2,var_opt_prev, by=c("coenti","damesano"),all = TRUE)
irres_2 <- merge(irres_2,res_fin, by=c("coenti","damesano"),all = TRUE)

irres_2 $indice <- -1*(irres_2 $res_resseg_seg +  
                         irres_2 $res_resseg_prev) / (  irres_2 $prem_ganho + 
                                                          irres_2 $rec_contr_prev +
                                                          irres_2 $resdas_tx_gestao +
                                                          irres_2 $var_opt_prev +
                                                          irres_2 $res_fin)


# IDA  --------------------------------------------------------------------------------------------

ida_2 <- merge(dsp_adm,  dsp_trib, by=c("coenti","damesano"),all = TRUE)
ida_2 <- merge(ida_2,  prem_ganho, by=c("coenti","damesano"),all = TRUE)
ida_2 <- merge(ida_2,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
ida_2 <- merge(ida_2,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
ida_2 <- merge(ida_2,var_opt_prev, by=c("coenti","damesano"),all = TRUE)
ida_2 <- merge(ida_2,res_fin, by=c("coenti","damesano"),all = TRUE)

ida_2 $indice <- -1*( ida_2$dep_adm +ida_2 $dsp_trib) / (  ida_2 $prem_ganho +
                                                             ida_2 $rec_contr_prev +
                                                             ida_2 $resdas_tx_gestao +
                                                             ida_2 $var_opt_prev +
                                                             ida_2 $res_fin )


# ICA  -------------------------------------------------------------------------------------------

dsp_adm <- acu(4069, "dsp_adm")

ica_2 <- merge(sin_oco,  dsp_ben, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,  custo_aq_seg, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,custo_aq_prev, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,out_rec_dsp_seg, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,rec_emissao_apolice_dpvat, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,  out_rec_dsp_prev, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,  res_resseg_seg, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,  res_resseg_prev, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,  dsp_adm, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,  dsp_trib, by=c("coenti","damesano"),all = TRUE)

ica_2 <- merge(ica_2,  prem_ganho, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,rec_contr_prev, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,resdas_tx_gestao, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,var_opt_prev, by=c("coenti","damesano"),all = TRUE)
ica_2 <- merge(ica_2,res_fin, by=c("coenti","damesano"),all = TRUE)

ica_2 $indice <- -1*( ica_2$sin_oco  + 
                        ica_2$dsp_ben  + 
                        ica_2$custo_aq_seg  + 
                        ica_2$custo_aq_prev  + 
                        ica_2$out_rec_dsp_seg  + 
                        ica_2$rec_emissao_apolice_dpvat  + 
                        ica_2$out_rec_dsp_prev  + 
                        ica_2$res_resseg_seg  + 
                        ica_2$res_resseg_prev  + 
                        ica_2$dsp_adm  + 
                        ica_2$dsp_trib ) / (ica_2$prem_ganho  + 
                                              ica_2$rec_contr_prev  + 
                                              ica_2$resdas_tx_gestao  + 
                                              ica_2$var_opt_prev +
                                              ica_2$res_fin)



#   1.2.2 ÍNDICES DE LIQUIDEZ  --------------------------------------------------------------------

#   ICL -------------------------------------------------------------------------------------------

#todos incrementais

ilc_2 <- merge(ativo_circ,  custo_aq_dif_cp, by=c("coenti","damesano"),all = TRUE)
ilc_2 <- merge(ilc_2,  dsp_ant_cp, by=c("coenti","damesano"),all = TRUE)
ilc_2 <- merge(ilc_2,  pasivo_circ, by=c("coenti","damesano"),all = TRUE)


ilc_2$indice <- (ilc_2$ativo_circ - ilc_2$custo_aq_dif_cp - ilc_2$dsp_ant_cp)/ ilc_2$pasivo_circ


#   ILT -------------------------------------------------------------------------------------------

ilt_2 <- merge(ativo_circ,  custo_aq_dif_cp, by=c("coenti","damesano"),all = TRUE)
ilt_2 <- merge(ilt_2,  dsp_ant_cp, by=c("coenti","damesano"),all = TRUE)
ilt_2 <- merge(ilt_2,  ativo_rlp, by=c("coenti","damesano"),all = TRUE)
ilt_2 <- merge(ilt_2,  custo_aq_diflp, by=c("coenti","damesano"),all = TRUE)
ilt_2 <- merge(ilt_2,  dsp_ant_lp, by=c("coenti","damesano"),all = TRUE)
ilt_2 <- merge(ilt_2,  pasivo_circ, by=c("coenti","damesano"),all = TRUE)
ilt_2 <- merge(ilt_2,  passivo_n_circ, by=c("coenti","damesano"),all = TRUE)


ilt_2$indice <-  ( ilt_2$ativo_circ 
                   - ilt_2$custo_aq_dif_cp 
                   - ilt_2$dsp_ant_cp 
                   + ilt_2$ativo_rlp 
                   - ilt_2$custo_aq_diflp 
                   - ilt_2$dsp_ant_lp  )  /  ( ilt_2$pasivo_circ 
                                               + ilt_2$passivo_n_circ )



#   1.2.3 ÍNDICES DE IMOBILIZAÇÃO   ---------------------------------------------------------------

#    IATIM   --------------------------------------------------------------------------------------


iatim_2 <- merge(ativo_imob,  im_urb_renda, by=c("coenti","damesano"),all = TRUE)
iatim_2 <- merge(iatim_2,  im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_2 <- merge(iatim_2,  red_val_rec_im_urb, by=c("coenti","damesano"),all = TRUE)
iatim_2 <- merge(iatim_2,  red_val_rec_im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_2 <- merge(iatim_2,  deprec_im_urb, by=c("coenti","damesano"),all = TRUE)
iatim_2 <- merge(iatim_2,  deprec_im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_2 <- merge(iatim_2,  ativo_total, by=c("coenti","damesano"),all = TRUE)

iatim_2$indice <- (iatim_2$ativo_imob 
                   + iatim_2$im_urb_renda 
                   + iatim_2$im_rural 
                   - iatim_2$red_val_rec_im_urb 
                   - iatim_2$red_val_rec_im_rural 
                   - iatim_2$deprec_im_urb 
                   - iatim_2$deprec_im_rural )  / iatim_2$ativo_total 



#    IIMOB    -------------------------------------------------------------------------------------

iimob_2 <- merge(ativo_imob,  im_urb_renda, by=c("coenti","damesano"),all = TRUE)
iimob_2 <- merge(iimob_2,  im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_2 <- merge(iimob_2,  red_val_rec_im_urb, by=c("coenti","damesano"),all = TRUE)
iimob_2 <- merge(iimob_2,  red_val_rec_im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_2 <- merge(iimob_2,  deprec_im_urb, by=c("coenti","damesano"),all = TRUE)
iimob_2 <- merge(iimob_2,  deprec_im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_2 <- merge(iimob_2,  patr_liq, by=c("coenti","damesano"),all = TRUE)
iimob_2 <- merge(iimob_2,  patr_soc, by=c("coenti","damesano"),all = TRUE)

iimob_2$indice <- (iimob_2$ativo_imob +
                     iimob_2$im_urb_renda +
                     iimob_2$im_rural -
                     iimob_2$red_val_rec_im_urb -
                     iimob_2$red_val_rec_im_rural -
                     iimob_2$deprec_im_urb -
                     iimob_2$deprec_im_rural )  / ( iimob_2$patr_liq +
                                                      iimob_2$patr_soc)



#   1.2.5   ÍNDICES DE PARTICIPAÇÕES SOCIETÁRIAS   ------------------------------------------------


#   IPAS      -------------------------------------------------------------------------------------

ipas_2 <- merge(part_soc_fin,  part_soc_n_fin, by=c("coenti","damesano"),all = TRUE)
ipas_2 <- merge(ipas_2,  part_soc_fin_ext, by=c("coenti","damesano"),all = TRUE)
ipas_2 <- merge(ipas_2,  part_soc_n_fin_ext, by=c("coenti","damesano"),all = TRUE)
ipas_2 <- merge(ipas_2,  red_val_rec, by=c("coenti","damesano"),all = TRUE)
ipas_2 <- merge(ipas_2,  patr_liq, by=c("coenti","damesano"),all = TRUE)
ipas_2 <- merge(ipas_2,  patr_soc, by=c("coenti","damesano"),all = TRUE)

ipas_2$indice <- (  ipas_2$part_soc_fin +
                      ipas_2$part_soc_n_fin +
                      ipas_2$part_soc_fin_ext +
                      ipas_2$part_soc_n_fin_ext -
                      ipas_2$red_val_rec ) / ( ipas_2$patr_liq + 
                                                 ipas_2$patr_soc )


#    ILPL   ---------------------------------------------------------------------------------------

{
  # patr_liq <- incremental(3333, 'patr_liq' )
  
  patr_liq_2 <- incremental(3333,"patr_liq_2")
  
  patr_liq_2$ ano <- as.numeric(strftime(patr_liq_2$damesano, "%Y"))
  
  
  for (i in 1:nrow(patr_liq_2)) {
    
    if ( as.numeric(strftime(patr_liq_2$damesano[i], "%m")) == 12) {
      
      patr_liq_2$patr_liq_dez[i] <-patr_liq_2$patr_liq_2[i]
      
    }else{
      
      patr_liq_2$patr_liq_dez[i] <- NA
      
    }
    
  }
  
  patri_liq_aux <- subset(patr_liq_2, subset = as.numeric(strftime(damesano, "%m")) == 12)
  
  colnames(patri_liq_aux)[colnames(patri_liq_aux)      # Rename two variable names
                          %in% "patr_liq_2"] <- "patr_liq_dez_anterior"
  
  # patri_liq_aux$patr_liq_2 <- NULL
  
  patri_liq_aux$ano <- patri_liq_aux$ano + 1
  patri_liq_aux$patr_liq_dez <- NULL
  patri_liq_aux$damesano <- NULL
  
  patr_liq_2 <- merge(patr_liq_2,  patri_liq_aux, by=c("coenti","ano"),all = TRUE)
  patr_liq_2 <- patr_liq_2[order(patr_liq_2$coenti,  patr_liq_2$damesano), ]
  patr_liq_2$ano <- NULL
  
  ##xxxxxxxxxx  caso especial  xxxxxxxxxxxxxxxxxx
  
  patr_soc_2 <- incremental(6151,"patr_soc_2")
  
  
  patr_soc_2$ ano <- as.numeric(strftime(patr_soc_2$damesano, "%Y"))
  
  for (i in 1:nrow(patr_soc_2)) {
    
    if ( as.numeric(strftime(patr_soc_2$damesano[i], "%m")) == 12) {
      
      patr_soc_2$patr_soc_dez[i] <-patr_soc_2$patr_soc_2[i]
      
    }else{
      
      patr_soc_2$patr_soc_dez[i] <- NA
      
    }
    
  }
  
  patri_soc_aux <- subset(patr_soc_2, subset = as.numeric(strftime(damesano, "%m")) == 12)
  
  
  colnames(patri_soc_aux)[colnames(patri_soc_aux)      # Rename two variable names
                          %in% "patr_soc_2"] <- "patr_soc_dez_anterior"
  
  # patri_soc_aux$patr_soc_2 <- NULL
  head(patri_soc_aux)
  patri_soc_aux$ano <- patri_soc_aux$ano + 1
  head(patri_soc_aux)
  patri_soc_aux$patr_soc_dez <- NULL
  patri_soc_aux$damesano <- NULL
  
  head(patri_soc_aux)
  patr_soc_2 <- merge(patr_soc_2,  patri_soc_aux, by=c("coenti","ano"),all = TRUE)
  patr_soc_2 <- patr_soc_2[order(patr_soc_2$coenti,  patr_soc_2$damesano), ]
  patr_soc_2$ano <- NULL
  
  
} ##xxxxxxxxxx  caso especial  xxxxxxxxxxxxxxxxxx


ilpl_2 <- merge(lucro_liq,  patr_liq_2, by=c("coenti","damesano"),all = TRUE)
ilpl_2 <- merge(ilpl_2,  patr_soc_2, by=c("coenti","damesano"),all = TRUE)



ilpl_2$indice <- ilpl_2$lucro_liq /  (  ( ilpl_2$patr_liq_2 + ilpl_2$patr_liq_dez_anterior+
                                            ilpl_2$patr_soc_2 + ilpl_2$patr_soc_dez_anterior) / 2)

remove(patri_soc_aux)
remove(patr_soc_2)
remove(patr_liq_2)
remove(patri_liq_aux)


#    IREPLL ---------------------------------------------------------------------------------------

irepll_2 <- merge( rec_aj_inv_ctrl_col,  dsp_aj_inv_ctrl_col, by=c("coenti","damesano"),all = TRUE)
irepll_2 <- merge( irepll_2 ,  lucro_liq , by=c("coenti","damesano"),all = TRUE)

irepll_2$ indice <- (irepll_2$rec_aj_inv_ctrl_col - irepll_2$dsp_aj_inv_ctrl_col ) / irepll_2$lucro_liq


#     IGDF     ------------------------------------------------------------------------------------

igdf_2 <- merge( res_fin , lucro_liq , by=c("coenti","damesano"),all = TRUE)

igdf_2$indice <- igdf_2$res_fin / igdf_2$lucro_liq

#------------------------------------------------------------------------------------
remove(list = block_2$name)

###################################################################################################
#                              SOCIEDADES DE CAPITALIZAÇÃO
###################################################################################################


df_all <- df_all_3 

for (i in 1:nrow(block_3)) {
  class(eval(parse(text=block_3[i,1])))
}


#      Índices para Análise Econômico-Financeira das Supervisionadas     ---------------------------
# Necessaria as funções acu(), ver(), info(), indice_grafico()


#     1.3     SOCIEDADES DE CAPITALIZAÇÃO     -----------------------------------------------------
#     1.3.1     ÍNDICES DE CUSTOS     -------------------------------------------------------------

#     IDC     -------------------------------------------------------------------------------------

idc_3 <- merge(custo_aq_cap, rec_liq_tc, by=c("coenti","damesano"),all = TRUE)
idc_3 <- merge(idc_3 ,res_fin, by=c("coenti","damesano"),all = TRUE)


idc_3$indice <-  -1 * (idc_3$custo_aq_cap / (idc_3$rec_liq_tc + idc_3$res_fin))


#     IORDO     -----------------------------------------------------------------------------------

iordo_3 <- merge(out_rec_op_cap, rec_liq_tc, by=c("coenti","damesano"),all = TRUE)
iordo_3 <- merge(iordo_3 ,res_fin, by=c("coenti","damesano"),all = TRUE)


iordo_3$indice <- -1 * (iordo_3$out_rec_op_cap / ( iordo_3$rec_liq_tc +
                                                     iordo_3$res_fin   ))

#     IDA     -------------------------------------------------------------------------------------

ida_3 <- merge(dsp_adm,  dsp_trib, by=c("coenti","damesano"),all = TRUE)
ida_3 <- merge(ida_3,  rec_liq_tc, by=c("coenti","damesano"),all = TRUE)
ida_3 <- merge(ida_3,res_fin, by=c("coenti","damesano"),all = TRUE)

ida_3 $indice <- -1*( ida_3$dsp_adm +ida_3 $dsp_trib) / (  ida_3 $rec_liq_tc +
                                                             ida_3 $res_fin )


#     IRSORT     ----------------------------------------------------------------------------------

irsort_3 <- merge(result_sort,  rec_liq_tc, by=c("coenti","damesano"),all = TRUE)
irsort_3 <- merge(irsort_3,  res_fin, by=c("coenti","damesano"),all = TRUE)

irsort_3$indice <- -1 * (irsort_3$result_sort / (irsort_3$rec_liq_tc + irsort_3$res_fin))


#     ICC     -------------------------------------------------------------------------------------

icc_3 <- merge(custo_aq_cap,  out_rec_op_cap, by=c("coenti","damesano"),all = TRUE)
icc_3 <- merge(icc_3,  dsp_adm, by=c("coenti","damesano"),all = TRUE)
icc_3 <- merge(icc_3, dsp_trib, by=c("coenti","damesano"),all = TRUE)
icc_3 <- merge(icc_3, result_sort, by=c("coenti","damesano"),all = TRUE)
icc_3 <- merge(icc_3, rec_liq_tc, by=c("coenti","damesano"),all = TRUE)
icc_3 <- merge(icc_3,  res_fin, by=c("coenti","damesano"),all = TRUE)

icc_3 $indice <- -1*( icc_3$custo_aq_cap  +
                        icc_3$out_rec_op_cap +
                        icc_3$dsp_adm  + 
                        icc_3$dsp_trib +
                        icc_3$result_sort  ) / ( icc_3$rec_liq_tc +
                                                   icc_3$res_fin)



#     1.3.2     ÍNDICES DE LIQUIDEZ     --------------------------------------------------------------

#     ICL     ----------------------------------------------------------------------------------------

#todos incrementais

ilc_3 <- merge(ativo_circ,  custo_aq_dif_cp, by=c("coenti","damesano"),all = TRUE)
ilc_3 <- merge(ilc_3,  dsp_ant_cp, by=c("coenti","damesano"),all = TRUE)
ilc_3 <- merge(ilc_3,  pasivo_circ, by=c("coenti","damesano"),all = TRUE)


ilc_3$indice <- (ilc_3$ativo_circ - ilc_3$custo_aq_dif_cp - ilc_3$dsp_ant_cp)/ ilc_3$pasivo_circ



#     ILT      -------------------------------------------------------------------------------------------

ilt_3 <- merge(ativo_circ,  custo_aq_dif_cp, by=c("coenti","damesano"),all = TRUE)
ilt_3 <- merge(ilt_3,  dsp_ant_cp, by=c("coenti","damesano"),all = TRUE)
ilt_3 <- merge(ilt_3,  ativo_rlp, by=c("coenti","damesano"),all = TRUE)
ilt_3 <- merge(ilt_3,  custo_aq_diflp, by=c("coenti","damesano"),all = TRUE)
ilt_3 <- merge(ilt_3,  dsp_ant_lp, by=c("coenti","damesano"),all = TRUE)
ilt_3 <- merge(ilt_3,  pasivo_circ, by=c("coenti","damesano"),all = TRUE)
ilt_3 <- merge(ilt_3,  passivo_n_circ, by=c("coenti","damesano"),all = TRUE)


ilt_3$indice <-  ( ilt_3$ativo_circ 
                   - ilt_3$custo_aq_dif_cp 
                   - ilt_3$dsp_ant_cp 
                   + ilt_3$ativo_rlp 
                   - ilt_3$custo_aq_diflp 
                   - ilt_3$dsp_ant_lp  )  /  ( ilt_3$pasivo_circ 
                                               + ilt_3$passivo_n_circ )



#     1.3.3     ÍNDICES DE IMOBILIZAÇÃO     ---------------------------------------------------------------

#     IATIM     --------------------------------------------------------------------------------------

iatim_3 <- merge(ativo_imob,  im_urb_renda, by=c("coenti","damesano"),all = TRUE)
iatim_3 <- merge(iatim_3,  im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_3 <- merge(iatim_3,  red_val_rec_im_urb, by=c("coenti","damesano"),all = TRUE)
iatim_3 <- merge(iatim_3,  red_val_rec_im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_3 <- merge(iatim_3,  deprec_im_urb, by=c("coenti","damesano"),all = TRUE)
iatim_3 <- merge(iatim_3,  deprec_im_rural, by=c("coenti","damesano"),all = TRUE)
iatim_3 <- merge(iatim_3,  ativo_total, by=c("coenti","damesano"),all = TRUE)

iatim_3$indice <- (iatim_3$ativo_imob 
                   + iatim_3$im_urb_renda 
                   + iatim_3$im_rural 
                   - iatim_3$red_val_rec_im_urb 
                   - iatim_3$red_val_rec_im_rural 
                   - iatim_3$deprec_im_urb 
                   - iatim_3$deprec_im_rural )  / iatim_3$ativo_total 



#     IIMOB     -------------------------------------------------------------------------------------

iimob_3 <- merge(ativo_imob,  im_urb_renda, by=c("coenti","damesano"),all = TRUE)
iimob_3 <- merge(iimob_3,  im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_3 <- merge(iimob_3,  red_val_rec_im_urb, by=c("coenti","damesano"),all = TRUE)
iimob_3 <- merge(iimob_3,  red_val_rec_im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_3 <- merge(iimob_3,  deprec_im_urb, by=c("coenti","damesano"),all = TRUE)
iimob_3 <- merge(iimob_3,  deprec_im_rural, by=c("coenti","damesano"),all = TRUE)
iimob_3 <- merge(iimob_3,  patr_liq, by=c("coenti","damesano"),all = TRUE)


iimob_3$indice <- (  iimob_3$ativo_imob 
                     + iimob_3$im_urb_renda 
                     + iimob_3$im_rural 
                     - iimob_3$red_val_rec_im_urb 
                     - iimob_3$red_val_rec_im_rural 
                     - iimob_3$deprec_im_urb 
                     - iimob_3$deprec_im_rural )  / ( iimob_3$patr_liq )


#     1.3.5     ÍNDICES DE PARTICIPAÇÕES SOCIETÁRIAS     -----------------------------------------------


#     IPAS     -------------------------------------------------------------------------------------

ipas_3 <- merge(part_soc_fin,  part_soc_n_fin, by=c("coenti","damesano"),all = TRUE)
ipas_3 <- merge(ipas_3,  part_soc_fin_ext, by=c("coenti","damesano"),all = TRUE)
ipas_3 <- merge(ipas_3,  part_soc_n_fin_ext, by=c("coenti","damesano"),all = TRUE)
ipas_3 <- merge(ipas_3,  red_val_rec, by=c("coenti","damesano"),all = TRUE)
ipas_3 <- merge(ipas_3,  patr_liq, by=c("coenti","damesano"),all = TRUE)

ipas_3$indice <- (  ipas_3$part_soc_fin +
                      ipas_3$part_soc_n_fin +
                      ipas_3$part_soc_fin_ext +
                      ipas_3$part_soc_n_fin_ext -
                      ipas_3$red_val_rec ) / ( ipas_3$patr_liq  )



#     ILPL     ---------------------------------------------------------------------------------------

# patr_liq <- incremental(3333, 'patr_liq' )

patr_liq_3 <- incremental(3333,"patr_liq_3")

patr_liq_3$ ano <- as.numeric(strftime(patr_liq_3$damesano, "%Y"))

for (i in 1:nrow(patr_liq_3)) {
  
  if ( as.numeric(strftime(patr_liq_3$damesano[i], "%m")) == 12) {
    
    patr_liq_3$patr_liq_dez[i] <-patr_liq_3$patr_liq_3[i]
    
  }else{
    
    patr_liq_3$patr_liq_dez[i] <- NA
    
  }
  
}


patri_liq_aux <- subset(patr_liq_3, subset = as.numeric(strftime(damesano, "%m")) == 12)

colnames(patri_liq_aux)[colnames(patri_liq_aux)      # Rename two variable names
                        %in% "patr_liq_3"] <- "patr_liq_dez_anterior"

# patri_liq_aux$patr_liq_3 <- NULL

patri_liq_aux$ano <- patri_liq_aux$ano + 1
patri_liq_aux$patr_liq_dez <- NULL
patri_liq_aux$damesano <- NULL

patr_liq_3 <- merge(patr_liq_3,  patri_liq_aux, by=c("coenti","ano"),all = TRUE)
patr_liq_3 <- patr_liq_3[order(patr_liq_3$coenti,  patr_liq_3$damesano), ]
patr_liq_3$ano <- NULL

##xxxxxxxxxxxxxxxxxxxxxxxxxxxx



ilpl_3 <- merge(lucro_liq,  patr_liq_3, by=c("coenti","damesano"),all = TRUE)




ilpl_3$indice <- ilpl_3$lucro_liq /  ((ilpl_3$patr_liq_3
                                       + ilpl_3$patr_liq_dez_anterior) / 2)





remove(patr_liq_3)
remove(patri_liq_aux)


#    IREPLL ---------------------------------------------------------------------------------------

irepll_3 <- merge( rec_aj_inv_ctrl_col,  dsp_aj_inv_ctrl_col, by=c("coenti","damesano"),all = TRUE)
irepll_3 <- merge( irepll_3 ,  lucro_liq , by=c("coenti","damesano"),all = TRUE)

irepll_3$ indice <- (irepll_3$rec_aj_inv_ctrl_col - irepll_3$dsp_aj_inv_ctrl_col ) / irepll_3$lucro_liq


#   IGDF     --------------------------------------------------------------------------------------


igdf_3 <- merge( res_fin , lucro_liq , by=c("coenti","damesano"),all = TRUE)

igdf_3$indice <- igdf_3$res_fin / igdf_3$lucro_liq



remove(list = block_3$name)


############# Empilhamento de indices: GERAÇÃO DE OUTPUT  ############################################

{
  irets_1 = irets_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'irets')
  isr_1 = isr_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'isr')
  idc_1  = idc_1  %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'idc')
  iordo_1 = iordo_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iordo')
  irres_1 = irres_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'irres')
  ida_1 = ida_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ida')
  ic_1 = ic_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ic')
  ica_1 = ica_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ica')
  ilc_1 = ilc_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilc')
  ilt_1 = ilt_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilt')
  iatim_1 = iatim_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iatim')
  iimob_1 = iimob_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iimob')
  ipas_1 = ipas_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ipas')
  ilpl_1 = ilpl_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilpl')
  irepll_1 = irepll_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'irepll')
  igdf_1 = igdf_1 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'igdf')
  
  
  
  indices_01 <- dplyr:: bind_rows(irets_1,  isr_1,  idc_1,  iordo_1,  irres_1, ida_1, ic_1,  ica_1, ilc_1,  ilt_1,  iatim_1, iimob_1,  ipas_1, ilpl_1,  irepll_1,  igdf_1)
  
  remove(irets_1, isr_1,  idc_1,  iordo_1,  irres_1, ida_1,  ic_1,   ica_1, ilc_1,   ilt_1,  iatim_1,  iimob_1,  ipas_1,  ilpl_1,  irepll_1,  igdf_1)
  
}  # seguradora

{
  isr_2 = isr_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'isr')
  idc_2 = idc_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'idc')
  iordo_2 = iordo_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iordo')
  irres_2 = irres_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'irres')
  ida_2 = ida_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ida')
  ica_2 = ica_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ica')
  ilc_2 = ilc_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilc')
  ilt_2 = ilt_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilt')
  iatim_2 = iatim_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iatim')
  iimob_2 = iimob_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iimob')
  ipas_2 = ipas_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ipas')
  ilpl_2 = ilpl_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilpl')
  irepll_2 = irepll_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'irepll')
  igdf_2 = igdf_2 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'igdf')
  
  
  indices_02 <- dplyr:: bind_rows(isr_2, idc_2,  iordo_2, irres_2, ida_2, ica_2,ilc_2, ilt_2,  iatim_2, iimob_2,  ipas_2, ilpl_2,  irepll_2, igdf_2) 
  
  
  remove(isr_2, idc_2, iordo_2, irres_2, ida_2, ica_2, ilc_2, ilt_2,iatim_2,  iimob_2,  ipas_2,  ilpl_2,  irepll_2,   igdf_2)
  
} # previdencia complementar

{
  
  idc_3 = idc_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'idc')
  iordo_3 = iordo_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iordo')
  ida_3 = ida_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ida')
  irsort_3 = irsort_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'irsort')
  icc_3 = icc_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'icc')
  ilc_3 = ilc_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilc')
  ilt_3 = ilt_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilt')
  iatim_3 = iatim_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iatim')
  iimob_3 = iimob_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'iimob')
  ipas_3 = ipas_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ipas')
  ilpl_3 = ilpl_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'ilpl')
  irepll_3 = irepll_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'irepll')
  igdf_3 = igdf_3 %>% select(.,coenti, damesano, indice) %>% mutate( tipo = 'igdf')
  
  
  indices_03 <- dplyr:: bind_rows(idc_3, iordo_3, ida_3, irsort_3, icc_3,ilc_3, ilt_3, iatim_3, iimob_3,  ipas_3,ilpl_3,  irepll_3, igdf_3) 
  
  
  remove(idc_3, iordo_3,  ida_3, irsort_3, icc_3, ilc_3, ilt_3, iatim_3,iimob_3, ipas_3, ilpl_3, irepll_3, igdf_3)
  
}  # capitalização

#------------------------------------------------------------------------------------
remove(df_all,df_all_1,df_all_2,df_all_3)



# Gerando descrição dos indices para dashboard no power BI

ind = c('IRETS',
        'ISR',
        'IDC',
        'IORDO',
        'IRRES',
        'IDA',
        'IC',
        'ICA',
        'ILC',
        'ILT',
        'IATIM',
        'IIMOB',
        'IPAS',
        'ILPL',
        'IREPLL',
        'IGDF')

nome_indi = c('Índice de Retenção de Seguros',
              'Índice de Sinistralidade',
              'Índice de Custos de Aquisição',
              'Índice de Outras Receitas e Despesas Operacionais',
              'Índice de Resultado de Resseguro',
              'Índice de Despesas Administrativas',
              'Índice Combinado',
              'Índice Combinado Ampliado',
              'Índice de Liquidez Corrente',
              'Índice  de  Liquidez  Total',
              'Índice de Imobilização do Ativo',
              'Índice de Imobilização de Capitais Próprios',
              'Índice de Participações Societárias',
              'Índice  de Lucratividade  do  Patrimônio  Líquido',
              'Índice de Resultado de Equivalência Patrimonial sobre o Lucro Líquido',
              'Índice de Grau de Dependência Financeira') 

descricao =c('Afere, em termos percentuais, a representatividade dos prêmios retidos em relação ao total dos prêmios de seguros da Sociedade Seguradora.',
             'Aferea representatividade, em termos percentuais, dos ‘Sinistros Ocorridos’ e ‘Despesas com Benefícios’ daSeguradora em relação aos ‘Prêmios Ganhos’ e receitas com produtos em regime de capitalização.',
             'Afere a representatividade, em termos percentuais, dos ‘Custos de Aquisição’em relação aos ‘Prêmios Ganhos’ e receitascom produtos em regime de capitalização.',
             'Afere a representatividade, em termos percentuais, das ‘Outras Receitas e Despesas Operacionais’ em relação aos ‘Prêmios Ganhos’ e receitas com produtos em regime de capitalização.',
             'Afere a representatividade, em termos percentuais, do ‘Resultado com Resseguro’ em relação aos ‘Prêmios Ganhos’ e receitas com produtos em regime de capitalização.',
             'Afere a representatividade, em termos percentuais, das ‘Despesas Administrativas’ e ‘Despesas com Tributos’ em relação aos ‘Prêmios Ganhos’ e receitas com produtos em regime de capitalização.',
             'Afere a representatividade dos custosoperacionaistotais em relação aos ‘Prêmios Ganhos’ e receitas com produtos em regime de capitalização.',
             'Afere a representatividade dos custosoperacionaistotais em relação aos ‘Prêmios Ganhos’, receitas com produtos em regime de capitalização e Resultado Financeiro.',
             'Afere, em termos percentuais, aliquidez correnteda empresa, ou seja, se seus bens e direitos realizáveis nocurto prazo são suficientes para honrar seus compromissos de curto prazo.',
             'Afere, em termos percentuais, a liquidez total da empresa, ou seja, se seus bens e direitos realizáveis no curto e no longo prazo são suficientes para honrar seus compromissos de curto e longo prazo.',
             'Afere,   em   termos   percentuais,   a   representatividade   das imobilizações da empresa com relação ao seu ativo total.',
             'Afere, em termos percentuais, a representatividade das imobilizações da empresa com relação ao seu capital próprio.',
             'Afere o percentual do Patrimônio Líquido da Companhia que está aplicado em Participações Societárias.',
             'Afere, em termos percentuais,a lucratividade da empresa sobre a média do ‘Patrimônio Líquido’ no período considerado.',
             'Afere, em termos percentuais, a representatividade do Resultado de Equivalência Patrimonialsobre o Lucro Líquido do período.',
             'Afere, em termos percentuais, a representatividade do Resultado Financeiro sobre oLucro Líquido do período.')

ind_desc = data.frame(ind, nome_indi, descricao) ; remove(ind, nome_indi, descricao)



# Exportar para planilha    --------------------
# 
# 
# coenti_seguradoras$ramo= NULL
# coenti_seguradoras$CNPJ= NULL
# 
# 
# 
# indices_01 <- merge(indices_01, LISTAEMPRESAS, by="coenti", all.x = TRUE)
# 
# write.table(
#   indices_01,
#   file = 'C:/ ... /indices_01.csv',
#   sep = ';',
#   dec = '.',
#   row.names = FALSE
# )


