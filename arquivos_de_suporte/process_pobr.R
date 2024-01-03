process_pobr = function (data, municipality_data = TRUE) 
{
  
  require(lubridate,quietly = TRUE)
  require(dplyr,quietly = TRUE)
  
  #load support tables
  load("arquivos_de_suporte/process_pobr.RData")
  
  #adding POBR data transformation
  
  variables_names <- names(data)
  if ("ANOMES_DIA" %in% variables_names){
    data$ANOMES_DIA = ymd(data$ANOMES_DIA, truncated = 1)
    data$ANOMES_DIA =  format(data$ANOMES_DIA, "%Y-%m")
  }
  if ("ANOMES_TRA" %in% variables_names){
    data$ANOMES_TRA = ymd(data$ANOMES_TRA, truncated = 1)
    data$ANOMES_TRA =  format(data$ANOMES_TRA, "%Y-%m")
  }
  if ("UF_RESID" %in% variables_names) {
    data$UF_RESID[data$UF_RESID == '11'] <- 'Rondonia'
    data$UF_RESID[data$UF_RESID == '12'] <- 'Acre'
    data$UF_RESID[data$UF_RESID == '13'] <- 'Amazonas'
    data$UF_RESID[data$UF_RESID == '14'] <- 'Roraima'
    data$UF_RESID[data$UF_RESID == '15'] <- 'Para'
    data$UF_RESID[data$UF_RESID == '16'] <- 'Amapa'
    data$UF_RESID[data$UF_RESID == '17'] <- 'Tocantins'
    data$UF_RESID[data$UF_RESID == '21'] <- 'Maranhão'
    data$UF_RESID[data$UF_RESID == '22'] <- 'Piaui'
    data$UF_RESID[data$UF_RESID == '23'] <- 'Ceara'
    data$UF_RESID[data$UF_RESID == '24'] <- 'Rio Grande do Norte'
    data$UF_RESID[data$UF_RESID == '25'] <- 'Paraiba'
    data$UF_RESID[data$UF_RESID == '26'] <- 'Pernambuco'
    data$UF_RESID[data$UF_RESID == '27'] <- 'Alagoas'
    data$UF_RESID[data$UF_RESID == '28'] <- 'Sergipe'
    data$UF_RESID[data$UF_RESID == '29'] <- 'Bahia'
    data$UF_RESID[data$UF_RESID == '31'] <- 'Minas Gerais'
    data$UF_RESID[data$UF_RESID == '32'] <- 'Espirito Santo'
    data$UF_RESID[data$UF_RESID == '33'] <- 'Rio de Janeiro'
    data$UF_RESID[data$UF_RESID == '35'] <- 'Sao Paulo'
    data$UF_RESID[data$UF_RESID == '41'] <- 'Parana'
    data$UF_RESID[data$UF_RESID == '42'] <- 'Santa Catarina'
    data$UF_RESID[data$UF_RESID == '43'] <- 'Rio Grande do Sul'
    data$UF_RESID[data$UF_RESID == '50'] <- 'Mato Grosso do Sul'
    data$UF_RESID[data$UF_RESID == '51'] <- 'Mato Grosso'
    data$UF_RESID[data$UF_RESID == '52'] <- 'Goias'
    data$UF_RESID[data$UF_RESID == '53'] <- 'Distrito Federal'
    data$UF_RESID[data$UF_RESID == '0'] <- 'Ignorado/exterior'
  }
  if ("MUN_RESID" %in% variables_names & municipality_data == TRUE) {
    colnames(tabMun)[1] <- "MUN_RESID"
    colnames(tabMun)[4] <- "MUN_RESID_NOME"    
    colnames(tabMun)[6] <- "MUN_RESID_LAT"
    colnames(tabMun)[7] <- "MUN_RESID_LON"
    colnames(tabMun)[8] <- "MUN_RESID_ALT"
    tabMun$MUN_RESID <- as.character(tabMun$MUN_RESID)
    data <- dplyr::left_join(data, tabMun[,c(1,4,6,7,8)], by = "MUN_RESID")
  }
  if ("UF_TRATAM" %in% variables_names) {
    data$UF_TRATAM[data$UF_TRATAM == '11'] <- 'Rondonia'
    data$UF_TRATAM[data$UF_TRATAM == '12'] <- 'Acre'
    data$UF_TRATAM[data$UF_TRATAM == '13'] <- 'Amazonas'
    data$UF_TRATAM[data$UF_TRATAM == '14'] <- 'Roraima'
    data$UF_TRATAM[data$UF_TRATAM == '15'] <- 'Para'
    data$UF_TRATAM[data$UF_TRATAM == '16'] <- 'Amapa'
    data$UF_TRATAM[data$UF_TRATAM == '17'] <- 'Tocantins'
    data$UF_TRATAM[data$UF_TRATAM == '21'] <- 'Maranhão'
    data$UF_TRATAM[data$UF_TRATAM == '22'] <- 'Piaui'
    data$UF_TRATAM[data$UF_TRATAM == '23'] <- 'Ceara'
    data$UF_TRATAM[data$UF_TRATAM == '24'] <- 'Rio Grande do Norte'
    data$UF_TRATAM[data$UF_TRATAM == '25'] <- 'Paraiba'
    data$UF_TRATAM[data$UF_TRATAM == '26'] <- 'Pernambuco'
    data$UF_TRATAM[data$UF_TRATAM == '27'] <- 'Alagoas'
    data$UF_TRATAM[data$UF_TRATAM == '28'] <- 'Sergipe'
    data$UF_TRATAM[data$UF_TRATAM == '29'] <- 'Bahia'
    data$UF_TRATAM[data$UF_TRATAM == '31'] <- 'Minas Gerais'
    data$UF_TRATAM[data$UF_TRATAM == '32'] <- 'Espirito Santo'
    data$UF_TRATAM[data$UF_TRATAM == '33'] <- 'Rio de Janeiro'
    data$UF_TRATAM[data$UF_TRATAM == '35'] <- 'Sao Paulo'
    data$UF_TRATAM[data$UF_TRATAM == '41'] <- 'Parana'
    data$UF_TRATAM[data$UF_TRATAM == '42'] <- 'Santa Catarina'
    data$UF_TRATAM[data$UF_TRATAM == '43'] <- 'Rio Grande do Sul'
    data$UF_TRATAM[data$UF_TRATAM == '50'] <- 'Mato Grosso do Sul'
    data$UF_TRATAM[data$UF_TRATAM == '51'] <- 'Mato Grosso'
    data$UF_TRATAM[data$UF_TRATAM == '52'] <- 'Goias'
    data$UF_TRATAM[data$UF_TRATAM == '53'] <- 'Distrito Federal'
    data$UF_TRATAM[data$UF_TRATAM == '0'] <- 'Ignorado/exterior'
  }
  if ("MUN_TRATAM" %in% variables_names & municipality_data == TRUE) {
    colnames(tabMun)[1] <- "MUN_TRATAM"
    colnames(tabMun)[4] <- "MUN_TRATAM_NOME"    
    colnames(tabMun)[6] <- "MUN_TRATAM_LAT"
    colnames(tabMun)[7] <- "MUN_TRATAM_LON"
    colnames(tabMun)[8] <- "MUN_TRATAM_ALT"
    tabMun$MUN_TRATAM <- as.character(tabMun$MUN_TRATAM)
    data <- dplyr::left_join(data, tabMun[,c(1,4,6,7,8)], by = "MUN_TRATAM")
  }
  if ("UF_DIAGN" %in% variables_names) {
    data$UF_DIAGN[data$UF_DIAGN == '11'] <- 'Rondonia'
    data$UF_DIAGN[data$UF_DIAGN == '12'] <- 'Acre'
    data$UF_DIAGN[data$UF_DIAGN == '13'] <- 'Amazonas'
    data$UF_DIAGN[data$UF_DIAGN == '14'] <- 'Roraima'
    data$UF_DIAGN[data$UF_DIAGN == '15'] <- 'Para'
    data$UF_DIAGN[data$UF_DIAGN == '16'] <- 'Amapa'
    data$UF_DIAGN[data$UF_DIAGN == '17'] <- 'Tocantins'
    data$UF_DIAGN[data$UF_DIAGN == '21'] <- 'Maranhão'
    data$UF_DIAGN[data$UF_DIAGN == '22'] <- 'Piaui'
    data$UF_DIAGN[data$UF_DIAGN == '23'] <- 'Ceara'
    data$UF_DIAGN[data$UF_DIAGN == '24'] <- 'Rio Grande do Norte'
    data$UF_DIAGN[data$UF_DIAGN == '25'] <- 'Paraiba'
    data$UF_DIAGN[data$UF_DIAGN == '26'] <- 'Pernambuco'
    data$UF_DIAGN[data$UF_DIAGN == '27'] <- 'Alagoas'
    data$UF_DIAGN[data$UF_DIAGN == '28'] <- 'Sergipe'
    data$UF_DIAGN[data$UF_DIAGN == '29'] <- 'Bahia'
    data$UF_DIAGN[data$UF_DIAGN == '31'] <- 'Minas Gerais'
    data$UF_DIAGN[data$UF_DIAGN == '32'] <- 'Espirito Santo'
    data$UF_DIAGN[data$UF_DIAGN == '33'] <- 'Rio de Janeiro'
    data$UF_DIAGN[data$UF_DIAGN == '35'] <- 'Sao Paulo'
    data$UF_DIAGN[data$UF_DIAGN == '41'] <- 'Parana'
    data$UF_DIAGN[data$UF_DIAGN == '42'] <- 'Santa Catarina'
    data$UF_DIAGN[data$UF_DIAGN == '43'] <- 'Rio Grande do Sul'
    data$UF_DIAGN[data$UF_DIAGN == '50'] <- 'Mato Grosso do Sul'
    data$UF_DIAGN[data$UF_DIAGN == '51'] <- 'Mato Grosso'
    data$UF_DIAGN[data$UF_DIAGN == '52'] <- 'Goias'
    data$UF_DIAGN[data$UF_DIAGN == '53'] <- 'Distrito Federal'
    data$UF_DIAGN[data$UF_DIAGN == '0'] <- 'Ignorado/exterior'
  }
  if ("MUN_DIAG" %in% variables_names & municipality_data == TRUE) {
    colnames(tabMun)[1] <- "MUN_DIAG"
    colnames(tabMun)[4] <- "MUN_DIAG_NOME"    
    colnames(tabMun)[6] <- "MUN_DIAG_LAT"
    colnames(tabMun)[7] <- "MUN_DIAG_LON"
    colnames(tabMun)[8] <- "MUN_DIAG_ALT"
    tabMun$MUN_DIAG <- as.character(tabMun$MUN_DIAG)
    data <- dplyr::left_join(data, tabMun[,c(1,4,6,7,8)], by = "MUN_DIAG")
  }
  if ("TRATAMENTO" %in% variables_names) {
    data$TRATAMENTO[data$TRATAMENTO == 1] <- "Cirurgia"
    data$TRATAMENTO[data$TRATAMENTO == 2] <- "Quimioterapia"
    data$TRATAMENTO[data$TRATAMENTO == 3] <- "Radioterapia"
    data$TRATAMENTO[data$TRATAMENTO == 4] <- "Ambos"
    data$TRATAMENTO[data$TRATAMENTO == 5] <- "Sem informação de tratamento"
  }
  if ("DIAGNOSTIC" %in% variables_names) {
    data$DIAGNOSTIC[data$DIAGNOSTIC == '01'] <- 'Neoplasias Malignas (Lei no 12.732/12)'
    data$DIAGNOSTIC[data$DIAGNOSTIC == '02'] <- 'Neoplasias in situ'
    data$DIAGNOSTIC[data$DIAGNOSTIC == '03'] <- 'Neoplasias de comportamento incerto ou desconhecido'
    data$DIAGNOSTIC[data$DIAGNOSTIC == '04'] <- 'C44+C73'
  }
  if ("IDADE" %in% variables_names) {
    data$IDADE[data$IDADE == '000'] <- '<1'
    data$IDADE[data$IDADE == '001'] <- '1'
    data$IDADE[data$IDADE == '002'] <- '2'
    data$IDADE[data$IDADE == '003'] <- '3'
    data$IDADE[data$IDADE == '004'] <- '4'
    data$IDADE[data$IDADE == '005'] <- '5'
    data$IDADE[data$IDADE == '006'] <- '6'
    data$IDADE[data$IDADE == '007'] <- '7'
    data$IDADE[data$IDADE == '008'] <- '8'
    data$IDADE[data$IDADE == '009'] <- '9'
    data$IDADE[data$IDADE == '010'] <- '10'
    data$IDADE[data$IDADE == '011'] <- '11'
    data$IDADE[data$IDADE == '012'] <- '12'
    data$IDADE[data$IDADE == '013'] <- '13'
    data$IDADE[data$IDADE == '014'] <- '14'
    data$IDADE[data$IDADE == '015'] <- '15'
    data$IDADE[data$IDADE == '016'] <- '16'
    data$IDADE[data$IDADE == '017'] <- '17'
    data$IDADE[data$IDADE == '018'] <- '18'
    data$IDADE[data$IDADE == '019'] <- '19'
    data$IDADE[data$IDADE == '020'] <- '20'
    data$IDADE[data$IDADE == '021'] <- '21'
    data$IDADE[data$IDADE == '022'] <- '22'
    data$IDADE[data$IDADE == '023'] <- '23'
    data$IDADE[data$IDADE == '024'] <- '24'
    data$IDADE[data$IDADE == '025'] <- '25'
    data$IDADE[data$IDADE == '026'] <- '26'
    data$IDADE[data$IDADE == '027'] <- '27'
    data$IDADE[data$IDADE == '028'] <- '28'
    data$IDADE[data$IDADE == '029'] <- '29'
    data$IDADE[data$IDADE == '030'] <- '30'
    data$IDADE[data$IDADE == '031'] <- '31'
    data$IDADE[data$IDADE == '032'] <- '32'
    data$IDADE[data$IDADE == '033'] <- '33'
    data$IDADE[data$IDADE == '034'] <- '34'
    data$IDADE[data$IDADE == '035'] <- '35'
    data$IDADE[data$IDADE == '036'] <- '36'
    data$IDADE[data$IDADE == '037'] <- '37'
    data$IDADE[data$IDADE == '038'] <- '38'
    data$IDADE[data$IDADE == '039'] <- '39'
    data$IDADE[data$IDADE == '040'] <- '40'
    data$IDADE[data$IDADE == '041'] <- '41'
    data$IDADE[data$IDADE == '042'] <- '42'
    data$IDADE[data$IDADE == '043'] <- '43'
    data$IDADE[data$IDADE == '044'] <- '44'
    data$IDADE[data$IDADE == '045'] <- '45'
    data$IDADE[data$IDADE == '046'] <- '46'
    data$IDADE[data$IDADE == '047'] <- '47'
    data$IDADE[data$IDADE == '048'] <- '48'
    data$IDADE[data$IDADE == '049'] <- '49'
    data$IDADE[data$IDADE == '050'] <- '50'
    data$IDADE[data$IDADE == '051'] <- '51'
    data$IDADE[data$IDADE == '052'] <- '52'
    data$IDADE[data$IDADE == '053'] <- '53'
    data$IDADE[data$IDADE == '054'] <- '54'
    data$IDADE[data$IDADE == '055'] <- '55'
    data$IDADE[data$IDADE == '056'] <- '56'
    data$IDADE[data$IDADE == '057'] <- '57'
    data$IDADE[data$IDADE == '058'] <- '58'
    data$IDADE[data$IDADE == '059'] <- '59'
    data$IDADE[data$IDADE == '060'] <- '60'
    data$IDADE[data$IDADE == '061'] <- '61'
    data$IDADE[data$IDADE == '062'] <- '62'
    data$IDADE[data$IDADE == '063'] <- '63'
    data$IDADE[data$IDADE == '064'] <- '64'
    data$IDADE[data$IDADE == '065'] <- '65'
    data$IDADE[data$IDADE == '066'] <- '66'
    data$IDADE[data$IDADE == '067'] <- '67'
    data$IDADE[data$IDADE == '068'] <- '68'
    data$IDADE[data$IDADE == '069'] <- '69'
    data$IDADE[data$IDADE == '070'] <- '70'
    data$IDADE[data$IDADE == '071'] <- '71'
    data$IDADE[data$IDADE == '072'] <- '72'
    data$IDADE[data$IDADE == '073'] <- '73'
    data$IDADE[data$IDADE == '074'] <- '74'
    data$IDADE[data$IDADE == '075'] <- '75'
    data$IDADE[data$IDADE == '076'] <- '76'
    data$IDADE[data$IDADE == '077'] <- '77'
    data$IDADE[data$IDADE == '078'] <- '78'
    data$IDADE[data$IDADE == '079'] <- '79'
    data$IDADE[data$IDADE == '080'] <- '80'
    data$IDADE[data$IDADE == '081'] <- '81'
    data$IDADE[data$IDADE == '082'] <- '82'
    data$IDADE[data$IDADE == '083'] <- '83'
    data$IDADE[data$IDADE == '084'] <- '84'
    data$IDADE[data$IDADE == '085'] <- '85'
    data$IDADE[data$IDADE == '086'] <- '86'
    data$IDADE[data$IDADE == '087'] <- '87'
    data$IDADE[data$IDADE == '088'] <- '88'
    data$IDADE[data$IDADE == '089'] <- '89'
    data$IDADE[data$IDADE == '090'] <- '90'
    data$IDADE[data$IDADE == '091'] <- '91'
    data$IDADE[data$IDADE == '092'] <- '92'
    data$IDADE[data$IDADE == '093'] <- '93'
    data$IDADE[data$IDADE == '094'] <- '94'
    data$IDADE[data$IDADE == '095'] <- '95'
    data$IDADE[data$IDADE == '096'] <- '96'
    data$IDADE[data$IDADE == '097'] <- '97'
    data$IDADE[data$IDADE == '098'] <- '98'
    data$IDADE[data$IDADE == '099'] <- '99'
    data$IDADE[data$IDADE == '100'] <- '100'
    data$IDADE[data$IDADE == '101'] <- '101'
    data$IDADE[data$IDADE == '102'] <- '102'
    data$IDADE[data$IDADE == '103'] <- '103'
    data$IDADE[data$IDADE == '104'] <- '104'
    data$IDADE[data$IDADE == '105'] <- '105'
    data$IDADE[data$IDADE == '106'] <- '106'
    data$IDADE[data$IDADE == '107'] <- '107'
    data$IDADE[data$IDADE == '108'] <- '108'
    data$IDADE[data$IDADE == '109'] <- '109'
    data$IDADE[data$IDADE == '110'] <- '110'
    data$IDADE[data$IDADE == '111'] <- '111'
    data$IDADE[data$IDADE == '112'] <- '112'
    data$IDADE[data$IDADE == '113'] <- '113'
    data$IDADE[data$IDADE == '114'] <- '114'
    data$IDADE[data$IDADE == '115'] <- '115'
    data$IDADE[data$IDADE == '116'] <- '116'
    data$IDADE[data$IDADE == '117'] <- '117'
    data$IDADE[data$IDADE == '118'] <- '118'
    data$IDADE[data$IDADE == '119'] <- '119'
    data$IDADE[data$IDADE == '120'] <- '120'
    data$IDADE[data$IDADE == '121'] <- '121'
    data$IDADE[data$IDADE == '122'] <- '122'
    data$IDADE[data$IDADE == '123'] <- '123'
    data$IDADE[data$IDADE == '124'] <- '124'
    data$IDADE[data$IDADE == '125'] <- '125'
    data$IDADE[data$IDADE == '126'] <- '126'
    data$IDADE[data$IDADE == '127'] <- '127'
    data$IDADE[data$IDADE == '128'] <- '128'
    data$IDADE[data$IDADE == '129'] <- '129'
    data$IDADE[data$IDADE == '130'] <- '130'
    data$IDADE[data$IDADE == '999'] <- NA
}
  if ("SEXO" %in% variables_names) {
    data$SEXO[data$SEXO == "0"] <- NA
    data$SEXO[data$SEXO == "9"] <- NA
    data$SEXO[data$SEXO == "1"] <- "M"
    data$SEXO[data$SEXO == "2"] <- "F"
  }
  if ("ESTADIAM" %in% variables_names) {
    data$ESTADIAM[data$ESTADIAM == 0] <- "0"
    data$ESTADIAM[data$ESTADIAM == 1] <- "1"
    data$ESTADIAM[data$ESTADIAM == 2] <- "2"
    data$ESTADIAM[data$ESTADIAM == 3] <- "3"
    data$ESTADIAM[data$ESTADIAM == 4] <- "4"
    data$ESTADIAM[data$ESTADIAM == 5] <- "5"
    data$ESTADIAM[data$ESTADIAM == 9] <- "Ignorado"

    data$ESTADIAM <- factor(data$ESTADIAM)
  }
  if ("CNES_DIAG" %in% variables_names) {
    colnames(cnes)[1] = "CNES_DIAG"
    colnames(cnes)[2] = "CNES_DIAG_NOME"
    cnes$CNES_DIAG <- as.character(cnes$CNES_DIAG)
    data <- dplyr::left_join(data, cnes, by = "CNES_DIAG")
  }
  if ("CNES_TRAT" %in% variables_names) {
    colnames(cnes)[1] = "CNES_TRAT"
    colnames(cnes)[2] = "CNES_TRAT_NOME"
    cnes$CNES_TRAT <- as.character(cnes$CNES_TRAT)
    data <- dplyr::left_join(data, cnes, by = "CNES_TRAT")
  }
  if ("TEMPO_TRAT" %in% variables_names) {
    data$TEMPO_TRAT = as.numeric(data$TEMPO_TRAT)
    data$TEMPO_TRAT[data$TEMPO_TRAT == 99999] <- NA
  }
  if ("DIAG_DETH" %in% variables_names) {
    data <- dplyr::left_join(data, cid, by = "DIAG_DETH")
  }
  if ("DT_DIAG" %in% variables_names) {
    data$DT_DIAG = as.Date(data$DT_DIAG, tryFormats = c("%d/%m/%Y"))
  }
  if ("DT_TRAT" %in% variables_names) {
    data$DT_TRAT = as.Date(data$DT_TRAT, tryFormats = c("%d/%m/%Y"))
  }
  if ("DT_NASC" %in% variables_names) {
    data$DT_NASC = as.Date(data$DT_NASC, tryFormats = c("%d/%m/%Y"))
  }
  data = data %>%
    mutate(IDADE_DIAG = DT_DIAG - DT_NASC, IDADE_DIAG) %>%
    mutate(IDADE_DIAG = as.integer(IDADE_DIAG) %/% 365.25) %>%
    relocate(IDADE_DIAG, .after = "DT_DIAG")

  #finishing data transformation
  return(data)
}
