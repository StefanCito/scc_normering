# Dit script wordt gebruikt om de leerachterstanden en eventuele coronacorrectie voor de IRT1-normering uit te rekenen

# De output van dit script bestaat per toetsaanbieder uit:
# - Behaalde toetsadviezen
#   Overzichtstabel met percentages behaalde toetsadviezen aldus de coronacorrectie
#   bestandsnaam TOETS_toetsadviezen_irt1_coronacorrectie.csv

# Daarnaast worden twee bestanden weggeschreven:
# Overzicht landelijk behaalde en (indien nodig) gecorrigeerde toetsadviezen: coronacorrectie_irt1.csv
# Globale cesuren toetsadviezen coronacorrectie IRT1: cesuren_coronacorrectie_irt1.csv (alleen als correctie nodig is)

# De packages openxlsx, plyr en dplyr zijn noodzakelijk om dit script te kunnen draaien

# Invoer ==================================================

# Folder met alle databestanden
# Per toetsaanbieder wordt een bestand met leerlingdata (bestandsnaam TOETS_leerlingen.csv) verwacht
data_folder = 'dummy_data'

# Folder met resultaten uit IRT1-normering
# De volgende resultaatbestanden worden verwacht per toetsaanbieder:
# - Leerlingvaardigheden IRT1 (TOETS_vaardigheden.csv)
# - Toetsadviezen IRT1 (TOETS_toetsadviezen_irt1.csv)
# Deze folder wordt ook gebruikt voor de output van de coronacorrectie weg te schrijven
resultaten_folder = 'dummy_resultaten'

# Bestand met normeringsgegevens, bevat tabbladen toetsadvies_cesuren, coronanormering en modellen
normeringsgegevens_file = 'normeringsgegevens_dummy.xlsx'

# Code ====================================================

options(warn = 1)

# Mapping voor toetsadviezen
toetsadvies_mapping = list('1' = 'pro/vmbo bb', '2' = 'vmbo bb/kb', '3' = 'vmbo kb/gl-tl', '4' = 'vmbo gl-tl/havo', '5' = 'havo/vwo', '6' = 'vwo')

# Lees normeringsgegevens in
toetsadvies_cesuren = openxlsx::read.xlsx(normeringsgegevens_file, 'toetsadvies_cesuren')
streefnormering = openxlsx::read.xlsx(normeringsgegevens_file, 'coronanormering')
voorkeursmodellen = openxlsx::read.xlsx(normeringsgegevens_file, 'modellen')

streefnormering$toetsadvies = sapply(streefnormering$grenspunt, function(x) strsplit(x, split = ' -> ')[[1]][1])

# Deduceer lijst van toetsadviezen op volgorde uit toetsadvies-cesuren
toetsadviezen = unique(unlist(strsplit(toetsadvies_cesuren[order(toetsadvies_cesuren$cesuur_1pl), 'grenspunt'], split = ' -> ')))

# Inventariseer alle bestanden
ability_files = list.files(path = resultaten_folder, pattern = '_vaardigheden.csv')
toetsadvies_files = list.files(path = resultaten_folder, pattern = '_toetsadviezen_irt1.csv')

#### Coronacorrectie voor IRT1-normering

# Lees alle vaardigheden uit de IRT1-normering in
all_abilities = list()
aanbieders = NULL # Vector met aanbieders
for (ability_file in ability_files) {

  aanbieder = gsub('_vaardigheden.csv', '', ability_file, fixed = TRUE)
  aanbieders = c(aanbieders, aanbieder)

  aanbieder_abilities = read.csv2(file.path(resultaten_folder, ability_file))
  aanbieder_abilities = aanbieder_abilities[aanbieder_abilities[, 'schooltype'] == 1, c('glv_1pl', 'glv_2pl')] # Alleen regulier BO
  aanbieder_abilities$aanbieder = aanbieder
  all_abilities[[aanbieder]] = aanbieder_abilities
}
all_abilities = dplyr::bind_rows(all_abilities)

# Check of we alle aanbieders hebben om landelijk beeld te krijgen (verifieer nhv voorkeursmodellen)
if (!setequal(aanbieders, voorkeursmodellen$aanbieder)) {
  stop(paste0('Er ontbreken aanbieders in de beschikbare data, hierdoor kan geen compleet landelijk beeld geconstrueerd worden. Het gaat om:\n', paste(aanbieders[!aanbieders %in% voorkeursmodellen$aanbieder], collapse = '\n')))
}

# Pas cesuren IRT1-normering toe
for (model in c('1pl', '2pl')) {
  all_abilities[, paste0('toetsadvies_irt1_', model)] = as.character(cut(all_abilities[, paste0('glv_', model)], c(-Inf, sort(toetsadvies_cesuren[, paste0('cesuur_', model)]), Inf), right = FALSE, labels = toetsadviezen))
}

# Tel toetsadviezen IRT1-normering, gebruik voorkeursmodel van iedere aanbieder
all_toetsadviezen_irt1 = NULL
for (aanbieder in aanbieders) {
  voorkeursmodel = voorkeursmodellen[voorkeursmodellen[, 'aanbieder'] == aanbieder, 'voorkeursmodel']
  aanbieder_toetsadviezen = as.data.frame(table(all_abilities[all_abilities[, 'aanbieder'] == aanbieder, paste0('toetsadvies_irt1_', voorkeursmodel)]), stringsAsFactors = FALSE)
  colnames(aanbieder_toetsadviezen) = c('toetsadvies', 'n')
  all_toetsadviezen_irt1 = rbind(all_toetsadviezen_irt1, aanbieder_toetsadviezen)
}

# Bereken totale (cumulatieve) percentages
all_toetsadviezen_irt1 = plyr::ddply(all_toetsadviezen_irt1, 'toetsadvies', function(x) data.frame('n' = sum(x$n)))
all_toetsadviezen_irt1$perc = prop.table(all_toetsadviezen_irt1$n) * 100
all_toetsadviezen_irt1 = all_toetsadviezen_irt1[order(match(all_toetsadviezen_irt1$toetsadvies, toetsadviezen)), ]
all_toetsadviezen_irt1$cum_perc = cumsum(all_toetsadviezen_irt1$perc)
all_toetsadviezen_irt1 = dplyr::left_join(all_toetsadviezen_irt1, streefnormering[, c('toetsadvies', 'cum_2019', 'bovengrens')], by = 'toetsadvies')
all_toetsadviezen_irt1 = all_toetsadviezen_irt1[, colnames(all_toetsadviezen_irt1) != 'n']

# Bekijk of er overschrijdingen_irt1 van de bovengrenzen zijn
overschrijdingen_irt1 = all_toetsadviezen_irt1[!all_toetsadviezen_irt1$toetsadvies %in% c('pro/vmbo bb', 'vwo') & all_toetsadviezen_irt1[, 'cum_perc'] > all_toetsadviezen_irt1[, 'bovengrens'], c('toetsadvies', 'cum_2019', 'bovengrens', 'cum_perc')]

# Bereken indien nodig nieuwe cesuurpunten voor de coronanormering

if (nrow(overschrijdingen_irt1) > 0) {
  message(paste0('Er waren ', nrow(overschrijdingen_irt1), ' van de bovengrenzen voor de IRT1-normering overschreden, er zal een coronacorrectie toegepast moeten worden:\n', paste(capture.output(print(overschrijdingen_irt1, row.names = FALSE)), collapse = '\n')))

  cesuren_coronanormering_irt1 = NULL
  for (i in 1:nrow(overschrijdingen_irt1)) {
    streefpercentage = overschrijdingen_irt1[i, 'cum_2019']
    for (model in c('1pl', '2pl')) {
      model_glv = sort(all_abilities[, paste0('glv_', model)])
      target_row = ceiling(streefpercentage * 0.01 * length(model_glv))
      cesuur = model_glv[target_row]
      grenspunt = toetsadvies_cesuren[grepl(paste0('^', overschrijdingen_irt1[i, 'toetsadvies']), toetsadvies_cesuren[, 'grenspunt']), 'grenspunt']
      cesuren_coronanormering_irt1 = rbind(cesuren_coronanormering_irt1, data.frame('grenspunt' = grenspunt, 'model' = model, 'cesuur_correctie' = cesuur))
    }
  }

  cesuren_coronanormering_irt1 = reshape(cesuren_coronanormering_irt1, idvar = 'grenspunt', timevar = 'model', direction = 'wide', sep = '_')
  
  all_cesuren_irt1 = dplyr::left_join(toetsadvies_cesuren, cesuren_coronanormering_irt1, by = 'grenspunt')
  for (model in c('1pl', '2pl')) {
    all_cesuren_irt1[is.na(all_cesuren_irt1[, paste0('cesuur_correctie_', model)]), paste0('cesuur_correctie_', model)] = all_cesuren_irt1[is.na(all_cesuren_irt1[, paste0('cesuur_correctie_', model)]), paste0('cesuur_', model)]
  }

  # Schrijf nieuwe cesuurpunten weg
  write.csv2(all_cesuren_irt1[, colnames(all_cesuren_irt1) != 'cesuur_standaardscore'], file.path(resultaten_folder, 'cesuren_coronacorrectie_irt1.csv'), row.names = FALSE, quote = FALSE)

  # Pas nieuwe cesuurpunten toe op aanbiederdata
  all_toetsadviezen_gecorrigeerd = NULL
  for (aanbieder in aanbieders) {

    # Lees cesuren oorspronkelijke IRT1-normering in
    toetsadviezen_irt1 = read.csv2(file.path(resultaten_folder, toetsadvies_files[grepl(aanbieder, toetsadvies_files)]))

    # Vervang cesuur pro/bb met gecorrigeerde cesuur pro/bb-correctie
    aanbieder_cesuren = all_cesuren_irt1
    for (model in c('1pl', '2pl')) {
      aanbieder_cesuren[aanbieder_cesuren[, 'grenspunt'] == 'pro/vmbo bb -> vmbo bb/kb', paste0('cesuur_correctie_', model)] = toetsadviezen_irt1[toetsadviezen_irt1[, 'toetsadvies'] == 'pro/vmbo bb', paste0('cesuur_', model)]
    }

    # Pas gecorrigeerde cesuren toe
    aanbieder_toetsadviezen = NULL
    for (model in c('1pl', '2pl')) {
      toetsadviezen_cor = as.character(cut(all_abilities[, paste0('glv_', model)], c(-Inf, sort(aanbieder_cesuren[, paste0('cesuur_correctie_', model)]), Inf), right = FALSE, labels = toetsadviezen))

      # Bereken behaalde toetsadviezen
      toetsadviezen_behaald = as.data.frame(table(toetsadviezen_cor), stringsAsFactors = FALSE)
      colnames(toetsadviezen_behaald) = c('toetsadvies', 'n')
      toetsadviezen_behaald$perc_behaald = round(prop.table(toetsadviezen_behaald$n) * 100.0, 2)
      toetsadviezen_behaald$aanbieder = aanbieder
      toetsadviezen_behaald$model = model
      toetsadviezen_behaald = toetsadviezen_behaald[order(match(toetsadviezen_behaald$toetsadvies, toetsadviezen)), ]   

      aanbieder_toetsadviezen = rbind(aanbieder_toetsadviezen, toetsadviezen_behaald)
    }

    # Schrijf gecorrigeerde toetsadviezen van de aanbieder weg
    write.csv2(reshape(aanbieder_toetsadviezen[, !colnames(aanbieder_toetsadviezen) %in% c('n', 'aanbieder')], idvar = 'toetsadvies', timevar = 'model', direction = 'wide', sep = '_'),
              paste0(resultaten_folder, '/', aanbieder, '_toetsadviezen_irt1_coronacorrectie.csv'), row.names = FALSE, quote = FALSE)

    voorkeursmodel = voorkeursmodellen[voorkeursmodellen[, 'aanbieder'] == aanbieder, 'voorkeursmodel']

    all_toetsadviezen_gecorrigeerd = rbind(all_toetsadviezen_gecorrigeerd, aanbieder_toetsadviezen[aanbieder_toetsadviezen[, 'model'] == voorkeursmodel, c('aanbieder', 'toetsadvies', 'perc_behaald', 'n')])
  }

  # Overzichtstabel gecorrigeerde toetsadviezen
  all_toetsadviezen_gecorrigeerd = reshape(all_toetsadviezen_gecorrigeerd, idvar = 'toetsadvies', timevar = 'aanbieder', direction = 'wide', sep = '_')
  all_toetsadviezen_gecorrigeerd$n = rowSums(all_toetsadviezen_gecorrigeerd[, grepl('^n_', colnames(all_toetsadviezen_gecorrigeerd))])
  all_toetsadviezen_gecorrigeerd$perc_covid = prop.table(all_toetsadviezen_gecorrigeerd$n) * 100.0
  all_toetsadviezen_gecorrigeerd$cum_perc_covid = cumsum(all_toetsadviezen_gecorrigeerd$perc_covid)
  all_toetsadviezen_gecorrigeerd = all_toetsadviezen_gecorrigeerd[, !grepl('^n', colnames(all_toetsadviezen_gecorrigeerd))]
  colnames(all_toetsadviezen_gecorrigeerd) = gsub('behaald', 'covid', colnames(all_toetsadviezen_gecorrigeerd))

  all_toetsadviezen_irt1 = dplyr::left_join(all_toetsadviezen_irt1, all_toetsadviezen_gecorrigeerd, by = 'toetsadvies')

} else {
  message('Het was niet nodig om een coronacorrectie uit te voeren op de IRT1-normering')
}

all_toetsadviezen_irt1 = dplyr::mutate_if(all_toetsadviezen_irt1, is.numeric, round, digits = 2)
write.csv2(all_toetsadviezen_irt1, file.path(resultaten_folder, 'coronacorrectie_irt1.csv'), row.names = FALSE, quote = FALSE, na = '')
