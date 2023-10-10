# Dit script wordt gebruikt om de IRT-normering uit te voeren voor een of meerdere toetsaanbieders

# De output van dit script bestaat per toetsaanbieder uit:
# - Uitgeschakelde items
#   Lijst van items die uitgeschakeld zijn omdat ze een discriminatieparameter lager dan 0.1 hebben
#   bestandsnaam TOETS_uitgeschakeld.csv
# - itemparameters
#   Parameterschattingen in het 2PL-model
#   bestandsnaam TOETS_itemparameters.csv
# - (MML-)populatieparameters
#   Populatieparameters in het 2PL-model
#   bestandsnaam TOETS_populatieparameters.csv
# - Vaardigheidsschattingen
#   Voor alle leerlingen vaardigheidsschattingen, GLV's en toetsadviezen
#   bestandsnaam TOETS_vaardigheden.csv
# - Behaalde referentieniveaus
#   Overzichtstabel met percentages behaalde referentieniveaus
#   bestandsnaam TOETS_referentieniveaus.csv
# - Behaalde toetsadviezen
#   Overzichtstabel met percentages behaalde toetsadviezen en cesuren
#   bestandsnaam TOETS_toetsadviezen.csv
# Daarnaast worden er twee bestanden weggeschreven met overzichtstabellen met alle aanbieders met behaalde referentieniveaus en
# behaalde toetsadviezen, met bestandsnamen overzicht_referentieniveaus.csv en overzicht_toetsadviezen.csv
# Tot slot is er een Excelbestand overzicht_correlatiematrices.xlsx met correlatiematrices van alle onderdelen voor alle toetsaanbieders

# De volgende R-packages worden gebruikt in dit script:
# dexterMML (https://github.com/dexter-psychometrics/dexterMML), openxlsx, reshape2, plyr en dplyr

# Invoer ==================================================

# Folder met alle databestanden
# De volgende databestanden worden verwacht per toetsaanbieder:
# - Leerlingdata (TOETS_leerlingen.csv)
# - Scoredata per onderdeel (TOETS_ONDERDEEL.csv)
# - Optioneel noscore-items (TOETS_noscore.csv)
data_folder = 'dummy_data'

# Aanbieders waar we rekening houden met verschillende MML-populaties
populatie_aanbieders = NULL

# Bestand met bestaande gezamenlijk ankerparameters (uitvoer van kalibreer_anker.R)
anker_file = 'dummy_resultaten/ankerparameters_2024.csv'

# Bestand met uitgeschakelde en losgekoppelde ankeritems, bevat kolommen aanbieder, item_id, onderdeel en actie (los of uit)
items_off_file = 'anker_off.csv'

# Bestand met cesuren en onderdeelgewichten, bevat tabbladen ref_cesuren, toetsadvies_cesuren en onderdeelgewichten
normeringsgegevens_file = 'normeringsgegevens_dummy.xlsx'

# Output folder
output_folder = 'dummy_resultaten'

# Code ====================================================

options(warn = 1)
options(width = 200)

# Inventariseer alle bestanden
leerling_files = list.files(path = data_folder, pattern = '_leerlingen.csv')
onderdeel_files = list.files(path = data_folder, pattern = '.csv')
onderdeel_files = onderdeel_files[!grepl('leerlingen|controle|representiviteit|representativiteit', onderdeel_files)]
noscore_files = list.files(path = data_folder, pattern = '_noscore.dat')

# Lees parameters gezamenlijk anker in
anker_parameters = read.csv2(anker_file)

items_off = read.csv2(items_off_file)

# Check op items_off
if (!all(items_off$actie %in% c('los', 'uit'))) {
  warning(paste0('Niet alle acties in het bestand met uitgeschakelde items zijn gelijk aan "los" of "uit"'))
}

# Lees normeringsgegevens in
ref_cesuren = openxlsx::read.xlsx(normeringsgegevens_file, 'ref_cesuren')
toetsadvies_cesuren = openxlsx::read.xlsx(normeringsgegevens_file, 'toetsadvies_cesuren')
toetsadvies_cesuren = toetsadvies_cesuren[, colnames(toetsadvies_cesuren) != 'cesuur_standaardscore']
onderdeelgewichten = openxlsx::read.xlsx(normeringsgegevens_file, 'onderdeelgewichten')
onderdeelgewichten = onderdeelgewichten[names(onderdeelgewichten) != 'totaal']

# Deduceer lijst van toetsadviezen op volgorde uit toetsadvies-cesuren
toetsadviezen = unique(unlist(strsplit(toetsadvies_cesuren[order(toetsadvies_cesuren$cesuur), 'grenspunt'], split = ' -> ')))

summary_refs = NULL
summary_toetsadviezen = NULL
wb = openxlsx::createWorkbook() # workbook voor correlaties
for (leerling_file in leerling_files) {

  aanbieder = gsub('_leerlingen.csv', '', leerling_file, fixed = TRUE)

  message(paste0('Bezig met aanbieder ', aanbieder, '...'))

  # Lees leerlingenbestand in
  leerlingen = read.csv2(file.path(data_folder, leerling_file))
  leerlingen$person_id = as.character(leerlingen$person_id) # Anders gaan joins met abilities uit dexterMML verkeerd

  # Als er geen MML-populaties zijn schrijven we hier de aanbiedernaam als placeholder
  if (!aanbieder %in% populatie_aanbieders) {
    leerlingen$populatie = aanbieder
  } 
  leerlingen = leerlingen[, c('person_id', 'schooltype', 'populatie', 'schooladvies')]

  # Lees noscore-items in indien nodig
  noscore_items = NULL
  if (length(noscore_files[grepl(aanbieder, noscore_files)]) > 0) {
    noscore_items = readLines(file.path(data_folder, noscore_files[grepl(aanbieder, noscore_files)]))
  }

  # Sheet voor correlaties
  openxlsx::addWorksheet(wb, aanbieder)
  wb_rows = 1

  all_abilities = NULL
  all_removed_items = NULL
  aanbieder_parameters = list()
  aanbieder_populations = list()
  # Loop over ieder onderdeel om de kalibratie uit te voeren
  for (onderdeel_file in onderdeel_files[grepl(aanbieder, onderdeel_files)]) {

    onderdeel = gsub(paste0(aanbieder, '_|\\.csv'), '', onderdeel_file)

    message(paste0('  Bezig met onderdeel ', onderdeel, '...'))

    # Lees onderdeelscores in
    score_data = read.csv2(file.path(data_folder, onderdeel_file), check.names = FALSE)
    score_data$person_id = as.character(score_data$person_id) # Anders gaan joins met abilities uit dexterMML verkeerd

    # Maak long-format van wide-format data indien nodig
    if (!'item_id' %in% colnames(score_data)) {
      score_data = reshape2::melt(score_data, id.vars = 'person_id', value.name = 'item_score')
      colnames(score_data)[colnames(score_data) == 'variable'] = 'item_id'
      score_data$item_id = as.character(score_data$item_id)
    }

    # Verwijder missings
    score_data = score_data[!is.na(score_data$item_score) & score_data$item_score != 9, ]

    # Verwijder uitgeschakelde ankeritems
    score_data = score_data[!score_data[, 'item_id'] %in% items_off[items_off[, 'onderdeel'] == onderdeel & items_off[, 'aanbieder'] == aanbieder & items_off[, 'actie'] == 'uit', 'item_id'], ]

    # Koppel eventueel ook ankeritems los
    for (item_id in items_off[items_off[, 'onderdeel'] == onderdeel & items_off[, 'aanbieder'] == aanbieder & items_off[, 'actie'] == 'los', 'item_id']) {
      score_data[score_data[, 'item_id'] == item_id, 'item_id'] = paste0('x', item_id, '_', aanbieder)
      if (item_id %in% noscore_items) {
        noscore_items[noscore_items == item_id] = paste0('x', item_id, '_', aanbieder) # Zorg ook dat dit met de noscore-items goed gaat
      }
    }

    # Voeg persoonsparameters toe en maak kalibratiedata
    score_data = dplyr::inner_join(score_data, leerlingen, by = 'person_id')
    cal_data = score_data[score_data[, 'schooltype'] == 1, ]

    # Bepaal aantal score-categorieen en observaties per item
    score_categories = plyr::ddply(cal_data, 'item_id', function(x) {
      return(data.frame('ncat' = length(unique(x$item_score)),
                        'n' = nrow(x)))
    })

    # Haal items eruit met maar 1 score-categorie, dit gaat niet goed in de kalibratie, hou ook minimum van 150 observaties aan
    cal_data = cal_data[cal_data[, 'item_id'] %in% score_categories[score_categories[, 'ncat'] > 1 & score_categories[, 'n'] > 150, 'item_id'], ]

    message('    Bezig met initiele 2pl-kalibratie')

    # Initiele kalibratie om slechte items eruit te gooien
    fixed_parameters = anker_parameters[anker_parameters[, 'onderdeel'] == onderdeel &
                                        anker_parameters[, 'item_id'] %in% score_categories[score_categories[, 'ncat'] > 1 &score_categories[, 'n'] > 150, 'item_id'], ]
    if (nrow(fixed_parameters) == 0) {
      fixed_parameters = NULL
    }
    cal = dexterMML::fit_2pl(cal_data, fixed_param = fixed_parameters, group = 'populatie')

    parameters = as.data.frame(coef(cal))

    # Verwijder items met discriminatieparameter lager dan 0.1
    remove_items = parameters[parameters[, 'alpha'] < 0.1, ]
    if (nrow(remove_items) > 0) {
      remove_items$onderdeel = onderdeel
      remove_items = remove_items[, c('item_id', 'onderdeel', colnames(remove_items)[!colnames(remove_items) %in% c('item_id', 'onderdeel')])] # Sorteer kolommen
      all_removed_items = rbind(all_removed_items, remove_items) # Hou verwijderde items bij
      cal_data = cal_data[!cal_data[, 'item_id'] %in% remove_items$item_id, ]
    }

    message(paste0('    Bezig met 2pl-kalibratie'))
   
    # Kalibratie
    cal = dexterMML::fit_2pl(cal_data, fixed_param = fixed_parameters, group = 'populatie')

    # Populatieparameters voor diagnostische doeleinden
    populations = as.data.frame(coef(cal, 'populations'))
    populations$onderdeel = onderdeel
    aanbieder_populations[[length(aanbieder_populations) + 1]] = populations

    # Itemparameters
    parameters = as.data.frame(coef(cal))
    parameters$onderdeel = onderdeel
    aanbieder_parameters[[length(aanbieder_parameters) + 1]] = parameters[, colnames(parameters) != 'item_score']

    # Maak data voor vaardigheidsschatting, alleen met items die we hebben kunnen kalibreren maar nu inclusief alle onderwijstypen
    ability_data = score_data[score_data[, 'item_id'] %in% parameters$item_id, ]

    # Verwijder noscore-items indien nodig
    if (!is.null(noscore_items)) {
      ability_data = ability_data[!ability_data[, 'item_id'] %in% noscore_items, ]
    }

    # Vaardigheidsschatter
    abilities = as.data.frame(dexterMML::ability.mml(ability_data, parms = parameters, method = 'WLE'))

    # Sla op voor latere analyse
    colnames(abilities)[colnames(abilities) != 'person_id'] = paste0(colnames(abilities)[colnames(abilities) != 'person_id'], '_', onderdeel)
    if (is.null(all_abilities)) {
      all_abilities = abilities
    } else {
      all_abilities = dplyr::full_join(all_abilities, abilities, by = 'person_id')
    }
  }

  # Voeg persoonsparameters (populatie, schooltype) in
  all_abilities = dplyr::left_join(all_abilities, leerlingen, by = 'person_id')

  # Kijk of er leerlingen zijn met ontbrekende onderdelen, deze gaan we negeren
  na_leerlingen = all_abilities[!complete.cases(all_abilities[, paste0('theta_', names(onderdeelgewichten))]), c('person_id', colnames(all_abilities)[grepl('theta_', colnames(all_abilities))])]
  if (nrow(na_leerlingen) > 0) {
    warning(paste0('Er waren ', nrow(na_leerlingen), ' leerlingen bij ', aanbieder, ' met een of meer onderdelen zonder vaardigheid, deze gaan we negeren:\n', paste(capture.output(print(na_leerlingen, row.names = FALSE)), collapse = '\n')))
    all_abilities = all_abilities[!all_abilities[, 'person_id'] %in% na_leerlingen$person_id, ]
  }

  # Bepaal behaalde referentieniveaus
  aanbieder_ref_behaald = NULL
  for (onderdeel in unique(ref_cesuren$onderdeel)) {
    for (refniveau in unique(ref_cesuren[ref_cesuren[, 'onderdeel'] == onderdeel, 'niveau'])) {
      cesuur = ref_cesuren[ref_cesuren[, 'onderdeel'] == onderdeel & ref_cesuren[, 'niveau'] == refniveau, 'cesuur']
      perc_behaald = round(nrow(all_abilities[all_abilities[, 'schooltype'] == 1 & !is.na(all_abilities[, paste0('theta_', onderdeel)]) & all_abilities[, paste0('theta_', onderdeel)] >= cesuur, ]) / 
                           nrow(all_abilities[all_abilities[, 'schooltype'] == 1 & !is.na(all_abilities[, paste0('theta_', onderdeel)]), ]) * 100.0, 2)
      aanbieder_ref_behaald = rbind(aanbieder_ref_behaald, data.frame('aanbieder' = aanbieder, 'onderdeel' = onderdeel, 'niveau' = refniveau, 'perc_behaald' = perc_behaald))
    }
  }  

  # Bepaal correlatiematrix tussen de verschillende onderdelen
  cor_matrix = cor(all_abilities[, paste0('theta_', names(onderdeelgewichten))], use = 'complete.obs')
  colnames(cor_matrix) = gsub('theta_', '', colnames(cor_matrix))
  rownames(cor_matrix) = gsub('theta_', '', rownames(cor_matrix))
  if (any(cor_matrix < 0.4)) {
    warning(paste0('Er waren correlaties tussen onderdelen lager dan 0.4 bij ', aanbieder, ' in het ', model, ' model.'))
  } 
  openxlsx::writeData(wb, aanbieder, as.data.frame(cor_matrix), borders = 'none', rowNames = TRUE)

  # Bereken GLV
  all_abilities$glv = apply(all_abilities[, paste0('theta_', names(onderdeelgewichten))], 1, weighted.mean, onderdeelgewichten)

  # Voeg GLVs bij abilities, bepaal pro/bb-correctie, bereken toetsadviezen en bereken percentages behaalde toetsadviezen
  aanbieder_toetsadviezen = NULL
  aanbieder_cesuren = toetsadvies_cesuren

  all_abilities$toetsadvies = as.character(cut(all_abilities$glv, c(-Inf, sort(toetsadvies_cesuren$cesuur), Inf), right = FALSE, labels = toetsadviezen))

  # Bereken behaalde toetsadviezen
  toetsadviezen_behaald = as.data.frame(table(all_abilities[all_abilities[, 'schooltype'] == 1, 'toetsadvies']), stringsAsFactors = FALSE)
  colnames(toetsadviezen_behaald) = c('toetsadvies', 'n')
  toetsadviezen_behaald$perc_behaald = round(prop.table(toetsadviezen_behaald$n) * 100.0, 2)
  toetsadviezen_behaald$aanbieder = aanbieder

  aanbieder_toetsadviezen = rbind(aanbieder_toetsadviezen, toetsadviezen_behaald[, colnames(toetsadviezen_behaald) != 'n'])

  # aanbieder_toetsadviezen = reshape(aanbieder_toetsadviezen, idvar = c('aanbieder', 'toetsadvies'), timevar = 'model', direction = 'wide', sep = '_')
  aanbieder_toetsadviezen = aanbieder_toetsadviezen[order(match(aanbieder_toetsadviezen$toetsadvies, toetsadviezen)), c('aanbieder', 'toetsadvies', 'perc_behaald')]

  # Vanaf hier genereren we output

  # Schrijf verwijderde items weg
  if (!is.null(all_removed_items)) {
    write.csv2(dplyr::mutate_if(all_removed_items[, colnames(all_removed_items) != 'item_score'], is.numeric, round, digits = 5), paste0(output_folder, '/', aanbieder, '_uitgeschakeld.csv'), row.names = FALSE, quote = FALSE)
  } else {
    file.create(paste0(output_folder, '/', aanbieder, '_uitgeschakeld.csv')) # Schrijf leeg bestand
  }

  # Schrijf itemparameters weg
  aanbieder_parameters = dplyr::bind_rows(aanbieder_parameters)
  write.csv2(dplyr::mutate_if(aanbieder_parameters, is.numeric, round, digits = 5), paste0(output_folder, '/', aanbieder, '_itemparameters.csv'), row.names = FALSE, quote = FALSE, na = '')

  # Schrijf populatieparameters weg
  aanbieder_populations = dplyr::bind_rows(aanbieder_populations)
  aanbieder_populations = aanbieder_populations[, c('onderdeel', colnames(aanbieder_populations)[colnames(aanbieder_populations) != 'onderdeel'])]
  write.csv2(dplyr::mutate_if(aanbieder_populations, is.numeric, round, digits = 5), paste0(output_folder, '/', aanbieder, '_populatieparameters.csv'), row.names = FALSE, quote = FALSE, na = '')

  # Schrijf vaardigheid-data weg
  all_abilities = all_abilities[, c('person_id', 'schooltype', 'populatie', colnames(all_abilities)[grepl('theta_|se_|glv|toetsadvies', colnames(all_abilities))])]
  if (!aanbieder %in% populatie_aanbieders) {
    all_abilities = all_abilities[, colnames(all_abilities) != 'populatie']
  }
  write.csv2(dplyr::mutate_if(all_abilities, is.numeric, round, digits = 5), paste0(output_folder, '/', aanbieder, '_vaardigheden.csv'), row.names = FALSE, quote = FALSE, na = '')

  # Schrijf ook behaalde referentieniveaus en toetsasviezen weg
  write.csv2(aanbieder_ref_behaald[, colnames(aanbieder_ref_behaald) != 'aanbieder'], paste0(output_folder, '/', aanbieder, '_referentieniveaus.csv'), row.names = FALSE, quote = FALSE)
  write.csv2(aanbieder_toetsadviezen[, colnames(aanbieder_toetsadviezen) != 'aanbieder'], paste0(output_folder, '/', aanbieder, '_toetsadviezen.csv'), row.names = FALSE, quote = FALSE, na = '')

  summary_refs = rbind(summary_refs, aanbieder_ref_behaald)
  summary_toetsadviezen = rbind(summary_toetsadviezen, aanbieder_toetsadviezen)
}

# Schrijf samenvattingsbestanden weg
summary_refs = reshape(summary_refs, idvar = c('onderdeel', 'niveau'), timevar = 'aanbieder', direction = 'wide', sep = '_')
summary_toetsadviezen = reshape(summary_toetsadviezen, idvar = c('toetsadvies'), timevar = 'aanbieder', direction = 'wide', sep = '_')
write.csv2(summary_refs, file.path(output_folder, 'overzicht_referentieniveaus.csv'), row.names = FALSE, quote = FALSE)
write.csv2(summary_toetsadviezen, file.path(output_folder, 'overzicht_toetsadviezen.csv'), row.names = FALSE, quote = FALSE)

# Excelbestand met correlatiematrices
openxlsx::saveWorkbook(wb, file.path(output_folder, 'overzicht_correlatiematrices.xlsx'), overwrite = TRUE)
