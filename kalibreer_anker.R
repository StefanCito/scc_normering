# Dit script kalibreert nieuwe ankeritems bij op de schaal van het bestaande gezamenlijk anker
# Data van alle toetsaanbieders is noodzakelijk als input

# Aangenomen wordt dat met het controlescript controle.R gecontroleerd is of de juiste bestanden aanwezig zijn en dat deze intern consistent zijn

# De output van dit script bestaat per toetsaanbieder uit:
# - Uitgeschakelde items
#   Lijst van items die uitgeschakeld zijn omdat ze een discriminatieparameter lager dan 0.1 hebben
#   bestandsnaam TOETS_uitgeschakeld_ankerkalibratie.csv

# Verder worden de volgende bestanden met globale gegevens weggeschreven:
# - (MML) populatieparameters van alle aanbieders
#   Bestand met de populatieparameters van ieder onderdeel uit de grote ankerkalibratie
#   Bestandsnaam ankerkalibratie_populaties.csv
# - itemparameters van alle items
#   Bestand met alle gekalibreerde itemparameters van alle onderdelen
#   Bestandsnaam ankerkalibratie_itemparameters.xlsx
# - itemparameters van het gezamenlijk anker
#   Bestand met parameters van bestaande gezamenlijk ankeritems aangevuld met nieuw bijgekalibreerde gezamenlijk ankeritems
#   Kan gebruikt worden als input voor kalibraties per aanbieder (zie irt1_normering.R)
#   Bestandsnaam ankerparameters_2024.xlsx

# De volgende R-packages worden gebruikt in dit script:
# dexterMML (https://github.com/dexter-psychometrics/dexterMML), openxlsx, reshape2, plyr en dplyr

# Invoer ==================================================

# Folder met alle databestanden
# De volgende databestanden worden verwacht van alle toetsaanbieders:
# - Leerlingdata (TOETS_leerlingen.csv)
# - Scoredata per onderdeel (TOETS_ONDERDEEL.csv)
data_folder = 'dummy_data'

# Bestand met bestaande gezamenlijk ankerparameters
anker_file = 'ankerparameters_2024.csv'

# Bestand met uitgeschakelde en losgekoppelde ankeritems, bevat kolommen aanbieder, item_id, onderdeel en actie (los of uit)
items_off_file = 'anker_off.csv'

# Aanbieders waar we rekening houden met verschillende MML-populaties
populatie_aanbieders = NULL

# Folder waar output naar weggeschreven moet worden
output_folder = 'dummy_resultaten'

# Huidig afnamejaar
jaar = 2025

# Code ====================================================

options(warn = 1)

# Inventariseer alle bestanden
leerling_files = list.files(path = data_folder, pattern = '_leerlingen.csv')
onderdeel_files = list.files(path = data_folder, pattern = '.csv')
onderdeel_files = onderdeel_files[!grepl('leerlingen|controle', onderdeel_files)]

items_off = read.csv2(items_off_file)

# Check op items_off
if (!all(items_off$actie %in% c('los', 'uit'))) {
  warning(paste0('Niet alle acties in het bestand met uitgeschakelde items zijn gelijk aan "los" of "uit"'))
}

# Lees parameters gezamenlijk anker in
anker_parameters = read.csv2(anker_file)

referentieonderdelen = unique(anker_parameters$onderdeel)

# Lees leerlingdata in
all_leerlingen = list()
for (leerling_file in leerling_files) {

  aanbieder = gsub('_leerlingen.csv', '', leerling_file, fixed = TRUE)

  leerlingen = read.csv2(file.path(data_folder, leerling_file))
  if (!aanbieder %in% populatie_aanbieders) {
    leerlingen$populatie = aanbieder
  } else {
    if (!'populatie' %in% colnames(leerlingen)) {
      stop(paste0('De leerlingdata van ', aanbieder, ' bevat geen populatie terwijl dit wel zou moeten.'))
    }
    leerlingen$populatie = paste0(aanbieder, '_', leerlingen$populatie)
  }

  all_leerlingen[[aanbieder]] = leerlingen[leerlingen[, 'schooltype'] == 1, c('person_id', 'populatie')] # We nemen alleen regulier BO mee
}

message(paste0('Leerlingdata van de volgende aanbieders is ingelezen:\n', paste(names(all_leerlingen), collapse = ', ')))

all_removed_items = list()
all_populations = list()
all_parameters = list()
# Loop over ieder onderdeel om de kalibratie uit te voeren
for (onderdeel in referentieonderdelen) {

  message(paste0('Bezig met onderdeel ', onderdeel, '...'))

  onderdeel_scores = list()
  # Lees van iedere aanbieder de scores van dit onderdeel in
  for (onderdeel_file in onderdeel_files[grepl(onderdeel, onderdeel_files, fixed = TRUE)]) {

    aanbieder = gsub(paste0('_', onderdeel, '.csv'), '', onderdeel_file, fixed = TRUE)

    score_data = read.csv2(file.path(data_folder, onderdeel_file), check.names = FALSE)

    # Maak long-format van wide-format data indien nodig
    if (!'item_id' %in% colnames(score_data)) {
      score_data = reshape2::melt(score_data, id.vars = 'person_id', value.name = 'item_score')
      colnames(score_data)[colnames(score_data) == 'variable'] = 'item_id'
      score_data$item_id = as.character(score_data$item_id)
    }

    score_data = dplyr::inner_join(score_data, all_leerlingen[[aanbieder]], by = 'person_id')

    # Verwijder missings, sta ook 9 toe voor missings
    score_data = score_data[!is.na(score_data$item_score) & score_data$item_score != 9, ]

    # Verwijder uitgeschakelde ankeritems
    score_data = score_data[!score_data[, 'item_id'] %in% items_off[items_off[, 'onderdeel'] == onderdeel & items_off[, 'aanbieder'] == aanbieder & items_off[, 'actie'] == 'uit', 'item_id'], ]

    # Koppel eventueel ook ankeritems los
    for (item_id in items_off[items_off[, 'onderdeel'] == onderdeel & items_off[, 'aanbieder'] == aanbieder & items_off[, 'actie'] == 'los', 'item_id']) {
      score_data[score_data[, 'item_id'] == item_id, 'item_id'] = paste0('x', item_id, '_', aanbieder)
    }

    score_data$person_id = paste0(as.character(score_data$person_id), '_', aanbieder) # Zorg voor zekerheid voor unieke person_ids
    score_data$aanbieder = aanbieder

    onderdeel_scores[[aanbieder]] = score_data
  }

  rm(score_data) # Maak wat geheugen vrij

  onderdeel_scores = dplyr::bind_rows(onderdeel_scores)

  # Kijk aantal score-categorieen per item, schrijf ook aanbieder weg zodat we items kunnen thuisbrengen bij verwijderde aanbieders
  score_categories = plyr::ddply(onderdeel_scores, 'item_id', function(x) {
    return(data.frame('ncat' = length(unique(x$item_score)),
                      'n' = nrow(x),
                      'aanbieder' = paste(unique(x$aanbieder), collapse = ',')))
  })

  # Haal items eruit met maar 1 score-categorie, dit vindt Dexter niet leuk, hou ook minimum van 200 observaties aan
  onderdeel_scores = onderdeel_scores[onderdeel_scores[, 'item_id'] %in% score_categories[score_categories[, 'ncat'] > 1 & score_categories[, 'n'] > 200, 'item_id'], ]

  message('Bezig met initiele 2pl-kalibratie')

  # Initiele 2pl-kalibratie om slechte items eruit te gooien
  fixed_parameters = anker_parameters[anker_parameters[, 'onderdeel'] == onderdeel, c('item_id', 'item_score', 'alpha', 'beta')]
  cal = dexterMML::fit_2pl(onderdeel_scores, fixed_param = fixed_parameters, group = 'populatie')

  parameters = as.data.frame(coef(cal))

  # Verwijder items met discriminatieparameter lager dan 0.1 of hoger dan 10
  remove_items = parameters[(parameters[, 'alpha'] < 0.1 | parameters[, 'alpha'] > 10) & (!parameters[, 'item_id'] %in% anker_parameters$item_id & !grepl('^A', parameters[, 'item_id'])), ]
  if (nrow(remove_items) > 0) {
    remove_items$onderdeel = onderdeel
    remove_items = dplyr::left_join(remove_items, score_categories[, c('item_id', 'aanbieder')], by = 'item_id')
    all_removed_items[[onderdeel]] = remove_items # Hou verwijderde items bij

    onderdeel_scores = onderdeel_scores[!onderdeel_scores[, 'item_id'] %in% remove_items$item_id, ] # Verwijder items
  }

  message(paste0('Bezig met 2pl-kalibratie'))

  # Daadwerkelijke kalibratie
  cal = dexterMML::fit_2pl(onderdeel_scores, fixed_param = fixed_parameters, group = 'populatie')
 
  # Populatieparameters
  populations = as.data.frame(coef(cal, 'populations'))
  populations$onderdeel = onderdeel
  populations$aanbieder = sapply(populations$populatie, function(x) strsplit(x, '_')[[1]][1])
  all_populations[[length(all_populations) + 1]] = populations
 
  # Itemparameters
  parameters = as.data.frame(coef(cal))
  parameters$onderdeel = onderdeel
  parameters = dplyr::left_join(parameters, score_categories[, c('item_id', 'aanbieder')], by = 'item_id')
  all_parameters[[onderdeel]] = parameters
}

# Schrijf verwijderde items weg per aanbieder
all_removed_items = dplyr::bind_rows(all_removed_items)
for (aanbieder in names(all_leerlingen)) {
  if (nrow(all_removed_items) > 0 && aanbieder %in% all_removed_items$aanbieder) {
    aanbieder_removed_items = all_removed_items[grepl(aanbieder, all_removed_items$aanbieder), !colnames(all_removed_items) %in% c('item_score', 'aanbieder')]
    write.csv2(aanbieder_removed_items, paste0(output_folder, '/', aanbieder, '_uitgeschakeld_ankerkalibratie.csv'), row.names = FALSE, quote = FALSE)
  } else {
    file.create(paste0(output_folder, '/', aanbieder, '_uitgeschakeld_ankerkalibratie.csv')) # Schrijf leeg bestand
  }
}

# Schrijf totale populatiegegevens weg
all_populations = dplyr::bind_rows(all_populations)
write.csv2(dplyr::mutate_if(all_populations, is.numeric, round, digits = 5), file.path(output_folder, 'ankerkalibratie_populaties.csv'), row.names = FALSE, quote = FALSE)

# Schrijf bestanden weg met itemparameters
parameters = dplyr::bind_rows(all_parameters)
write.csv2(parameters, file.path(output_folder, 'ankerkalibratie_itemparameters.csv'), row.names = FALSE, quote = FALSE) # Alle itemparameters (diagnostisch)

nieuw_anker = parameters[grepl('^A', parameters[, 'item_id']) & !parameters[, 'item_id'] %in% anker_parameters$item_id, ]
nieuw_anker$n_aanbieder = stringr::str_count(nieuw_anker$aanbieder, ',') + 1

if (nrow(nieuw_anker) > 0) {
  message(paste0('De volgende nieuwe gezamenlijk ankeritems zijn gevonden:\n', paste(nieuw_anker$item_id, collapse = '\n')))
}

if (nrow(nieuw_anker[nieuw_anker$n_aanbieder == 1, ]) > 0) {
  warning(paste0('Er waren gezamenlijk ankeritems die maar bij 1 aanbieder zijn afgenomen, deze zullen genegeerd worden:\n', paste(nieuw_anker[nieuw_anker$n_aanbieder == 1, 'item_id'], collapse = '\n')))
}

write.csv2(rbind(anker_parameters, nieuw_anker[nieuw_anker$n_aanbieder > 1, colnames(anker_parameters)]), file.path(output_folder, paste0('ankerparameters_', jaar, '.csv')), row.names = FALSE, quote = FALSE) # Alleen gezamenlijk anker
