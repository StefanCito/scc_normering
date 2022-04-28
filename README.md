# SCC normering

Code om de landelijke normering uit te voeren, inclusief een dummy-dataset.

De verschillende analysescripts worden in deze volgorde uitgevoerd:
1. `controle.R` - controle op datakwaliteit en consistentie
2. `kalibreer_anker.R` - kalibratie nieuwe gezamenlijk ankerparameters
3. `irt1_normering.R` - uitvoeren IRT1-normering
4. `irt2_normering.R` - uitvoeren IRT2-normering
5. `coronacorrectie_irt1.R` - bepaling eventuele correctie leerachterstanden IRT1-normering
6. `coronacorrectie_irt2.R` - bepaling eventuele correctie leerachterstanden IRT2-normering

In deze volgorde is alle code uit te voeren op basis van de dummy-dataset.

De volgende hulpbestanden worden hierbij gebruikt:
- `normeringsgegevens_dummy.xlsx` - normeringsgegevens zoals cesuren, onderdeelgewichten, enzovoorts (dummy)
- `ankerparameters_2021.xlsx` - bestaande parameters gezamenlijk anker gebaseerd op de geharmoniseerde normering
- `anker_off.csv` - uitgeschakelde of losgekoppelde gezamenlijk ankeritems in huidige uitgavejaar

Zie het normeringshandboek voor een uitgebreide beschrijving van alle analysestappen.
