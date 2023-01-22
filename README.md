# PAC4: Dades alquilers barcelona

Aquest projecte és de l'assignatura Visualització de dades de la UOC sobre la PAC4.

L'autor es Marc Trepat.

## Contingut
Dintre del projecte hi han dos carpetes:

src/: Conté els codis per a executar aquesta activitat. Conté també els codis de sortida un cop executat cada procès per a tenir constància.

data/: Conté el dataset amb format CSV despres d'aplicar el data cleaning.

## Procediment
Els pasos per a poder tenir la visualització interactiva realitzada per la llibreria r-shiny i shinydashboard són:

Executar el proces preparacio_dataset.Rmd on es creara un fitxer CSV amb la informació netejada, també es genera un fitxer HTML per a veure els resutats de les operacions que hi han dintre del proces.
Un cop s'ha executat el punt anterior, es pot executar el proces app.R que aquest s'executa una visualització interactiva del dataset carregat.