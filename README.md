# eLatim-vallex
A shiny app for browsing through a latin sentence database using valency data as selectors.

## Types (and sources) of data

* Parsed data ([UD\_Latin-Perseus](https://github.com/UniversalDependencies/UD_Latin-Perseus) and [UD\_Latin-Proiel](https://github.com/UniversalDependencies/UD_Latin-PROIEL) treebanks),
* Lemma descriptions: [LiLa Lemma Bank](https://github.com/CIRCSE/LiLa_Lemma-Bank),
* Headword list [Latin Core Vocabulary](https://dcc.dickinson.edu/latin-core-list1),
* Semantic features: [Diccionario Didáctico Digital de Latín](http://repositorios.fdi.ucm.es/DiccionarioDidacticoLatin/) and 
* Portuguese translations: special contribution from [José Dejalma Dezotti](http://lattes.cnpq.br/8771278588762734).

## How to install
1. Download and install **R** (for [Mac](https://cran.r-project.org/bin/macosx/), for [Linux](https://cran.r-project.org/bin/linux/), for [Windows](https://cran.r-project.org/bin/windows/))
2. Download and unzip [eLatim-vallex](https://github.com/lucascdz/eLatim-vallex/archive/refs/heads/main.zip) repository.
3. Copy **eLatim-vallex-main** folder to your **Documents** folder (check for duplication, i.e. two folders with the same name, one inside the other)
4. Open the app by running:
   * in Windows Prompt:
     ```
     "C:\\Program Files\\R\\R-4.4.3\\bin\\R.exe" -e "setwd('~\\eLatim-vallex-main') ; getwd() ; source('.\\app.R') ; runApp( MyApp(DataFrame),launch.browser = T)"
     ```
     > NOTE: it doesn't work on Windows Terminal
   * in Mac/Linux Terminal:
     ```
     R -e "path<-c(path.expand('~/Documents/eLatim-vallex-main')) ; setwd(path) ; getwd() ; source('./app.R') ; runApp( MyApp(DataFrame),launch.browser = T)"
     ```

## How to cite this software:

Lucas Dezotti. (2025). eLatim-vallex: v.1.0. Zenodo. https://doi.org/10.5281/zenodo.14985092

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14985092.svg)](https://doi.org/10.5281/zenodo.14985092)



