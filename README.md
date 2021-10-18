## CGTN App

## A network analysis of the global cephalopod trade

Authors:
- Andrés Ospina-Alvarez [*]
- Silvia de Juan; Pablo Pita
- Gillian B. Ainsworth
- Fábio L. Matos
- Cristina Pita
- Sebastián Villasante

* Corresponding Author:
Mediterranean Institute for Advanced Studies IMEDEA (UIB-CSIC), C/ Miquel Marques 21, CP 07190 Esporles, Balearic Islands, Spain. 

## Instructions

This is a fully operational shinny app. Shiny is an R package that makes it easy to build interactive web applications (apps) straight from R.

All the code necessary to analyse and chart the global cephalopod trade network can be found here. It can be downloaded and run on an RStudio installation.

To make it even easier to visualise, we have launched a fully operable web app at https://aospina.shinyapps.io/CGTN_app/.

Once you have launched the shinny app, locally or via the web, please follow the steps below:

- Step 1: Choose which taxonomic group to analyse and whether the trade relationships represented will be based on the quantity of product traded (mass in tonnes, also called metric tonnes in the USA, 1 tonne = 1 metric ton = 1,000 kg) or on the monetary value (millions of USD) of the transactions.

- Step 2: Choose the centrality measure that will represent the relative importance of traders (nodes) and transaction linkages (edges) in the network. The definitions of each of the centrality measures and their rationale in the context of the global seafood market can be consulted in the Readme tab.

- Step 3: Choose date range (years) to be analysed.

- Step 4: Six-digit commodity codes are used worldwide, and by the United Nations (UN COMTRADE), to monitor trade volumes and apply international trade measures to goods. Choose the commodity code(s) that match the type of product and preparation to be analysed. For example, code 030751 covers trade transactions related to live, fresh or chilled octopus. If no commodity code(s) is(are) selected the network will be constructed using available commodity codes for the elaborated presentations of the taxonomic group (e.g., frozen, brine, smoked, etc).
