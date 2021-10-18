#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(mapdata)
library(ggraph)
library(RColorBrewer)
library(scales)
library(knitr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  # Application title
  titlePanel("Cephalopod Global Trade Network (CGTN) "),
  h6(paste0("Authors")),
  p(
    HTML(
      "<a href='https://orcid.org/0000-0003-2454-7169'> Andrés Ospina-Alvarez</a> [*]; <a href='https://orcid.org/0000-0002-8891-8371'>Silvia de Juan</a>; <a href='https://orcid.org/0000-0001-9273-1481'>Pablo Pita</a>; <a href='https://orcid.org/0000-0003-0460-6563'>Gillian B. Ainsworth</a>; <a href='https://orcid.org/0000-0002-5447-4055'>Fábio L. Matos</a>; <a href='https://orcid.org/0000-0003-1824-3396'>Cristina Pita</a> & <a href='https://orcid.org/0000-0001-6296-4479'> Sebastián Villasante</a>"
    )
  ),
  h6(paste0("* Corresponding Author:")),
  HTML(
    "<address>
                    Mediterranean Institute for Advanced Studies <a href='https://imedea.uib-csic.es'>IMEDEA (UIB-CSIC)</a>, C/ Miquel Marques 21, CP 07190 Esporles, Balearic Islands, Spain. <br>
                  </address>"
  ),
  #### Instructions ####
  h4(paste0("Instructions")),
  p(
    "Step 1: Choose which taxonomic group to analyse and whether the trade relationships represented will be based on the quantity of product traded (mass in tonnes, also called metric tonnes in the USA, 1 tonne = 1 metric ton = 1,000 kg) or on the monetary value (millions of USD) of the transactions."
  ),
  
  p(
    "Step 2: Choose the centrality measure that will represent the relative importance of traders (nodes) and transaction linkages (edges) in the network. The definitions of each of the centrality measures and their rationale in the context of the global seafood market can be consulted in the Readme tab."
  ),
  
  p("Step 3: Choose date range (years) to be analysed."),
  
  HTML(
    "Step 4: <a href='https://unstats.un.org/unsd/tradekb/Knowledgebase/50018/Harmonized-Commodity-Description-and-Coding-Systems-HS'>Six-digit commodity codes</a> are used worldwide, and by the United Nations (<a href='https://comtrade.un.org'>UN COMTRADE</a>), to monitor trade volumes and apply international trade measures to goods. Choose the commodity code(s) that match the type of product and preparation to be analysed. For example, code 030751 covers trade transactions related to live, fresh or chilled octopus. If no commodity code(s) is(are) selected the network will be constructed using available commodity codes for the elaborated presentations of the taxonomic group (e.g., frozen, brine, smoked, etc)."
  ),
  tabsetPanel(
    tabPanel("CGTN",
             # Sidebar with a slider input for number of bins ####
             sidebarLayout(
               sidebarPanel(
                 h3(paste0("Configuration Panel")),
                 #### Choosing Taxonomic Group ####
                 inputPanel(
                   selectInput(
                     inputId = "taxonomic",
                     label = "Taxonomic group",
                     choices = c("Octopus", "Squid and cuttlefish"),
                     selected = "Octopus"
                   ),
                   #### Choosing kg or USD ####
                   selectInput(
                     inputId = "type",
                     label = "Graph based on:",
                     choices = c("mass (tonnes)", "currency (USD millions)"),
                     selected = "mass (tonnes)"
                   )
                 ),
                 #### Choosing date range ####
                 conditionalPanel(
                   condition = "input.taxonomic == 'Octopus'",
                   sliderInput(
                     "range_octopus",
                     "Date range",
                     min = 2000,
                     max = 2019,
                     value = c(2015, 2019),
                     step = 1,
                     dragRange = TRUE
                   )
                 ),
                 conditionalPanel(
                   condition = "input.taxonomic == 'Squid and cuttlefish'",
                   sliderInput(
                     "range_squid",
                     "Date range",
                     min = 2000,
                     max = 2019,
                     value = c(2015, 2019),
                     step = 1,
                     dragRange = TRUE
                   )
                 ),
                 
                 #### Choosing Centrality Measures ####
                 inputPanel(
                   # Reactive expression for the data subsetted to what the user selected
                   selectInput(
                     inputId = "node_measure",
                     label = "Node centrality measure:",
                     choices = c(
                       "Strength",
                       "In-strength",
                       "Out-strength",
                       "Betweenness",
                       "Closeness",
                       "Page's Rank",
                       "Eigenvector",
                       "Authority score",
                       "Hub score"
                     ),
                     selected = "Strength"
                   ),
                   
                   selectInput(
                     inputId = "edge_measure",
                     label = "Edge measure:",
                     choices = c("Edge strength",
                                 "Edge betweenness"),
                     selected = "Edge strength"
                   )
                 ),
                 
                 #### Choosing Commodity Codes ####
                 conditionalPanel(
                   condition = "input.taxonomic == 'Octopus'",
                   checkboxGroupInput(
                     inputId = "cc_octopus",
                     label = "Select commodity codes(s):",
                     choices = c(
                      "030751	- Octopus spp., live, fresh or chilled" = "fresh",
                      "030752, 030759 & 160555 - Octopus spp., frozen, dried, salted, in brine, or smoked, cooked or not before or during the smoking process, prepared or preserved" = "elaborated"
                     ),
                     selected = "fresh"
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.taxonomic == 'Squid and cuttlefish'",
                   checkboxGroupInput(
                     inputId = "cc_squid",
                     label = "Select commodity codes(s):",
                     choices = c(
                       "030741, 030742 -	Cuttle fish and squid, whether in shell or not, live, fresh or chilled" = "fresh",
                       "030743, 030749 & 160554 -	Cuttle fish and squid, whether in shell or not, includes flours, meals, and pellets of molluscs, fit for human consumption, frozen, dried, salted, in brine, or smoked, cooked or not before or during the smoking process, prepared or preserved" = "elaborated"
                     ),
                     selected = "fresh"
                   )
                 )
               ),
               
               #### Header ####
               mainPanel(
                 #### Results ####
                 h4(paste0("Results")),
                 plotOutput("GraphPlot"),
                 p(
                   textOutput("fig_caption", inline = T),
                   textOutput("cc_caption", inline = T),
                   paste0("commodity codes.")
                 ),
                 
                 h5(
                   paste0("Ranking of the most important traders in the network based on "),
                   textOutput("text_type", inline = T),
                   "between Jan. 1, ",
                   textOutput("text_year_ini", inline = T),
                   " and Dec. 31, ",
                   textOutput("text_year_end", inline = T)
                 ),
                 fluidRow(column(7, h6(
                   paste("Top Exporters")
                 )),
                 column(4, h6(
                   paste("Top Importers")
                 ))),
                 
                 fluidRow(column(7, tableOutput("ExportsTable")),
                          column(4, tableOutput("ImportsTable"))),
                 
                 fluidRow(column(7, h6(
                   paste("Top flows between traders")
                 )),
                 column(4, h6(
                   paste("Top "),
                   textOutput("text_node_measure", inline = T)
                 ))),
                 
                 fluidRow(column(7, tableOutput("FlowsTable")),
                          column(4, tableOutput("MeasureTable"))),
                 
                 fluidRow(column(7, h6(
                   paste("Number of trade links")
                 ))),
                 fluidRow(column(7, tableOutput("LinksTable")))
               )
             )),
    tabPanel(
      "Readme",
      h4(
        paste0("A brief introduction to Graph theory in trade network analysis")
      ),
      HTML(
        "Graph theory is the mathematical study of a <a href='https://mathinsight.org/network_introduction'>network</a> of interacting elements. This approach provides a quantitative but simplified view of the multiple factors involved in the connection (<a href='https://mathinsight.org/definition/network_edge'>edges</a>) among elements (<a href='https://mathinsight.org/definition/network_node'>nodes</a>) contained in a network. In a network of traders, graph theory provides insights into the trading relationships’ properties and identifies critical nodes (traders) with high centrality that are connected to many other traders, or clusters of well-connected traders. In plain words, it goes beyond the mere interchange between network elements, by considering the network as a whole, where a node might play a crucial role independently on the dimension of the individual connections (i.e. transactions in a trading network). These emerging properties are measured through different <a href='https://www.sciencedirect.com/topics/computer-science/centrality-measure'>centrality measures</a>."
      ),
      h5(
        paste0(
          "Measures of centrality, definitions, and rationales in a context of a seafood trade network"
        )
      ),
      p("See the list of references at the end for even more information."),
      HTML((
        "<table>
                       <thead>
                       <tr>
                       <th><strong>measure</strong></th>
                       <th><strong>reference</strong></th>
                       <th><strong>definition</strong></th>
                       <th><strong>rationale</strong></th>
                       </thead>
                       <tbody>
                       <tr>
                       <td><strong>Degree</strong></td>
                       <td>Barrat et al. 2004</td>
                       <td>The degree of a node is the number of edges connected to the node.</td>
                       <td>The degree is the number of other traders with which a trader has relationships. </td>
                       </tr>
                       <tr>
                       <td><strong>In-degree</strong></td>
                       <td>Barrat et al. 2004</td>
                       <td>In a directed network, each node has two degrees. The in-degree is the number of incoming edges onto a node.</td>
                       <td>In-degree indicates the number of connections coming into each trader (imports). This metric does not take into account the weight or strength of the edges.</td>
                       </tr>
                       <tr>
                       <td><strong>Out-degree</strong></td>
                       <td>Barrat et al. 2004</td>
                       <td>In a directed network, each node has two degrees. The out-degree is the number of outgoing edges emanating from a node.</td>
                       <td>Out-degree indicates the number of connections going out from each trader (exports). This metric does not take into account the weight or strength of the edges.</td>
                       </tr>
                       <tr>
                       <td><strong>Strength</strong></td>
                       <td>Barrat  et al. 2004</td>
                       <td>Also  named weighted degree. In weighted networks, node Strength is the sum of weights of links  connected to the node.</td>
                       <td>Strength could indicate if a trader is involved in important (by weight) trades with other traders. Traders with high Strength can be  acting as keystones since they are connected by imports and exports to many  neighbouring traders.</td>
                       </tr>
                       <tr>
                       <td><strong>In-strength</strong></td>
                       <td>Barrat  et al. 2004</td>
                       <td>In directed networks, the In-strength is the  sum of inward link weights.</td>
                       <td>Traders  with a high in-strength could act as important importers or hubs for the  distribution of raw materials. High in-strength could also be targeting  traders acting as major consumers of products.</td>
                       </tr>
                       <tr>
                       <td><strong>Out-strength</strong></td>
                       <td>Barrat  et al. 2004</td>
                       <td>In directed networks, the Out-strength is  the sum of outward link weights.</td>
                       <td>Traders  with high out-strength may be acting as raw producers with high export flows.  This may indicate the geographical origin of the commodities and/or essential  habitats for the species. Out-strength can also indicate whether a trader is  a major exporter of processed products.</td>
                       </tr>
                       <tr>
                       <td><strong>Betweenness</strong></td>
                       <td>Freeman  1979</td>
                       <td>Betweenness  centrality is a measure of the influence of a node over the flow of  information between every pair of nodes under the assumption that information  primarily flows over the shortest paths between them.</td>
                       <td>Imports  and exports are important, but they are not the whole picture. Traders with  high Betweenness centralities have been called &quot;bottlenecks&quot; or  &quot;bridges&quot; and prevent network fragmentation. They connect many different partners in the network which would ‘fall apart’ if the trader’s trade flow was impaired. On the other hand any regulatory measure implemented in these traders can influence the network significantly.</td>
                       </tr>
                       <tr>
                       <td><strong>PageRank / Page's Rank</strong></td>
                       <td>Brin and Page 1988</td>
                       <td>PageRank provides an alternative measure of a node's importance in the network, resulting from an iterative algorithm that assigns higher values to nodes with a greater number of incoming connections from other high-value nodes</td>
                       <td>In a global trade network there are countries that are essential because they are connected to other countries critical to the network and those critical countries, in turn, have no other significant connections. Traders with high PageRank centralities have  a high flow of imports mainly with traders that also have a  high flow of imports.</td>
                       </tr>
                       <tr>
                       <td><strong>Eigenvector centrality</strong></td>
                       <td>Bonacich  1987</td>
                       <td>The  Eigenvector centrality network metric takes into consideration not only how  many connections a node has (i.e., its Degree or Strength), but also the  centrality of the vertices that it is connected to.</td>
                       <td>It  is a measure of a trader&#39;s influence in the network. In general, a  connection to a well-connected trader is more important than a connection to  a poorly connected trader. Traders with high Eigenvector centralities have  a high flow of imports and exports mainly with traders that also have a  high flow of imports and exports.</td>
                       </tr>
                       </tr>
                       </tbody>
                       </table>
                       <h4 id='references'>References</h4>
                       <ul>
                       <li>Barrat, A., Barthélemy, M., Pastor-Satorras, R., &amp; Vespignani, A. (2004). The architecture of complex weighted networks. Proceedings of the National Academy of Sciences of the United States of America, 101(11), 3747–3752. <a href='https://doi.org/10.1073/pnas.0400087101'>https://doi.org/10.1073/pnas.0400087101</a></li>
                       <li>Brin, S., & Page, L. (1998). The anatomy of a large-scale hypertextual web search engine. Computer Networks, 30, 107–117.</li>
                       <li>Bonacich, P. (1987). Power and centrality: A family of measures. American Journal of Sociology, 92(5), 1170–1182. <a href='https://doi.org/10.1086/228631'>https://doi.org/10.1086/228631</a></li>
                       <li>Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social Networks, 1(3), 215–239.</li>
                       </ul>
                "
      )
      )
    ),
    tabPanel(
      "Country names and ISO codes",
      h4(paste0(
        "Countries and territories with their ISO-2 and ISO-3 codes"
      )),
      p(
        "The ISO (International Organization for Standardization) country codes are internationally recognized codes that designate every country and most of the dependent areas a two-letter combination or a three-letter combination; it is like an acronym, that stands for a country or a state. Here is displayed the used list of countries and territories with their ISO 3166-1 alpha-2 codes, also called ISO2 codes, and their ISO 3166-1 alpha-3 codes, also called ISO3 codes, which are two and three-letter codes, respectively, assigned in the ISO 3166-1 standard."
      ),
      HTML(
        "<table>
<thead>
<tr><th><strong>Country or territory name</strong></th><th><strong>ISO-2</strong></th><th><strong>ISO-3</strong></th></tr>
<tr><th>___________________________________________</th><th>______________</th><th>______________</th></tr>
<thead>
<tbody><tr><td>Afghanistan</td><td>AF</td><td>AFG</td></tr><tr><td>Albania</td><td>AL</td><td>ALB</td></tr><tr><td>Algeria</td><td>DZ</td><td>DZA</td></tr><tr><td>American Samoa</td><td>AS</td><td>ASM</td></tr><tr><td>Andorra</td><td>AD</td><td>AND</td></tr><tr><td>Angola</td><td>AO</td><td>AGO</td></tr><tr><td>Anguilla</td><td>AI</td><td>AIA</td></tr><tr><td>Antarctica</td><td>AQ</td><td>ATA</td></tr><tr><td>Antigua and Barbuda</td><td>AG</td><td>ATG</td></tr><tr><td>Argentina</td><td>AR</td><td>ARG</td></tr><tr><td>Armenia</td><td>AM</td><td>ARM</td></tr><tr><td>Aruba</td><td>AW</td><td>ABW</td></tr><tr><td>Australia</td><td>AU</td><td>AUS</td></tr><tr><td>Austria</td><td>AT</td><td>AUT</td></tr><tr><td>Azerbaijan</td><td>AZ</td><td>AZE</td></tr><tr><td>Bahamas</td><td>BS</td><td>BHS</td></tr><tr><td>Bahrain</td><td>BH</td><td>BHR</td></tr><tr><td>Bangladesh</td><td>BD</td><td>BGD</td></tr><tr><td>Barbados</td><td>BB</td><td>BRB</td></tr><tr><td>Belarus</td><td>BY</td><td>BLR</td></tr><tr><td>Belgium</td><td>BE</td><td>BEL</td></tr><tr><td>Belize</td><td>BZ</td><td>BLZ</td></tr><tr><td>Benin</td><td>BJ</td><td>BEN</td></tr><tr><td>Bermuda</td><td>BM</td><td>BMU</td></tr><tr><td>Bhutan</td><td>BT</td><td>BTN</td></tr><tr><td>Bolivia</td><td>BO</td><td>BOL</td></tr><tr><td>Bonaire, Saint Eustatius and Saba</td><td>BQ</td><td>BES</td></tr><tr><td>Bosnia Herzegovina</td><td>BA</td><td>BIH</td></tr><tr><td>Botswana</td><td>BW</td><td>BWA</td></tr><tr><td>Bouvet Island</td><td>BV</td><td>BVT</td></tr><tr><td>Brazil</td><td>BR</td><td>BRA</td></tr><tr><td>British Indian Ocean Territory</td><td>IO</td><td>IOT</td></tr><tr><td>British Virgin Isds</td><td>VG</td><td>VGB</td></tr><tr><td>Brunei Darussalam</td><td>BN</td><td>BRN</td></tr><tr><td>Bulgaria</td><td>BG</td><td>BGR</td></tr><tr><td>Burkina Faso</td><td>BF</td><td>BFA</td></tr><tr><td>Burundi</td><td>BI</td><td>BDI</td></tr><tr><td>Cabo Verde</td><td>CV</td><td>CPV</td></tr><tr><td>Cambodia</td><td>KH</td><td>KHM</td></tr><tr><td>Cameroon</td><td>CM</td><td>CMR</td></tr><tr><td>Canada</td><td>CA</td><td>CAN</td></tr><tr><td>Cayman Isds</td><td>KY</td><td>CYM</td></tr><tr><td>Central African Rep.</td><td>CF</td><td>CAF</td></tr><tr><td>Chad</td><td>TD</td><td>TCD</td></tr><tr><td>Chile</td><td>CL</td><td>CHL</td></tr><tr><td>China</td><td>CN</td><td>CHN</td></tr><tr><td>China, Hong Kong SAR</td><td>HK</td><td>HKG</td></tr><tr><td>China, Macao SAR</td><td>MO</td><td>MAC</td></tr><tr><td>Christmas Island</td><td>CX</td><td>CXR</td></tr><tr><td>Cocos [Keeling] Isds</td><td>CC</td><td>CCK</td></tr><tr><td>Colombia</td><td>CO</td><td>COL</td></tr><tr><td>Comoros</td><td>KM</td><td>COM</td></tr><tr><td>Congo, Dem. Rep.</td><td>CD</td><td>COD</td></tr><tr><td>Congo, Rep.</td><td>CG</td><td>COG</td></tr><tr><td>Cook Isds</td><td>CK</td><td>COK</td></tr><tr><td>Costa Rica</td><td>CR</td><td>CRI</td></tr><tr><td>Cote d&#39;Ivoire</td><td>CI</td><td>CIV</td></tr><tr><td>Croatia</td><td>HR</td><td>HRV</td></tr><tr><td>Cuba</td><td>CU</td><td>CUB</td></tr><tr><td>Curaçao</td><td>CW</td><td>CUW</td></tr><tr><td>Cyprus</td><td>CY</td><td>CYP</td></tr><tr><td>Czech Rep.</td><td>CZ</td><td>CZE</td></tr><tr><td>Denmark</td><td>DK</td><td>DNK</td></tr><tr><td>Djibouti</td><td>DJ</td><td>DJI</td></tr><tr><td>Dominica</td><td>DM</td><td>DMA</td></tr><tr><td>Dominican Rep.</td><td>DO</td><td>DOM</td></tr><tr><td>Ecuador</td><td>EC</td><td>ECU</td></tr><tr><td>Egypt</td><td>EG</td><td>EGY</td></tr><tr><td>El Salvador</td><td>SV</td><td>SLV</td></tr><tr><td>Equatorial Guinea</td><td>GQ</td><td>GNQ</td></tr><tr><td>Eritrea</td><td>ER</td><td>ERI</td></tr><tr><td>Estonia</td><td>EE</td><td>EST</td></tr><tr><td>Ethiopia</td><td>ET</td><td>ETH</td></tr><tr><td>EU-28</td><td>EU</td><td>EU2</td></tr><tr><td>Faeroe Isds</td><td>FO</td><td>FRO</td></tr><tr><td>Falkland Isds (Malvinas)</td><td>FK</td><td>FLK</td></tr><tr><td>Fiji</td><td>FJ</td><td>FJI</td></tr><tr><td>Finland</td><td>FI</td><td>FIN</td></tr><tr><td>France</td><td>FR</td><td>FRA</td></tr><tr><td>French Guiana</td><td>GF</td><td>GUF</td></tr><tr><td>French Polynesia</td><td>PF</td><td>PYF</td></tr><tr><td>French Southern Territories</td><td>TF</td><td>ATF</td></tr><tr><td>Gabon</td><td>GA</td><td>GAB</td></tr><tr><td>Gambia</td><td>GM</td><td>GMB</td></tr><tr><td>Gaza Strip</td><td>PS</td><td>PSE</td></tr><tr><td>Georgia</td><td>GE</td><td>GEO</td></tr><tr><td>Germany</td><td>DE</td><td>DEU</td></tr><tr><td>Ghana</td><td>GH</td><td>GHA</td></tr><tr><td>Gibraltar</td><td>GI</td><td>GIB</td></tr><tr><td>Greece</td><td>GR</td><td>GRC</td></tr><tr><td>Greenland</td><td>GL</td><td>GRL</td></tr><tr><td>Grenada</td><td>GD</td><td>GRD</td></tr><tr><td>Guadeloupe</td><td>GP</td><td>GLP</td></tr><tr><td>Guam</td><td>GU</td><td>GUM</td></tr><tr><td>Guatemala</td><td>GT</td><td>GTM</td></tr><tr><td>Guernsey</td><td>GG</td><td>GGY</td></tr><tr><td>Guinea</td><td>GN</td><td>GIN</td></tr><tr><td>Guinea-Bissau</td><td>GW</td><td>GNB</td></tr><tr><td>Guyana</td><td>GY</td><td>GUY</td></tr><tr><td>Haiti</td><td>HT</td><td>HTI</td></tr><tr><td>Heard Island and McDonald Isds</td><td>HM</td><td>HMD</td></tr><tr><td>Honduras</td><td>HN</td><td>HND</td></tr><tr><td>Hungary</td><td>HU</td><td>HUN</td></tr><tr><td>Iceland</td><td>IS</td><td>ISL</td></tr><tr><td>India</td><td>IN</td><td>IND</td></tr><tr><td>Indonesia</td><td>ID</td><td>IDN</td></tr><tr><td>Iran</td><td>IR</td><td>IRN</td></tr><tr><td>Iraq</td><td>IQ</td><td>IRQ</td></tr><tr><td>Ireland</td><td>IE</td><td>IRL</td></tr><tr><td>Isle of Man</td><td>IM</td><td>IMN</td></tr><tr><td>Israel</td><td>IL</td><td>ISR</td></tr><tr><td>Italy</td><td>IT</td><td>ITA</td></tr><tr><td>Jamaica</td><td>JM</td><td>JAM</td></tr><tr><td>Japan</td><td>JP</td><td>JPN</td></tr><tr><td>Jersey</td><td>JE</td><td>JEY</td></tr><tr><td>Jordan</td><td>JO</td><td>JOR</td></tr><tr><td>Kazakhstan</td><td>KZ</td><td>KAZ</td></tr><tr><td>Kenya</td><td>KE</td><td>KEN</td></tr><tr><td>Kiribati</td><td>KI</td><td>KIR</td></tr><tr><td>Korea, Dem. Rep.</td><td>KP</td><td>PRK</td></tr><tr><td>Korea, Rep.</td><td>KR</td><td>KOR</td></tr><tr><td>Kosovo</td><td>XK</td><td>XKX</td></tr><tr><td>Kuwait</td><td>KW</td><td>KWT</td></tr><tr><td>Kyrgyzstan</td><td>KG</td><td>KGZ</td></tr><tr><td>Lao PDR</td><td>LA</td><td>LAO</td></tr><tr><td>Latvia</td><td>LV</td><td>LVA</td></tr><tr><td>Lebanon</td><td>LB</td><td>LBN</td></tr><tr><td>Lesotho</td><td>LS</td><td>LSO</td></tr><tr><td>Liberia</td><td>LR</td><td>LBR</td></tr><tr><td>Libya</td><td>LY</td><td>LBY</td></tr><tr><td>Liechtenstein</td><td>LI</td><td>LIE</td></tr><tr><td>Lithuania</td><td>LT</td><td>LTU</td></tr><tr><td>Luxembourg</td><td>LU</td><td>LUX</td></tr><tr><td>Macedonia, FYR</td><td>MK</td><td>MKD</td></tr><tr><td>Madagascar</td><td>MG</td><td>MDG</td></tr><tr><td>Malawi</td><td>MW</td><td>MWI</td></tr><tr><td>Malaysia</td><td>MY</td><td>MYS</td></tr><tr><td>Maldives</td><td>MV</td><td>MDV</td></tr><tr><td>Mali</td><td>ML</td><td>MLI</td></tr><tr><td>Malta</td><td>MT</td><td>MLT</td></tr><tr><td>Marshall Isds</td><td>MH</td><td>MHL</td></tr><tr><td>Martinique</td><td>MQ</td><td>MTQ</td></tr><tr><td>Mauritania</td><td>MR</td><td>MRT</td></tr><tr><td>Mauritius</td><td>MU</td><td>MUS</td></tr><tr><td>Mayotte</td><td>YT</td><td>MYT</td></tr><tr><td>Mexico</td><td>MX</td><td>MEX</td></tr><tr><td>Micronesia</td><td>FM</td><td>FSM</td></tr><tr><td>Moldova</td><td>MD</td><td>MDA</td></tr><tr><td>Monaco</td><td>MC</td><td>MCO</td></tr><tr><td>Mongolia</td><td>MN</td><td>MNG</td></tr><tr><td>Montenegro</td><td>ME</td><td>MNE</td></tr><tr><td>Montserrat</td><td>MS</td><td>MSR</td></tr><tr><td>Morocco</td><td>MA</td><td>MAR</td></tr><tr><td>Mozambique</td><td>MZ</td><td>MOZ</td></tr><tr><td>Myanmar</td><td>MM</td><td>MMR</td></tr><tr><td>N. Mariana Isds</td><td>MP</td><td>MNP</td></tr><tr><td>Namibia</td><td>NA</td><td>NAM</td></tr><tr><td>Nauru</td><td>NR</td><td>NRU</td></tr><tr><td>Nepal</td><td>NP</td><td>NPL</td></tr><tr><td>Netherlands</td><td>NL</td><td>NLD</td></tr><tr><td>Netherlands Antilles</td><td>AN</td><td>ANT</td></tr><tr><td>New Caledonia</td><td>NC</td><td>NCL</td></tr><tr><td>New Zealand</td><td>NZ</td><td>NZL</td></tr><tr><td>Nicaragua</td><td>NI</td><td>NIC</td></tr><tr><td>Niger</td><td>NE</td><td>NER</td></tr><tr><td>Nigeria</td><td>NG</td><td>NGA</td></tr><tr><td>Niue</td><td>NU</td><td>NIU</td></tr><tr><td>Norfolk Island</td><td>NF</td><td>NFK</td></tr><tr><td>Norway</td><td>NO</td><td>NOR</td></tr><tr><td>Oman</td><td>OM</td><td>OMN</td></tr><tr><td>Pakistan</td><td>PK</td><td>PAK</td></tr><tr><td>Palau</td><td>PW</td><td>PLW</td></tr><tr><td>Panama</td><td>PA</td><td>PAN</td></tr><tr><td>Papua New Guinea</td><td>PG</td><td>PNG</td></tr><tr><td>Paraguay</td><td>PY</td><td>PRY</td></tr><tr><td>Peru</td><td>PE</td><td>PER</td></tr><tr><td>Philippines</td><td>PH</td><td>PHL</td></tr><tr><td>Pitcairn Isds</td><td>PN</td><td>PCN</td></tr><tr><td>Poland</td><td>PL</td><td>POL</td></tr><tr><td>Portugal</td><td>PT</td><td>PRT</td></tr><tr><td>Puerto Rico</td><td>PR</td><td>PRI</td></tr><tr><td>Qatar</td><td>QA</td><td>QAT</td></tr><tr><td>Réunion</td><td>RE</td><td>REU</td></tr><tr><td>Romania</td><td>RO</td><td>ROU</td></tr><tr><td>Russian Federation</td><td>RU</td><td>RUS</td></tr><tr><td>Rwanda</td><td>RW</td><td>RWA</td></tr><tr><td>Saint Helena</td><td>SH</td><td>SHN</td></tr><tr><td>Saint Kitts and Nevis</td><td>KN</td><td>KNA</td></tr><tr><td>Saint Lucia</td><td>LC</td><td>LCA</td></tr><tr><td>Saint Pierre and Miquelon</td><td>PM</td><td>SPM</td></tr><tr><td>Saint Vincent and the Grenadines</td><td>VC</td><td>VCT</td></tr><tr><td>Saint-Barthelemy</td><td>BL</td><td>BLM</td></tr><tr><td>Samoa</td><td>WS</td><td>WSM</td></tr><tr><td>San Marino</td><td>SM</td><td>SMR</td></tr><tr><td>Sao Tome and Principe</td><td>ST</td><td>STP</td></tr><tr><td>Saudi Arabia</td><td>SA</td><td>SAU</td></tr><tr><td>Senegal</td><td>SN</td><td>SEN</td></tr><tr><td>Serbia</td><td>RS</td><td>SRB</td></tr><tr><td>Serbia and Montenegro</td><td>CS</td><td>SCG</td></tr><tr><td>Seychelles</td><td>SC</td><td>SYC</td></tr><tr><td>Sierra Leone</td><td>SL</td><td>SLE</td></tr><tr><td>Singapore</td><td>SG</td><td>SGP</td></tr><tr><td>Sint Maarten (Dutch part)</td><td>SX</td><td>SXM</td></tr><tr><td>Slovakia</td><td>SK</td><td>SVK</td></tr><tr><td>Slovenia</td><td>SI</td><td>SVN</td></tr><tr><td>Solomon Isds</td><td>SB</td><td>SLB</td></tr><tr><td>Somalia</td><td>SO</td><td>SOM</td></tr><tr><td>South Africa</td><td>ZA</td><td>ZAF</td></tr><tr><td>South Georgia and the South Sandwich Isds</td><td>GS</td><td>SGS</td></tr><tr><td>South Sudan</td><td>SS</td><td>SSD</td></tr><tr><td>Spain</td><td>ES</td><td>ESP</td></tr><tr><td>Sri Lanka</td><td>LK</td><td>LKA</td></tr><tr><td>Sudan</td><td>SD</td><td>SDN</td></tr><tr><td>Suriname</td><td>SR</td><td>SUR</td></tr><tr><td>Svalbard and Jan Mayen</td><td>SJ</td><td>SJM</td></tr><tr><td>Swaziland</td><td>SZ</td><td>SWZ</td></tr><tr><td>Sweden</td><td>SE</td><td>SWE</td></tr><tr><td>Switzerland</td><td>CH</td><td>CHE</td></tr><tr><td>Syria</td><td>SY</td><td>SYR</td></tr><tr><td>Taiwan</td><td>TW</td><td>TWN</td></tr><tr><td>Tajikistan</td><td>TJ</td><td>TJK</td></tr><tr><td>Tanzania</td><td>TZ</td><td>TZA</td></tr><tr><td>Thailand</td><td>TH</td><td>THA</td></tr><tr><td>Timor-Leste</td><td>TL</td><td>TLS</td></tr><tr><td>Togo</td><td>TG</td><td>TGO</td></tr><tr><td>Tokelau</td><td>TK</td><td>TKL</td></tr><tr><td>Tonga</td><td>TO</td><td>TON</td></tr><tr><td>Trinidad and Tobago</td><td>TT</td><td>TTO</td></tr><tr><td>Tunisia</td><td>TN</td><td>TUN</td></tr><tr><td>Turkey</td><td>TR</td><td>TUR</td></tr><tr><td>Turkmenistan</td><td>TM</td><td>TKM</td></tr><tr><td>Turks and Caicos Isds</td><td>TC</td><td>TCA</td></tr><tr><td>Tuvalu</td><td>TV</td><td>TUV</td></tr><tr><td>U.S. Virgin Isds</td><td>VI</td><td>VIR</td></tr><tr><td>Uganda</td><td>UG</td><td>UGA</td></tr><tr><td>Ukraine</td><td>UA</td><td>UKR</td></tr><tr><td>United Arab Emirates</td><td>AE</td><td>ARE</td></tr><tr><td>United Kingdom</td><td>GB</td><td>GBR</td></tr><tr><td>United States</td><td>US</td><td>USA</td></tr><tr><td>United States Minor Outlying Isds</td><td>UM</td><td>UMI</td></tr><tr><td>Uruguay</td><td>UY</td><td>URY</td></tr><tr><td>Uzbekistan</td><td>UZ</td><td>UZB</td></tr><tr><td>Vanuatu</td><td>VU</td><td>VUT</td></tr><tr><td>Vatican City</td><td>VA</td><td>VAT</td></tr><tr><td>Venezuela</td><td>VE</td><td>VEN</td></tr><tr><td>Vietnam</td><td>VN</td><td>VNM</td></tr><tr><td>Wallis and Futuna</td><td>WF</td><td>WLF</td></tr><tr><td>Western Sahara</td><td>EH</td><td>ESH</td></tr><tr><td>World</td><td>WD</td><td>WLD</td></tr><tr><td>Yemen</td><td>YE</td><td>YEM</td></tr><tr><td>Zambia</td><td>ZM</td><td>ZMB</td></tr><tr><td>Zimbabwe</td><td>ZW</td><td>ZWE</td></tr></tbody>
</table>"
        
      )
    )
  )
)

# Define functions with the logic required to draw the network graph and generate tables ####

get_taxon <- function(taxonomic) {
  if (taxonomic == "Octopus") {
    # Import databases
    load("df_octopus_1_250_countries_from_2000_to_2004.RData")
    load("df_octopus_1_250_countries_from_2005_to_2009.RData")
    load("df_octopus_1_250_countries_from_2010_to_2014.RData")
    load("df_octopus_1_250_countries_from_2015_to_2019.RData")
    db_x <- rbind(
      df_sp_1_250_2000_2004,
      df_sp_1_250_2005_2009,
      df_sp_1_250_2010_2014,
      df_sp_1_250_2015_2019
    )
  } else if (taxonomic == "Squid and cuttlefish") {
    # Import databases
    load("df_squid_1_250_countries_from_2000_to_2004.RData")
    load("df_squid_1_250_countries_from_2005_to_2009.RData")
    load("df_squid_1_250_countries_from_2010_to_2014.RData")
    load("df_squid_1_250_countries_from_2015_to_2019.RData")
    
    db_x <- rbind(
      df_sp_1_250_2000_2004,
      df_sp_1_250_2005_2009,
      df_sp_1_250_2010_2014,
      df_sp_1_250_2015_2019)
  }
  return(db_x)
}

get_range <- function(taxonomic, range_octopus, range_squid, x) {
  if (taxonomic == "Octopus") {
    range <- range_octopus
    if (range[1] == range[2]) {
      db_y <- filter(x, year ==   range[2])
    }
    else if (range[1] != range[2]) {
      db_y <- filter(x, year >=   range[1] & year <=   range[2])
    }
    return(db_y)
  }
  else if (taxonomic == "Squid and cuttlefish") {
    range <- range_squid
    if (range[1] == range[2]) {
      db_y <- filter(x, year ==   range[2])
    }
    else if (range[1] != range[2]) {
      db_y <- filter(x, year >=   range[1] & year <=   range[2])
    }
    return(db_y)
  }
}

get_qty <- function(type, x) {
  if (type == "mass (tonnes)") {
    x.export <- filter(x, trade_flow == "Export")
    x.export <-
      data.frame(
        from = x.export$reporter_iso,
        to = x.export$partner_iso,
        qty = as.numeric(as.character(x.export$qty)),
        year = x.export$year,
        cc = as.factor(x.export$commodity_code),
        cn = x.export$commodity
      )
    
    x.re_export <- filter(x, trade_flow == "Re-Export")
    x.re_export <-
      data.frame(
        from = x.re_export$reporter_iso,
        to = x.re_export$partner_iso,
        qty = as.numeric(as.character(x.re_export$qty)),
        year = x.re_export$year,
        cc = as.factor(x.re_export$commodity_code),
        cn = x.re_export$commodity
      )
    
    x.import <- filter(x, trade_flow == "Import")
    x.import <-
      data.frame(
        from = x.import$partner_iso,
        to = x.import$reporter_iso,
        qty = as.numeric(as.character(x.import$qty)),
        year = x.import$year,
        cc = as.factor(x.import$commodity_code),
        cn = x.import$commodity
      )
    
    x.re_import <- filter(x, trade_flow == "Re-Import")
    x.re_import <-
      data.frame(
        from = x.re_import$partner_iso,
        to = x.re_import$reporter_iso,
        qty = as.numeric(as.character(x.re_import$qty)),
        year = x.re_import$year,
        cc = as.factor(x.re_import$commodity_code),
        cn = x.re_import$commodity
      )
    
    x.all <-
      merge(
        x = x.export,
        y = x.import,
        by = c("from", "to", "year", "cc", "cn"),
        all = T
      )
    x.re_all <-
      merge(
        x = x.re_export,
        y = x.re_import,
        by = c("from", "to", "year", "cc", "cn"),
        all = T
      )
    
    x.total <-
      merge(
        x = x.all,
        y = x.re_all,
        by = c("from", "to", "year", "cc", "cn"),
        all = T
      )
    x.final <-
      data.frame(
        from = x.total$from,
        to = x.total$to,
        avg_qty = rowMeans(x.total[6:7], na.rm = T),
        year = x.total$year,
        cc = x.total$cc,
        cn = x.total$cn
      )
  } else if (type == "currency (USD millions)") {
    x.export <- filter(x, trade_flow == "Export")
    x.export <-
      data.frame(
        from = x.export$reporter_iso,
        to = x.export$partner_iso,
        qty = as.numeric(as.character(x.export$trade_value_usd)),
        year = x.export$year,
        cc = as.factor(x.export$commodity_code),
        cn = x.export$commodity
      )
    
    x.re_export <- filter(x, trade_flow == "Re-Export")
    x.re_export <-
      data.frame(
        from = x.re_export$reporter_iso,
        to = x.re_export$partner_iso,
        qty = as.numeric(as.character(x.re_export$trade_value_usd)),
        year = x.re_export$year,
        cc = as.factor(x.re_export$commodity_code),
        cn = x.re_export$commodity
      )
    
    x.import <- filter(x, trade_flow == "Import")
    x.import <-
      data.frame(
        from = x.import$partner_iso,
        to = x.import$reporter_iso,
        qty = as.numeric(as.character(x.import$trade_value_usd)),
        year = x.import$year,
        cc = as.factor(x.import$commodity_code),
        cn = x.import$commodity
      )
    
    x.re_import <- filter(x, trade_flow == "Re-Import")
    x.re_import <-
      data.frame(
        from = x.re_import$partner_iso,
        to = x.re_import$reporter_iso,
        qty = as.numeric(as.character(x.re_import$trade_value_usd)),
        year = x.re_import$year,
        cc = as.factor(x.re_import$commodity_code),
        cn = x.re_import$commodity
      )
    
    x.all <-
      merge(
        x = x.export,
        y = x.import,
        by = c("from", "to", "year", "cc", "cn"),
        all = T
      )
    x.re_all <-
      merge(
        x = x.re_export,
        y = x.re_import,
        by = c("from", "to", "year", "cc", "cn"),
        all = T
      )
    
    x.total <-
      merge(
        x = x.all,
        y = x.re_all,
        by = c("from", "to", "year", "cc", "cn"),
        all = T
      )
    x.final <-
      data.frame(
        from = x.total$from,
        to = x.total$to,
        avg_qty = rowMeans(x.total[6:7], na.rm = T),
        year = x.total$year,
        cc = x.total$cc,
        cn = x.total$cn
      )
  }
  return(x.final)
}
get_cc <- function(taxonomic, cc_octopus, cc_squid, x.final) {
  # ------------------ #
  # Filtering the DBs
  # ------------------ #
  x.filt <- filter(x.final,
                   avg_qty > 0 &
                     from != "WLD" & to != "WLD" &
                     from != "EU2" & to != "EU2")
  
  # Labeling type of process
  x.filt$type <- ifelse(
    (
      (x.filt$cc %in% c("030751", "030741", "030742"))
    ),
    "fresh",  # if condition is met, put 1
    "elaborated"   # else put 0
  )
  
  
  if (taxonomic == "Octopus")
    filt_codes <- cc_octopus
  else if (taxonomic == "Squid and cuttlefish")
    filt_codes <- cc_squid
  
  x.filt <- filter(x.filt, type %in% filt_codes)
  x.filt <- x.filt[complete.cases(x.filt),]
  
  return(x.filt)
}
get_graph <- function(x.filt) {
  # ------------------ #
  #  Creating the graphs
  # ------------------ #
  library(igraph)
  sp.web.yearly.links <-
    graph_from_data_frame(x.filt, directed = TRUE)
  E(sp.web.yearly.links)$weight <-
    as.numeric(E(sp.web.yearly.links)$avg_qty)
  
  sp.web.yearly <-
    induced_subgraph(sp.web.yearly.links, V(sp.web.yearly.links)[components(sp.web.yearly.links)$membership == which.max(components(sp.web.yearly.links)$csize)])
  
  x.TotalFlow <- x.filt %>%
    group_by(from, to) %>%
    summarise(TotalFlow = sum(as.numeric(avg_qty), na.rm = TRUE),
              n = n())
  
  sp.web.TotalFlow <-
    graph_from_data_frame(x.TotalFlow, directed = TRUE)
  
  E(sp.web.TotalFlow)$weight <-
    as.numeric(E(sp.web.TotalFlow)$TotalFlow)
  
  sp.web.total <-
    induced_subgraph(sp.web.TotalFlow, V(sp.web.TotalFlow)[components(sp.web.TotalFlow)$membership == which.max(components(sp.web.TotalFlow)$csize)])
  
  
  # Please choose between:
  # 1) The database with the transactions between countries by year (i.e., sp.web.yearly)
  # 2) The database with the year-on-year sum of transactions between countries (i.e., sp.web.total)
  
  sp.web <- sp.web.total
  
  # ------------------ #
  # Geocoding countries
  # ------------------ #
  # Two versions depending on whether the designation "Palestinian Territories" or "Gaza Strip" is used. Please edit the source to change it.
  library(readr)
  
  # ------------------ #
  #Geocoding countries
  # ------------------ #
  countriesgeocodes <- read_csv("geocode_gaza.csv", na = "null")
  
  # Repositioning WORLD node
  countriesgeocodes[which(countriesgeocodes$name == "World"), "longitude"] <-
    179
  
  
  vertices      <- data.frame(id = as.character(V(sp.web)$name))
  vertices$iso2 <- NaN
  vertices$lat  <- NaN
  vertices$lon  <- NaN
  
  for (i in 1:nrow(vertices)) {
    idx <- which(countriesgeocodes$isoAlpha3 == vertices$id[i])
    vertices$iso2[i]  <- countriesgeocodes$isoAlpha2[idx]
    vertices$lat[i]   <- countriesgeocodes$latitude[idx]
    vertices$lon[i]   <- countriesgeocodes$longitude[idx]
  }
  
  
  # ------------------ #
  # Calculate centrality
  # ------------------ #
  # ------------------ #
  #Normalize function
  # ------------------ #
  normalize_fun <- function(x) {
    a <- min(x)
    b <- max(x)
    (x - a) / (b - a)
  }
  
  #--------------------------
  ### Create graphs as a list
  #--------------------------
  gs <- list()
  gs[[1]] <- sp.web
  
  # ------------------------------- #
  # Calculate centrality measuses
  # ------------------------------- #
  
  # Esta sección determina que el "strength" sea la fuerza de la conexión y que el costo sea su valor inverso.
  gs <- lapply(gs, function(x) {
    E(x)$strength                  <- E(x)$weight
    E(x)$cost                      <-
      mean(E(x)$weight) / E(x)$weight
    return(x)
  })
  
  # Esta sección fuerza que los "weights" en el grafo representen el costo de conexión, lo que parece ser la manera por defecto en la que trabaja igraph
  gs <- lapply(gs, function(x) {
    E(x)$weight                    <- E(x)$cost
    return(x)
  })
  
  
  gs <- lapply(gs, function(x) {
    # Alternate name
    V(x)$iso2                       <- vertices$iso2
    
    # Geo-position for each node
    V(x)$x                          <- vertices$lon
    V(x)$y                          <- vertices$lat
    
    # Centrality measures
    V(x)$degree       <- degree(x, mode = "all")
    V(x)$in_degree    <- degree(x, mode = "in")
    V(x)$out_degree   <- degree(x, mode = "out")
    
    V(x)$strength     <- strength(x,
                                  weights = E(x)$strength,
                                  mode = "all",
                                  loops = T)
    V(x)$in_strength  <- strength(x,
                                  weights = E(x)$strength,
                                  mode = "in",
                                  loops = T)
    V(x)$out_strength <- strength(x,
                                  weights = E(x)$strength,
                                  mode = "out",
                                  loops = T)
    
    V(x)$evcent                    <- evcent(x, weights = E(x)$strength)$vector
    V(x)$hub_score                 <- hub.score(x, weights = E(x)$strength)$vector
    V(x)$auth_score                <- authority.score(x, weights = E(x)$strength)$vector
    V(x)$page_rank                 <- page_rank(x, weights = E(x)$strength)$vector
    
    # Here we calculate node betweenness with respect to the cost
    V(x)$betweenness           <- betweenness(x)
    # Here we calculate edge betweenness with respect to the cost
    E(x)$edge_betweenness      <- edge.betweenness(x)
    
    # Normalized Centrality measures
    V(x)$n.degree              <- normalize_fun(V(x)$degree)
    V(x)$n.strength            <- normalize_fun(V(x)$strength)
    V(x)$n.in_strength         <- normalize_fun(V(x)$in_strength)
    V(x)$n.out_strength        <- normalize_fun(V(x)$out_strength)
    V(x)$n.betweenness         <- normalize_fun(V(x)$betweenness)
    V(x)$n.evcent              <- normalize_fun(V(x)$evcent+1E-16)
    V(x)$n.page_rank            <- normalize_fun(V(x)$page_rank)
    V(x)$n.auth_score          <- normalize_fun(V(x)$auth_score)
    V(x)$n.hub_score           <- normalize_fun(V(x)$hub_score)
    
    E(x)$n.edge_betweenness    <- normalize_fun(E(x)$edge_betweenness)
    E(x)$n.edge_weight         <- normalize_fun(as.numeric(E(x)$weight))
    E(x)$n.edge_strength       <- normalize_fun(as.numeric(E(x)$strength ))
    E(x)$n.edge_cost           <- normalize_fun(as.numeric(E(x)$cost))
    
    if (is.connected(x) == TRUE){
      V(x)$closeness             <- closeness(x)
      V(x)$n.closeness           <- normalize_fun(V(x)$closeness)
    }
    return(x)
  })
  
  vstats <- do.call('rbind', lapply(1:length(gs), function(x) {
    o <- get.data.frame(gs[[x]], what = 'vertices')
    o$network <- get.graph.attribute(gs[[x]], "name")
    o$time <- x
    return(o)
  }))
  
  estats <- do.call('rbind', lapply(1:length(gs), function(x) {
    o <- get.data.frame(gs[[x]], what = 'edges')
    o$network <- get.graph.attribute(gs[[x]], "name")
    o$time <- x
    return(o)
  }))
  
  gstats <- do.call('rbind', lapply(gs, function(y) {
    ga <- list.graph.attributes(y)
    ga <- ga[1:length(ga)]
    sapply(ga, function(x) {
      get.graph.attribute(y, x)
    })
  }))
  
  # ------------------------------- #
  # Making some calculations in tnet
  # ------------------------------- #
  library(tnet)
  mat_q <- as.matrix(as_adjacency_matrix(sp.web, attr = "weight"))
  mat_n <- data.frame(id = c(1:ncol(mat_q)), name = colnames(mat_q))
  sp.web.tnet <- as.tnet(mat_q, type = "weighted one-mode tnet")
  tnet.vstat <-
    full_join(mat_n, as.data.frame(betweenness_w(sp.web.tnet)), by = c("id" = "node"))
    if (is.connected(sp.web) == TRUE){
      tnet.vstat <- full_join(tnet.vstat, as.data.frame(closeness_w(sp.web.tnet, gconly = FALSE)), by = c("id" = "node"))
      V(gs[[1]])$closeness.tnet               <- tnet.vstat$closeness
      V(gs[[1]])$n.closeness.tnet             <- normalize_fun(tnet.vstat$closeness)
    }
  
  # ------------------------------- #
  # Writing tnet calculations
  # ------------------------------- #
  
  V(gs[[1]])$betweenness.tnet             <- tnet.vstat$betweenness
  V(gs[[1]])$n.betweenness.tnet           <-
    normalize_fun(tnet.vstat$betweenness)

  
  return(list(
    gt1 = gs[[1]],
    ggg = gs[1],
    vstats = vstats,
    estats = estats,
    gstats = gstats
  ))
  
}

server <- function(input, output) {
  db_taxon <- reactive({
    get_taxon(taxonomic = input$taxonomic)
  })
  db_taxon_year <- reactive({
    x <- db_taxon()
    get_range(
      taxonomic = input$taxonomic,
      range_octopus = input$range_octopus,
      range_squid = input$range_squid,
      x = x
    )
  })
  db_taxon_year_type <- reactive({
    x <- db_taxon_year()
    get_qty(type = input$type, x = x)
  })
  db_taxon_year_type_cc <- reactive({
    x.final <- db_taxon_year_type()
    if (length(input$cc_octopus) == 0 | length(input$cc_squid) == 0) {
      get_cc(
        taxonomic = input$taxonomic,
        cc_octopus = c(
          "fresh"
        ),
        cc_squid = c(
          "fresh"
          ),
        x.final = x.final
      )
    } else if (length(input$cc_octopus) >= 1 |
               length(input$cc_squid) >= 1) {
      get_cc(
        taxonomic = input$taxonomic,
        cc_octopus = input$cc_octopus,
        cc_squid = input$cc_squid,
        x.final = x.final
      )
    }
  })
  graph_obj <- reactive({
    x.filt <- db_taxon_year_type_cc()
    get_graph(x.filt = x.filt)
  })
  
  # The Network Graph ####
  output$GraphPlot <- renderPlot({
    # ------------------ #
    # Map theme config.
    # ------------------ #
    ################# GEOPOSITIONATED NETWORK PLOT (WHITE) #################
    library(mapdata)
    library(ggraph)
    
    maptheme <-
      theme_minimal(base_size = 24) %+replace% #Relative size of plot
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank(),
        # Remove overall border
        legend.key = element_blank(),
        # Remove border around each item
        panel.background = element_rect(fill = "#ffffff"),
        plot.margin = unit(c(0, 0, 0, 0), 'cm')
      )
    
    country_shapes <-
      geom_polygon(
        aes(x = long, y = lat, group = group),
        data = map_data('world'),
        fill = "#f1f2ef",
        color = "#515151",
        size = 0.15
      )
    mapcoords <- coord_fixed(xlim = c(-180, 180), ylim = c(-78, 80))
    
    
    
    gt1 <- graph_obj()$gt1
    
    node_measure <-
      if (input$node_measure == "Strength")
        V(gt1)$n.strength
    else if (input$node_measure == "In-strength")
      V(gt1)$n.in_strength
    else if (input$node_measure == "Out-strength")
      V(gt1)$n.out_strength
    else if (input$node_measure == "Betweenness")
      V(gt1)$n.betweenness
    else if (input$node_measure == "Closeness")
      V(gt1)$n.closeness.tnet
    else if (input$node_measure == "Eigenvector")
      V(gt1)$n.evcent
    else if (input$node_measure == "Page's Rank")
      V(gt1)$n.page_rank
    else if (input$node_measure == "Authority score")
      V(gt1)$n.auth_score
    else if (input$node_measure == "Hub score")
      V(gt1)$n.hub_score
    
    edge_measure <-
      if (input$edge_measure == "Edge strength")
        E(gt1)$n.edge_strength
    else if (input$edge_measure == "Edge betweenness")
      E(gt1)$n.edge_betweenness
    
    node_legend <- paste("normalised", input$node_measure)
    edge_legend <- paste("norm.", input$edge_measure)
    
    legend_1 <- paste(input$taxonomic, "Global Trade Network")
    legend_2 <- if (input$taxonomic == "Octopus") {
      range <- input$range_octopus
      paste(input$type, "| Jan. ", range[1], " - Dec. ", range[2])
    } else if (input$taxonomic == "Squid and cuttlefish") {
      range <- input$range_squid
      paste(input$type, "| Jan. ", range[1], " - Dec. ", range[2])
    }
    legend_3 <- "Data from https://comtrade.un.org | United Nations"
    
    library(ggraph)
    library(RColorBrewer)
    library(scales)
    
    
    ### Which colorblind-friendly brewer palettes for nodes and arcs do you want to use?
    cbpalette_arcs_low <- "#87CEFF" # Light blue
    cbpalette_arcs_high <- "#27408B" # Dark blue
    cbpalette_arcs <- "GnBu"
    cbpalette_nodes <- "RdPu"
    
    ### Color of the nodes labels on the network graph.
    df_col_lab <- data.frame(measure = node_measure, color = "NA")
    df_col_lab$color <-
      ifelse(df_col_lab$measure >= 0.51, "white", "black")
    col_label_1 <- df_col_lab$color
    
    
    ### Legend position (top, bottom, right, left)
    legend_pos <- "right"
    
    # ---------------------------------
    # Make geo-positioned network graph
    ggg <- graph_obj()$ggg
    lapply(ggg, function(x) {
      g <- x
      ggraph(g,
             layout = "manual",
             x = V(g)$x,
             y = V(g)$y) +
        country_shapes +
        
        geom_edge_arc(
          aes(
            edge_color = edge_measure,
            edge_alpha = edge_measure,
            edge_width = edge_measure
          ),
          arrow = arrow(
            length = unit(1.6, 'mm'),
            type = "closed",
            angle = 15
          ),
          start_cap = circle(0.8, 'mm'),
          end_cap = circle(2.4, 'mm'),
          strength = 0.30
        ) +
        
        scale_edge_colour_gradient(low = cbpalette_arcs_low,
                                   high = cbpalette_arcs_high,
                                   limits = c(0, 1)) +
        
        scale_edge_alpha(range = c(0.1, 1),
                         limits = c(0, 1)) +
        
        scale_edge_width_continuous(range = c(0.8, 1.6),
                                    limits = c(0, 1)) +
        
        geom_node_point(aes(size = node_measure, color = node_measure)) +
        scale_color_gradientn(colours = brewer_pal(palette = cbpalette_nodes)(5),
                              limits = c(0, 1)) +
        scale_size_continuous(range = c(3, 6),
                              limits = c(0, 1)) +
        scale_alpha_continuous(range = c(0.4, 1),
                               limits = c(0, 1)) +
        
        geom_node_text(aes(label = iso2),
                       color = col_label_1,
                       size = 2) +
        
        guides(
          size = guide_legend(paste(node_legend), title.theme = element_text(size = 12)),
          color = guide_legend(paste(node_legend), title.theme = element_text(size = 12)),
          alpha = guide_legend(paste(node_legend), title.theme = element_text(size = 12)),
          edge_alpha = guide_legend(paste(edge_legend), title.theme = element_text(size = 12)),
          edge_width = guide_legend(paste(edge_legend), title.theme = element_text(size = 12)),
          edge_color = guide_legend(paste(edge_legend), title.theme = element_text(size = 12))
        ) +
        
        mapcoords +
        maptheme +
        # theme(legend.position = "none")
        theme(
          legend.text = element_text(size = 9),
          legend.position = "bottom",
          legend.box.spacing = unit(0.001, "cm"),
          legend.box.margin = unit(c(0, 0, 0, 0), 'cm'),
          legend.box = "vertical",
          legend.box.just = "bottom",
          legend.margin = margin(0, 0, 0, 0, 'cm'),
          legend.spacing = unit(0.2, "cm")
        ) +
        ggplot2::annotate(
          "text",
          x = -190,
          y = -53,
          hjust = 0,
          size = 4,
          label = paste(legend_1),
          color = "black"
        ) +
        
        ggplot2::annotate(
          "text",
          x = -190,
          y = -60,
          hjust = 0,
          size = 3.4,
          label = paste(legend_2),
          color = "black"
        ) +
        
        ggplot2::annotate(
          "text",
          x = -190,
          y = -67,
          hjust = 0,
          size = 3,
          label = paste(legend_3),
          color = "black",
          alpha = 0.5
        )
      
    })
  })
  
  # The Texts and Captions ####
  output$fig_caption <- renderText({
    if (input$taxonomic == "Octopus")
      range <- input$range_octopus
    else if (input$taxonomic == "Squid and cuttlefish")
      range <- input$range_squid
    paste0(
      "Figure 1. Global Trade Network for ",
      input$taxonomic,
      " between Jan. 1, ",
      range[1],
      " and Dec. 31, ",
      range[2],
      ". The numbers correspond to the normalised amount of ",
      input$type,
      " traded. Each node represents a trader and each edge represents the relationship between two traders. The size and color of the node represent the relative importance of the trader in the network in terms of its ",
      input$node_measure,
      ". The width and color of the edge represent the relative importance of the relationship between two traders in terms of their ",
      input$edge_measure,
      ". This graph is based on UN COMTRADE all avalaible"
    )
  })
  
  output$cc_caption <- renderText({
    if (input$taxonomic == "Octopus") {
      filt_codes <- input$cc_octopus
      if (length(filt_codes) == 0)
        filt_codes <- "fresh"
    }
    else if (input$taxonomic == "Squid and cuttlefish") {
      filt_codes <- input$cc_squid
      if (length(filt_codes) == 0)
        filt_codes <- "fresh"
    }
    paste0(filt_codes, collapse = " and ", recycle0 = T)
  })
  
  output$text_type <- renderText({
    paste0(input$type)
  })
  
  output$text_node_measure <- renderText({
    paste0(input$node_measure)
  })
  
  output$text_year_ini <- renderText({
    if (input$taxonomic == "Octopus")
      range <- input$range_octopus
    else if (input$taxonomic == "Squid and cuttlefish")
      range <- input$range_squid
    paste0(range[1])
  })
  
  output$text_year_end <- renderText({
    if (input$taxonomic == "Octopus")
      range <- input$range_octopus
    else if (input$taxonomic == "Squid and cuttlefish")
      range <- input$range_squid
    paste0(range[2])
  })
  
  # The Tables ####
  output$ExportsTable <- renderTable({
    vstats <- graph_obj()$vstats
    if (input$type == "mass (tonnes)") {
      vstats$out_strength <- vstats$out_strength / 1E3
      head(
        vstats %>% arrange(desc(out_strength)) %>% select(name, out_strength) %>% rename (trader = name, tonnes = out_strength)
      )
    } else if (input$type == "currency (USD millions)") {
      vstats$out_strength <- vstats$out_strength / 1E6
      head(
        vstats %>% arrange(desc(out_strength)) %>% select(name, out_strength) %>% rename (trader = name, "USD millions" = out_strength)
      )
    }
  }, digits = 2)
  output$ImportsTable <- renderTable({
    vstats <- graph_obj()$vstats
    if (input$type == "mass (tonnes)") {
      vstats$in_strength <- vstats$in_strength / 1E3
      head(
        vstats %>% arrange(desc(in_strength)) %>% select(name, in_strength) %>% rename (trader = name, tonnes = in_strength)
      )
    } else if (input$type == "currency (USD millions)") {
      vstats$in_strength <- vstats$in_strength / 1E6
      head(
        vstats %>% arrange(desc(in_strength)) %>% select(name, in_strength) %>% rename (trader = name, "USD millions" = in_strength)
      )
    }
  }, digits = 2)
  output$MeasureTable <- renderTable({
    vstats <- graph_obj()$vstats
    if (input$node_measure == "Strength") {
      head(vstats %>% arrange(desc(strength)) %>% select(name) %>% rename (trader = name))
    } else if (input$node_measure == "In-strength") {
      head(vstats %>% arrange(desc(in_strength)) %>% select(name) %>% rename (trader = name))
    } else if (input$node_measure == "Out-strength") {
      head(vstats %>% arrange(desc(out_strength)) %>% select(name) %>% rename (trader = name))
    } else if (input$node_measure == "Betweenness") {
      head(vstats %>% arrange(desc(betweenness)) %>% select(name) %>% rename (trader = name))
    } else if (input$node_measure == "Closeness") {
      head(tnet.vstat %>% arrange(desc(closeness)) %>% select(name) %>% rename (trader = name))
    } else if (input$node_measure == "Eigenvector") {
      head(vstats %>% arrange(desc(evcent)) %>% select(name) %>% rename (trader = name))
    } else if (input$node_measure == "Authority score") {
      head(vstats %>% arrange(desc(auth_score)) %>% select(name) %>% rename (trader = name))
    } else if (input$node_measure == "Hub score") {
      head(vstats %>% arrange(desc(hub_score)) %>% select(name) %>% rename (trader = name))
  } else if (input$node_measure == "Page's Rank") {
    head(vstats %>% arrange(desc(page_rank)) %>% select(name) %>% rename (trader = name))
  }
  }, digits = 2)
  output$FlowsTable <- renderTable({
    estats <- graph_obj()$estats
    if (input$type == "mass (tonnes)") {
      estats$TotalFlow <- estats$TotalFlow / 1E3
      head(
        estats %>% arrange(desc(TotalFlow)) %>% select(from, to, TotalFlow) %>% rename (
          exporter = from,
          importer = to,
          tonnes = TotalFlow
        )
      )
    } else if (input$type == "currency (USD millions)") {
      estats$TotalFlow <- estats$TotalFlow / 1E6
      head(
        estats %>% arrange(desc(TotalFlow)) %>% select(from, to, TotalFlow) %>% rename (
          exporter = from,
          importer = to,
          "USD millions" = TotalFlow
        )
      )
    }
  }, digits = 2)
  output$LinksTable <- renderTable({
    vstats <- graph_obj()$vstats
    head(
      vstats %>% arrange(desc(degree)) %>% select(name, degree, out_degree, in_degree) %>% rename (
        trader = name,
        'total links' = degree,
        'export links' = out_degree,
        'import links' = in_degree
      )
    )
  }, digits = 0)
  
}

# Run the application
shinyApp(ui = ui, server = server)
