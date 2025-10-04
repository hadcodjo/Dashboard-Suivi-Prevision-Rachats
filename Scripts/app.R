# app.R

# ─── Packages & options ──────────────────────────────────────────────────────
options(shiny.maxRequestSize = 50 * 1024^2)
# Vérifier que R_LIBS_USER a été pris en compte
#cat(".libPaths() =", .libPaths(), "\n")

# Désactiver toute tentative d'installation
#options(repos = c(CRAN = ""))

# Liste des packages à charger
#pkgs <- c("shiny","tidyr","MASS","splines","purrr","shinydashboard","readxl","dplyr","RInno","lubridate","ggplot2","scales","tibble","tinytex","tools","DT")
library(shiny)
library(shinydashboard)
library(tidyr)
library(purrr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(tibble)
library(tools)
library(DT)
library(MASS)
library(splines)
library(readxl)

# Vérification et chargement
#missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
#if (length(missing)) {
#  stop("Packages manquants : ", paste(missing, collapse = ", "))}
#invisible(lapply(pkgs, library, character.only = TRUE))

# ─── Fonctions globales ──────────────────────────────────────────────────────
compute_metrics <- function(df) {
  if(nrow(df)==0) return(tibble(
    Montant.Total=NA, Coût.Moy=NA, Duree.Moy.Contrat=NA,
    Duree.Moy.Rachat=NA, Montant.Min=NA, Montant.Max=NA,
    Produit.le.plus.racheté=NA))
  total_montant     <- sum(df$Montant.Rachat.Net, na.rm=TRUE)
  moy_montant       <- mean(df$Montant.Rachat.Net, na.rm=TRUE)
  moy_duree_contrat <- mean(df$Duree.du.contrat, na.rm=TRUE)
  moy_duree_rachat  <- mean(df$Duree_Mois_Rachat, na.rm=TRUE)
  min_montant       <- min(df$Montant.Rachat.Net, na.rm=TRUE)
  max_montant       <- max(df$Montant.Rachat.Net, na.rm=TRUE)
  tab_prod <- df %>% count(Nom.Produit) %>% arrange(desc(n))
  prod_top_str <- if(nrow(tab_prod)>0) {
    paste0(tab_prod$Nom.Produit[1], " (", round(tab_prod$n[1]/nrow(df)*100), "% )")
  } else NA_character_
  tibble(
    Montant.Total=total_montant, Coût.Moy=round(moy_montant),
    Duree.Moy.Contrat=round(moy_duree_contrat),
    Duree.Moy.Rachat=round(moy_duree_rachat),
    Montant.Min=min_montant, Montant.Max=max_montant,
    Produit.le.plus.racheté=prod_top_str
  )}
build_gender <- function(df) {
  if(nrow(df)==0) return(ggplot() + annotate("text",0.5,0.5,label="Pas de données") + theme_void())
  df_props<-df %>%  mutate(
    Sexe_clean = case_when(
      Sexe == "M" ~ "Homme",
      Sexe == "F" ~ "Femme",
      TRUE        ~ NA_character_)) %>%
    filter(!is.na(Sexe_clean),
           Type.de.sinistre %in% c("Rachat Total", "Rachat Partiel")) %>%
    count(Type.de.sinistre, Sexe_clean) %>%
    group_by(Type.de.sinistre) %>%
    mutate(
      pct  = n / sum(n) * 100,ymax = cumsum(pct),ymin = lag(ymax, default = 0),
      xmin = ifelse(Type.de.sinistre == "Rachat Total", 0.00, 0.63),
      xmax = ifelse(Type.de.sinistre == "Rachat Total", 0.61, 1.00),
      mid  = (ymin + ymax) / 2) %>% ungroup()
  ggplot(df_props) +
    geom_rect(aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = Sexe_clean),
              color = "white", size = 0.3) +coord_polar(theta = "y") +
    xlim(c(0, 1.05)) +  # Ajuste légèrement pour meilleure centration
    scale_fill_manual(breaks = c("Homme", "Femme"),
                      values = c("Homme" = "#4C72B0", "Femme" = "#C44E52"),labels = c("Homme", "Femme")) +
    geom_text(aes(x= (xmin + xmax) / 2, y= mid,label = paste0(
      ifelse(Type.de.sinistre == "Rachat Total", "RT : ", "RP : "),
      round(pct), "%")),color = "white",size = 4,fontface = "bold") +
    labs(title = "Répartition des Rachats par Genre et Type") +
    guides(fill = guide_legend(title = NULL)) +theme_void() + 
    theme(legend.position = "right",legend.text = element_text(size = 12),
          legend.key.size = unit(1, "cm"))}

build_age_rate <- function(df) {
  dfa <- df %>% filter(!is.na(Age_Rachat)&Age_Rachat>0)
  if(nrow(dfa)==0) return(ggplot() + annotate("text",0.5,0.5,label="Pas de données") + theme_void())
  dfa2 <- dfa %>% 
    filter(Type.de.sinistre %in% c("Rachat Total", "Rachat Partiel"),
           !is.na(Age_Rachat), Age_Rachat > 0) %>% group_by(Type.de.sinistre, Age_Rachat) %>%
    summarise(n = n(), .groups = "drop") %>%  group_by(Type.de.sinistre) %>%
    mutate(taux = 100 * n / sum(n)) %>% ungroup()
  
  age_medians <- dfa2 %>%   filter(Type.de.sinistre %in% c("Rachat Total", "Rachat Partiel")) %>%
    filter(!is.na(Age_Rachat) & Age_Rachat > 0) %>% group_by(Type.de.sinistre) %>%
    summarise(median_age = median(Age_Rachat, na.rm = TRUE)) %>%
    mutate(label = paste0(Type.de.sinistre, " : ", median_age, " ans"))
  
  ggplot(dfa2, aes(x = Age_Rachat, y = taux, color = Type.de.sinistre)) +
    geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 0.8) +
    scale_color_manual(values = c("Rachat Total" = "#E41A1C", "Rachat Partiel" = "#4C72B0")
    ) + labs(title = "",x = "Âge de l'assuré lors du rachat",
             y = "Taux de rachats en %",color = "Type de rachat") +
    scale_x_continuous(breaks = seq(15, 90, 6)) + theme_minimal() + 
    theme(
      plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text     = element_text(size = 10),
      axis.title    = element_text(size = 12),
      legend.position = "bottom",legend.title  = element_blank()) + 
    annotate("text",
             x = 85, y = max(dfa2$taux, na.rm = TRUE),
             label = paste0("Âge médian
", paste(age_medians$label, collapse = "
")),
             hjust = 1, vjust = 1, size = 4, fontface = "italic", color = "black")}
build_monthly <- function(df) {
  if(nrow(df)==0) return(ggplot() + annotate("text",0.5,0.5,label="Pas de données") + theme_void())
  dfm <- df %>%
    mutate(Mois=factor(month(Date.de.saisie.du.rachat.sinistres, label=TRUE,abbr=TRUE))) %>%
    count(Type.de.sinistre,Mois) %>%
    group_by(Type.de.sinistre) %>%
    mutate(pct=100*n/sum(n), rank_pct=dense_rank(desc(pct)),
           label=if_else(rank_pct<=2,paste0(round(pct),"%"),"")) %>%
    ungroup()
  ggplot(dfm, aes(Mois,pct,fill=Type.de.sinistre)) +
    geom_col(position=position_dodge(0.8),width=0.7) +
    geom_text(aes(label=label),position=position_dodge(0.8),vjust=-0.3,size=3) +
    scale_fill_manual(name="",values=c("Rachat Total"="#C44E52","Rachat Partiel"="#377EB8")) +
    scale_y_continuous(labels=scales::percent_format(scale=1),expand=expansion(mult=c(0,0.1))) +
    labs(title="",x="Mois",y="Taux (%)") +
    theme_minimal(base_size=10) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.major.y=element_line(color="grey80",linetype="dotted"),
          axis.text.x=element_text(angle=45,hjust=1),
          legend.position="top")
}
# Nouvelle fonction pour la courbe mensuelle
build_monthly_line <- function(df) {
  if(nrow(df)==0) return(ggplot() + annotate("text",0.5,0.5,label="Pas de données") + theme_void())
  dfm <- df %>%
    mutate(Mois = factor(month(Date.de.saisie.du.rachat.sinistres, label = TRUE, abbr = TRUE))) %>%
    filter(Type.de.sinistre %in% c("Rachat Total", "Rachat Partiel")) %>%
    group_by(Type.de.sinistre, Mois) %>%
    summarise(nb = n(), .groups = "drop") %>%
    group_by(Type.de.sinistre) %>%
    mutate(
      pct = 100 * nb / sum(nb),
      # repérer le minimum
      is_min = pct == min(pct),
      rank_pct = dense_rank(desc(pct)),
      label = if_else(rank_pct <= 2 | is_min, paste0(round(pct), "%"), "")
    ) %>% ungroup()
  
  min_pct <- min(dfm$pct)
  max_pct <- max(dfm$pct)
  buffer <- (max_pct - min_pct) * 0.1
  
  ggplot(dfm, aes(x = Mois, y = pct, color = Type.de.sinistre, group = Type.de.sinistre)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text(aes(label = label),
              vjust = ifelse(dfm$is_min, 1.5, -0.8), size = 3, show.legend = FALSE) +
    scale_color_manual(name = "", values = c("Rachat Total" = "#E41A1C", "Rachat Partiel" = "#377EB8")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = function(x) paste0(x, "%")) +
    coord_cartesian(ylim = c(min_pct - buffer, max_pct + buffer)) +
    labs(title = "Évolution mensuelle des rachats (Courbe)", x = "Mois", y = "Taux (%)") +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}


# ─── UI ───────────────────────────────────────────────
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = tagList(
      div(style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          span("Suivi des Rachats START LIFE", style = "color: white; font-size: 22px; font-weight: bold; margin-left: 10px;"),
          div(style = "display: flex; align-items: center;"       )
      )),titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML(
      ".row-eq-height .box {display:flex;align-items:center;justify-content:center;}
         .bg-purple {background-color: #A33478 !important;color: white !important;}
         .h2 {font-family: 'Helvetica Neue', sans-serif; font-weight: bold; color: #039800; margin-top: 5px;}
         .h3 {font-family: 'Helvetica Neue', sans-serif;font-weight: normal;color: #ffffff;font-size: 1.5em;margin-top: 5px;line-height: 1.4;text-align: center;}
         .btn-primary {font-family: 'Helvetica Neue', sans-serif;background-color: #039800 !important; border-color: #039800 !important; color: white !important;margin-top: 5px;}
         .logo-import {text-align:center; margin-top:20px;}
         .small-box.bg-green {background-color: #039800 !important; color: white !important;}
         .skin-green .main-header .logo, .skin-green .main-header .navbar {background-color: #039800 !important;}
         .custom-header-logo {position: absolute; top: 10px; right: 10px;}
         .center-box {display: flex; flex-direction: column; align-items: center;}
         .shiny-file-input-label {text-align: center; display: block; width: 100%;}
         .box-title {font-size: 20px !important;width: 100% !important; text-align: center !important;padding-bottom: 5px !important;}
        "
    ))),
    tabsetPanel(type = "tabs",
                tabPanel("Import",
                         div(class = "logo-import", img(src="logo_afg.png", height = "265px")),
                         h2("Tableau de Bord de suivi des Rachats", align="center"), br(),
                         fluidRow(
                           column(4),
                           column(4, div(class="center-box",
                                         fileInput("file_base", "Choisir baseRachats (xlsx/csv)", accept=c(".xlsx",".csv")),
                                         actionButton("go","Charger les données", class="btn-primary btn-lg"))),
                           column(4)
                         )
                ),
                tabPanel("Tableau de bord",
                         fluidRow(class = "row-eq-height",
                                  box(title = "Année",  status = "danger", solidHeader = TRUE, width = 4,
                                      selectInput("year", "Année", choices = NULL, selected = "all", multiple = TRUE)),
                                  box(title = "Produit",status = "danger",solidHeader = TRUE,width = 4,
                                      selectInput("produit","Produit",choices = NULL, selected = "all", multiple = TRUE)),
                                  box(title = "Mois",   status = "danger",solidHeader = TRUE,width = 4,
                                      selectInput("mois","Mois",choices = c("Tout"="all", setNames(month.abb, month.abb)),
                                                  selected = "all", multiple = TRUE))
                         ),
                         fluidRow(class = "row-eq-height",status = "success",
                                  valueBoxOutput("vb_nb_total", width = 4),
                                  valueBoxOutput("vb_nb_RT",    width = 4),
                                  valueBoxOutput("vb_nb_RP",    width = 4)),
                         fluidRow(class = "row-eq-height",status = "danger",
                                  valueBoxOutput("mon_total", width = 4),
                                  valueBoxOutput("mon_rt",    width = 4),
                                  valueBoxOutput("mon_rp",    width = 4)),
                         fluidRow(class = "row-eq-height",status = "danger",
                                  valueBoxOutput("Dur_rt",    width = 6),
                                  valueBoxOutput("Dur_rp",    width = 6)),
                         fluidRow(
                           box(title ="Indicateurs Clés",status = "success", solidHeader = TRUE, width = 12,
                               div(style = "text-align: center;", tableOutput("table_kpi")
                               ))
                         ),
                         fluidRow(
                           box(title = "Répartition par Genre", status = "success", solidHeader = TRUE, width = 6,
                               plotOutput("plot_gender", height = "300px")),
                           box(title = "Évolution du taux de rachats par âge", status = "success", solidHeader = TRUE, width = 6,
                               plotOutput("plot_age_rate", height = "300px"))
                         ),
                         fluidRow(
                           box(title = "Taux mensuel par Type (Barres)", status = "success", solidHeader = TRUE, width = 6,
                               plotOutput("plot_monthly", height = "350px")),
                           box(title = "Taux mensuel par Type (Courbe)", status = "success", solidHeader = TRUE, width = 6,
                               plotOutput("plot_monthly_line", height = "350px"))
                         )),
                tabPanel("Prévision",
                         fluidRow(class = "row-eq-height",
                                  box(title = "Année",  status = "danger", solidHeader = TRUE, width = 4,
                                      selectInput("year_prev", "Année", choices = NULL, selected = "all", multiple = TRUE)),
                                  box(title = "Produit",status = "danger",solidHeader = TRUE,width = 4,
                                      selectInput("produit_prev","Produit",choices = NULL, selected = "all", multiple = TRUE)),
                                  box(title = "Mois",   status = "danger",solidHeader = TRUE,width = 4,
                                      selectInput("mois_prev","Mois",choices = c("Tout"="all", setNames(month.abb, month.abb)),
                                                  selected = "all", multiple = TRUE))
                         ),
                         fluidRow(
                           valueBoxOutput("vb_total_prev", width = 4),
                           valueBoxOutput("vb_rt_prev", width = 4),
                           valueBoxOutput("vb_rp_prev", width = 4)
                         ),
                         fluidRow(class = "row-eq-height",status = "danger",
                                  valueBoxOutput("mon_total_prev", width = 4),
                                  valueBoxOutput("mon_rt_prev",    width = 4),
                                  valueBoxOutput("mon_rp_prev",    width = 4)),
                         fluidRow(
                           box(title = "Prévision mensuelle du nombre des rachats", status = "success", solidHeader = TRUE, width = 6,
                               plotOutput("plot_monthly_prev", height = "350px")),
                           box(title = "Prévision mensuelle des montants des rachats", status = "success", solidHeader = TRUE, width = 6,
                               plotOutput("plot_monthly_prev_amt", height = "350px"))
                         ))
    )
  )
)
# ─── Server ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  baseR <- eventReactive(input$go, {
    req(input$file_base)
    ext <- tools::file_ext(input$file_base$name)
    if(ext=="csv") read.csv(input$file_base$datapath, stringsAsFactors=FALSE)
    else readxl::read_excel(input$file_base$datapath)
  })
  
  observeEvent(baseR(), {
    df <- baseR()
    yrs   <- c("all", sort(unique(lubridate::year(df$Date.de.saisie.du.rachat.sinistres))))
    prods <- c("all", sort(unique(df$Nom.Produit)))
    mois  <- c("all", sort(unique(lubridate::month(df$Date.de.saisie.du.rachat.sinistres, label = TRUE))))
    Ids <- c("all",sort(unique(df$Identifiant)))
    updateSelectInput(session, "year",   choices = yrs,   selected = "all")
    updateSelectInput(session, "produit",choices = prods, selected = "all")
    updateSelectInput(session, "mois",   choices = mois, selected = "all")
    updateSelectInput(session, "Identifiant", choices = Ids, selected = "all")
  })
  
  filteredData <- reactive({
    df <- baseR()
    if(!("all" %in% input$year))
      df <- df %>% filter(lubridate::year(Date.de.saisie.du.rachat.sinistres) %in% input$year)
    if(!("all" %in% input$produit))
      df <- df %>% filter(Nom.Produit %in% input$produit)
    if(!("all" %in% input$mois))
      df <- df %>% filter(lubridate::month(Date.de.saisie.du.rachat.sinistres) %in% input$mois)
    df<- df %>% filter(Type.de.sinistre %in% c("Rachat Total", "Rachat Partiel"))
  })
  
  # KPI1
  output$vb_nb_total <- renderValueBox({
    n <- n <- sum(filteredData()$Type.de.sinistre %in% c("Rachat Total", "Rachat Partiel"),
                  na.rm = TRUE)
    valueBox(n, "Nombre total de rachats", icon = icon("file-invoice"),color ="teal")})
  output$vb_nb_RT <- renderValueBox({
    v <- sum(filteredData()$Type.de.sinistre == "Rachat Total", na.rm = TRUE)
    valueBox(v, "Nombre de Rachats Totaux", icon = icon("file-invoice"),color ="teal")})
  output$vb_nb_RP <- renderValueBox({
    v <- sum(filteredData()$Type.de.sinistre == "Rachat Partiel", na.rm = TRUE)
    valueBox(v, "Nombre de Rachats Partiels", icon = icon("file-invoice"),color ="teal")})

  # KPI2
  output$mon_total <- renderValueBox({
    M <- scales::comma(sum(filteredData()$Montant.Rachat.Net, na.rm = TRUE), accuracy=1, big.mark=" ", suffix=" CFA")
    valueBox(M, "Montant total des rachats", icon = icon("sack-dollar"),color ="red")})
  output$mon_rt <- renderValueBox({
    M1 <- scales::comma(sum(filteredData()$Montant.Rachat.Net[filteredData()$Type.de.sinistre == "Rachat Total"], na.rm = TRUE), accuracy=1, big.mark=" ", suffix=" CFA")
    valueBox(M1, "Montant des Rachats Totaux", icon = icon("sack-dollar"),color ="red")})
  output$mon_rp <- renderValueBox({
    M2 <- scales::comma(sum(filteredData()$Montant.Rachat.Net[filteredData()$Type.de.sinistre == "Rachat Partiel"], na.rm = TRUE), accuracy=1, big.mark=" ", suffix=" CFA")
    valueBox(M2, "Montants des Rachats Partiels", icon = icon("sack-dollar"),color ="red")})

  # KPI3 – Durées moyennes
  #output$Dur_total <- renderValueBox({
    #D <- filteredData()$Duree_Traitement_Rachat %>%
     # discard(~ .x <= 0) %>%
    #  mean(na.rm = TRUE) %>%
     # scales::comma(accuracy = 1, big.mark = " ", suffix = " Jours")
   # valueBox(D, "Durée moyenne de traitement des rachats", icon = icon("hourglass-half"), color = "purple")
  #})
  
  output$Dur_rt <- renderValueBox({
    D1 <- filteredData() %>%
      filter(Type.de.sinistre == "Rachat Total", Duree_Traitement_Rachat > 0) %>%
      pull(Duree_Traitement_Rachat) %>%
      mean(na.rm = TRUE) %>%
      scales::comma(accuracy = 1, big.mark = " ", suffix = " Jours")
    valueBox(D1, "Durée moyenne de traitement des Rachats Totaux", icon = icon("stopwatch"), color = "purple")
  })
  
  output$Dur_rp <- renderValueBox({
    D2 <- filteredData() %>%
      filter(Type.de.sinistre == "Rachat Partiel", Duree_Traitement_Rachat > 0) %>%
      pull(Duree_Traitement_Rachat) %>%
      mean(na.rm = TRUE) %>%
      scales::comma(accuracy = 1, big.mark = " ", suffix = " Jours")
    valueBox(D2, "Durée moyenne de traitement des Rachats Partiels", icon = icon("stopwatch"), color = "purple")
  })
  
  # Tableau
  
  output$table_kpi <- renderTable({
    df <- filteredData()
    
    m_rt <- compute_metrics(df %>% filter(Type.de.sinistre == "Rachat Total")) %>% 
      mutate(Métrique = "RT")
    m_rp <- compute_metrics(df %>% filter(Type.de.sinistre == "Rachat Partiel")) %>% 
      mutate(Métrique = "RP")
    m_en <- compute_metrics(df) %>% 
      mutate(Métrique = "Ensemble")
    
    bind_rows(m_rt, m_rp, m_en) %>% 
      dplyr::select(Métrique, everything()) %>%
      mutate(
        across(c(Montant.Total, Coût.Moy, Montant.Min, Montant.Max),
               ~ ifelse(is.na(.), "-", scales::comma(., accuracy = 1, big.mark = " ", suffix = " CFA"))),
        across(c(Duree.Moy.Contrat, Duree.Moy.Rachat),
               ~ ifelse(is.na(.), "-", paste(round(.), "mois"))),
        Produit.le.plus.racheté = ifelse(is.na(Produit.le.plus.racheté), "-", Produit.le.plus.racheté)
      ) %>%
      rename(
        "Montant total" = Montant.Total,
        "Montant moy Racheté" = Coût.Moy,
        "Durée moy contrat" = Duree.Moy.Contrat,
        "Durée moy rachat" = Duree.Moy.Rachat,
        "Montant minimum" = Montant.Min,
        "Montant maximum" = Montant.Max,
        "Produit le plus racheté" = Produit.le.plus.racheté
      )
  }, rownames = FALSE)
  # Graphiques
  output$plot_gender        <- renderPlot({ build_gender(filteredData()) })
  output$plot_age_rate      <- renderPlot({ build_age_rate(filteredData()) })
  output$plot_monthly      <- renderPlot({ build_monthly(filteredData()) })
  output$plot_monthly_line <- renderPlot({ build_monthly_line(filteredData()) })

output$table_pred <- DT::renderDataTable({
    req(input$Identifiant)
    df0 <- baseR()  
    
    # 2. Si l'utilisateur n'a PAS coché "all", on filtre
    if (!("all" %in% input$Identifiant)) {
      df0 <- df0 %>%filter(Identifiant %in% input$Identifiant)}
    df0 %>%
      dplyr::select(
        Identifiant,
        `Date d'effet` = Date.effet.du.contrat,
        `Date d'échéance` = Date.de.fin.du.contrat,
        `Durée du contrat` = Duree.du.contrat,
        `Type sinistre` = Type.de.sinistre,
        `Date du sinistre` = Date.de.saisie.du.rachat.sinistres,
        `Duree Traitement du Rachat` = Duree_Traitement_Rachat,
        `Montant` = Montant.Rachat.Net
      ) %>%
      mutate(
        Montant = scales::comma(Montant, accuracy=1, big.mark=" ", suffix=" CFA"),
        `Date d'effet` = as.character(`Date d'effet`),
        `Date d'échéance` = as.character(`Date d'échéance`),
        `Date du sinistre` = as.character(`Date du sinistre`),
        `Durée du contrat` = ifelse(is.na(`Durée du contrat`),
          "-",paste0(round(`Durée du contrat`), " mois")),
        `Duree Traitement du Rachat` = ifelse(is.na(`Duree Traitement du Rachat`),
                                    "-",paste0(round(`Duree Traitement du Rachat`), " jours"))) %>%
      DT::datatable(rownames=FALSE,
                    options=list(pageLength=5, lengthMenu=c(5,10,25), autoWidth=TRUE))})

  # Calcul des prévisions (réactif)
  forecast_res <- reactive({
    req(baseR())
    # 1) CONSTRUCTION DE LA SERIE AGRÉGÉE -----------------------
    
    # 1.1) calcul des bornes
    dates <- ymd(baseR()$Date.de.saisie.du.rachat.sinistres)
    date_min <- min(dates, na.rm=TRUE)
    date_max <- max(dates, na.rm=TRUE)
    
    # 1.2) on crée un calendrier mensuel complet
    cal_mois <- tibble(
      d      = seq(date_min, date_max, by="1 month")) %>% 
      mutate(
        année = year(d),
        mois  = month(d),
        t     = row_number())
    
    # 1.3) on agrége counts & montants par mois et type
    agrég <- baseR() %>%
      mutate(d = ymd(Date.de.saisie.du.rachat.sinistres),
             année = year(d),
             mois  = month(d),
             type = case_when(
               Type.de.sinistre=="Rachat Total"   ~ "total",
               Type.de.sinistre=="Rachat Partiel" ~ "partiel")) %>%
      group_by(année, mois, type) %>%
      summarise(
        n       = n(),
        montant = sum(Montant.Rachat.Net, na.rm=TRUE),
        .groups="drop") %>%
      pivot_wider(
        names_from  = type,
        values_from = c(n, montant),
        values_fill = list(n = 0))
    # 1.4) Correction finale du replace_na (supprimer n_autre)
    serie_globale <- cal_mois %>%
      dplyr::select(année, mois, t) %>%
      left_join(agrég, by = c("année", "mois")) %>%
      replace_na(list(n_total   = 0,
                      n_partiel = 0)) %>% 
      arrange(t)
    
    # 2) AJUSTEMENT DES MODÈLES ------------------------------------
    
    # 2.1) facteur mois pour la saisonnalité
    serie_globale <- serie_globale %>%
      mutate(facteur_mois = factor(mois, levels = 1:12))
    
    # 2.2) Initialiser les objets de modèle
    fit_tot <- list()
    fit_part <- list()
    
    # 2.2) Poisson sur counts
    fit_tot$count <- glm(n_total ~ facteur_mois + ns(t, df = 4),
                         family = poisson(link = "log"), 
                         data = serie_globale)
    fit_part$count <- glm(n_partiel ~ facteur_mois + ns(t, df = 4),
                          family = poisson(link = "log"), 
                          data = serie_globale)
    
    # 2.3) Gamma sur montants (mois où montant)
    df_amt_tot  <- filter(serie_globale, !is.na(montant_total))
    df_amt_part <- filter(serie_globale, !is.na(montant_partiel))
    fit_tot$amt  <- glm(montant_total ~ facteur_mois + ns(t, df = 4),
                        family = Gamma(link = "log"),
                        data = df_amt_tot)
    fit_part$amt <- glm(montant_partiel ~ facteur_mois + ns(t, df = 4),
                        family = Gamma(link = "log"),
                        data = df_amt_part)
    
    # --- 3) PRÉVISION DES 12 MOIS SUIVANTS --------------------------
    dernier_t     <- max(serie_globale$t)
    dernier_date  <- cal_mois %>% filter(t == dernier_t) %>% pull(d)
    # Créons new_period avec année et mois
    new_period <- tibble(
      d            = seq(dernier_date %m+% months(1),
                         by = "1 month",
                         length.out = 12)) %>%
      mutate(
        année        = year(d),
        mois         = month(d),
        t            = (dernier_t + 1):(dernier_t + 12),
        facteur_mois = factor(mois, levels = 1:12),
        h            = row_number())
    
    forecast_globale <- new_period %>%
      transmute(
        année,mois,t,h,
        pred_n_total     = predict(fit_tot$count,     new_period, type="response"),
        pred_n_partiel   = predict(fit_part$count,    new_period, type="response"),
        pred_amt_total   = predict(fit_tot$amt,       new_period, type="response"),
        pred_amt_partiel = predict(fit_part$amt,      new_period, type="response")
      )
    
    # --- 4) CALCUL DES PROPORTIONS HISTORIQUES ----------------------
    prop_count <- baseR() %>%
      mutate(d = ymd(Date.de.saisie.du.rachat.sinistres),
             mois = month(d)) %>%
      count(Code.Produit, mois, name = "ni") %>%
      group_by(mois) %>% mutate(prop_count = ni / sum(ni)) %>%
      ungroup()
    
    prop_amt <- baseR() %>%
      mutate(d = ymd(Date.de.saisie.du.rachat.sinistres),
             mois = month(d)) %>%
      group_by(Code.Produit, mois) %>%
      summarise(mi = sum(Montant.Rachat.Net, na.rm=TRUE), .groups="drop") %>%
      group_by(mois) %>% mutate(prop_amt = mi / sum(mi)) %>%
      ungroup()
    
    prod_lookup <- baseR() %>%
      distinct(Code.Produit, Nom.Produit, .keep_all = FALSE)
    # --- 5) REDISTRIBUTION PAR PRODUIT -------------------------------
    produits <- unique(baseR()$Code.Produit)
    
    res <- expand_grid(
      Code.Produit = produits,
      h            = 1:12) %>%
      left_join(forecast_globale, by = "h") %>%
      left_join(prop_count,       by = c("Code.Produit", "mois")) %>%
      left_join(prop_amt,         by = c("Code.Produit", "mois")) %>%
      replace_na(list(prop_count = 0, prop_amt = 0)) %>%
      mutate(
        fc_n_total       = round(pred_n_total   * prop_count),
        fc_n_partiel     = round(pred_n_partiel * prop_count),
        fc_amt_total     = round(pred_amt_total * prop_amt),
        fc_amt_partiel   = round(pred_amt_partiel * prop_amt)) %>%
      left_join(prod_lookup, by = "Code.Produit") %>%
      dplyr::select(
        Code.Produit,Nom.Produit,année,mois,h,fc_n_total,
        fc_n_partiel,fc_amt_total,fc_amt_partiel)  # ... [votre code complet de prévision] ...
    return(res)
  })
  
  # Mise à jour des filtres de prévision
  observeEvent(forecast_res(), {
    df <- forecast_res()
    yrs   <- c("all", sort(unique(df$année)))
    prods <- c("all", sort(unique(df$Nom.Produit)))
    mois  <- c("all", sort(unique(month.abb[df$mois])))
    
    updateSelectInput(session, "year_prev", choices = yrs, selected = "all")
    updateSelectInput(session, "produit_prev", choices = prods, selected = "all")
    updateSelectInput(session, "mois_prev", choices = mois, selected = "all")
  })
  
  # Données filtrées pour les prévisions
  filteredPrev <- reactive({
    df <- forecast_res()
    if(!("all" %in% input$year_prev))
      df <- df %>% filter(année %in% input$year_prev)
    if(!("all" %in% input$produit_prev))
      df <- df %>% filter(Nom.Produit %in% input$produit_prev)
    if(!("all" %in% input$mois_prev))
      df <- df %>% filter(month.abb[mois] %in% input$mois_prev)
    df
  })
  
  # ValueBoxes pour les prévisions
  output$vb_total_prev <- renderValueBox({
    df <- filteredPrev()
    total <- sum(df$fc_n_total + df$fc_n_partiel, na.rm = TRUE)
    valueBox(total, "Prévision Globale", icon = icon("chart-line"), color = "teal")
  })
  
  output$vb_rt_prev <- renderValueBox({
    df <- filteredPrev()
    rt <- sum(df$fc_n_total, na.rm = TRUE)
    valueBox(rt, "Prévision Rachats Totaux", icon = icon("chart-line"), color = "teal")
  })
  
  output$vb_rp_prev <- renderValueBox({
    df <- filteredPrev()
    rp <- sum(df$fc_n_partiel, na.rm = TRUE)
    valueBox(rp, "Prévision Rachats Partiels", icon = icon("chart-line"), color = "teal")
  })
  
  
  # ValueBoxes pour les prévisions
  output$mon_total_prev <- renderValueBox({
    df <- filteredPrev()
    total <- scales::comma(sum(df$fc_amt_total + df$fc_amt_partiel, na.rm = TRUE), accuracy=1, big.mark=" ", suffix=" CFA")
    valueBox(total, "Montant Global", icon = icon("sack-dollar"), color = "purple")})
  
  output$mon_rt_prev <- renderValueBox({
    df <- filteredPrev()
    rt <- scales::comma(sum(df$fc_amt_total, na.rm = TRUE), accuracy=1, big.mark=" ", suffix=" CFA")
    valueBox(rt, "Montant Rachats Totaux", icon = icon("sack-dollar"), color = "purple")})
  
  output$mon_rp_prev <- renderValueBox({
    df <- filteredPrev()
    rp <- scales::comma(sum(df$fc_amt_partiel, na.rm = TRUE), accuracy=1, big.mark=" ", suffix=" CFA")
    valueBox(rp, "Montant Rachats Partiels", icon = icon("money-check-alt"), color = "purple")})
  
  # Graphique des prévisions mensuelles
  output$plot_monthly_prev <- renderPlot({
    df <- filteredPrev() %>%
      group_by(année, mois) %>%
      summarise(
        fc_n_total = sum(fc_n_total),
        fc_n_partiel = sum(fc_n_partiel),
        .groups = "drop"
      ) %>%
      mutate(
        Mois = factor(month.abb[mois], levels = month.abb),
        Date = ymd(paste(année, mois, "01", sep = "-")))
    
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = fc_n_total, color = "Rachats Totaux"), size = 1) +
      geom_line(aes(y = fc_n_partiel, color = "Rachats Partiels"), size = 1) +
      geom_point(aes(y = fc_n_total, color = "Rachats Totaux"), size = 2) +
      geom_point(aes(y = fc_n_partiel, color = "Rachats Partiels"), size = 2) +
      scale_color_manual(
        name = "",
        values = c("Rachats Totaux" = "#E41A1C", "Rachats Partiels" = "#377EB8")
      ) +
      labs(title = "", x = "Mois", y = "Nombre prévu") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )})
  
  output$plot_monthly_prev_amt <- renderPlot({
    df <- filteredPrev() %>%
      group_by(année, mois) %>%
      summarise(
        fc_amt_total = sum(fc_amt_total),
        fc_amt_partiel = sum(fc_amt_partiel),
        .groups = "drop"
      ) %>%
      mutate(
        Mois = factor(month.abb[mois], levels = month.abb),
        Date = ymd(paste(année, mois, "01", sep = "-")))
    
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = fc_amt_total, color = "Rachats Totaux"), size = 1) +
      geom_line(aes(y = fc_amt_partiel, color = "Rachats Partiels"), size = 1) +
      geom_point(aes(y = fc_amt_total, color = "Rachats Totaux"), size = 2) +
      geom_point(aes(y = fc_amt_partiel, color = "Rachats Partiels"), size = 2) +
      scale_color_manual(
        name = "",
        values = c("Rachats Totaux" = "#E41A1C", "Rachats Partiels" = "#377EB8")
      ) +
      labs(title = "", x = "Mois", y = "Nombre prévu") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )})
}
shinyApp(ui, server)