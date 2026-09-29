#######################################################################################
#                                                                                     #
# APPLICATION DE SUIVI de l'Enquête Permanente Agricole (EPA) : VERSION DEMO           #
# Author: Innocent KIENDREBEOGO                                                       #
#                                                                                     #
# All data loaded by this demo are SYNTHETIC (see data-raw/generer_donnees_fictives.R).#
# Demo access codes: "demo" = national supervisor (all regions);                      #
#                    "demo1" ... "demo5" = regional supervisor (Région A ... E).      #
#                                                                                     #
#######################################################################################


###############################################################################
# 0.  Packages ────────────────────────────────────────────────────────────────
###############################################################################
library(shiny)
library(shinyMobile)
library(shinyjs)
library(plotly)
library(dplyr)
library(DT)
library(readxl)
library(openxlsx)

###############################################################################
# 1.  Données, utils, labels                        ---------------------------
###############################################################################



#   Importation des données

MEMBRE_data      <- readRDS("MEMBRE.rds")
PARCELLE_data    <- readRDS("PARCELLE.rds")
CHEPTEL_data     <- readRDS("CHEPTEL.rds")
MARAICHAGE_data     <- readRDS("MARAICHAGE.rds")
PREVISION_data     <- readRDS("PREVISION.rds")
ABANDONNEES_data     <- readRDS("ABANDONNEES.rds")

MEMBRE_NT_data   <- readRDS("MEMBRE_NT.rds")
PARCELLE_NT_data <- readRDS("PARCELLE_NT.rds")
CHEPTEL_NT_data  <- readRDS("CHEPTEL_NT.rds")
MARAICHAGE_NT_data  <- readRDS("MARAICHAGE_NT.rds")
PREVISION_NT_data  <- readRDS("PREVISION_NT.rds")
ABANDONNEES_NT_data  <- readRDS("ABANDONNEES_NT.rds")
data0            <- readRDS("data0.rds")



# Importation des taux de progression par formulaire. Chaque feuille excel concerne les progresseions 
# d'un formulaire. Le code suivant permet de recupérer les informations formulaire par formulaire. 
# La feuille LABEL contient le label des variable. Par exemple "Menage_touche" signifie	"Ménage touché".

chemin_taux <- "TAUX_PROGRESSION.xlsx"
forms       <- c("MEMBRE","PARCELLE","CHEPTEL","MARAICHAGE","PREVISION","ABANDONNEES")
evo_info <- lapply(forms, function(frm){
  raw  <- read_excel(chemin_taux, sheet = frm) |>
    mutate(across(c(REG,PROV,COM,VIL), as.character))
  c1 <- names(raw)[5]; c2 <- names(raw)[6]
  list(data = raw |> mutate(MENAGE_plot=.data[[c1]], MEMBRE_plot=.data[[c2]]),
       col1 = c1, col2 = c2)
})
names(evo_info) <- forms

labels_df <- read_excel(chemin_taux,"LABEL") |> mutate(across(everything(),as.character))
label_map <- setNames(labels_df$Label, labels_df$Variable)
get_label <- function(v) ifelse(!is.na(label_map[v]), label_map[v], v)


# Ici, j'ai choisi les élément a afficher dans les anneau de progression en fonction du filtre. 
# Par exemple, si la region est filtré sur "tous", on affiche les progressions on niveau national car c'est pour toutes les régions.

get_evo_row <- function(data,reg,prov,com,vil){
  if(reg=="Tous"&&prov=="Tous"&&com=="Tous"&&vil=="Tous")
    return(filter(data,is.na(REG)|REG==""|REG=="NATIONAL") |> slice(1))
  if(reg!="Tous"&&prov=="Tous"&&com=="Tous"&&vil=="Tous")
    return(filter(data,REG==reg&(is.na(PROV)|PROV==""))    |> slice(1))
  if(prov!="Tous"&&com=="Tous"&&vil=="Tous")
    return(filter(data,PROV==prov&(is.na(COM)|COM==""))    |> slice(1))
  if(com!="Tous"&&vil=="Tous")
    return(filter(data,COM==com&(is.na(VIL)|VIL==""))      |> slice(1))
  if(vil!="Tous") return(filter(data, REG == reg, PROV == prov, COM == com, VIL == vil) |> slice(1))
  slice(data,0)
}
make_filename <- function(section,form,reg,prov,com,vil,ext="xlsx"){
  filt <- c(REG=reg,PROV=prov,COM=com,VIL=vil); filt <- filt[filt!="Tous"]
  paste0(section,"_",form,"_",
         ifelse(length(filt)==0,"NATIONAL",paste(filt,collapse="_")),
         "_",format(Sys.Date(),"%d-%m"),".",ext)
}

###############################################################################
# 2.  Palette de couleur                            ---------------------------
###############################################################################


# J'ai definit des fonctions pour créer une palette personnalisée de taux qui permet aux grapgique de changer
# de couleur selon l'évolution. Il sont rouges pour des taux bas. On a deux options, palette continuous ou par classes. 
# Fonction pour obtenir une couleur continue basée sur le taux.
get_color_continuous <- function(taux) {
  taux_norm <- pmax(0, pmin(100, taux)) / 100
  rouge <- c(255, 0, 0)    # #FF0000
  vert  <- c(0, 176, 80)   # #00B050
  r <- round(rouge[1] + (vert[1] - rouge[1]) * taux_norm)
  g <- round(rouge[2] + (vert[2] - rouge[2]) * taux_norm)
  b <- round(rouge[3] + (vert[3] - rouge[3]) * taux_norm)
  sprintf("#%02X%02X%02X", r, g, b)
}

# C de 10 classes prédéfinies
PALETTE_10_CLASSES <- c(
  "#FF0000", "#FF4000", "#FF8000", "#FFBF00", "#FFFF00",
  "#BFFF00", "#80FF00", "#40FF00", "#20FF40", "#00B050"
)

get_color_classes <- function(taux) {
  classe <- pmin(10, pmax(1, ceiling(pmax(0.1, taux) / 10)))
  PALETTE_10_CLASSES[classe]
}

###############################################################################
# 3.  Codes d'accès   ---------------------------------------------------------
###############################################################################
# Chaque superviseur régional a son code : son filtre Région est verrouillé.
# Codes de DEMONSTRATION uniquement (publics). En production, ne jamais écrire
# les codes dans le code source : les lire par ex. depuis une variable
# d'environnement (Sys.getenv("APP_ACCESS_CODES")).
ACCESS_CODES <- list(
  "Région A"="demo1","Région B"="demo2","Région C"="demo3",
  "Région D"="demo4","Région E"="demo5","SUPERVISEUR"="demo")



###############################################################################
# 4.  Interface  de connection ------------------------------------------------
###############################################################################

# Dans cette partie, je travail sur l'interface de connection. Elle est fait en HTML et CSS.
# J'ai aussi utilisé du JavaScript pour limiter la saisie des codes d'accès aux chiffres et à 4 caractères

app_opts <- list(theme="ios", dark=FALSE, filled=TRUE, color="#005A47")

ui <- f7Page(
  allowPWA=TRUE, title="Suivi Enquete", options=app_opts,
  useShinyjs(),
  
  ## ---------- overlay login ----------
  tags$head(tags$style(HTML("#login_page{
  position:fixed;
  inset:0;
  z-index:9999;
  display:flex;
  justify-content:center;
  align-items:center;
  padding: 20px; 
  background:linear-gradient(130deg,#00734D,#FFFFFF,#009E6D);
  background-size:300% 300%;
  animation:bgFlow 6s ease infinite;
}

.dt-buttons .buttons-pdf {
  background: #D62828 !important;
  color: #fff !important;
  border: none !important;
  border-radius: 9999px !important;
  padding: 8px 16px !important;
  font-weight: 700 !important;
  box-shadow: 0 2px 6px rgba(0,0,0,.15) !important;
  min-width: 60px !important;
  max-width: 60px !important;
  width: 60px !important;
  height: 36px !important;
  display: inline-flex !important;
  align-items: center !important;
  justify-content: center !important;
  text-align: center !important;
  font-size: 12px !important;
  white-space: nowrap !important;
  overflow: hidden !important;
  flex-shrink: 0 !important;
}

.dt-buttons .buttons-pdf:hover {
  filter: brightness(0.95);
}  

/* Style pour le conteneur des boutons DataTable */
.dt-buttons {
  display: flex !important;
  align-items: center !important;
  gap: 8px !important;
  flex-wrap: nowrap !important;
}

/* Responsive : sur mobile, garder la même taille */
@media (max-width: 768px) {
  .dt-buttons .buttons-pdf {
    min-width: 60px !important;
    max-width: 60px !important;
    width: 60px !important;
    height: 36px !important;
    font-size: 11px !important;
  }
}

/* Pour très petits écrans */
@media (max-width: 480px) {
  .dt-buttons .buttons-pdf {
    min-width: 55px !important;
    max-width: 55px !important;
    width: 55px !important;
    height: 32px !important;
    font-size: 10px !important;
    padding: 6px 12px !important;
  }
}

#login_card{
  width:100%;
  max-width:420px;
  margin: 0 20px; /* Marges latérales */
  padding:32px 28px;
  border-radius:24px;
  background:#FFF;
  box-shadow:0 8px 32px rgba(0,0,0,.25);
  text-align:center;
  color:#00432E;
  transform:translateY(30px);
  opacity:0;
  animation:cardFade .8s forwards cubic-bezier(.23,1,.32,1);
}

/* Responsive pour petits écrans */
@media (max-width: 480px) {
  #login_page {
    padding: 15px;
  }
  
  #login_card {
    margin: 0 15px;
    padding: 24px 20px;
    max-width: none; /* Permet à la carte de s'adapter */
  }
}

/* Responsive pour très petits écrans */
@media (max-width: 360px) {
  #login_page {
    padding: 10px;
  }
  
  #login_card {
    margin: 0 10px;
    padding: 20px 16px;
  }
}

@keyframes bgFlow{
  0%{background-position:0% 50%;}
  50%{background-position:100% 50%;}
  100%{background-position:0% 50%;}
}

@keyframes cardFade{
  to{transform:translateY(0);opacity:1;}
}

#login_card img{
  max-width:110px;
  margin-bottom:20px;
}

#login_card .title{
  font-size:20px;
  font-weight:700;
  line-height:1.3;
  color:#00734D;
}

#submit{
  width:100%;
  padding:14px 0;
  border:none;
  border-radius:14px;
  font-weight:700;
  font-size:16px;
  color:#FFF;
  background:#00734D;
}

.image-container{
  display:flex;
  justify-content:space-around;
  margin-top:24px;
}

.image-container img{
  max-width:80px;
  filter:drop-shadow(0 2px 6px rgba(0,0,0,.3));
}
    /* styles OBSERVATION + dlBtn (originaux) */
    # En fait, dans notre application, les colonnes OBSERVATION contiennent beaucoup de texte. Nous les reservons donc beaucoup d'espace ici
    .dataTables_wrapper table th:contains('OBSERVATION'),
    .dataTables_wrapper table td:nth-child(n){min-width:100px;}
    .dataTables_wrapper table th[data-column='OBSERVATION'],
    .dataTables_wrapper table td[data-column='OBSERVATION'],
    .dataTables_wrapper table th:last-child,
    .dataTables_wrapper table td:last-child{
      min-width:400px!important;max-width:400px!important;
      word-wrap:break-word;white-space:normal;}
    .dlBtn{border-radius:12px!important;
           box-shadow:0 2px 6px rgba(0,0,0,.15)!important;font-weight:600!important;}
  ")),
            
            # JavaScript pour limiter la saisie aux lettres/chiffres et à 8 caractères
            # (version originale : 4 chiffres ; la démo accepte les codes "demo", "demo1"...)
            tags$script(HTML("
    $(document).ready(function() {
      function clean(v) {
        v = v.replace(/[^0-9a-zA-Z]/g, '').toLowerCase();
        return v.length > 8 ? v.substring(0, 8) : v;
      }
      $('#access_code').on('input', function() {
        $(this).val(clean($(this).val()));
      });
      $('#access_code').on('paste', function(e) {
        setTimeout(function() {
          $('#access_code').val(clean($('#access_code').val()));
        }, 10);
      });
      // Touche Entrée = connexion
      $('#access_code').on('keyup', function(e) {
        if (e.key === 'Enter') { $('#submit').click(); }
      });
    });
  "))),
  
  div(id="login_page",
      div(id="login_card",
          img(src="icon.png"),
          div(class="title",HTML("APPLICATION DE SUIVI<br>EPA : VERSION DEMO")),
          tags$input(id="access_code", type="text", placeholder="Code d'accès (demo)",
                     maxlength="8", style="width:100%;padding:14px;margin:26px 0 14px;border:none;
                     border-radius:14px;font-size:16px;background:#EAF9F1;color:#00432E;
                     text-align:center;letter-spacing:2px;font-weight:600;"),
          actionButton("submit","Entrer"),
          div(style="margin-top:18px;font-size:13px;color:#4A6B5E;line-height:1.4;",
              HTML("Données fictives (synthétiques).<br>
                    Code <b>demo</b> : toutes les régions ;
                    <b>demo1</b> à <b>demo5</b> : une région."))
      )
  ),
  

  ###############################################################################
  # 5.  Interface de l'application shiny ----------------------------------------
  ###############################################################################
  
  f7TabLayout(
    panels = tagList(
      f7Panel(id="mypanel1",side="left",title="Formulaires",effect="push",
              f7PanelMenu(id="menu",
                          f7PanelItem(tabName="MEMBRE",title="MEMBRE",icon=f7Icon("person_3_fill"),active=TRUE),
                          f7PanelItem(tabName="PARCELLE",title="PARCELLE",icon=f7Icon("square_grid_2x2")),
                          f7PanelItem(tabName="CHEPTEL",title="CHEPTEL",icon=f7Icon("cat")),
                          f7PanelItem(tabName="MARAICHAGE", title="MARAICHAGE", icon=f7Icon("leaf_arrow_circlepath")),
                          f7PanelItem(tabName="PREVISION", title="PREVISION", icon=f7Icon("chart_bar_square")),
                          f7PanelItem(tabName="ABANDONNEES", title="ABANDONNEES", icon=f7Icon("square_stack_3d_up")))),
      f7Panel(id="mypanel2",side="right",title="Filtres",effect="cover",
              selectInput("reg","Région",choices=c("Tous",unique(data0$REG)),selected="Tous"),
              selectInput("prov","Province",choices=c("Tous"),selected="Tous"),
              selectInput("com","Commune",choices=c("Tous"),selected="Tous"),
              selectInput("vil","Village",choices=c("Tous"),selected="Tous"),
              f7Button("reset_filters","Réinitialiser",color="red"))
    ),
    navbar = f7Navbar(
      title = div(style="display:flex;align-items:center;justify-content:center;width:100%;",
                  img(src="icon.png",height=40),
                  span("Démo : données fictives",style="margin-left:10px;color:white;font-weight:bold;")),
      leftPanel=TRUE,rightPanel=TRUE,style="background-color:#00734D;"),
    f7Tabs(id="tabs",animated=TRUE,
           f7Tab(title="Evolution",tabName="Evolution",icon=f7Icon("chart_pie_fill"),
                 div(style="height:calc(100vh - 120px);display:flex;flex-direction:column;justify-content:space-around;padding:10px;",
                     uiOutput("evolution_title"),
                     plotlyOutput("plot_menage",height="250px"),
                     plotlyOutput("plot_membre",height="250px"))),
           f7Tab(title="Non touché",tabName="Non_touche",icon=f7Icon("nosign"),
                 div(style="height:calc(100vh - 120px);display:flex;flex-direction:column;padding:10px;",
                     uiOutput("non_touche_header"),
                     div(style="flex:1;overflow-x:auto;overflow-y:auto;min-height:400px;",
                         DTOutput("table_non_touche")))),
           f7Tab(title="Incoherence",tabName="Incoherence",icon=f7Icon("exclamationmark_triangle_fill"),
                 div(style="height:calc(100vh - 120px);display:flex;flex-direction:column;padding:10px;",
                     uiOutput("incoherence_header"),
                     div(style="flex:1;overflow-x:auto;overflow-y:auto;min-height:400px;",
                         DTOutput("table_incoherence"))))
    )
  )
)



###############################################################################
# 3.  Serveur -----------------------------------------------------------------
###############################################################################
server <- function(input, output, session){
  
  ## 3-A. Authentification + verrouillage filtre REG --------------------------
  # La technique utilisé est la suivante. En fontion du code d'accès entré, on filtre une region 
  # et on desactive la possibilité de choisir une autre région. De cette manère, chaque superviseur 
  # regional à son code et ne peut pas observer pour les autres.
  
  user_role <- reactiveVal(NULL)
  observeEvent(input$submit,{
    # Vérifier le format du code (lettres/chiffres, 1 à 8 caractères)
    code <- tolower(trimws(if (is.null(input$access_code)) "" else input$access_code))
    if(!grepl("^[0-9a-z]{1,8}$", code)) {
      showNotification("Code d'accès invalide (essayez : demo)", type="error")
      return()
    }

    role <- names(ACCESS_CODES)[ACCESS_CODES == code]
    if(length(role)){
      role <- role[1]; user_role(role)
      shinyjs::hide("login_page",anim=TRUE,animType="fade")
      
      if(role=="SUPERVISEUR"){
        updateSelectInput(session,"reg",
                          choices=c("Tous",unique(data0$REG)),selected="Tous")
        shinyjs::enable("reg")
      } else {
        updateSelectInput(session,"reg",choices=role,selected=role)
        shinyjs::disable("reg")
      }
      
      ## — déclenche immédiatement la mise à jour cascade -------------------
      updateSelectInput(session,"prov",choices=c("Tous"),selected="Tous")
      updateSelectInput(session,"com", choices=c("Tous"),selected="Tous")
      updateSelectInput(session,"vil", choices=c("Tous"),selected="Tous")
      isolate({
        # retrigger observer on reg
        updateSelectInput(session,"reg",selected=input$reg)
      })
    } else {
      showNotification("Code d'accès incorrect",type="error")
    }
  })
  
  ## 3-B. États réactifs & logique
  current_data      <- reactiveVal(MEMBRE_data)
  current_data_NT   <- reactiveVal(MEMBRE_NT_data)
  current_evo_info  <- reactiveVal(evo_info[["MEMBRE"]])
  current_form_name <- reactiveVal("MEMBRE")
  
  observeEvent(input$menu,{
    updateF7Tabs(id="tabs",selected=input$menu)
    runjs('app.panel.close("left");')
    frm <- input$menu
    current_data     (switch(frm,"MEMBRE"=MEMBRE_data,"PARCELLE"=PARCELLE_data,"CHEPTEL"=CHEPTEL_data,"MARAICHAGE"=MARAICHAGE_data,"PREVISION"=PREVISION_data,"ABANDONNEES"=ABANDONNEES_data))
    current_data_NT  (switch(frm,"MEMBRE"=MEMBRE_NT_data,"PARCELLE"=PARCELLE_NT_data,"CHEPTEL"=CHEPTEL_NT_data,"MARAICHAGE"=MARAICHAGE_NT_data,"PREVISION"=PREVISION_NT_data,"ABANDONNEES"=ABANDONNEES_NT_data))
    current_evo_info (evo_info[[frm]])
    current_form_name(frm)
  })
  
  observeEvent(input$reset_filters,{
    role <- user_role()
    if(!is.null(role) && role!="SUPERVISEUR"){
      updateSelectInput(session,"reg",selected=role); shinyjs::disable("reg")
    } else {
      updateSelectInput(session,"reg",selected="Tous"); shinyjs::enable("reg")
    }
    updateSelectInput(session,"prov",choices=c("Tous"),selected="Tous")
    updateSelectInput(session,"com", choices=c("Tous"),selected="Tous")
    updateSelectInput(session,"vil", choices=c("Tous"),selected="Tous")
  })
  
  observeEvent(input$reg,{
    choix <- if(input$reg=="Tous") unique(data0$PROV) else unique(data0$PROV[data0$REG==input$reg])
    updateSelectInput(session,"prov",choices=c("Tous",choix),selected="Tous")
    updateSelectInput(session,"com",choices=c("Tous"),selected="Tous")
    updateSelectInput(session,"vil",choices=c("Tous"),selected="Tous")
  })
  observeEvent(input$prov,{
    choix <- if(input$prov=="Tous") unique(data0$COM) else unique(data0$COM[data0$PROV==input$prov])
    updateSelectInput(session,"com",choices=c("Tous",choix),selected="Tous")
    updateSelectInput(session,"vil",choices=c("Tous"),selected="Tous")
  })
  observeEvent(input$com,{
    choix <- if(input$com=="Tous") unique(data0$VIL) else unique(data0$VIL[data0$COM==input$com])
    updateSelectInput(session,"vil",choices=c("Tous",choix),selected="Tous")
  })
  
  filtered_data <- reactive({
    d <- current_data()
    if(input$reg  != "Tous") d <- filter(d,REG == input$reg)
    if(input$prov != "Tous") d <- filter(d,PROV== input$prov)
    if(input$com  != "Tous") d <- filter(d,COM == input$com)
    if(input$vil  != "Tous") d <- filter(d,VIL == input$vil)
    d
  })
  filtered_data_NT <- reactive({
    d <- current_data_NT()
    if(input$reg  != "Tous") d <- filter(d,REG == input$reg)
    if(input$prov != "Tous") d <- filter(d,PROV== input$prov)
    if(input$com  != "Tous") d <- filter(d,COM == input$com)
    if(input$vil  != "Tous") d <- filter(d,VIL == input$vil)
    d
  })
  
  evolution_row <- reactive({
    get_evo_row(current_evo_info()$data,
                input$reg,input$prov,input$com,input$vil)
  })
  
  output$evolution_title <- renderUI(f7BlockTitle(title=current_form_name()))
  
  make_donut <- function(taux, titre) {
    # Chox de la palette : 'continuous' ou 'classes'
    approche <- "classes"  
    
    couleur_principale <- if(approche == "classes") {
      get_color_continuous(taux)
    } else {
      get_color_classes(taux)
    }
    
    plot_ly(
      labels = c("Complété", "Restant"),
      values = c(taux, 100 - taux),
      type = "pie",
      hole = 0.6,
      marker = list(
        colors = c(couleur_principale, "#E5E5E5"),
        line = list(color = "white", width = 3)
      ),
      textinfo = "none",
      showlegend = FALSE
    ) |>
      add_annotations(
        text = paste0(taux, "%"),
        x = 0.5,
        y = 0.5,
        font = list(size = 24, color = couleur_principale, family = "Arial Black"),
        showarrow = FALSE
      ) |>
      layout(
        title = list(
          text = paste0("<b>", titre, "</b>"),
          font = list(size = 18, color = couleur_principale)
        ),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        margin = list(t = 50, b = 50, l = 50, r = 50)
      ) |>
      config(displayModeBar = FALSE)
  }
  
  output$plot_menage <- renderPlotly({
    d <- evolution_row(); if(!nrow(d)) return(NULL)
    make_donut(as.numeric(d$MENAGE_plot[1]), get_label(current_evo_info()$col1))
  })
  output$plot_membre <- renderPlotly({
    d <- evolution_row(); if(!nrow(d)) return(NULL)
    make_donut(as.numeric(d$MEMBRE_plot[1]), get_label(current_evo_info()$col2))
  })
  
  render_dt <- function(d) datatable(head(d,100),
                                     options=list(pageLength=25,scrollX=TRUE,searching=FALSE,lengthChange=FALSE,info=FALSE),
                                     rownames=FALSE)
  output$table_non_touche <- renderDT({
    d <- filtered_data_NT()
    datatable(
      head(d, 100),
      extensions = "Buttons",
      options = list(
        pageLength   = 25,
        scrollX      = TRUE,
        searching    = FALSE,
        lengthChange = FALSE,
        info         = FALSE,
        dom          = "Bfrtip",
        buttons      = list(
          list(
            extend      = "pdf",
            text        = "PDF",
            title       = NULL,               # pas de titre en tête
            filename    = make_filename("Non_touche", current_form_name(),
                                        input$reg, input$prov, input$com, input$vil,
                                        ext = "pdf"),
            orientation = "landscape",
            pageSize    = "A4",
            exportOptions = list(
              columns = ":visible"           # exporte les colonnes visibles
            )
          )
        )
      ),
      rownames = FALSE
    )
  })
  
  output$table_incoherence <- renderDT({
    d <- filtered_data()
    datatable(
      head(d, 100),
      extensions = "Buttons",
      options = list(
        pageLength   = 25,
        scrollX      = TRUE,
        searching    = FALSE,
        lengthChange = FALSE,
        info         = FALSE,
        dom          = "Bfrtip",
        buttons      = list(
          list(
            extend      = "pdf",
            text        = "PDF",
            title       = NULL,
            filename    = make_filename("Incoherence", current_form_name(),
                                        input$reg, input$prov, input$com, input$vil,
                                        ext = "pdf"),
            orientation = "landscape",
            pageSize    = "A4",
            exportOptions = list(
              columns = ":visible"
            )
          )
        )
      ),
      rownames = FALSE
    )
  })
  
  
  output$non_touche_header <- renderUI({
    div(style="display:flex;align-items:center;justify-content:space-between;",
        f7BlockTitle(title=current_form_name()),
        f7DownloadButton("xls_non_touche","Excel",
                         icon=f7Icon("doc_on_doc_fill"),color="blue",fill=TRUE,
                         bold=TRUE,class="dlBtn"))
  })
  output$incoherence_header <- renderUI({
    div(style="display:flex;align-items:center;justify-content:space-between;",
        f7BlockTitle(title=current_form_name()),
        f7DownloadButton("xls_incoherence","Excel",
                         icon=f7Icon("doc_on_doc_fill"),color="blue",fill=TRUE,
                         bold=TRUE,class="dlBtn"))
  })
  
  output$xls_non_touche <- downloadHandler(
    filename=function(){make_filename("Non_touche",current_form_name(),
                                      input$reg,input$prov,input$com,input$vil)},
    content=function(file){
      wb<-createWorkbook(); addWorksheet(wb,"data")
      writeDataTable(wb,"data",filtered_data_NT(),withFilter=TRUE)
      saveWorkbook(wb,file,overwrite=TRUE)
    })
  output$xls_incoherence <- downloadHandler(
    filename=function(){make_filename("Incoherence",current_form_name(),
                                      input$reg,input$prov,input$com,input$vil)},
    content=function(file){
      wb<-createWorkbook(); addWorksheet(wb,"data")
      writeDataTable(wb,"data",filtered_data(),withFilter=TRUE)
      saveWorkbook(wb,file,overwrite=TRUE)
    })
}

###############################################################################
###############################################################################
###############################################################################

shinyApp(ui, server)

###############################################################################
###############################################################################