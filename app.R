#
# This is the BERTA Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(leaflet)
library(stringi)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(stringr)
library(leaflet.extras)
library(knitr)
# devtools::install_github('rstudio/leaflet')
# devtools::install_github('bhaskarvk/leaflet.extras')

data <- readRDS("berta_data.rds")

# rmdfiles <- c("readme.Rmd")
# sapply(rmdfiles, knit, quiet = T)

# Create the appropriate .rds
# data <- data %>% group_by(name) %>%
#     filter(length(unique(year)) > 2)
#
# data
#
# my_data <- data %>% group_by(name) %>%
#     summarize(X_long,
#               Y_lat) %>%
#     distinct()
#
# saveRDS(my_data, "format_data/berta_data.rds")

names <- unique(data$name)

lngs <- data$X_long
lats <- data$Y_lat
names <- data$name

popup_string <- '<a href="https://github.com/ChrisFishCahill/BERTA-WAP/raw/main/tableau__R0G.pdf", 
target="_blank"></a>'

popup_string <- rep(popup_string, length(names))

idx <- stri_locate_all(popup_string, regex = "_")[[1]][1]

for (i in 1:length(names)) {
  stri_sub(popup_string[i], idx + 1, idx) <- names[i]
  idx2 <- stri_locate_all(popup_string[i], regex = ">")[[1]][1]
  stri_sub(popup_string[i], idx2 + 1, idx2) <- "click here for tableau" #names[i]

  idx3 <- stri_locate_all(popup_string[i], regex = " ")[[1]][2]
  stri_sub(popup_string[i], idx3, idx3) <- "_"
  
  if (nrow(stri_locate_all(popup_string[i], regex = " ")[[1]]) > 3) {
    idx3 <- stri_locate_all(popup_string[i], regex = " ")[[1]][2]
    stri_sub(popup_string[i], idx3, idx3) <- "_"
    if (nrow(stri_locate_all(popup_string[i], regex = " ")[[1]]) > 3) {
      idx3 <- stri_locate_all(popup_string[i], regex = " ")[[1]][2]
      stri_sub(popup_string[i], idx3, idx3) <- "_"
    }
  }
}

#Get rid of a stupid `_`
popup_string <- str_replace_all(popup_string, "_t", " t")
popup_string <- str_replace_all(popup_string, "click_here", "click here")

#set up 4 strings (ricker skinny, wide, bh skinny, wide)
#skinny = N(,0.1); wide = N(,0.5)
#"_bh_R0G_narrow.pdf"     #popup_string3
#"_bh_R0G_wide.pdf"       #popup_string4
#"_ricker_R0G_narrow.pdf" #popup_string 
#"_ricker_R0G_wide.pdf"   #popup_string2

popup_string1 <- str_replace_all(popup_string, "R0G.pdf", "ricker_R0G_narrow.pdf")
popup_string2 <- str_replace_all(popup_string, "R0G.pdf", "ricker_R0G_wide.pdf")
popup_string3 <- str_replace_all(popup_string, "R0G.pdf", "bh_R0G_narrow.pdf")
popup_string4 <- str_replace_all(popup_string, "R0G.pdf", "bh_R0G_wide.pdf")

#cr = 12 strings
popup_string5 <- str_replace_all(popup_string1, "narrow.pdf", "narrow_cr12.pdf")
popup_string6 <- str_replace_all(popup_string3, "narrow.pdf", "narrow_cr12.pdf")

#add in the popup crap
#Get all the hogwash between "tableau and .pdf"
popup_string1 <- paste0(popup_string1, '<img src = "https://github.com/ChrisFishCahill/BERTA-WAP/raw/main/.png" width="500" height="350px" type="application/pdf"/>') 
popup_string2 <- paste0(popup_string2, '<img src = "https://github.com/ChrisFishCahill/BERTA-WAP/raw/main/.png" width="500" height="350px" type="application/pdf"/>') 
popup_string3 <- paste0(popup_string3, '<img src = "https://github.com/ChrisFishCahill/BERTA-WAP/raw/main/.png" width="500" height="350px" type="application/pdf"/>') 
popup_string4 <- paste0(popup_string4, '<img src = "https://github.com/ChrisFishCahill/BERTA-WAP/raw/main/.png" width="500" height="350px" type="application/pdf"/>') 
popup_string5 <- paste0(popup_string5, '<img src = "https://github.com/ChrisFishCahill/BERTA-WAP/raw/main/.png" width="500" height="350px" type="application/pdf"/>') 
popup_string6 <- paste0(popup_string6, '<img src = "https://github.com/ChrisFishCahill/BERTA-WAP/raw/main/.png" width="500" height="350px" type="application/pdf"/>') 

pattern <- "tableau\\s*(.*?)\\s*.pdf"
which_png <- str_extract(popup_string1, pattern)
which_png <- gsub("tableau", "trajectory", which_png) 
which_png <- gsub(".pdf", ".png", which_png)
popup_string1 <- str_replace_all(popup_string1, ".png", which_png)

which_png <- str_extract(popup_string2, pattern)
which_png <- gsub("tableau", "trajectory", which_png) 
which_png <- gsub(".pdf", ".png", which_png)
popup_string2 <- str_replace_all(popup_string2, ".png", which_png)

which_png <- str_extract(popup_string3, pattern)
which_png <- gsub("tableau", "trajectory", which_png) 
which_png <- gsub(".pdf", ".png", which_png)
popup_string3 <- str_replace_all(popup_string3, ".png", which_png)

which_png <- str_extract(popup_string4, pattern)
which_png <- gsub("tableau", "trajectory", which_png) 
which_png <- gsub(".pdf", ".png", which_png)
popup_string4 <- str_replace_all(popup_string4, ".png", which_png)

which_png <- str_extract(popup_string5, pattern)
which_png <- gsub("tableau", "trajectory", which_png) 
which_png <- gsub(".pdf", ".png", which_png)
popup_string5 <- str_replace_all(popup_string5, ".png", which_png)

which_png <- str_extract(popup_string6, pattern)
which_png <- gsub("tableau", "trajectory", which_png) 
which_png <- gsub(".pdf", ".png", which_png)
popup_string6 <- str_replace_all(popup_string6, ".png", which_png)

my_data <- data.frame(name = names, lat = lats, long = lngs, 
                      popup_string1, 
                      popup_string2, 
                      popup_string3, 
                      popup_string4, 
                      popup_string5,
                      popup_string6
) 

ui <- fluidPage(
  navbarPage("Bayesian Estimation of Recruitment Trends in Alberta Walleye Access Portal (BERTA-WAP)",
    id = "main",
    tabPanel(
      "Map", leafletOutput("mymap", height = 1000),
      absolutePanel(
        top = 80, right = 40,
        pickerInput(
          inputId = "lake",
          label = "Select a lake:",
          choices = c("all lakes", eval(names)),
          selected = "all lakes",
          options = list(
            `live-search` = TRUE,
            "max-options-group" = 1
          ),
        )
      ),

      absolutePanel(
        top = 150, right = 40,
        pickerInput(
          inputId = "fit",
          label = "Select a model run:",
          choices = list(
            `dome-shaped recruitment` = list(
              "Ricker cr = 6",
              "Ricker cr = 12"
            ),
            `asymptotic recruitment` = list(
              "Beverton-Holt cr = 6",
              "Beverton-Holt cr = 12"
            )
          ),
          selected = "Beverton-Holt cr = 6",
          options = list(
            "max-options-group" = 1,
            `live-search` = TRUE
          )
        )
      ),
    ),
    tabPanel("User Guide", includeMarkdown("readme.md")),
    tabPanel("Stan Code", includeMarkdown("stan_code.md"))
  ),
  p()
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({

    # Pointers for Ricker fits:
    if (input$lake == "all lakes" && input$fit == "Ricker cr = 6") {
      filtered_data <-
        my_data %>% mutate(marker_string = popup_string1)
    }
    if (input$lake == "all lakes" && input$fit == "Ricker cr = 12") {
      filtered_data <-
        my_data %>% mutate(marker_string = popup_string5)
    }
    else if (input$lake != "all lakes" && input$fit == "Ricker cr = 6") {
      filtered_data <-
        my_data %>%
        mutate(marker_string = popup_string1) %>%
        filter(name == input$lake)
    }
    else if (input$lake != "all lakes" && input$fit == "Ricker cr = 12") {
      filtered_data <- my_data %>%
        mutate(marker_string = popup_string5) %>%
        filter(name == input$lake)
    }

    # Pointers for BH fits:
    if (input$lake == "all lakes" && input$fit == "Beverton-Holt cr = 6") {
      filtered_data <-
        my_data %>% mutate(marker_string = popup_string3)
    }
    if (input$lake == "all lakes" && input$fit == "Beverton-Holt cr = 12") {
      filtered_data <-
        my_data %>% mutate(marker_string = popup_string6)
    }
    else if (input$lake != "all lakes" && input$fit == "Beverton-Holt cr = 6") {
      filtered_data <-
        my_data %>%
        mutate(marker_string = popup_string3) %>%
        filter(name == input$lake)
    }
    else if (input$lake != "all lakes" && input$fit == "Beverton-Holt cr = 12") {
      filtered_data <- my_data %>%
        mutate(marker_string = popup_string6) %>%
        filter(name == input$lake)
    }
    
    filtered_data %>%
      leaflet() %>%
      addTiles() %>% # Add default OpenStreetMap map tiles
      addProviderTiles("Stamen.Terrain") %>%
      addMarkers(
        lng = filtered_data$long, lat = filtered_data$lat,
        popup = filtered_data$marker_string, popupOptions = list(maxWidth = 500)
      ) 
  })
}

shinyApp(ui, server)
