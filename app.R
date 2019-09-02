## Postcoder ##

library(shiny) ; library(shinyWidgets) ; library(tidyverse) ; library(PostcodesioR) ; library(leaflet) ; library(htmlwidgets)

server <- function(input, output) {

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -2.330256, lat= 53.421829, zoom = 12)
  })

  click <- eventReactive(input$map_click,{
    event <- input$map_click
  })

  observe({

    if (input$selection == "postcode") {

      observeEvent(input$submit, {

        result <- postcode_lookup(input$target)

        tryCatch({
        leafletProxy("map") %>%
          clearMarkers() %>% clearPopups() %>%
          setView(lng = result$longitude, lat = result$latitude, zoom = 18) %>%
          addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 5, maxZoom = 17)) %>%
          addCircleMarkers(lng = result$longitude, lat = result$latitude, fill = TRUE, stroke = TRUE,
                           weight = 2, color = "#fc6721", opacity = 1,
                           group = "marker") %>%
          addPopups(lng = result$longitude, lat = result$latitude,
                    popup = paste0("<h4>", result$postcode, "</h4>",
                                   "<b> Local authority: </b>", result$admin_district,
                                   "<br><b> CCG: </b>", result$ccg,
                                   "<br><b> Constituency: </b>", result$parliamentary_constituency,
                                   "<br><b>Ward: </b>", result$admin_ward,
                                   "<br><b>MSOA: </b>", result$msoa,
                                   "<br><b>LSOA: </b>", result$lsoa)) %>%
          onRender(paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'",");}"))
        },
        error = function(e) {
          showModal(modalDialog(tags$h5("Please enter a valid postcode")))
        })
      })
    }
    else {

      result <- reverse_geocoding(longitude = click()$lng,
                                  latitude = click()$lat,
                                  limit = 1)

     leafletProxy("map") %>%
        clearMarkers() %>% clearPopups() %>%
        setView(lng = click()$lng, lat = click()$lat, zoom = 18) %>%
        addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 5, maxZoom = 17), group = "Road map") %>%
        addCircleMarkers(lng = click()$lng, lat = click()$lat, fill = TRUE, stroke = TRUE,
                         weight = 2, color = "#fc6721", opacity = 1) %>%
        addPopups(lng = click()$lng, lat = click()$lat,
                  popup = ifelse(
                    is.null(result),
                    paste0("<h5>Sorry, there is no postcode at this location.</h5>"),
                    paste0("<h4>", result[[1]]$postcode, "</h4>",
                           "<b> Local authority: </b>", result[[1]]$admin_district,
                           "<br><b> CCG: </b>", result[[1]]$ccg,
                           "<br><b> Constituency: </b>", result[[1]]$parliamentary_constituency,
                           "<br><b>Ward: </b>", result[[1]]$admin_ward,
                           "<br><b>MSOA: </b>", result[[1]]$msoa,
                           "<br><b>LSOA: </b>", result[[1]]$lsoa)
                  )) %>%
        onRender(paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'",");}"))
    }
  })

}

ui <- bootstrapPage(
  tags$head(includeCSS("styles.css"),
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
            titlePanel(
              div(
                class = "headerContainer",
                a(
                  img(
                    src = "https://github.com/traffordDataLab/traffordDataLab.github.io/raw/master/images/trafford_council_logo_black_on_white_100px.png",
                    style = "position: relative; top: -5px;",
                    height = 60
                  ),
                  href = "https://www.trafford.gov.uk",
                  target = "_blank"
                ),
                "Postcoder"
              ),
              windowTitle = "Postcoder"
            ),
            leafletOutput("map", width = "100%", height = "87%"),
            absolutePanel(class = "panel panel-default controls", top = 100, left = 70,
                          width = "185px",
                          radioButtons(inputId = "selection",
                                       label = NULL,
                                       choices = c("Click location" = "location", "Enter postcode" = "postcode"),
                                       selected = "location"),
                          conditionalPanel(
                            condition = "input.selection == 'postcode'",
                            tags$div(class = "div1", textInput("target", label = NULL , width = '105px', placeholder = NULL)),
                            tags$div(class = "div1", actionButton("submit", "", icon("paper-plane-o")))),
                          tags$br(),
                          tags$div(class = "div2", tags$small("Powered by ", tags$a(href="http://postcodes.io/", "Postcodes.io")))
            ),
            div(
              style = "position: absolute; left: 1.3em; bottom: 4.5em;",
              dropdown(
                includeMarkdown("help.md"),
                icon = icon("question"),
                size = "xs",
                style = "jelly",
                width = "300px",
                up = TRUE
              ),
              tags$style(
                HTML(
                  '.fa {color: #212121;}
            .bttn-jelly.bttn-default{color:#f0f0f0;}
            .bttn-jelly:hover:before{opacity:1};'
                ))),
            tags$footer(
              fluidRow(
                "Developed in ",
                a(href = "https://cran.r-project.org/", target = "_blank", "R"),
                " by the ",
                a(href = "https://www.trafforddatalab.io", target = "_blank", "Trafford Data Lab"),
                " under the ",
                a(href = "https://www.trafforddatalab.io/LICENSE.txt", target = "_blank", "MIT"),
                " licence"
              ),
              style = "position:fixed; text-align:center; left: 0; bottom:0; width:100%; z-index:1000; height:30px; color: #7C7C7C; padding: 5px 20px; background-color: #E7E7E7"
            )))

shinyApp(ui = ui, server = server)