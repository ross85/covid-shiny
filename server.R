#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    get_box_single_value <- function(df, field="gio_totale_casi", filter_field=NULL, filter_value=NULL, digits = 0, suffix="", multiplier = 1) {
        
        if (is.null(filter_field))
            value = df %>% filter(data == max(data)) %>% pull(get(field))
        else
            value = df %>% filter(get(filter_field) == filter_value & data == max(data)) %>% pull(get(field))
        
        valueBox(
            paste0(formatC(multiplier*value, digits = digits, big.mark = ",", format="f"),suffix), paste0(field," ",filter_value), icon = icon("list"),
            color = "purple"
        )
    }
    
    get_box_ratio_value <- function(df, field_num="gio_totale_casi", field_den="set_totale_casi", filter_field=NULL, filter_value=NULL, digits = 1, suffix="%", multiplier = 100, offset = 0) {
        
        if (is.null(filter_field)) {
            num = df %>% filter(data == max(data)) %>% pull(get(field_num))
            den = df %>% filter(data == (max(data)-offset)) %>% pull(get(field_den))
        } else {
            num = df %>% filter(get(filter_field) == filter_value & data == max(data)) %>% pull(get(field_num))
            den = df %>% filter(get(filter_field) == filter_value & data == (max(data)-offset)) %>% pull(get(field_den))
        }
        
        valueBox(
            paste0(formatC(multiplier*((num-den)/(abs(den))), digits = digits, big.mark = ",", format="f"),suffix), paste0("Variazione Percentuale Rispetto alla Settimana ",filter_value), icon = icon("list"),
            color = "purple"
        )
    }
    
    output$bigNumbers1 <- renderValueBox({
        get_box_single_value(dati.province.plus, "gio_totale_casi", "denominazione_provincia", "Brescia")
    })
    output$bigNumbers2 <- renderValueBox({
        get_box_single_value(dati.province.plus, "set_totale_casi", "denominazione_provincia", "Brescia")
    })
    output$bigNumbers3 <- renderValueBox({
        get_box_ratio_value(dati.province.plus, "set_totale_casi", "set_totale_casi", "denominazione_provincia", "Brescia", offset = 7)
    })
    output$bigNumbers4 <- renderValueBox({
        get_box_single_value(dati.province.plus, "gio_totale_casi", "denominazione_provincia", "Milano")
    })
    output$bigNumbers5 <- renderValueBox({
        get_box_single_value(dati.province.plus, "set_totale_casi", "denominazione_provincia", "Milano")
    })
    output$bigNumbers6 <- renderValueBox({
        get_box_ratio_value(dati.province.plus, "set_totale_casi", "set_totale_casi", "denominazione_provincia", "Milano", offset = 7)
    })
    output$bigNumbers7 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "gio_totale_casi", "denominazione_regione", "Lombardia")
    })
    output$bigNumbers8 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "set_totale_casi", "denominazione_regione", "Lombardia")
    })
    output$bigNumbers9 <- renderValueBox({
        get_box_ratio_value(dati.regione.plus, "set_totale_casi", "set_totale_casi", "denominazione_regione", "Lombardia", offset = 7)
    })
    output$bigNumbers10 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "gio_terapia_intensiva", "denominazione_regione", "Lombardia")
    })
    output$bigNumbers11 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "set_terapia_intensiva", "denominazione_regione", "Lombardia")
    })
    output$bigNumbers12 <- renderValueBox({
        get_box_ratio_value(dati.regione.plus, "set_terapia_intensiva", "set_terapia_intensiva", "denominazione_regione", "Lombardia", offset = 7)
    })
    output$bigNumbers13 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "gio_ricoverati_con_sintomi", "denominazione_regione", "Lombardia")
    })
    output$bigNumbers14 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "set_ricoverati_con_sintomi", "denominazione_regione", "Lombardia")
    })
    output$bigNumbers15 <- renderValueBox({
        get_box_ratio_value(dati.regione.plus, "set_ricoverati_con_sintomi", "set_ricoverati_con_sintomi", "denominazione_regione", "Lombardia", offset = 7)
    })
    output$bigNumbers16 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "gio_totale_casi_prc_rapporto", "denominazione_regione", "Lombardia", digits = 1, suffix="%", multiplier = 100)
    })
    output$bigNumbers17 <- renderValueBox({
        get_box_single_value(dati.regione.plus, "set_totale_casi_prc_rapporto", "denominazione_regione", "Lombardia", digits = 1, suffix="%", multiplier = 100)
    })
    output$bigNumbers18 <- renderValueBox({
        get_box_ratio_value(dati.regione.plus, "set_totale_casi_prc_rapporto", "set_totale_casi_prc_rapporto", "denominazione_regione", "Lombardia", offset = 7)
    })
    output$bigNumbers19 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "gio_totale_casi")
    })
    output$bigNumbers20 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "set_totale_casi")
    })
    output$bigNumbers21 <- renderValueBox({
        get_box_ratio_value(dati.nazione.plus, "set_totale_casi", "set_totale_casi", offset = 7)
    })
    output$bigNumbers22 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "gio_terapia_intensiva")
    })
    output$bigNumbers23 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "set_terapia_intensiva")
    })
    output$bigNumbers24 <- renderValueBox({
        get_box_ratio_value(dati.nazione.plus, "set_terapia_intensiva", "set_terapia_intensiva", offset = 7)
    })
    output$bigNumbers25 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "gio_ricoverati_con_sintomi")
    })
    output$bigNumbers26 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "set_ricoverati_con_sintomi")
    })
    output$bigNumbers27 <- renderValueBox({
        get_box_ratio_value(dati.nazione.plus, "set_ricoverati_con_sintomi", "set_ricoverati_con_sintomi", offset = 7)
    })
    output$bigNumbers28 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "gio_totale_casi_prc_rapporto", digits = 1, suffix="%", multiplier = 100)
    })
    output$bigNumbers29 <- renderValueBox({
        get_box_single_value(dati.nazione.plus, "set_totale_casi_prc_rapporto", digits = 1, suffix="%", multiplier = 100)
    })
    output$bigNumbers30 <- renderValueBox({
        get_box_ratio_value(dati.nazione.plus, "set_totale_casi_prc_rapporto", "set_totale_casi_prc_rapporto", offset = 7)
    })
    
    get_trends_plot <- function(df, y1="gio_totale_casi", y2="set_totale_casi", filter_field=NULL, filter_value=NULL, digits = 1, suffix="%", multiplier = 100) {
        
        if (!is.null(filter_field))
            df <- df %>% filter(get(filter_field) == filter_value)
        
        df <- df %>%
            mutate(y1 = get(y1), y2 = get(y2)) %>%
            filter(data >= input$start.date)
        
        ggplotly(ggplot(df, aes(x = data, y = y1)) +
                     geom_line(color = "#e60000") +
                     geom_point(color = "#e60000") +
                     geom_line(aes(y = y2), linetype = "dashed", color = "#4a4d4e") +
                     geom_point(aes(y = y2), color = "#4a4d4e") +
                     labs(y = substr(y1,13,100), x = NULL) +
                     scale_x_date(date_breaks = "1 week") +
                     theme(axis.text.x = element_text(angle=45, hjust = 1))) %>%
            plotly::layout(xaxis = list(autorange = TRUE),
                           yaxis = list(autorange = TRUE))
    }
    
    output$plot1 <- renderPlotly({
        get_trends_plot(dati.province.plus, filter_field="denominazione_provincia", filter_value="Brescia")
    })
    output$plot2 <- renderPlotly({
        get_trends_plot(dati.province.plus, filter_field="denominazione_provincia", filter_value="Milano")
    })
    output$plot3 <- renderPlotly({
        get_trends_plot(dati.regione.plus, filter_field="denominazione_regione", filter_value="Lombardia")
    })
    output$plot4 <- renderPlotly({
        get_trends_plot(dati.regione.plus, y1="gio_terapia_intensiva", y2="set_terapia_intensiva", filter_field="denominazione_regione", filter_value="Lombardia")
    })
    output$plot5 <- renderPlotly({
        get_trends_plot(dati.regione.plus, y1="gio_ricoverati_con_sintomi", y2="set_ricoverati_con_sintomi", filter_field="denominazione_regione", filter_value="Lombardia")
    })
    output$plot6 <- renderPlotly({
        get_trends_plot(dati.regione.plus, y1="gio_totale_casi_prc_rapporto", y2="set_totale_casi_prc_rapporto", filter_field="denominazione_regione", filter_value="Lombardia")
    })
    output$plot7 <- renderPlotly({
        get_trends_plot(dati.nazione.plus)
    })
    output$plot8 <- renderPlotly({
        get_trends_plot(dati.nazione.plus, y1="gio_terapia_intensiva", y2="set_terapia_intensiva")
    })
    output$plot9 <- renderPlotly({
        get_trends_plot(dati.nazione.plus, y1="gio_ricoverati_con_sintomi", y2="set_ricoverati_con_sintomi")
    })
    output$plot10 <- renderPlotly({
        get_trends_plot(dati.nazione.plus, y1="gio_totale_casi_prc_rapporto", y2="set_totale_casi_prc_rapporto")
    })
    
    output$provincePlot <- renderPlotly({
        
        df.render <- dati.province.plus %>%
            filter(data >= input$start.date & denominazione_regione==input$region)
        
        if (input$metric == "Overall") {
            df.render$y <- df.render$totale_casi
        }
        if (input$metric == "Daily") {
            df.render$y <- df.render$gio_totale_casi
        }
        if (input$metric == "Weekly") {
            df.render$y <- df.render$set_totale_casi
        }
        if (input$province.relative) {
            df.render$y <- df.render$y/df.render$popolazione*1000
        }
            
        
        
        ggplotly(ggplot(df.render, aes(x = data, y = y, color = denominazione_provincia)) +
                     geom_line() +
                     geom_point() +
                     scale_x_date(date_breaks = "1 week") +
                     theme(axis.text.x = element_text(angle=45, hjust = 1))) %>%
            plotly::layout(xaxis = list(autorange = TRUE),
                           yaxis = list(autorange = TRUE))
        
    })
    
    output$regioniPlot <- renderPlotly({
        
        vars.selected <- switch (input$regioni.agg,
                                 "Overall" = overall.vars,
                                 "Daily" = daily.vars,
                                 "Weekly" = weekly.vars
        )
        
        df.render <- dati.regione.plus %>%
            filter(data >= input$start.date & denominazione_regione %in% input$regione) %>%
            select(c("data", vars.selected)) %>%
            group_by(data) %>%
            summarise_all(funs(sum)) %>%
            pivot_longer(-data)
        
        ggplotly(ggplot(df.render, aes(x = data, y = value, color = name)) +
                     geom_line() +
                     geom_point() +
                     scale_x_date(date_breaks = "1 week") +
                     theme(axis.text.x = element_text(angle=45, hjust = 1))
        ) %>%
            plotly::layout(xaxis = list(autorange = TRUE),
                           yaxis = list(autorange = TRUE))
    })
    
    output$nazionePlot <- renderPlotly({
        
        vars.selected <- switch (input$nazione.agg,
                                 "Overall" = overall.vars,
                                 "Daily" = daily.vars,
                                 "Weekly" = weekly.vars
        )
        
        df.render <- dati.nazione.plus %>%
            filter(data >= input$start.date) %>%
            select(c("data", vars.selected)) %>%
            pivot_longer(-data)
        
        ggplotly(ggplot(df.render, aes(x = data, y = value, color = name)) +
                     geom_line() +
                     geom_point() +
                     scale_x_date(date_breaks = "1 week") +
                     theme(axis.text.x = element_text(angle=45, hjust = 1))
        ) %>%
            plotly::layout(xaxis = list(autorange = TRUE),
                           yaxis = list(autorange = TRUE))
    })
    
    output$regioniConfrontoPlot <- renderPlotly({
        
        df.render <- dati.regione.plus %>%
            filter(data >= input$start.date) %>%
            select(c("data", denominazione_regione, input$confronto.metric)) %>%
            group_by(data, denominazione_regione) %>%
            summarise_all(funs(sum))
        
        
        ggplotly(ggplot(df.render, aes(x = data, y = get(input$confronto.metric), color = denominazione_regione)) +
                     geom_line() +
                     geom_point() +
                     scale_x_date(date_breaks = "1 week") +
                     theme(axis.text.x = element_text(angle=45, hjust = 1)) +
                     labs(y = input$confronto.metric)
        ) %>%
            plotly::layout(xaxis = list(autorange = TRUE),
                           yaxis = list(autorange = TRUE))
    })
    
})
