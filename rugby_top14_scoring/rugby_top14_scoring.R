### Load packages ----------------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(rvest)
library(shiny)
library(conflicted)

#how to handle overloaded functions
conflict_prefer(name = "pluck", winner = "purrr")
conflict_prefer(name = "filter", winner = "dplyr")

theme_set(theme_bw())

### Scrape Data ------------------------------------------------------------------------
# www.lnr.fr


# get URL
url <- read_html("https://www.lnr.fr/rugby-top-14/statistiques-rugby-top-14/statistiques-detaillees")

# Extract the table of interest form the per webpage

top14 <- url %>% 
    html_table(fill = T)

# save as data frame object the table of interest
top14<- as.data.frame(top14[[1]])

# aggregating data & creating per game (match) values
top14_main <- top14 %>%
    group_by(Clubs) %>%
    summarize(MJ = max(Matchs),
              Essais_m = Essais / Matchs,
              Transf_m = Transf. / Matchs,
              Drops_m = Drops / Matchs,
              Penalite_m = Pénalites / Matchs,
              Points_m = `Total points` / Matchs)%>%
    select(-MJ)


# Remove first row 
top14_main <- top14_main[-1,]

### Normalize all columns -------------------------------------------------

# z-score function 
z_score <- function(x){
    z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
    return(z)
}

# calculate z-scores
top14_main <- top14_main%>%
    mutate_at(vars(Essais_m:Points_m), .funs = z_score)


# t-score function
t_score <- function(x){
    t = (x * 10) + 50
    t = ifelse(t > 100, 100, 
               ifelse(t < 0, 0, t))
    return(t)
}

# calculate t-scores
top14_main<- top14_main %>%
    mutate_at(vars(Essais_m:Points_m), .funs = t_score)

# Now everything is on a scale of 1 to 100
top14_main%>%
    pivot_longer(cols = 2:6, names_to = "variable", values_to = "value")

# rename variables to a more readble format
top14_long <- top14_main %>%
    rename('Trans.\nPar match' = Transf_m,
           'Points\nPar match' = Points_m,
           'Pénalités\nPar match' = Penalite_m,
           'Essais\nPar match' = Essais_m,
           'Drops\nPar match' = Drops_m) %>%
    pivot_longer(cols = 2:6, names_to = "variable", values_to = "value")


# Create a vector of unique clubs names
Clubs <- unique(top14_long$Clubs)

##################################
######### Shiny App ##############
##################################



### Create Shiny User Interface ---------------------------------------------------

ui <- fluidPage(
    titlePanel("Scoring Top 14 - 2020/2021 "),
    mainPanel(width = 12,
              splitLayout(
                  cellWidths = c("50%", "50%"),
                  div(
                      selectInput(
                          input = "club1",
                          label = "Club 1",
                          choices = Clubs,
                          width = "100%"
                      ),
                      plotOutput(outputId = "plot1")
                  ),
                  div(
                      selectInput(
                          input = "club2",
                          label = "Club 2",
                          choices = Clubs,
                          width = "100%"
                      )
                      ,
                      plotOutput(outputId = "plot2")
                  )
              ),
              tableOutput(outputId = "Clubs.table")
    ))

server <- function(input, output){
    
    # create data sets for plotting
    dat1 <- reactive({
        dataset1 <- subset(top14_long, Clubs == input$club1)
        dataset1
    })
    
    dat2 <- reactive({
        dataset2 <- subset(top14_long, Clubs == input$club2)
        dataset2
    })
    
    dat3 <- reactive({
        dataset3 <- subset(top14, Clubs %in% c(input$club1, input$club2))
        dataset3
    })
    
    
    ## create plots
    output$plot1 <- renderPlot({
        d1 <- dat1()
        
        plot1 <- ggplot(d1, aes(x = variable, y = value, fill = variable)) +
            geom_col(color = "white", width = 0.9) +
            coord_polar(theta = "x") +
            geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
            theme(axis.text.x = element_text(face = "bold", size = 10),
                  legend.title = element_blank(),
                  legend.position = "none",
                  strip.background = element_rect(fill = "black"),
                  strip.text = element_text(face = "bold", color = "white", size = 13)) +
            labs(x = "", y = "") +
            facet_wrap(~Clubs) +
            ylim(0, 100) +
            scale_fill_brewer(palette="Set1")
        
        print(plot1)
    })
    
    output$plot2 <- renderPlot({
        d2 <- dat2()
        
        plot2 <- ggplot(d2, aes(x = variable, y = value, fill = variable)) +
            geom_col(color = "white", width = 0.9) +
            coord_polar(theta = "x") +
            geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
            theme(axis.text.x = element_text(face = "bold", size = 10),
                  legend.title = element_blank(),
                  legend.position = "none",
                  strip.background = element_rect(fill = "black"),
                  strip.text = element_text(face = "bold", color = "white", size = 13)) +
            labs(x = "", y = "") +
            facet_wrap(~Clubs) +
            ylim(0, 100) +
            scale_fill_brewer(palette="Set1")
        
        print(plot2)
    })
    
    ## create player stats tables
    output$Clubs.table <- renderTable(dat3(), align = "l")
}

### Run the shiny app ------------------------------------------------------------
shinyApp(ui = ui, server = server)