#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("main.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
   
   
   
   titlePanel(tags$h2("Estimated Distribution of Territorial Control")),
   titlePanel(tags$h3("Boko Haram (JAS/ISWAP) versus Nigerian Government/MNJTF")),

   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("years",
                     "Year",
                     min = 2010,
                     max = 2020,
                     value = 2010,
                     ticks = FALSE,
                     sep = "",
                     animate=TRUE),
         tags$p("Overlay Events"),
         checkboxInput("rebsel", "Rebel Selective Violence", FALSE),
         checkboxInput("rebind", "Rebel Indiscriminate Attacks", FALSE),
         checkboxInput("rebcon", "Rebel Conventional Attacks", FALSE),
         checkboxInput("battle", "Mutual Battles", FALSE),
         checkboxInput("govcon", "Gov. Conventional Attacks", FALSE),
         checkboxInput("govind", "Gov. Indiscriminate Attacks", FALSE),
         checkboxInput("govsel", "Gov. Selective Violence", FALSE),
         width=4
      ),

      # Show a plot of the generated distribution
      mainPanel(
         fluidRow(
            splitLayout(cellWidths=c("50%","50%"), plotOutput("mapPlot", height=600), plotOutput("mapPlot2", height=600))
         ),
         fluidRow(
            splitLayout(cellWidths=c("50%","50%"), plotOutput("distPlot"), plotOutput("distPlot2"))
         ),
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, rebselinput) {

   output$mapPlot <- renderPlot({
      
      if(input$rebsel){rebselFilter <- rebselEvents %>% filter(year == input$years)}else{rebselFilter <- rebselEvents %>% filter(year == 0)}
      if(input$rebind){rebindFilter <- rebindEvents %>% filter(year == input$years)}else{rebindFilter <- rebindEvents %>% filter(year == 0)}
      if(input$rebcon){rebconFilter <- rebconEvents %>% filter(year == input$years)}else{rebconFilter <- rebconEvents %>% filter(year == 0)}
      if(input$battle){battleFilter <- battleEvents %>% filter(year == input$years)}else{battleFilter <- battleEvents %>% filter(year == 0)}
      if(input$govcon){govconFilter <- govconEvents %>% filter(year == input$years)}else{govconFilter <- govconEvents %>% filter(year == 0)}
      if(input$govind){govindFilter <- govindEvents %>% filter(year == input$years)}else{govindFilter <- govindEvents %>% filter(year == 0)}
      if(input$govsel){govselFilter <- govselEvents %>% filter(year == input$years)}else{govselFilter <- govselEvents %>% filter(year == 0)}
      
      map_number <- input$years - 2009
      mapData <- locationsNGA %>% mutate(value = meanControlNGA[map_number,])
      
      ggplot()+
         geom_sf(data=mapData, aes(fill=value), color=NA)+
         scale_fill_viridis_c(breaks=seq(-1,1,by=0.4), limits=c(-1,1), option="turbo", name="Government     Estimate     Boko Haram")+
         geom_sf(data=rebselFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=rebindFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=rebconFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=battleFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=govconFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=govindFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=govselFilter, alpha=0.4, shape=16, size=2)+
         coord_sf(xlim=c(8.9,15), ylim=c(6.7,14.3))+
         theme_void(base_size=20)+
         theme(
            legend.position="bottom",
            legend.key.width = unit(3, "lines"),
            legend.key.height = unit(0.5, "lines"),
            legend.title.position = "bottom",
            legend.title = element_text(hjust = 0.5)
         )
      
   }, height="auto")
   
   output$mapPlot2 <- renderPlot({
      
      if(input$rebsel){rebselFilter <- rebselEvents %>% filter(year == input$years)}else{rebselFilter <- rebselEvents %>% filter(year == 0)}
      if(input$rebind){rebindFilter <- rebindEvents %>% filter(year == input$years)}else{rebindFilter <- rebindEvents %>% filter(year == 0)}
      if(input$rebcon){rebconFilter <- rebconEvents %>% filter(year == input$years)}else{rebconFilter <- rebconEvents %>% filter(year == 0)}
      if(input$battle){battleFilter <- battleEvents %>% filter(year == input$years)}else{battleFilter <- battleEvents %>% filter(year == 0)}
      if(input$govcon){govconFilter <- govconEvents %>% filter(year == input$years)}else{govconFilter <- govconEvents %>% filter(year == 0)}
      if(input$govind){govindFilter <- govindEvents %>% filter(year == input$years)}else{govindFilter <- govindEvents %>% filter(year == 0)}
      if(input$govsel){govselFilter <- govselEvents %>% filter(year == input$years)}else{govselFilter <- govselEvents %>% filter(year == 0)}
      
      map_number <- input$years - 2009
      mapData <- locationsNGA %>% mutate(value = log(meanIntensityNGA[map_number,]+1))
      
      ggplot()+
         geom_sf(data=mapData, aes(fill=value), color=NA)+
         scale_fill_viridis_c(breaks=seq(0,6,by=2), limits=c(0,6), option="turbo", name="Conflict Intensity (Logged)")+
         geom_sf(data=rebselFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=rebindFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=rebconFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=battleFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=govconFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=govindFilter, alpha=0.4, shape=16, size=2)+
         geom_sf(data=govselFilter, alpha=0.4, shape=16, size=2)+
         coord_sf(xlim=c(8.9,15), ylim=c(6.7,14.3))+
         theme_void(base_size=20)+
         theme(
            legend.position="bottom",
            legend.key.width = unit(3, "lines"),
            legend.key.height = unit(0.5, "lines"),
            legend.title.position = "bottom",
            legend.title = element_text(hjust = 0.5)
         )
      
   }, height="auto")
   
   output$distPlot <- renderPlot({
      
      map_number <- input$years - 2009
      plotData <- locationsNGA %>% mutate(value = meanControlNGA[map_number,])
      
      ggplot()+
         geom_histogram(data=plotData, aes(x=value, fill=after_stat(x)), binwidth=0.05)+
         scale_fill_viridis_b(breaks=seq(-1,1,by=0.05), limits=c(-1,1), option="turbo")+
         scale_y_continuous(breaks=seq(0,25,by=5))+
         scale_x_continuous(breaks=seq(-1,1,by=0.4), limits=c(-1,1))+
         labs(
            x="Mean Territorial Control Estimate",
            y="Number of Locations"
         )+
         coord_cartesian(ylim=c(0,25))+
         theme_bw(base_size=20)+
         theme(
            panel.grid.minor = element_blank(),
            legend.position = "none"
         )
      
   }, height=250)
   
   output$distPlot2 <- renderPlot({
      
      map_number <- input$years - 2009
      plotData <- locationsNGA %>% mutate(value = log(meanIntensityNGA[map_number,]+1))
      
      ggplot()+
         geom_histogram(data=plotData, aes(x=value, fill=after_stat(x)), binwidth=0.2)+
         scale_fill_viridis_b(breaks=seq(0,6,by=0.2), limits=c(0,6), option="turbo")+
         scale_y_continuous(breaks=seq(0,25,by=5))+
         scale_x_continuous(breaks=seq(0,6,by=2), limits=c(0,6))+
         labs(
            x="Mean Conflict Intensity Estimate",
            y="Number of Locations"
         )+
         coord_cartesian(ylim=c(0,25))+
         theme_bw(base_size=20)+
         theme(
            panel.grid.minor = element_blank(),
            legend.position = "none"
         )
      
   }, height=250)
   
}

# Run the application 
shinyApp(ui = ui, server = server)
