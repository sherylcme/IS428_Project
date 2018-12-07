
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# packages = c('shiny','quanteda','wordcloud2',
#              'tidyverse','tinytex','RColorBrewer',
#              'classInt','ggthemes','ggplot2','dplyr','plotly','memoise','shinythemes','visNetwork','treemap','DT','networkD3')
# 
# for( p in packages){
#   if(!require(p, character.only = T)){
#     install.packages(p)
#   }
#   library(p, character.only = T)
# } 

library(shiny)
library(quanteda)
library(wordcloud2)
library(tidyverse)
library(tinytex)
library(RColorBrewer)
library(classInt)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(plotly)
library(memoise)
library(shinythemes)
library(visNetwork)
library(treemap)
library(DT)
library(networkD3)
library(lubridate)

#loading in the df
df <- read_csv('gebiz_data_completed.csv') %>% filter( tender_detail_status != "Awarded to No Suppliers") %>%
  mutate(year = year(as.Date(award_date,"%d/%m/%Y")) ) 

#View(df)
#colnames(df)
#View(df)
#unique(df$category)


#only need to design the UI and the server 

# Define UI for application that draws a histogram




ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Visualising Government Procurement",
                           tabPanel("Introduction",
                                    mainPanel(HTML(
                                      '<div class="jumbotron">
                                      <h2>Government Procurement Visualisation Tool</h2>
                                      <p class="lead">Currently, there is no available tool to aid the public to understand and gain insights on the procurements made by the government. </p>
                                      <hr class="my-4">
                                      <h4>We created a visualisation tool which allows public and ministries to:<ol>
                                      <h5><li>Gain an overview of procurement spending made by the ministries and agencies</li></h5>
                                      <h5><li>Identify the relationships between ministries, agencies and suppliers</li></h5>
                                      <h5><li>Identify what are the goods and services procured by ministries and agencies under each category</li></h5></ol></h4>
                                      <br/>
                                      <h4> Created by:</h4>
                                      <h5><a href="https://www.linkedin.com/in/sofiakung/">Sofia Kung Jung Wen</a></h5>
                                      <h><a href="https://www.linkedin.com/in/sherylcme/">Sheryl Chong Man Er</a></h5>
                                      <h5><a href="https://www.linkedin.com/in/ha-nguyen-dang-thanh/">Nguyen Dang Thanh Ha</a></h5>
                                      <h6>Last Updated: 22nd November 2018</h6>
                                      </div>'
                                    )
                                    )),
                           tabPanel("Overview",
                                    sidebarPanel(HTML("<b>Description</b><br/>The size of the treemap respresents the number of procurement contracts and the colour intensity represents the spending amount under each category.<br/><br/>"),
                                                 selectInput(inputId = 'year_t',
                                                             label = 'Year:',
                                                             choices = unique(df$year),
                                                             selected = 2017),
                                                 selectInput(inputId = 'Ministry_treemap',
                                                             label = 'Ministry:',
                                                             choices = unique(df$ministry),
                                                             selected = "Prime Minister's Office"
                                                 )),
                                    mainPanel( plotOutput('treemap'))
                           ),
                           
                           tabPanel("Relationship between Ministry & Suppliers",
                                    sidebarPanel(HTML("<b>Description</b><br/>The network graph created allows us to identify which agencies procured from the same suppliers.<br/><br/>"),
                                                 selectInput(inputId = 'year_v',
                                                             label = 'Year:',
                                                             choices = unique(df$year),
                                                             selected = 2017),
                                                 selectInput(inputId = 'r_Ministry',
                                                             label = 'Ministry:',
                                                             choices = unique(df$ministry), 
                                                             selected = "Prime Minister's Office"),
                                                 sliderInput(inputId = "awarded_amt",
                                                             label = 'Awarded Amount:',
                                                             min = min(df$awarded_amt),
                                                             max = max(df$awarded_amt),
                                                             value = c(min(df$awarded_amt), max(df$awarded_amt)),
                                                             ticks = FALSE)),
                                    mainPanel(visNetworkOutput('visnetwork'))
                           ),
                           tabPanel("Cash Flow between Ministry & Suppliers",
                                    sidebarPanel(HTML("<b>Description</b><br/>The sankey diagram allows us to view the spending amount which is represented by the thickness of the path and direction of the cashflow between the agency and supplier under each category and subcategory.<br/><br/>"),
                                                 selectInput(inputId = 'year_c',
                                                             label = 'Year:',
                                                             choices = unique(df$year),
                                                             selected = 2017),
                                                 selectInput(inputId = 'Ministry_c',
                                                             label = 'Ministry:',
                                                             choices = unique(df$ministry), 
                                                             selected = "Prime Minister's Office"),
                                                 selectInput(inputId = 'Agency_c',
                                                             label = 'Agency:',
                                                             choices = unique(df$agency), 
                                                             selected = 'Government Technology Agency (GovTech)'),
                                                 selectInput(inputId = 'Category_c',
                                                             label = 'Category:',
                                                             choices = unique(df$category),
                                                             selected = unique(df$category))),
                                    mainPanel(sankeyNetworkOutput('sankey'))
                           ),
                           tabPanel("Goods & Service Procured",
                                    sidebarPanel(HTML("<b>Description</b><br/>The word cloud show the top key words within the procurement category.
                                                      The size of the words reflect the frequency of the goods and services procured,
                                                      as well as the demands of each of agency.<br/><br/>"),
                                                 selectInput(inputId = 'year_w',
                                                             label = 'Year:',
                                                             choices = unique(df$year),
                                                             selected = 2017),
                                                 selectInput(inputId = 'Ministry',
                                                             label = 'Ministry:',
                                                             choices = unique(df$ministry), 
                                                             selected = "Prime Minister's Office"),
                                                 selectInput(inputId = 'Agency',
                                                             label = 'Agency:',
                                                             choices = unique(df$agency), 
                                                             selected = 'Government Technology Agency (GovTech)'),
                                                 selectInput(inputId = 'Category',
                                                             label = 'Category:',
                                                             choices = unique(df$category),
                                                             selected = unique(df$category))
                                    ),
                                    mainPanel(plotOutput('WordCloud', width = "100%",height=375),
                                              div(DT::dataTableOutput('wordcloudtable'), style = "font-size: 75%; width: 100%")
                                    )
                           )
                                    ))

server <- function(input, output, session) {
  #to shows the values available based on the previous filter selected 
  #======================INTERACTIVE FILTER FOR WORDCLOUD =======================
  observe({
    print(input$Ministry)
    x <- df %>% filter(year == input$year_w , ministry == input$Ministry) %>% select(agency)
    updateSelectInput(session, 'Agency', 'Agency:',  choices= unique(x)) 
  })
  observe({
    print(input$Agency)
    y <- df %>% filter(year == input$year_w ,ministry == input$Ministry, agency == input$Agency) %>% select(category)
    updateSelectInput(session, 'Category', 'Category:',  choices= unique(y)) 
  })
  #======================INTERACTIVE FILTER FOR CASHFLOW =======================
  observe({
    print(input$Ministry_c)
    z <- df %>% filter( year == input$year_c ,ministry == input$Ministry_c) %>% select(agency)
    updateSelectInput(session, 'Agency_c', 'Agency:',  choices= unique(z)) 
  })
  observe({
    print(input$Agency_c)
    a <- df %>% filter(year == input$year_c ,ministry == input$Ministry_c, agency == input$Agency_c) %>% select(category)
    updateSelectInput(session, 'Category_c', 'Category:',  choices= unique(a)) 
  })
  # observe({
  #   print(input$Category_c)
  #   b <- df %>% filter(ministry == input$Ministry_c, agency == input$Agency_c, category == input$Category_c) %>% select(Subcategory)
  #   updateSelectInput(session, 'Subcategory_c', 'Select Subcategory:',  choices= unique(b)) 
  # })
  observe({
    
    f <- df %>% filter(year == input$year_v , ministry == input$r_Ministry) %>% select(awarded_amt)
    updateSliderInput(session, 'awarded_amt', 'Awarded Amount:',
                      min = min(f$awarded_amt),
                      max = max(f$awarded_amt),
                      value = c(min(f$awarded_amt), max(f$awarded_amt)) ) 
  })
  
  
  #OUTPUT FOR WORD CLOUD
  output$WordCloud <- renderPlot({
    dfw <- df %>% filter( year == input$year_w ,ministry == input$Ministry, agency == input$Agency, category == input$Category)
    corp <- corpus(dfw$tender_description)
    #Corpus consisting of 18,543 documents and 0 docvars
    #docvars( corp, field = 'category') <- df$category
    tdm<-dfm(corp,
             stem=FALSE,
             remove=c('contract','years','year','tender','provision','period','term','services','proposed',
                      'invitation','option','extend','supply','delivery','appointment','singapore','months','month',
                      'provide','annual','renew', 'works', 'two-year',
                      'service', stopwords(source='smart')),  #remove stop
             remove_punct=TRUE,
             remove_numbers=TRUE) %>%
      dfm_trim(min_termfreq = 1,
               termfreq_type ="count")
    #v <- terms() #call the function and save the output in a variable 
    textplot_wordcloud(tdm,  max_words = 30, min_count = 2)
  })
  #OUTPUT FOR WORD CLOUD TABLE
  output$wordcloudtable = DT::renderDataTable({
    DT::datatable(df %>% filter( year == input$year_w, ministry == input$Ministry, agency == input$Agency, category == input$Category) %>%
                    select(ministry, agency, tender_description, awarded_amt),
                  options = list(lengthMenu = c(3), pageLength = 3))
  })
  
  #OUTPUT FOR VIZNETWORK 
  output$visnetwork <- renderVisNetwork({
    
    df_filtered<- df %>% filter(year == input$year_v) %>% group_by(ministry, supplier_name) %>%
      mutate(agency_counter = n_distinct(agency)) %>% ungroup() %>%
      filter(agency_counter > 1) %>% 
      filter(ministry == input$r_Ministry) %>% 
      filter(awarded_amt >= as.numeric(input$awarded_amt[1]) & awarded_amt <= as.numeric(input$awarded_amt[2]))
    
    edges = data.frame(from = c(df_filtered[['supplier_name']]), to = c(df_filtered[['agency']]), weight = c(df_filtered[['awarded_amt']]))
    nodes = data.frame(node = c(as.character(edges$from), as.character(edges$to)) %>% unique())
    
    f <- function(x) {
      if(x['node'] %in% df_filtered[['agency']]) {
        x['color'] <- 'agency'
      } else {
        x['color'] <- 'supplier'
      }
      x['color']
    }
    nodes$color <- apply(nodes, 1, f)
    
    nodes = data.frame(node = c(as.character(edges$from), as.character(edges$to)) %>% unique(),
                       group = c(nodes[['color']]),
                       label = c(as.character(edges$from), as.character(edges$to)) %>% unique())
    
    a=data.frame(x = c(df_filtered[['supplier_name']]),y = c(df_filtered[['agency']]))
    
    links=a%>% group_by(x,y)%>%tally()
    
    visNetwork(
      setNames(nodes, c("id", "group", "label")),
      setNames(links, c("from", "to", "weight")) # why the weight never works?
    ) %>%
      visLegend() %>%
      visGroups(groupname = "agency", color = "lightblue", shape = "triangle")%>%
      visGroups(groupname = "supplier", color = "yellow") %>%
      visEdges(arrows = "from") %>%
      visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T))
    
  }) 
  output$treemap <- renderPlot({
    #plot your treemap 
    dft <- df %>% filter(year == input$year_t)
    data_grouped <- group_by(dft, ministry, agency , category)
    data_grouped$count = 1
    
    # Sum of value 
    data_summarised <- summarise(data_grouped, awarded_amt = sum(awarded_amt, na.rm = TRUE), count = sum(count, na.rm = TRUE))
    data_summarised$awarded_amt = data_summarised$awarded_amt / 1000
    # Filter 
    data_selected <- data_summarised %>%
      filter(ministry == input$Ministry_treemap)
    
    data_selected$label <- paste(data_selected$category, round(data_selected$awarded_amt,2), sep = ", ")
    tm <- treemap(data_selected,
                  index = c('agency','label'),
                  vSize = 'count',
                  vColor = 'awarded_amt',
                  type = "manual",
                  palette = "Blues",
                  title = 'Procurement Spending by Category',
                  title.legend = "Total Amt (S$) in Thousands")
    
  })
  output$sankey <-renderSankeyNetwork({
    #plot your sankey
    df1 <- df %>% filter(year == input$year_c , ministry == input$Ministry_c, agency == input$Agency_c, category == input$Category_c) #, Subcategory == input$Subcategory_c )
    links = data.frame(source = c(df1[['agency']]), target = c(df1[['supplier_name']]), value = c(df1[['awarded_amt']]))
    nodes = data.frame(name = c(as.character(links$source), as.character(links$target)) %>% unique())
    links$IDsource=match(links$source, nodes$name)-1 
    links$IDtarget=match(links$target, nodes$name)-1
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name", 
                  sinksRight=FALSE)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


