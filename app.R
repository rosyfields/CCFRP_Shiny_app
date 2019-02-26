################
#Ryan Fields
#Feb 21 2019

#CCFRP Shiny app development
#Originally created spring 2017


################

library(shiny)
library(shinyWidgets)
 library(rsconnect)
 library(plyr)
 library(tidyverse)

 
image.size = 75  #pixels?
 
#  
ui <- navbarPage(title = "CCFRP Data App : 2007-2018",theme = 'bootstrap_yeti.css',inverse = TRUE,
                 
tabPanel('CPUE and Length Data',
   fluidRow(
    
    column(10,
        tags$h3('Welcome to the CA Collaborative Fisheries Research Program data app!'),
        tags$h5('Please select a', tags$em('Species '), ' and a',tags$em('Metric '), ' to investigate trends in catch-rates
                and lengths in central California',tags$a(href = 'https://www.wildlife.ca.gov/Conservation/Marine/MPAs/FAQsite','Marine Protected Areas'), 'since 2007'),
        tags$h5('Check out the', 
                tags$a(href = 'https://https://www.mlml.calstate.edu/fisheries/ccfrp/', 'Fisheries and Conservation Biology Lab'),
                'for additional information about this program'),
        tags$h6("*** MPA = Marine Protected Area***"),
        tags$h6("*** REF = Reference (outside MPA)***")),
    column(2, tags$img(height = image.size, width = image.size,src = 'ccfrp.png'))), #end of fluidRow
 
 
 
  fluidRow(column(4,
                  
             wellPanel(
                selectInput(inputId = 'fishspp',
                          label = 'Species',
                          choices = c('Black Rockfish', 'Blue Rockfish',
                                      'Canary Rockfish', 'China Rockfish',
                                      'Deacon Rockfish', 'Gopher Rockfish',
                                      'Kelp Greenling',  'Kelp Rockfish','Lingcod','Olive Rockfish', 
                                      'Rosy Rockfish',   'Starry Rockfish',
                                      'Vermilion Rockfish','Yellowtail Rockfish', 'Total')), #end Select Input
              
              # Metric
                selectInput(inputId = 'metric',
                          label = 'Metric',
                          choices = c('CPUE', 'Length (cm)','Length Boxplot'))), #end of selectInput and Well panel
             
             #HTML hack I found to limit size of fish plots
                    HTML("<div style='height: 150px;'>"),
                    plotOutput(outputId = 'fish.cartoon.plot'),
                    HTML("</div>")),
             
             
           
           column(8, plotOutput(outputId = 'fish.plot',height = 400, width = 550))) #end of Fluid Row
             
  
    ), #end of tabPanel

#Gear Analysis Tab

tabPanel('Gear Data',
         fluidRow(column(3,
                         wellPanel(
                           selectInput(inputId = 'gear.plot',
                                       label = 'Plot Type',
                                       choices = c('Total Gear','By Year',
                                                   'By Location','By Species')))),
                  column(9,
                         tags$h4('Gear Type Differences'),
                         tags$h5('Please select a', tags$em('Plot Type '),  'to investigate how different Gear types have fished in',
                                 tags$a(href = 'https://www.wildlife.ca.gov/Conservation/Marine/MPAs/FAQsite','Marine Protected Areas'), 'since 2007'),
                         tags$h5('Check out the',
                                 tags$a(href = 'https://https://www.mlml.calstate.edu/fisheries/ccfrp/', 'Fisheries and Conservation Biology Lab'),
                                 'for additional information about this program'))), #end fluidRow


#          
           fluidRow(
             column(8, plotOutput(outputId = 'gear.plot',height = 550, width = 750), offset = 1)), #end fluidrow
           fluidRow('...'),       
           fluidRow(
             column(5),
             column(1,tags$img(height = image.size , width = image.size, src = 'mlml.png')),
             column(1,tags$img(height = image.size, width = image.size,src = 'ccfrp.png')),
             column(5))#end fluidRow

)#end TabPanel

)#end ui
  



####################################
####################################
####################################

#Load Data

fish.cpue = read_csv("data/CPUE.per.IDcell_2018.csv") %>% 
  mutate(Site = factor(Site),
         Area = factor(Area,levels =c ('Ano Nuevo','Point Lobos', 'Piedras Blancas', 'Point Buchon')))
                          
fish.lengths = read.csv('data/Length.data.CCFRP_2018.csv') %>% 
  mutate(Area = factor(Area, levels =c ('Ano Nuevo','Point Lobos', 'Piedras Blancas', 'Point Buchon')))

species.maturities = read.csv('data/Species_Maturities_2017.csv')


gear.data = read.csv('data/Gear.data.CCFRP_2018.csv')


####################################
####################################
####################################


#R server
server <- function(input, output) {
  #font size 
  fs = 14
  gear.fs = 6
  
  #Larry Allen's artwork
  output$fish.cartoon.plot = renderImage({
    
    plot.name = paste0('www/',input$fishspp, '.PNG')
    
    list(src = plot.name,
         contentType = 'image/png',
         width = 250,
         height = 150)
    
    
  }, deleteFile = FALSE)
  
  
  
  
  #Plots for CPUE and Length Tab
  
 
  
   output$fish.plot = renderPlot({
    #R code here to build plot


     #############
     #Define the plot theme once:
     plot.theme = theme(
       axis.text.x = element_text(size = fs, colour = 'black', angle = 90, hjust = 1, vjust = .5), #small to get all years to fit
       axis.text.y = element_text(size = fs, colour = 'black'),
       axis.title.x = element_blank(),
       axis.title.y = element_text(size = fs),
       axis.ticks = element_line(size = 0.5, colour = 'black'),
       axis.ticks.length = unit(0.2, 'cm'),   #set length of tick marks
       plot.title = element_text(size = fs),

       strip.background = element_rect(fill = "white"),
       strip.text.x = element_text(size = fs),

       legend.position = c(0.92, 1.1),  # for two plot configurations
       legend.key = element_blank(),
       legend.background = element_blank(),
       legend.text = element_text(size = fs),
       
       #grid line parameters
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(),
       panel.border = element_rect(fill = NA), 
       axis.line.x = element_line(colour = 'black',size = 0.5),
       
       plot.margin = unit(c(0.25, 0.25, 0.5, 0.25),'cm'))
     
  ##########################################   
    
     #Selective plot based on input
     if(input$metric != 'CPUE' & input$fishspp == 'Total'){
    
       par(bg = 'black', ps = 14)
       plot(1,1, type = 'n',xaxt = 'n', yaxt = 'n', ylab = '', xlab = '')  # 
       text(1,1, labels = "'Total' not supported for Length Plots \n Please choose a different Species", col = 'white')
       
       
     } else if(input$metric =='CPUE'){
       
     data = fish.cpue
       
     d = data[,c('Site','Area','Year','ID.Cell.per.Trip',input$fishspp)] %>% 
       filter(!Area == 'Farallon Islands')
     
     #rename columns- really just care about the species name ; each species will internally be named 'Name'
     #need it to be consistent to do summary for ddply()
     colnames(d) = c('Site','Area','Year','IDCellperTrip','Name')
     
     
     #ddply() works like a pivot table and will summarize data by Area, then Site, then Year
     #ggplot needs this sumarized data to pull plot information from
     
     summary = ddply(d, c('Area', 'Site', 'Year'), summarize,
                     N = length(Name),
                     CPUE = round(mean(Name),1),
                     SE = sd(Name)/sqrt(N))
     
     #Scale plot based on years included in dataset
     years.used = range(as.numeric(summary$Year)) 
     
     #will use 'limits' to define upper and lower error bar limits 
     limits = aes(ymax = CPUE + SE, ymin = CPUE - SE) 
     
     
     ################
     
     #Define plot:
     cpue = ggplot(data = summary, aes(x = Year, y = CPUE, group = Site, colour = Site)) + 
       facet_wrap(~ Area, nrow = 2, ncol = 2) +
       geom_line(aes(linetype = Site), size = 0.5) +     
       geom_errorbar(limits, width = 0.15, size = 0.3) +
       expand_limits(y = 0) +                       # Set y range to include 0
       scale_colour_manual(name = "", values = c('firebrick', 'royalblue3')) +
       scale_shape_manual(name = "", values = c(22, 21)) +      # Use points with a fill color
       scale_linetype_discrete(name = "") +
       scale_x_continuous(breaks = seq(years.used[1], years.used[2], 1)) +
       
       xlab("") + 
       ylab("Mean CPUE (# Fish / Angler*Hour)") + # Set axis labels
       
       ggtitle(input$fishspp) +     # Set title
       
       plot.theme

      cpue
      
      }else if(input$metric =='Length (cm)'){
        
        data = fish.lengths %>% 
          filter(!Area == 'Farallon Islands')
        
          data.summary = filter(data, Common.Name == input$fishspp) %>% 
          
          ddply(c('Area','Site','Year'),summarize,
                N = length(Length.cm),
                avg.length = mean(Length.cm),
                SE = sd(Length.cm)/sqrt(N),#Standard error
                min.plot = avg.length-SE,  #to scale plots
                max.plot = avg.length+SE) #to scale plots
        
        #Scale plot based on years used
        years.used = range(as.numeric(data.summary$Year)) 
        
        
        #Use species.maturities table to query size 
        #Will be drawn as horizontal dashed line on graph
        #Set line 300 for undefined species - will not show up on plot
        
        if(input$fishspp %in% levels(species.maturities$Common.Name) == TRUE){
          maturity = species.maturities$Female.Maturity.cm[species.maturities$Common.Name == input$fishspp];
          h.line = geom_hline(yintercept = maturity, linetype = 'dotted')
        } else{
          h.line = geom_hline(yintercept = 0, color = 'white')}
        
        
        #Annoying but repeating to get integer values for maturity to scale plots
        if(input$fishspp %in% levels(species.maturities$Common.Name) == TRUE){
          mat.size = species.maturities$Female.Maturity.cm[species.maturities$Common.Name == input$fishspp]
        } else{
          mat.size = 18}
        
        
        
        #define filename to save image- This merges the input common name and puts it together with 'cpue plot.png'
        #can change file type (e.g. .tiff) if desired- i believe ggsave() below will automatically save in this format

        
        #set standard errors that are 'NaN' (due to sample size of 1) equal to zero, so that they will not be plotted
        data.summary$SE[data.summary$SE == 'NaN'] = 0
        # pt.size = 11
        
        #define what error bars will be +- standard error 
        limits <- aes(ymax = avg.length + SE, ymin = avg.length - SE) 
        
        #Define plot limits to auto-scale plots based on available sizes
        
        pmin = min(na.omit(data.summary$min.plot))
        pmax = max(na.omit(data.summary$max.plot))
        
        plot.min = min(mat.size, pmin) - 1
        plot.max = max(mat.size, pmax) + 1
        
        
        #define plot named 'length'
        
        lengthplot = ggplot(data = data.summary, aes(x = Year, y = avg.length, group = Site, colour = Site)) + 
          facet_wrap(~ Area, nrow = 2, ncol = 2) +
          
          geom_line(aes(linetype = Site), size = 0.5) +     # Set linetype by sex
          # geom_point(size=1, fill="white") + 
          geom_errorbar(limits, width = 0.15, size = 0.3) +
          expand_limits(y = 0) +                       # Set y range to include 0
          scale_colour_manual(name = "", values = c('firebrick', 'royalblue3')) +
          scale_shape_manual(name = "", values = c(22, 21)) +      # Use points with a fill color
          scale_linetype_discrete(name = "") +
          scale_x_continuous(breaks = seq(years.used[1], years.used[2], 1)) +
          scale_y_continuous(limits = c(plot.min, plot.max))+
          
          xlab("") + ylab("Mean Length (cm)") + # Set axis labels
          ggtitle(input$fishspp) +     # Set title
          
          plot.theme +
          h.line
        
        lengthplot
      }else {
        
        
        
        data = fish.lengths %>% 
          filter(!Area == 'Farallon Islands') %>% 
          filter(Common.Name == input$fishspp) %>% 
          mutate(Year.f = factor(Year)) %>% 
          droplevels()
        
        #Scale plot based on years used
        years.used = range(as.numeric(data$Year)) 
        
        # pt.size = 11
        
        
        #draw 50% maturity line
        
        if(input$fishspp %in% levels(species.maturities$Common.Name) == TRUE){
          maturity = species.maturities$Female.Maturity.cm[species.maturities$Common.Name == input$fishspp];
          h.line = geom_hline(yintercept = maturity, linetype = 'dotted')
        } else{
          h.line = geom_hline(yintercept = 15, color = 'white')}
        
        
        
      
        
        
        #define plot named 'length'
        
        length.boxplot = ggplot(data = data, aes(x = Site, y = Length.cm, fill = Year.f)) + 
          facet_wrap(~ Area, nrow = 2, ncol = 2) +
          
          theme(strip.background = element_rect(fill = "white"),
                strip.text.x = element_text(size =  fs)) +  #facet color
          geom_boxplot(width = .5,position = position_dodge(.7), size = .3, outlier.alpha = .2)+
          scale_colour_gradient(low = "white", high = "blue")+
          
          expand_limits(y = c(10, 40))+                 #to force these y limits to be in plot
          
          xlab("") + ylab("Mean Length (cm)") + # Set axis labels-- don't need x axis to say 'Year'
          ggtitle(input$fishspp) +     # Set title
          labs(fill="Year")+
          
          
          #can define all the theme() elements of ggplot at once, Note order does matter if there are conflicting commands
          #such as individually setting axis text size, then setting overall axis text size- the latter will override the former
          
         plot.theme+
          theme(legend.position = 'right')+
          # 
          h.line  #set y intercept to be user input for 50% maturity
        # 
        # 

        length.boxplot
        
        
        
        
        
      }
   
  })
  
  
   
  output$gear.plot = renderPlot({
    
  # fs = 12
  
   plot.theme = theme(
   axis.text.x = element_text(size = fs, colour = 'black', angle = 90, hjust = 1, vjust = .5), #small to get all years to fit
   axis.text.y = element_text(size = fs, colour = 'black'),
   axis.title.x = element_blank(),
   axis.title.y = element_text(size = fs),
   axis.ticks = element_line(size = 0.5, colour = 'black'),
   axis.ticks.length = unit(0.2, 'cm'),   #set length of tick marks
   plot.title = element_text(size = fs),
   
   #facet theme items
   strip.background = element_rect(fill = "white"),
   strip.text.x = element_text(size = fs),
   
   #legend parameters
   # legend.position = c(0.95, 1.1), #4 plot configurations
   legend.position = 'bottom',
   legend.direction = 'horizontal',# for two plot configurations
   legend.key = element_blank(),
   legend.background = element_blank(),
   legend.text = element_text(size = fs),
   #grid line parameters
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   panel.background = element_blank(),
   panel.border = element_rect(fill = NA), 
   axis.line.x = element_line(colour = 'black',size = 0.5),
   
   plot.margin = unit(c(0.25, 0.25, 0.5, 0.25),'cm'))



#Selective plot based on input
   # 'Total Gear','By Year',
   # 'By Location','By Species'
if(input$gear.plot == 'Total Gear'){
  
  gear.summary = ddply(gear.data, 'Gear.Type', summarize,
                       N = length(Gear.Type))%>% 
    mutate(label_height = 0.5 * N, total.fish = sum(N)) %>% 
    mutate(Percent = round(N/total.fish, 3) * 100 , percent.height = N * 0.4) %>% 
    na.omit()
  
  #Pie chart
  lbls <- paste(gear.summary$Gear.Type, "\n", gear.summary$N, "\n", " ", gear.summary$Percent, "%", sep="")
  par(mai = c(0,0,0,0), omi = c(0,0,0,0), ps = 18)
  pie(gear.summary$N, labels = '', main="",col = c('darkgoldenrod1','tomato3','steelblue'),
      radius = 1, lwd = 2, clockwise = T )
  
  text(.5,.25,labels = lbls[1]) #Shrimpflies no bait: Yellow
  text(0,-.5,labels = lbls[2]) #Shrimpflies with bait: Red
  text(-0.5,.25,labels = lbls[3]) #Bar: Blue
  
  
  
}else if(input$gear.plot== 'By Year'){
  
  gear.year.summary = ddply(gear.data, c('Gear.Type', 'Year'), summarize,
                            N = length(Gear.Type)) %>% 
    ddply( .(Year), transform, sum.fish = sum(N)) %>%  #total of each species
    ddply( .(Year), transform, 
           percent = N/sum.fish,
           position = 1-((cumsum(N)/sum.fish) - (0.5 * N/sum.fish))) %>% #create proportion 
    mutate(perc.lab = round(percent, 3) * 100) %>% #create new percent factor for label in plot
    na.omit()
  
  years = range(as.numeric(gear.year.summary$Year))
  
ggplot(gear.year.summary, aes(x=Year, y = N, fill = Gear.Type)) +
    geom_bar(stat="identity", colour='black',position='fill') +      # position = fill makes it percent of eachj
    scale_fill_manual(values = c('darkgoldenrod1','tomato3','steelblue'))+                             # colour palette
    scale_y_continuous(expand = c(0, 0)) + 
    scale_x_continuous(breaks = seq(years[1], years[2], 1))+# get rid of space under bars
    xlab("")+                                                       # x-axis label
    ylab("Proportion")+                                            #y-axis label (even though it is horizontal)
    labs(fill = "Gear Type")+                                      # label the legend title (by naming what is being filled by color)
    ggtitle("Gear Type by Year")  +
    coord_flip()+
    #label percent of each gear type
    geom_text(data = gear.year.summary, aes(x = Year, y = position, 
                                          label = paste0(perc.lab,"%")), size = gear.fs)+
    plot.theme
    
  

  
  
}else if(input$gear.plot == 'By Location'){
  
  gear.location.summary = gear.data %>% 
    group_by(Area, Gear.Type) %>% 
    summarise(counts = n()) %>% 
    mutate(percent = counts/sum(counts),
           position = 1-(cumsum(percent) - (0.5 * percent)),
           perc.lab = round(percent*100,1))
  
  
  
  ggplot(gear.location.summary,mapping = aes(x = Area, y = percent, fill = Gear.Type))+
    geom_bar(stat="identity", colour='black',position='fill') +      # position = fill makes it percent of eachj
    scale_fill_manual(values = c('darkgoldenrod1','tomato3','steelblue'))+                             # colour palette
    scale_y_continuous(expand = c(0, 0)) + 
    # scale_x_continuous(breaks = seq(years[1], years[2], 1))+# get rid of space under bars
    xlab("")+                                                       # x-axis label
    ylab("Proportion")+                                            #y-axis label (even though it is horizontal)
    labs(fill = "Gear Type")+                                      # label the legend title (by naming what is being filled by color)
    ggtitle("Gear Type by Location")  +
    coord_flip()+
    geom_text(data = gear.location.summary, aes(x = Area, y = position, 
                                                label = paste0(perc.lab,"%")), size = gear.fs) +
    
    plot.theme
  
  
}else if(input$gear.plot== 'By Species'){
  
  
  gear.species.summary = ddply(gear.data, c('Gear.Type', 'Common.Name'), summarize,
                               N = length(Gear.Type)) %>% 
    filter(Common.Name %in% c('Black Rockfish','Blue Rockfish', 'Canary Rockfish', 'China Rockfish',
                              'Copper Rockfish', 'Gopher Rockfish', 'Kelp Rockfish', 'Lingcod', 
                              'Olive Rockfish', 'Vermilion Rockfish','Yellowtail Rockfish')) %>% 
    droplevels() %>% 
    ddply( .(Common.Name), 
           transform, sum.fish = sum(N)) %>%  #total of each species
    ddply( .(Common.Name), transform,
           percent  = N/sum.fish,
           position = 1-((cumsum(N)/ sum.fish) - (0.5 * N/sum.fish))) %>% #create proportion 
    mutate(perc.lab = round(percent, 2)*100) %>%  #create new percent factor for label in plot
    na.omit()
  
  
  #plot
  ggplot(gear.species.summary, aes(x=Common.Name, y=N, colour = factor(Gear.Type), fill = Gear.Type)) +
    geom_bar(stat="identity", colour='black',position='fill') +      # position = fill makes it percent of eachj
    # geom_text(aes(y=label_height, label=N),       # add the labels
    #           colour="white") +                       # label colour
    scale_fill_manual(values = c('darkgoldenrod1','tomato3','steelblue'))+
    scale_y_continuous(expand = c(0,0)) +# colour palette
    xlab("") +                                     # x-axis label
    ylab("Proportion") +  #y-axis label (even though it is horizontal)
    labs(fill = "Gear Type")+# label the legend title (by naming what is being filled by color)
    ggtitle("Gear Type by Species") +
    coord_flip()+
    geom_text(data=gear.species.summary, aes(x = Common.Name, y = position, 
                                             label = paste0(perc.lab,"%")), size = gear.fs, colour = 'black') + #can now label percentages
    
    plot.theme +
    
    #reverse legend order
    guides(fill = guide_legend(reverse=TRUE))
  
  
  
}
  #   
  }) 
  
}

shinyApp(ui = ui, server = server)

