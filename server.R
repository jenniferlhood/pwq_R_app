library('shiny')
library('ggplot2')
library('dunn.test')
data = read.csv("top30.csv")
des = read.csv("parms3.txt")
city = read.csv("cities.txt")


data$Month <- ordered(data$Month, 
    levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
    labels = c("Jan","Feb","Mar",
    "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
    
data$Year <- (levels = as.character(data$Year))
    
data$Station <-(levels = as.character(data$Station))

river <- sort(as.character(unique(data$River))) 

stations_df <- unique(data.frame(data$Station,data$Lat, data$Long))


haversine <- function(lat1,lon1,lat2,lon2){
    dlat <- (pi/180)*(lat2-lat1)
    dlon <- (pi/180)*(lon2-lon1)
    
    a = sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2 
    
    c = 2* asin(sqrt(a))
    return(c*6371)
}



shinyServer(function(input, output,clientData,session) {
  
    #----reactive expressions----------
    
    #----for the first tab
    
    subset_1 <- reactive({
        clat = city[city$City == input$city,2]
        clon = city[city$City == input$city,3]       
        
      s <-subset(data, haversine(Lat,Long,clat,clon) < input$dist)    
      
      
      if (input$years != "all years"){
            s <- s[s$Year == input$years,]
        }    
       
            
      return (s)
             
    })
    
    
    subset_r <- reactive({
        s <- subset(subset_1(), River %in% input$river)
              
        return (s)
    })


    des_sub <- reactive({
        sel <- des[des$PARM == input$param,]
        sel_final <- paste(as.character(sel[1,2]), "Units:", as.character(sel[1,3]))
        return (sel_final)
        
    })

    bins <- reactive({
        sel <- subset_r()[,input$param]
        ra = range(sel,na.rm=T)[2] - range(sel,na.rm=T)[1]
        return (ra/input$bin)
    })

    stations <- reactive({
        return (as.character(unique(subset_r()[,"Station"])))
    })
    

    
    summaries <- reactive({
        
        s<-paste("Summary statistics for ", input$param,"\n")
        s <- paste(s,"Station\t N\t  Mean\t\t  Median\t St Dev.\t Range\t\tRiver\n")
        
              
          
        for ( i in stations()){
            d <- subset_r()[subset_r()$Station == i,input$param]
            river_name <- as.character(subset_r()[subset_r()$Station == i,"River"][1])
            e<-summary(d)
            
                s<-paste(s,i,"\t",
                    length(d),"\t",
                    sprintf("% .4f",e[4]),"\t",
                    sprintf("% .4f",e[3]),"\t",
                    sprintf("% .4f",sd(d, na.rm=T)),"\t",
                    sprintf("% .4f",e[6]-e[1]),"\t",
                    river_name,"\n")
             
        }
        return (s)
    })    

    #Reactive UI
    
    output$descr <- renderText({
        des_sub() 
     })

    observe({
        sel_rlist <- as.character(unique(subset_1()[,"River"]))
       
        updateCheckboxGroupInput(session,"river", choices = sel_rlist, selected = sel_rlist)
    })



    ##
    #----for the second tab



    subset_2 <- reactive({
        clat = city[city$City == input$city_2,2]
        clon = city[city$City == input$city_2,3]       
        
      s <-subset(data, haversine(Lat,Long,clat,clon) < input$dist_2)    
      
      
      if (input$years_2 != "all years"){
            s <- s[s$Year == input$years_2,]
        }    

      return (s)
             
    })
    
    
    subset_r2 <- reactive({
        
        s <- subset(subset_2(), River %in% input$river_2)
        
        return (s)
    })

    des_sub_1 <- reactive({
        sel <- des[des$PARM == input$param_1,]
        sel_final <- paste(as.character(sel[1,2]), "Units:", as.character(sel[1,3]))
        return (sel_final)
        
    })

    des_sub_2 <- reactive({
        sel <- des[des$PARM == input$param_2,]
        sel_final <- paste(as.character(sel[1,2]), "Units:", as.character(sel[1,3]))
        return (sel_final)
        
    })



    cor_p <- reactive({
        #pearson's correlation
        result <- paste ("Pearson's product-moment correlation\n\n", "Between: ", input$param_1, " and ", input$param_2,"\n",
                "with alpha = 0.95\t")

        if (input$cor_1 == "all" | input$colour_2 == "None"){
            
            c <-cor.test(subset_r2()[,input$param_1],subset_r2()[,input$param_2],
                use="pairwise.complete.obs", method="pearson")
                
            result <- paste(result,
                    "Grouping: All selected\n\n","Deg.freedom\tCor. coefficent\tP-value\t\tTest statistic\n")
            result <-paste(result,
                                c[2],"\t\t",
                                sprintf("% .3f", as.numeric(c[4])),"\t",
                                sprintf("% .3f", as.numeric(c[3])),"\t\t",
                                sprintf("%.3f",as.numeric(c[1])),"\n")
            
        } else if (input$cor_1 == "group" & input$colour_2 != "None"){
            group <- unique(subset_r2()[,input$colour_2])
            option <- switch(input$colour_2,
                    "River" = subset_r2()$River,
                    "Station" = subset_r2()$Station,
                    "Season" = subset_r2()$Season
                    )       
            
            result <- paste(result,
                    "Grouping:",input$colour_2, "\n\n",
                    "Deg.freedom\tCor. coefficent\t\tP-value\t\tT-statistic\t\t",
                        input$colour_2,"\n")
            
            for (i in group) {

                s <- subset_r2()[option == i,]
                 
                #result<-paste(result,"\n",s,i)
                c <- try(cor.test(s[,input$param_1],s[,input$param_2],use="pairwise.complete.obs",
                    method="pearson"))
                result <-paste(result,
                    c[2],"\t\t",
                    sprintf("% .3f",as.numeric(c[4])),"\t\t",
                    sprintf("% .3f", as.numeric(c[3])),"\t",
                    sprintf("% .3f", as.numeric(c[1])),"\t\t",
                    i,"\n")  
            }
        }
        return (result)
    })




    #reactive UI
    output$descr_1 <- renderText({
        des_sub_1() 
    })
    output$descr_2 <- renderText({
        des_sub_2() 
    })

    observe({
        sel_rlist_2 <- as.character(unique(subset_2()[,"River"]))
       
        updateCheckboxGroupInput(session,"river_2", choices = sel_rlist_2, selected = sel_rlist_2)
    })






    #-----------Plots and summaries -------------
    
    
    #histogram plot
    
    output$title1 <- renderText({
        paste("Histogram of ", input$param, "for sites near", 
                input$city, "for", input$years )
    })
    
    output$plot1 <- renderPlot({
       #palette = 6 is greyscale
       p <- ggplot(subset_r(), aes_string(input$param,fill= as.character(input$colour))) 
       p <- p + geom_histogram(colour="darkslategray", binwidth = bins()) 
       p <- p + scale_fill_brewer(palette = "YlGnBu") + theme(panel.background = element_rect(fill='grey80'))
       
       p <- p + theme(axis.text.x = element_text(size=15,colour='darkslategray'))
       p <- p + theme(axis.text.y = element_text(size=15,colour='darkslategray'))
       
       p <- p + theme(axis.title.x = element_text(size=15,colour='darkslategray'))
       p <- p + theme(axis.title.y = element_text(size=15,colour='darkslategray'))
       
       
       try(print(p))
        
    })
        
    #histogram summary
    output$summary1 <- renderPrint({
        cat(summaries())
    })
   
    
    #boxplots
    output$title2 <- renderText({
        paste("Boxplots of ", input$param, input$compare)
    })
    
    output$plot2 <- renderPlot({
       df <- data.frame(subset_r())
       p<- ggplot(df, aes_string(input$compare,input$param)) + geom_boxplot(fill="lightseagreen")
       
       p <- p + theme(panel.background = element_rect(fill='grey80'))
       p <- p + theme(axis.text.x = element_text(size=10,colour='darkslategray'))
       p <- p + theme(axis.text.y = element_text(size=15,colour='darkslategray'))
       
       p <- p + theme(axis.title.x = element_text(size=15,colour='darkslategray'))
       p <- p + theme(axis.title.y = element_text(size=15,colour='darkslategray'))


       if (input$log=="log"){
            p<-p + scale_y_log10()
       }
       
       print(p)
        
    })
    
    #boxplot summary (kruskal-wallis and dunn's test)
    output$summary2 <- renderPrint({
        factors <- factor(subset_r()[,input$compare])
        dunn.test(subset_r()[,input$param], factors,wrap=T)
    })
    
    #Scatter plots
    
    output$title_2<-renderText({
        paste("Bi-plot of ", input$param_1, " vs ", input$param_2)
    })
    output$plot_2 <- renderPlot({
        p <- ggplot(subset_r2(), aes_string(input$param_1,input$param_2)) 
       
        
        if (input$cor_1 == "group" & input$colour_2 != "None"){
            p <- p + geom_smooth(method=lm,alpha=0.3)
            p <- p + aes_string(colour=input$colour_2)  
        } else {
            p <- p + geom_smooth(method=lm,alpha=0.3,colour="lightseagreen")
        }
                
        if (input$colour_2 != "None"){
            p <- p + geom_point(aes_string(colour=input$colour_2),size=3) 
            
            
        } else {
            p <- p + geom_point(size=3)
        }
        
      
       
        if (input$log_x == T){
            p<- p + scale_x_log10()
            
        }
        if (input$log_y == T){
            p<- p + scale_y_log10()
        }
            
        p <- p + scale_colour_brewer(palette = "YlGnBu")  + theme(panel.background = element_rect(fill='grey80'))
     
        p <- p + theme(axis.text.x = element_text(size=15,colour='darkslategray'))
        p <- p + theme(axis.text.y = element_text(size=15,colour='darkslategray'))
       
        p <- p + theme(axis.title.x = element_text(size=15,colour='darkslategray'))
        p <- p + theme(axis.title.y = element_text(size=15,colour='darkslategray'))

       print(p)
    })
    output$summary_2 <- renderPrint({
        cat(cor_p())
    })

})
