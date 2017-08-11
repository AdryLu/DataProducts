#
#
library(ggplot2)
library(gridExtra)
library(shiny)
library(plotly)
library(plyr)
library(dplyr)
library(stringi)
wagedat<-readRDS("data/file2.rds")
occupations<-readRDS("data/file3.rds")
cpi<-readRDS("data/file4.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$choose_occupation<- renderUI({
    occ.data<-setNames(as.list(occupations),occupations)
    if (input$occup!="Enter Occupation"){
      occupation1<-as.character(input$occup)
      occ<-grep(occupation1,occupations,ignore.case=TRUE,value=TRUE)
      if (length(occ)==0){
        occ<-occupations
      }
      occ.data<-setNames(as.list(occ),occ)
    }
    selectInput("occid", "Select Occupation", occ.data,
                selected = occ.data[[1]])
  })
  
  subsetdata<-reactive({
   # sex1<-input$sex
    occup1<-ifelse(is.null(input$occid),"All occupations",as.character(input$occid))
    subdata=tbl_df(wagedat)%>%
      dplyr::filter(Occupation==occup1)%>%
      dplyr::select(province,Occupation,sex,Median.wages.and.salaries,PRUID)
    return(subdata)
  })
  
  output$text1<-renderText({
    input$occid
  })
  
  output$Plot1 <- renderPlot({
      
    occup1<-ifelse(is.null(input$occid),"All occupations",as.character(input$occid))
    
    ymax<-round_any(max(subsetdata()$Median.wages.and.salaries,na.rm=TRUE),1000,ceiling)+2000
    yrg<-c(0,ymax)
    male<-subsetdata()%>%
      dplyr::filter(sex=="male")
    
    g2<-ggplot(data=male,aes(x=province,y=Median.wages.and.salaries))+
      geom_col(color="blue",fill="lightblue")+
      ylim(sort(yrg,decreasing=TRUE))+
      coord_flip()+
      theme(axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank())+
      ggtitle("Males")+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    fem<-subsetdata()%>%
      dplyr::filter(sex=="female")
    
    g1<-ggplot(data=fem,aes(x=province,y=Median.wages.and.salaries))+
      geom_col(color="red",fill="pink")+ ylim(yrg)+coord_flip()+
      ggtitle("Females")+
      theme(plot.title = element_text(hjust = 0.5))
    
    grid.arrange(g2,g1,ncol=2)
  })
  
  output$Plot2<-renderPlotly({
    occup1<-ifelse(is.null(input$occid),"All occupations",as.character(input$occid))
    sex1<-input$sex
    prov<-input$province
    subdata=tbl_df(wagedat)%>%
      dplyr::filter(province==prov & sex==sex1 )%>%
      dplyr::select(province,Occupation,sex,Median.wages.and.salaries)%>%
      dplyr::mutate(Occupation=reorder(Occupation,Median.wages.and.salaries))%>%
      dplyr::filter_all(any_vars(!is.na(.)))
    
    ax <- list(
      title = "Occupations",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = TRUE
    )
    
    ocp1<-subdata$Occupation
    colbar1<-rep(c('rgba(204,204,204,0.7)'),length(ocp1))
    colbar1[subdata$Occupation==occup1]<-'rgba(255, 0, 0,1)'
   
     t <- list(
      family = "sans serif",
      size = 12 )
    
    p <- plot_ly(data=subdata,x=~Occupation,
                 y=~Median.wages.and.salaries,type = 'bar',
                 marker=list(color=colbar1))%>%
        layout(title=paste("Median Wages and Salaries for",paste0(sex1,"s"),"in",prov),
               font=t,
               xaxis = ax)
      })
  
  output$Plot3<-renderPlotly({
    sex1<-input$sex
    prov<-input$province
    mydata<-subsetdata()%>%
      dplyr::filter(province==prov & sex==sex1 )
    subcpi<-cpi%>%
      dplyr::filter(PRUID==mydata$PRUID)%>%
      dplyr::select(6:11)
    
    adjusted.wage<-vector(mode="numeric",length = 6)
    adjusted.wage[1]<-mydata$Median.wages.and.salaries
    prov.cpi<-vector(mode="numeric",length=6)
    prov.cpi[1]<-subcpi[[1]][1]
    for(i in 2:6){
      adjusted.wage[i]<-adjusted.wage[i-1]*subcpi[[i]][1]/subcpi[[i-1]][1]
      prov.cpi[i]<-subcpi[[i]][1]
    }
    
    salary<-data.frame(year=c(2011:2016),Median.wage=adjusted.wage,CPI=prov.cpi)
    
    t <- list(
      family = "sans serif",
      size = 12 )
    
    
    p <- plot_ly(salary) %>%
      add_lines(x = ~year, y = ~Median.wage, 
                name ="Median Wage", yaxis="Inflation adjusted wage") %>%
        layout(
        title = paste("Inflation adjusted wage in",prov,"for", 
                paste0(sex1,"s"), "working in <br> -", input$occid,"-"),
        font=t, 
        xaxis = list(title="year")
      )
      
  })
  
})
