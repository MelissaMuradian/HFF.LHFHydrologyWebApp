##############################################################################
# DESCRIPTION: HENRY'S FORK FOUNDATION LOWER HF HYDROLOGY WEBSITE
# Copyright (C) 2021  MELISSA MURADIAN AND ROB VAN KIRK
# This program is free software: you can redistribute it and/or modify it under the terms of the 
# GNU General Public License as published by the Free Software Foundation, either version 3 of the 
# License, or any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without 
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Post to a web page this pre-rendered png; png created daily during irrigation season
# URL: https://henrysforkdata.shinyapps.io/HydrologyApp/
#
# Written: June 2020
# Last updated: 
################################################################################
require(shiny)
require(shinydashboard) # Extra UI Features
##############################################################################
# INITIAL VALUES:

# DEFINE THE UI (USER INTERFACE)
ui <- dashboardPage(
  dashboardHeader(title = "Real-Time Flow of the Lower Henry's Fork", titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
        "Please email questions or comments to rob@henrysfork.org and visit henrysfork.org 
              for more information on The Henry's Fork Foundation, The Voice of the River"
      ), 
  fluidRow(
    column(6,
           tags$head(tags$style(
             type="text/css",
             "#lowerHF img {max-width: 100%; width: 100%; height: auto}"
           )),
           plotOutput('lowerHF')
           ))
  
)) # end UI definition 

# DEFINE THE SERVER LOGIC
server <- function(input, output) {
  ######### tag url links ########################
  # HFF Webpage
  urlHFF <- tags$a(href="https://henrysfork.org/", "Henry's Fork Foundation | The Voice of the River",  
                   style="color:white;")
  output$hffLink <- renderUI({
    tagList(urlHFF)
  })
 
# Pull data down from s3 and make it reactive
  dat <- reactive({
    s3load(object = "LowerHF/LHF.Rdata", bucket="hffwq")
    dat <- eval(parse(text = "LHF"))
    return(dat)
    })

# Create graphic
  output$lowerHF <- renderPlot({
    # load reactive data to plotting code
    LHF <- dat()
    
    plot(LHF$plot.time,c(LHF$StA.Q[(LHF$nrows-LHF$pos.n-7*96):LHF$nrows],rep(NA,72*4)),type="n",xaxt="n",
         ylim=c(LHF$mn,1.25*LHF$mx),ylab="Discharge (cfs)",xlab="Time",
         main=paste("Lower Henry's Fork Streamflow as of", LHF$last.data), # - 6*60*60)),
         cex.lab=1.2,cex.main=1.4)
    axis.POSIXct(1,at=LHF$plot.time[seq(1,12*96,96)],format="%b-%d")
    abline(v=LHF$plot.time[seq(1,12*96,96)],col="gray85",lty=2)
    grid(ny=NULL,nx=NA,col="gray85")
    abline(h=0); abline(h=1000,lwd=3)
    abline(h=350,lwd=3,col="darkorange")
    lines(LHF$data.time,LHF$StA.Q[(LHF$nrows-LHF$pos.n-7*96):LHF$nrows])
    lines(LHF$data.time,LHF$Trestle[(LHF$nrows-LHF$pos.n-7*96):LHF$nrows],col="gray50")
    lines(LHF$data.time,LHF$Parker[(LHF$nrows-LHF$pos.n-7*96):LHF$nrows],col="darkorange")
    lines(LHF$pred.time,LHF$sta.pred,lwd=2,lty=2)
    lines(LHF$pred.time,LHF$sta.lo,lwd=1,lty=2)
    lines(LHF$pred.time,LHF$sta.hi,lwd=1,lty=2)
    lines(LHF$pred.time,LHF$tre.pred,lwd=2,lty=2,col="gray50")
    lines(LHF$pred.time,LHF$par.pred,lwd=2,lty=2,col="darkorange")
    lines(LHF$pred.time,LHF$par.hi,lwd=1,lty=2,col="darkorange")
    lines(LHF$pred.time,LHF$par.lo,lwd=1,lty=2,col="darkorange")
    
    legend("topleft",lty=1,lwd=1,col=c(1,"gray50","darkorange"),
           c("St. Anthony","Trestle","Parker"),
           bg="white")
    legend("topright",lwd=c(2,3,3),col=c(1,1,"darkorange"),
           lty=c(2,1,1),c("Forecasts (w/ 95% interval for St. A. and Parker)","St. Anthony target","Parker target"),
           bg="white")
    
  }) # end Server definition 
  
}

# RETURN A SHINY APP OBJECT
shinyApp(ui = ui, server = server)

#######################################################################################################
