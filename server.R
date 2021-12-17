#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(deSolve)
library(tidyr)
library(reshape)
library(magrittr)
library(plyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shinybusy)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    output$SI <- renderPlot({
        #SI Deterministic Model
        SI.dyn <- function(t,var,par) {
            S <- var[1]
            I <- var[2]

            beta <- par[1]
            N<-S+I
            
            dS<--beta*S*I/N
            dI<-beta*S*I/N
            return(list(c(dS,dI)))
        }
        
        beta <- input$beta_SI
        I0 <- input$I0_SI
        S <- 100-I0
        SI.par <- c(beta)
        SI.init <- c(S,I0)
        SI.t <- seq(0,30,by=0.1)
        
        det.sol<-lsoda(SI.init,SI.t,SI.dyn,SI.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.I<-det.sol[,3]
        

        plot(0, 0, type='n', xlim=c(0,20),ylim=c(0,100), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        legend("right",legend=c("S","I"),col=c('green','red'),lty=rep(1,2),cex = 1.5,lwd = 2)
    }, height = 600 )
    ##################################################################
        
    
    output$SIS <- renderPlot({
        #SIS Deterministic Model
        
        SIS.dyn <- function(t,var,par) {
            S <- var[1]
            I <- var[2]
            
            beta <- par[1]
            gamma <- par[2]
            N<-S+I
            
            dS<--beta*S*I/N+gamma*I
            dI<-beta*S*I/N-gamma*I
            return(list(c(dS,dI)))
        }
        
        beta <- input$beta_SIS
        gamma <- input$gamma_SIS
        I0 <- input$I0_SIS
        S <- 100-I0
        SIS.par <- c(beta,gamma)
        SIS.init <- c(S,I0)
        SIS.t <- seq(0,30,by=0.1)
        
        det.sol<-lsoda(SIS.init,SIS.t,SIS.dyn,SIS.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.I<-det.sol[,3]
        
        plot(0, 0, type='n', xlim=c(0,20),ylim=c(0,100), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        legend("right",legend=c("S","I"),col=c('green','red'),lty=rep(1,2),cex = 1.5,lwd = 2)
    })
    
    output$SIR <- renderPlot({
        
        #SIR Deterministic Model
        
        SIR.dyn <- function(t,var,par) {
            S <- var[1]
            I <- var[2]
            R <- var[3]
            
            beta <- par[1]
            gamma <- par[2]
            N<-S+I+R
            
            dS<- -beta*S*I/N
            dI<- beta*S*I/N-gamma*I
            dR<- gamma*I
            return(list(c(dS,dI,dR)))
        }
        
        beta <- input$beta_SIR
        gamma <- input$gamma_SIR
        I0 <- input$I0_SIR
        S <- 100-I0
        SIR.par <- c(beta,gamma)
        SIR.init <- c(S,I0,0)
        SIR.t <- seq(0,50,by=0.1)
        
        det.sol<-lsoda(SIR.init,SIR.t,SIR.dyn,SIR.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.I<-det.sol[,3]
        det.R<-det.sol[,4]
        
        plot(0, 0, type='n', xlim=c(0,20),ylim=c(0,100), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        points(det.t,det.R,type='l',col='blue',lwd=3)
        legend("right",legend=c("S","I","R"),col=c('green','red','blue'),lty=rep(1,3),cex = 1.5,lwd = 2)
    })
    # 
    # #-----------------------------
    # 
    
    output$SIR2 <- renderPlot({
      
      #SIR Deterministic Model
      
      ageGroup <- 3  #number of age classes
      n <- c(18,42,15)/75 # fraction in each age class (assumption that life expectancy is 75 years)
      S0 <- c(0.05,0.01,0.008) # inital value for number of susceptible
      I0 <- c(input$I01_SIR2,input$I02_SIR2,input$I03_SIR2) # inital value for number of infectious
      R0 <- c(0, 0, 0) # inital value for number of recovered
      ND <- 365 # time to simulate
      beta <- matrix(c(input$beta1_SIR2, input$beta1_SIR2, input$beta1_SIR2,
                       input$beta2_SIR2, input$beta2_SIR2, input$beta2_SIR2,
                       input$beta3_SIR2, input$beta3_SIR2, input$beta3_SIR2 ), nrow=3, ncol=3)/200 # matrix of transmission rates

      gamma <- input$gamma_SIR2 # recovery rate
      TS <- 1 # time step to simualte is days
      
      # combining parameter and initial values
      parms <- list( beta=beta, gamma=gamma)
      INPUT <- c(S0, I0, R0)
      
      # constructing time vector
      t_start <- 0 # starting time
      t_end <- ND - 1 # ending time
      t_inc <- TS #time increment
      t_range <- seq(from= t_start, to=t_end+t_inc, by=t_inc) # vector with time steps
      
      # differential equations
      # RES = lsoda(INPUT, t_range, diff_eqs, parms)
      diff_eqs_sir <- function(times, Input, parms){
        dY <- numeric(length(Input))
        with(parms,{
          # creates an empty matrix
          for(i in 1:ageGroup){
            dY[i] <- -beta[,i]%*%Input[ageGroup + seq(1:ageGroup)] * Input[ageGroup + i]  # S_i
            dY[ageGroup+i] <- beta[,i] %*% Input[ageGroup + seq(1:ageGroup)] *Input[ageGroup + i] - gamma * Input[ageGroup + i]  #I_i
            dY[2*ageGroup+i] <- gamma * Input[ageGroup+i]   #R_i
          }
          list(dY)
        })
      }
      
      RES2_sir=rep(0,10)
      
      number_years <- 100 #set the number of years to simulate
      
      # initialize the loop
      k=1
      # yearly ageing
      for(k in 1:number_years) {
        RES = lsoda(INPUT, t_range, diff_eqs_sir, parms)
        #taking the last entry as the the new input that then is propagated accoring to the aging
        INPUT=RES[366,-1]
        # EX
        # AGE 4 groups
        # "1" = "0-6 years " 6 years
        # "2" = "6-10 years " 4 years
        # "3" = "10-20 years " 10 years
        # "4" = "20+ years "
        # INPUT[16]=INPUT[16]+INPUT[15]/10
        # INPUT[15]=INPUT[15]+INPUT[14]/4-INPUT[15]/10
        # INPUT[14]=INPUT[14]+INPUT[13]/6-INPUT[14]/4
        # INPUT[13]=INPUT[13]-INPUT[13]/6
        
        
        # AGE 3 groups
        # "1" = "0-18 years " 18 years
        # "2" = "18-60 years " 42 years
        # "3" = "60++ years "
        #S
        INPUT[1]=INPUT[1] - INPUT[1]/18
        INPUT[2]=INPUT[2] - INPUT[2]/42 + INPUT[1]/18
        INPUT[3]=INPUT[3] + INPUT[2]/42
        #I
        INPUT[4]=INPUT[4] - INPUT[4]/18
        INPUT[5]=INPUT[5] - INPUT[5]/42 + INPUT[4]/18
        INPUT[6]=INPUT[6] + INPUT[5]/42
        #R
        INPUT[7]=INPUT[7] - INPUT[7]/18
        INPUT[8]=INPUT[8] - INPUT[8]/42 + INPUT[7]/18
        INPUT[9]=INPUT[9] + INPUT[8]/42
        
        RES2_sir <- rbind(RES2_sir,RES)
        k=k+1
      }
      
      #rescaling time to years
      time <- seq(from=0, to=100*(ND+1))/(ND+1)
      # changing time to the rescaled time
      RES2_sir[ ,"time"] <- time
      
      #labeling of the output from ODE solver
      label_sir <- c("S1", "S2", "S3", "I1", "I2", "I3", "R1", "R2", "R3")
      label1_sir <- substr(label_sir, 1, 1)
      Age_sir <- substr(label_sir, 2, 2)
      
      df <- data.frame(time = RES2_sir[, 1],
                       label1_sir = rep(label1_sir, each =  nrow(RES2_sir)),
                       Age = rep(Age_sir, each =  nrow(RES2_sir)),
                       value = c(RES2_sir[, -1]))
      
      #plotting  the data
      df$label1 <- factor(df$label1, levels = c("S","I","R"))
      df$Age <- factor(df$Age)
      df %>% mutate(label1 = recode(label1, S = "Susceptible")) %>%
        mutate(label1 = recode(label1, I = "Infectious")) %>%
        mutate(label1 = recode(label1, R = "Recovered"))  %>%
        mutate(Age = recode(Age, "1" = "0-18 years ")) %>%
        mutate(Age = recode(Age, "2" = "18-60 yearss ")) %>%
        mutate(Age = recode(Age, "3" = "60+ years ")) %>%
        ggplot() +
        geom_line(aes(x = time, y = value, color = Age)) +
        facet_wrap( ~label1, ncol=1, scales =  "free_y")+
        xlab("Time (years)") + ylab(" Individuals")
      
    })
    # 
    # #-----------------------------
    # 
    
    output$SIRS <- renderPlot({
        
        #SIRS Deterministic Model
        
        SIRS.dyn <- function(t,var,par) {
            S <- var[1]
            I <- var[2]
            R <- var[3]
            
            beta <- par[1]
            gamma <- par[2]
            omega <- par[3]
            N<-S+I+R
            
            dS<- -beta*S*I/N+omega*R
            dI<- beta*S*I/N-gamma*I
            dR<- gamma*I-omega*R
            return(list(c(dS,dI,dR)))
        }
        
        beta <- input$beta_SIRS
        gamma <- input$gamma_SIRS
        omega <- input$omega_SIRS
        I0 <- input$I0_SIRS
        S <- 100-I0
        SIRS.par <- c(beta,gamma,omega)
        SIRS.init <- c(S,I0,0)
        SIRS.t <- seq(0,50,by=0.1)
        
        det.sol<-lsoda(SIRS.init,SIRS.t,SIRS.dyn,SIRS.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.I<-det.sol[,3]
        det.R<-det.sol[,4]
        
        plot(0, 0, type='n', xlim=c(0,20),ylim=c(0,100), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        points(det.t,det.R,type='l',col='blue',lwd=3)
        legend("topright",legend=c("S","I","R"),col=c('green','red','blue'),lty=rep(1,3),cex = 1.5,lwd = 2)
    })
    
    
    output$SEIR <- renderPlot({
        
        #SEIR Deterministic Model
        
        SEIR.dyn <- function(t,var,par) {
            S <- var[1]
            E <- var[2]
            I <- var[3]
            R <- var[4]
            
            beta <- par[1]
            gamma <- par[2]
            nui <- par[3]
            N<-S+E+I+R
            
            dS<- -beta*S*I/N
            dE<- beta*S*I/N-gamma*E
            dI<- gamma*E-nui*I
            dR<- nui*I
            return(list(c(dS,dE,dI,dR)))
        }
        
        beta <- input$beta_SEIR
        gamma <- input$gamma_SEIR
        nui <- input$nui_SEIR
        I0 <- input$I0_SEIR
        S <- 100-I0
        SEIR.par <- c(beta,gamma,nui)
        SEIR.init <- c(S,0,I0,0)
        SEIR.t <- seq(0,50,by=0.1)
        
        det.sol<-lsoda(SEIR.init,SEIR.t,SEIR.dyn,SEIR.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.E<-det.sol[,3]
        det.I<-det.sol[,4]
        det.R<-det.sol[,5]
        
        plot(0, 0, type='n', xlim=c(0,20),ylim=c(0,100), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.E,type='l',col='yellow2',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        points(det.t,det.R,type='l',col='blue',lwd=3)
        legend("right",legend=c("S","E","I","R"),col=c('green','yellow2','red','blue'),lty=rep(1,3),cex = 1.5,lwd = 2)
    })
    
    output$SEIR2 <- renderPlot({
        
        #SEIR Deterministic Model with birth&death
        
        SEIR2.dyn <- function(t,var,par) {
            S <- var[1]
            E <- var[2]
            I <- var[3]
            R <- var[4]
            
            beta <- par[1]
            gamma <- par[2]
            nui <- par[3]
            B <- par[4] #Birth rate
            D <- par[5] #Death rate
            N<-S+E+I+R
            
            dS<- B*N-D*S-beta*S*I/N
            dE<- beta*S*I/N-gamma*E-D*E
            dI<- gamma*E-nui*I-D*I
            dR<- nui*I-D*R
            return(list(c(dS,dE,dI,dR)))
        }
        
        beta <- input$beta_SEIR2
        gamma <- input$gamma_SEIR2
        nui <- input$nui_SEIR2
        I0 <- input$I0_SEIR2
        birth <- input$Birth_SEIR2
        death <- input$Death_SEIR2
        S <- 100-I0
        SEIR2.par <- c(beta,gamma,nui,birth,death)
        SEIR2.init <- c(S,0,I0,0)
        SEIR2.t <- seq(0,50,by=0.1)
        
        det.sol<-lsoda(SEIR2.init,SEIR2.t,SEIR2.dyn,SEIR2.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.E<-det.sol[,3]
        det.I<-det.sol[,4]
        det.R<-det.sol[,5]
        
        plot(0, 0, type='n', xlim=c(0,50),ylim=c(0,300), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.E,type='l',col='yellow2',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        points(det.t,det.R,type='l',col='blue',lwd=3)
        legend("topleft",legend=c("S","E","I","R"),col=c('green','yellow2','red','blue'),lty=rep(1,3),cex = 1.5,lwd = 2)
    })
    
    output$SEIRS <- renderPlot({
        
        #SEIRS Deterministic Model
        
        SEIRS.dyn <- function(t,var,par) {
            S <- var[1]
            E <- var[2]
            I <- var[3]
            R <- var[4]
            
            beta <- par[1]
            gamma <- par[2]
            nui <- par[3]
            omega <- par[4]
            N<-S+E+I+R
            
            dS<- -beta*S*I/N+omega*R
            dE<- beta*S*I/N-gamma*E
            dI<- gamma*E-nui*I
            dR<- nui*I-omega*R
            
            return(list(c(dS,dE,dI,dR)))
        }
        
        beta <- input$beta_SEIRS
        gamma <- input$gamma_SEIRS
        nui <- input$nui_SEIRS
        omega <- input$omega_SEIRS
        I0 <- input$I0_SEIRS
        S <- 100-I0
        SEIRS.par <- c(beta,gamma,nui,omega)
        SEIRS.init <- c(S,0,I0,0)
        SEIRS.t <- seq(0,50,by=0.1)
        
        det.sol<-lsoda(SEIRS.init,SEIRS.t,SEIRS.dyn,SEIRS.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.E<-det.sol[,3]
        det.I<-det.sol[,4]
        det.R<-det.sol[,5]
        
        plot(0, 0, type='n', xlim=c(0,20),ylim=c(0,100), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.E,type='l',col='yellow2',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        points(det.t,det.R,type='l',col='blue',lwd=3)
        legend("topright",legend=c("S","E","I","R"),col=c('green','yellow2','red','blue'),lty=rep(1,3),cex = 1.5,lwd = 2)
    })
    
    output$SEIRS2 <- renderPlot({
        
        #SEIRS Deterministic Model with birth&death
        
        SEIRS2.dyn <- function(t,var,par) {
            S <- var[1]
            E <- var[2]
            I <- var[3]
            R <- var[4]
            
            beta <- par[1]
            gamma <- par[2]
            nui <- par[3]
            omega <-par[4]
            B <- par[5] #Birth rate
            D <- par[6] #Death rate
            N<-S+E+I+R
            
            dS<- B*N-D*S-beta*S*I/N+omega*R
            dE<- beta*S*I/N-gamma*E-D*E
            dI<- gamma*E-nui*I-D*I
            dR<- nui*I-omega*R-D*R
            
            return(list(c(dS,dE,dI,dR)))
        }
        
        beta <- input$beta_SEIRS2
        gamma <- input$gamma_SEIRS2
        nui <- input$nui_SEIRS2
        omega <- input$omega_SEIRS2
        birth <- input$Birth_SEIRS2
        death <- input$Death_SEIRS2
        I0 <- input$I0_SEIRS2
        S <- 100-I0
        SEIRS2.par <- c(beta,gamma,nui,omega,birth,death)
        SEIRS2.init <- c(S,0,I0,0)
        SEIRS2.t <- seq(0,50,by=0.1)
        
        det.sol<-lsoda(SEIRS2.init,SEIRS2.t,SEIRS2.dyn,SEIRS2.par)
        
        
        det.t<-det.sol[,1]
        det.S<-det.sol[,2]
        det.E<-det.sol[,3]
        det.I<-det.sol[,4]
        det.R<-det.sol[,5]
        
        plot(0, 0, type='n', xlim=c(0,50),ylim=c(0,200), xlab='time', ylab='infected case')
        points(det.t,det.S,type='l',col='green',lwd=3)
        points(det.t,det.E,type='l',col='yellow2',lwd=3)
        points(det.t,det.I,type='l',col='red',lwd=3)
        points(det.t,det.R,type='l',col='blue',lwd=3)
        legend("topleft",legend=c("S","E","I","R"),col=c('green','yellow2','red','blue'),lty=rep(1,3),cex = 1.5,lwd = 2)
    })

    # #-------------
    # 
    # 
 
})
