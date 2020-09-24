#Hi, Krithika! I've tried to comment throughout--please let me know if you have any questions.

#Load any additional packages you need here:
library(shiny)
library(shinyBS)
library(shinyjs)
library(survival)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(png)
library(gridGraphics)

#Here is where you would load in the model objects, if you're going that route--you'd want to save the model
#objects/predictions in the same folder that this file is saved in, which should only have things needed
#to produce the app:

#load("score_model.RData")
#load("survival_randomforest_model.RData")
#load("interaction_model.RData")

load("ClinMods.RData")

# Defining the user interface:
ui <- fluidPage(
        titlePanel("STAR CAP Prostate Cancer Staging System"), #Feel free to change the title if you have thoughts!
            mainPanel(
                tabsetPanel(
                    tabPanel("STAR CAP Clinical Prognostic Stage Group",
                             sidebarLayout(
                                sidebarPanel(
                                        numericInput("age", 
                                                     "Age (years)", 
                                                     value = 65,
                                                     min = 0),
                                        selectInput("tstage", 
                                                    "Clinical T Stage", 
                                                    choices = list("T1a" = 1,
                                                                   "T1b" = 2,
                                                                   "T1c" = 3,
                                                                   "T2a" = 4,
                                                                   "T2b" = 5,
                                                                   "T2c" = 6,
                                                                   "T3a" = 7,
                                                                   "T3b" = 8,
                                                                   "T4" = 9),
                                                    selected = 3),
                                        selectInput("nstage", 
                                                    "Clinical N Stage", 
                                                    choices = list("N0" = 1, 
                                                                   "N1" = 2),
                                                    selected = 1),
                                        selectInput("primarygleason", 
                                                    "Primary Gleason", 
                                                    choices = list("3" = 3,
                                                                   "4" = 4,
                                                                   "5" = 5),
                                                    selected = 3),
                                        selectInput("secondarygleason",
                                                    "Secondary Gleason",
                                                    choices = list("3" = 3,
                                                                   "4" = 4,
                                                                   "5" = 5),
                                                    selected = 3),
                                        numericInput("pos_cores", 
                                                     "Number of positive cores", 
                                                     value = 3,
                                                     min = 0),
                                        numericInput("neg_cores", 
                                                     "Number of negative cores", 
                                                     value = 9,
                                                     min = 0),
                                        numericInput("psa", 
                                                     "PSA (ng/mL)", 
                                                     value = 4,
                                                     min = 0),
                                        submitButton("Update"),
                                        bsTooltip("age", "This is the patient's current age.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("pos_cores", "This is the number of biopsy cores that were positive for prostate cancer.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("neg_cores", "This is the number of biopsy cores that were negative for prostate cancer.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("primarygleason", "This is the patient's most recent primary Gleason score from biopsy.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("secondarygleason", "This is the patient's most recent secondary Gleason score from biopsy.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("psa", "This is the patient's last pre-treatment PSA result.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("tstage", "This is the patient's AJCC 8th edition T stage.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("nstage", "This is the patient's AJCC 8th edition N stage.", 
                                                  "right", options = list(container = "body"))
                                        ),
                                mainPanel(
                                  br(),
                                  "Our staging model is for patients diagnosed with prostate cancer who have not 
                                           yet begun treatment. We predict the long-term chances of dying from prostate cancer 
                                           after standard treatments including surgical removal of the prostate gland or 
                                           curative radiation therapy with or without hormonal therapy.",
                                  br(),
                                  br(),
                                  br(),
                                  tabsetPanel(
                                    tabPanel(
                                      "Stage",
                                      br(),
                                      textOutput("text0"),
                                      br(),
                                      textOutput("text1"), 
                                      br(),
                                      tableOutput("table1"),
                                      bsTooltip("table1", "This predicts a patient's probability of dying of his prostate cancer at 5 and 10 years.", 
                                                "right", options = list(container = "body"))
                                    ),
                                    tabPanel(
                                      "Pictogram",
                                      br(),
                                      textOutput("text2"),
                                      br(),
                                      plotOutput("pict1")
                                    ),
                                    tabPanel(
                                      "Cumulative Incidence",
                                      plotOutput("plot1"),
                                      fluidRow(
                                        column(sliderInput(inputId = "years", label="Years", min = 0, max = 15, value = 10), width = 10),
                                        column(submitButton("Update"), width = 2)
                                      ),
                                      br(),
                                      textOutput("info1"), 
                                     # textOutput("info2"), 
                                     # textOutput("info3"), 
                                      br(),
                                      br(),
                                      br()
                                    )
                                  )
                                )
        )
     ), 
     tabPanel("More Information", 
              br(),
              includeMarkdown("starcap_methods2.Rmd"),
              br(),
              tableOutput("infotable1"),
              br(),
              includeMarkdown("starcap_methods3.Rmd"),
              br(),
              tableOutput("infotable2"),
              br(),
              includeMarkdown("starcap_methods4.Rmd"))
 )
            ))


# This is where you do the math to get the sentence/plot given above. 
server <- function(input, output) {
        model <- reactive({
            #Here I'm building the scores that you used for your model; alternatively, you can just
            #create a data frame with all of this information if that's what you need for the random forest
            #and interaction models: 
            grade <- ifelse(input$primarygleason <= 3 & input$secondarygleason <= 3, 0, 
                                ifelse(input$primarygleason==3 & input$secondarygleason==4, 3, 
                                       ifelse(input$primarygleason==4 & input$secondarygleason==3, 5,
                                              ifelse((input$primarygleason==4 & input$secondarygleason==4) | 
                                                             (input$primarygleason==3 & input$secondarygleason==5), 6,
                                                     ifelse(input$primarygleason==4 & input$secondarygleason==5, 7,
                                                            ifelse(input$primarygleason==5, 8))))))
        
            nstage <- ifelse(input$nstage==1, 0, 8)
            tstage <- ifelse(input$tstage<=3, 0, 
                         ifelse(input$tstage==4 | input$tstage==5, 1,
                                ifelse(input$tstage==6 | input$tstage==7, 2, 3)))
            age_score <- ifelse(input$age > 50 & input$age <= 70, 0, 1)
            psa <- ifelse(input$psa <= 6, 0, 
                      ifelse(input$psa > 6 & input$psa <= 10, 1,
                             ifelse(input$psa > 10 & input$psa <= 20, 2,
                                    ifelse(input$psa > 20 & input$psa <= 50, 3, 4))))
            pct_score <- input$pos_cores/(input$pos_cores + input$neg_cores)
            pct_cores <- ifelse(pct_score <= 0.5, 0, 
                            ifelse(pct_score >0.5 & pct_score <= 0.75, 2, 3))     
            subscore <- grade + nstage + tstage + age_score + psa + pct_cores
            pcsm_dat <- data.frame("S1_Score_Comb_Final"=NA)
            pcsm_dat$S1_Score_Comb_Final <- ifelse(subscore==0, "0",
                                                   ifelse(subscore==1 | subscore==2, "1-2",
                                                          ifelse(subscore==3 | subscore==4, "3-4",
                                                                 ifelse(subscore==5 | subscore==6, "5-6",
                                                                        ifelse(subscore==7 | subscore==8, "7-8",
                                                                               ifelse(subscore==9 | subscore==10, "9-10",
                                                                                      ifelse(subscore==11 | subscore==12, "11-12",
                                                                                             ifelse(subscore>= 13 & subscore <= 16, "13-16",
                                                                                                    ">=17"))))))))
        
            patient_char <- data.frame("Age" = input$age, "Nstage" = input$nstage, "Tstage" = input$tstage, "PSA" = input$psa, 
                                       "Pct_cores" = round(pct_score*100, digits= 1) , "Gleason" = paste0(input$primarygleason, "+", input$secondarygleason),
                                       "Pos_cores" = input$pos_cores, "Tot_cores" = input$pos_cores + input$neg_cores)
            patient_char$Nstage <- factor(patient_char$Nstage, levels=c(1, 2), labels=c("N0", "N1"))
            patient_char$Tstage <- factor(patient_char$Tstage, levels=c(1:9), labels=c("T1a","T1b", "T1c", "T2a","T2b","T2c","T3a","T3b","T4"))
           
            #Here I'm getting the predictions from the score model (in my original formulation, I outputted the survival curves for each score
            #"pcsm_predictions" and loaded that into the app):
            # Cumulative incidence curves for each Score level 
            pcsm_predictions <- z.p.all_val

            pcsm_surv <- pcsm_predictions[,which(c("0","1-2","3-4","5-6","7-8","9-10","11-12", "13-16",">=17")==pcsm_dat$S1_Score_Comb_Final)+1]
            pcsm_time <- pcsm_predictions[,1]
            
            # Recoding of inputs --------------------------------------------------------------------------
            #Age <- input$age
            #Age_bin <- cut(input$age, breaks=c(0,50,70,100),include.lowest = TRUE)
            #Age_bin <- relevel(Age_bin, ref="(50,70]")
            #PSA_bin <- cut(input$psa, breaks=c(0,6,10,20,50,200),include.lowest = TRUE)
            #lPSA <- log(input$psa)
            # Pct_bin <- ifelse(input$pos_cores == 1, "[0,0.5]", 
            #                   ifelse(input$pos_cores == 2, "(0.5,0.75]", "(0.75,1]"))    
            # Pct_bin <- factor(Pct_bin, levels= c("[0,0.5]", "(0.5,0.75]", "(0.75,1]"))
            #PctCoresPositive <- pct_score
            #Pct_bin <- cut(pct_score, breaks=c(0,0.5,0.75,1),include.lowest = TRUE)
            
            #cTstage_Final <- ifelse(input$tstage <= 3, "T1a-c",
             #                       ifelse(input$tstage == 4 | input$tstage==5, "T2a/b",
              #                             ifelse(input$tstage == 6 | input$tstage==7, "T2c/T3a",
               #                                   ifelse(input$tstage == 8 | input$tstage==9, "T3b/T4", NA))))
            
            #cTstage_Final <- factor(cTstage_Final, levels = c("T1a-c", "T2a/b", "T2c/T3a", "T3b/T4"))
            #intNstage <- ifelse(input$nstage==1, 0, ifelse(input$nstage==2, 1))
            #cNstage <- factor(intNstage, levels = c(0, 1)) ##Bug with how I was adding in Krithika's model
                        
            #cGradeSep <- ifelse(input$primarygleason==3 & input$secondarygleason ==3, "<=6",
             #                   ifelse(input$primarygleason==3 & input$secondarygleason ==4, "3+4",
              #                         ifelse(input$primarygleason==4 & input$secondarygleason ==3, "4+3",
               #                               ifelse((input$primarygleason==4 & input$secondarygleason ==4) | 
                #                                             (input$primarygleason==3 & input$secondarygleason ==5), "4+4/3+5",
                 #                                    ifelse(input$primarygleason==4 & input$secondarygleason ==5, "4+5", 
                  #                                          ifelse((input$primarygleason==5 & input$secondarygleason ==3) | 
                   #                                                        (input$primarygleason==5 & input$secondarygleason ==4), "5+3/5+4", NA))))))
            #cGradeSep <- factor(cGradeSep, c("<=6", "3+4", "4+3", "4+4/3+5", "4+5", "5+3/5+4"))
            #TreatmentYear <- 2013
            
            # Prediction function 
           # pred.ctsMod <- function(mod, covs) {
            #        lhat <- cumsum(exp(sum(covs * mod$coef)) * mod$bfitj)
             #       lhat <- cbind(mod$uftime, 1 - exp(-lhat))
               #     lhat
            #}
            
            #covs_me <- model.matrix(~ Age_bin + cTstage_Final + cNstage + cGradeSep + Pct_bin + PSA_bin + TreatmentYear )[-1]
            #pcsm_me_predictions <- pred.ctsMod(Cts_me_pred, covs=covs_me)
            
            #covs_nonlin_full <- model.matrix(~ cGradeSep  
             #                                +cNstage
              #                               +cTstage_Final
               #                              +Age
                #                             +lPSA
                 #                            +PctCoresPositive
                  #                           +TreatmentYear
                   #                          +cGradeSep*cTstage_Final
                    #                         +cGradeSep*lPSA
                     #                        +cGradeSep*Age
                      #                       +cGradeSep*PctCoresPositive
                       #                      +cTstage_Final*lPSA
                        #                     +cTstage_Final*Age
                         #                    +cTstage_Final*PctCoresPositive
                          #                   +cNstage*lPSA
                           #                  +cNstage*Age
                            #                 +cNstage*PctCoresPositive
                             #                +lPSA*Age
                              #               +lPSA*PctCoresPositive
                               #              +Age*PctCoresPositive)[,-1]
            
           # covs_nonlin <- covs_nonlin_full[c(1:13, 22, 27, 34, 36, 37, 39, 40, 42, 44:46, 52, 57, 58)]
            #pcsm_nonlin_predictions <- pred.ctsMod(Cts_nonlin_pred, covs = covs_nonlin)
            
            # #Combine this into a data-frame:
            #alldat <- data.frame("Time" = c(0, pcsm_time, 0, pcsm_me_predictions[,1], 0, pcsm_nonlin_predictions[,1]),
             #                    "Risk" = c(0, pcsm_surv, 0, pcsm_me_predictions[,2], 0, pcsm_nonlin_predictions[,2]),
              #                   "Model" = c(rep("Score", length(pcsm_time)+1), 
               #                              rep("Main effects", nrow(pcsm_me_predictions)+1),
                #                             rep("Interaction", nrow(pcsm_nonlin_predictions)+1)))
            alldat <- data.frame("Time" = c(0, pcsm_time),
                                "Risk" = c(0, pcsm_surv),
                               "Model" = c(rep("Score", length(pcsm_time)+1)))
            
            dat_score <- data.frame("Time" = c(0, pcsm_time), "Risk" = c(0, pcsm_surv))
            #dat_me <- data.frame("Time" = c(0, pcsm_me_predictions[,1]), "Risk" = c(0, pcsm_me_predictions[,2]))
           # dat_int <- data.frame("Time" = c(0, pcsm_nonlin_predictions[,1]), "Risk" = c(0, pcsm_nonlin_predictions[,2]))
            
            #Here you might output each patient's predicted stage:
            stagepred <- case_when(
                    pcsm_dat$S1_Score_Comb_Final=="0" ~ "IA",
                    pcsm_dat$S1_Score_Comb_Final=="1-2" ~ "IB",
                    pcsm_dat$S1_Score_Comb_Final=="3-4" ~ "IC",
                    pcsm_dat$S1_Score_Comb_Final=="5-6" ~ "IIA",
                    pcsm_dat$S1_Score_Comb_Final=="7-8" ~ "IIB",
                    pcsm_dat$S1_Score_Comb_Final=="9-10" ~ "IIC",
                    pcsm_dat$S1_Score_Comb_Final=="11-12" ~ "IIIA",
                    pcsm_dat$S1_Score_Comb_Final=="13-16" ~ "IIIB",
                    pcsm_dat$S1_Score_Comb_Final==">=17" ~ "IIIC")
            
            riskten <- dat_score$Risk[which.min(ifelse((120-dat_score$Time) < 0, NA, (120-dat_score$Time)))]
            riskfive <- dat_score$Risk[which.min(ifelse((60-dat_score$Time) < 0, NA, (60-dat_score$Time)))]
            
            resultstab <- data.frame("Metric" = c("Stage", "5-Year Prostate Cancer Specific Mortality", "10-Year Prostate Cancer Specific Mortality"), 
                                     "Prediction" = c(stagepred, paste0(round(riskfive*100, digits = 1), "%"),
                                                paste0(round(riskten*100, digits = 1), "%")))
          
            mypred <- round(dat_score$Risk[which.min(ifelse((input$years*12-dat_score$Time) < 0, NA, (input$years*12-dat_score$Time)))]*100, digits=1)
            #mypred_me <- round(dat_me$Risk[which.min(ifelse((input$years*12-dat_me$Time) < 0, NA, (input$years*12-dat_me$Time)))]*100, digits=2)
            #mypred_int <- round(dat_int$Risk[which.min(ifelse((input$years*12-dat_int$Time) < 0, NA, (input$years*12-dat_int$Time)))]*100, digits=1)
            
            #Output results:
            #list(alldat = alldat, stagepred = stagepred, riskten = riskten, resultstab = resultstab, mypred = mypred, mypred_me = mypred_me, mypred_int = mypred_int)
            list(alldat = alldat, stagepred = stagepred, riskten = riskten, resultstab = resultstab, mypred = mypred, patient_char = patient_char)
            
            
        })
       
       output$text0 <- renderText({
               
        paste0("This patient is  ", model()$patient_char$Age, " years old with clinical ", model()$patient_char$Tstage, " ", 
               model()$patient_char$Nstage, " M0 prostate adenocarcinoma, Gleason ", model()$patient_char$Gleason, " with ",
               model()$patient_char$Pos_cores, "/", model()$patient_char$Tot_cores, " (", model()$patient_char$Pct_cores,
               "%) core biopsies positive, and a PSA of ", model()$patient_char$PSA, " ng/mL.")
               
       })
       
       output$text1 <- renderText({
        paste0("This patient is grouped in STAR CAP Stage ", model()$stagepred, ".")
       })
       
       output$table1 <- function(){kable(model()$resultstab) %>% column_spec(column = c(1:2), width = "4cm") %>%
                       kable_styling()}
       
       output$text2 <- renderText({
               number <- ifelse(round(model()$riskten*100, digits=1) < 1, "less than 1", round(model()$riskten*100, digits=0))
               paste0("Of 100 men with prostate cancer like yours, ", number, " will have died of their prostate cancer in 10 years.")
       })
       
       output$pict1 <- renderPlot({
               #pictdat <- data.frame("x" = rep(1:10, 10), "y" = c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10), rep(5, 10), rep(6, 10),
                                                                  #rep(7, 10), rep(8, 10), rep(9, 10), rep(10, 10)), 
                                    # "alive" = c(rep("Alive", 100 - round(model()$riskten*100, digits=0)), rep("Dead", round(model()$riskten*100, digits=0))))
               
               #ggplot() + geom_point(data = pictdat, aes(x = x, y = y, color = alive), shape = 17, size = 5) + scale_color_manual(values = c("black", "red"),
                                                                                                                       # name = "Outcome") + 
                       #theme(rect = element_blank(), axis.ticks = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(), 
                            # axis.text.y=element_blank()) + xlab("") + ylab("")
               img_red <- readPNG("man_red.png")
               img_blue <- readPNG("man_blue.png")
               deadman <- rasterGrob(img_red, interpolate=FALSE)
               liveman <- rasterGrob(img_blue, interpolate=FALSE)
               
               myplot <- qplot(0:10, 0:10, geom="blank") 
               k <- 1
               dead <- round(model()$riskten*100, digits=0)
               vec1 <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
               for (j in vec1){
                       for (i in 1:10){
                               if (k <= dead){
                                       myplot <- myplot + annotation_custom(deadman, xmin=(i-1), xmax=i, ymin=(j-1), ymax=j)
                               } else{
                                       myplot <- myplot + annotation_custom(liveman, xmin=(i-1), xmax=i, ymin=(j-1), ymax=j)
                               }
                               k <- k+1
                       }
               }
               
               myplot + theme(rect = element_blank(), axis.ticks = element_blank(), axis.line=element_blank(),axis.text.x=element_blank(), 
                                        axis.text.y=element_blank()) + xlab("") + ylab("") 
       })
       
       output$plot1 <- renderPlot({
               #ggplot() + geom_step(data = model()$alldat, aes(x = Time, y = Risk*100, group = Model, color = Model), size = 1.5,
                 #                   direction = "hv", alpha = 1) +
                #       scale_x_continuous("Years", limits = c(0, 192), breaks = seq(0, 192, by=48), labels = c("0", "4", "8", "12", "16")) +
                  #     scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) +
                   #    scale_color_manual(values = c("gray74", "mediumpurple4", "red")) + theme_bw() + 
                    #   geom_hline(yintercept = model()$mypred) + geom_hline(yintercept = model()$mypred_int)  + 
                     #  geom_vline(xintercept = input$years*12)
               ggplot() + geom_step(data = model()$alldat, aes(x = Time, y = Risk*100), size = 1.5,
                                    direction = "hv", alpha = 1, color = "mediumpurple4") +
                       scale_x_continuous("Years", limits = c(0, 192), breaks = seq(0, 192, by=48), labels = c("0", "4", "8", "12", "16")) +
                       scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) + theme_bw() + 
                       geom_hline(yintercept = model()$mypred) + geom_vline(xintercept = input$years*12)
       })
       
       output$info1 <- renderText({
               paste0("At ", round(input$years, digits = 2), " years, the probability of dying of prostate cancer is ", model()$mypred, "%.")
       })
       #output$info2 <- renderText({
        #       paste0("Main effect model: At ", round(input$years, digits = 2), " years, the probability of dying of prostate cancer is ", model()$mypred_me, "%.")
       #})
       #output$info3 <- renderText({
        #       paste0("Interaction model: At ", round(input$years, digits = 2), " years, the probability of dying of prostate cancer is ", model()$mypred_int, "%.")
       #})
       
       output$infotable2 <- function(){
               testdat <- data.frame("Var" = c("Total Score", "Stage Group"), "zero" = c("0", "IA"), 
                                     "onetwo" = c("1-2", "IB"), "threefour" = c("3-4", "IC"), 
                                     "fivesix" = c("5-6", "IIA"), "seveight" = c("7-8", "IIB"), 
                                     "nineten" = c("9-10", "IIC"), "eleventwelve" = c("11-12", "IIIA"), 
                                     "thirtsixt" = c("13-16", "IIIB"), "sevent" = c("17 or more", "IIIC"))
               kable(testdat, col.names = NULL) %>% kable_styling()}
       
       output$infotable1 <- function(){
               testdat <- data.frame("Characteristic" = c("Age", "   0-50","   51-70", "   71+", "Grade", "   3+3", "   3+4", "   4+3", 
                                                          "   4+4, 3+5", "   4+5", "   5+3, 5+4, 5+5", "Percent Positive Cores", 
                                                          "   0-50%", "   51-75%", "   76-100%", "Clinical N Stage", "   N0", 
                                                          "   N1", "Clinical T Stage", "   T1a-c", "   T2a-b", "   T2c, T3a", 
                                                          "   T3b, T4", "PSA", "   0-6", "   7-10", "   11-20", "   21-50", "   51-200"), 
                                     "Points" = c("", "1", "0", "1", "", "0", "3", "5", "6", "7", "8", "", "0", "2", "3", "", "0", "8", "", "0", 
                                                  "1", "2", "3", "", "0", "1", "2", "3", "4"))
               
               kable(testdat, booktabs=T) %>% kable_styling() %>% add_indent(c(2:4, 6:11, 13:15, 17:18, 20:23, 25:29))}
 
}

# Run the application 
shinyApp(ui = ui, server = server)
