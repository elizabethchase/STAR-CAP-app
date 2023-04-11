#Load any additional packages you need here:
library(shiny)
library(shinyBS)
library(shinyjs)
library(survival)
library(ggplot2)
library(pammtools)
library(dplyr)
library(kableExtra)
library(markdown)
library(ggwaffle)
library(emojifont)

load("ClinMods2.RData")

# Defining the user interface:
ui <- fluidPage(
        tags$head(HTML(
                "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-179472953-1'></script>
                <script>
                window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                
                gtag('config', 'UA-179472953-1');
                </script>
                "
        )),
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
                                                     min = 0,
                                                     max = 200),
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
                                        bsTooltip("psa", "This is the patient's last pre-treatment PSA result. It should be between 0-200 ng/mL.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("tstage", "This is the patient's AJCC 8th edition T stage.", 
                                                  "right", options = list(container = "body")),
                                        bsTooltip("nstage", "This is the patient's AJCC 8th edition N stage.", 
                                                  "right", options = list(container = "body"))
                                        ),
                                mainPanel(
                                  br(),
                                  "Our staging model is for patients diagnosed with prostate cancer who have not 
                                           yet begun treatment. We predict the long-term outcomes", 
                                           em("with standard curative treatments"), "including surgical removal of the prostate gland or 
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
                                      htmlOutput("text0a"),
                                      br(),
                                      htmlOutput("text1"), 
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
                                      plotOutput("pict2"),
                                      br(),
                                      plotOutput("pict1")
                                    ),
                                    tabPanel(
                                      "Cumulative Incidence",
                                      plotOutput("plot1"),
                                      fluidRow(
                                        column(sliderInput(inputId = "years", label="Years", min = 0, max = 14, value = 10), width = 10),
                                        column(submitButton("Update"), width = 2)
                                      ),
                                      br(),
                                      textOutput("info1"), 
                                      br(),
                                      br(),
                                      br()
                                    )
                                  )
                                )
        ),
        hr(),
        print("Disclaimer: The content provided does not provide medical advice. 
              By proceeding, you acknowledge that viewing or use of this content does 
              not create a medical professional-patient relationship, and does not 
              constitute an opinion, medical advice, professional service or treatment 
              recommendation of any condition. Content provided is for educational purposes only. 
              The information and Content provided are not substitutes for medical or professional care, 
              and you should not use the information in place of a visit, call, consultation or the 
              advice of your physician or other healthcare provider. You are liable or responsible for 
              any advice, course of treatment, or any other information, services that are based on 
              Content through this site."),
        br(),
        br()
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
              includeMarkdown("starcap_methods4.Rmd"),
              hr(),
              print("Disclaimer: The content provided does not provide medical advice. 
              By proceeding, you acknowledge that viewing or use of this content does 
              not create a medical professional-patient relationship, and does not 
              constitute an opinion, medical advice, professional service or treatment 
              recommendation of any condition. Content provided is for educational purposes only. 
              The information and Content provided are not substitutes for medical or professional care, 
              and you should not use the information in place of a visit, call, consultation or the 
              advice of your physician or other healthcare provider. You are liable or responsible for 
              any advice, course of treatment, or any other information, services that are based on 
              Content through this site.")),
              br(),
              br()
 )
            ))#)


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
            
            nccn_highrisk <- sum(c(input$tstage==7, input$psa > 20, grade >= 6))
            nccn_veryhigh <- sum(c(input$tstage >= 8, input$primarygleason==5))
            nccn_intermed <- sum(c(input$tstage==5 | input$tstage==6, grade == 3, grade == 5,
                                  input$psa >= 10 & input$psa <= 20))
            nccn <- ifelse(input$tstage==3 & input$primarygleason==3 & input$secondarygleason==3 & input$psa < 10 &
                                   input$pos_cores < 3 & pct_score <= 0.5, "Very Low", 
                           ifelse(input$tstage <= 4 & input$primarygleason==3 & input$secondarygleason==3 & 
                                          input$psa < 10, "Low", 
                                  ifelse(nccn_veryhigh >= 1 | nccn_highrisk >= 2, "Very High",
                                         ifelse(nccn_veryhigh==0 & nccn_highrisk==1, "High",
                                                ifelse(nccn_highrisk==0 & nccn_veryhigh==0 & nccn_intermed==1 & input$primarygleason==3 & pct_score < 0.5, 
                                                       "Favorable Intermediate", "Unfavorable Intermediate"))
                                                 )))
            
            nccn <- ifelse(nccn=="Very Low", "Low", ifelse(nccn=="Very High", "High", nccn))
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
            dm_predictions <- z.p.all_dm

            pcsm_surv <- pcsm_predictions[,which(c("0","1-2","3-4","5-6","7-8","9-10","11-12", "13-16",">=17")==pcsm_dat$S1_Score_Comb_Final)]
            dm_surv <- dm_predictions[,which(c("0","1-2","3-4","5-6","7-8","9-10","11-12", "13-16",">=17")==pcsm_dat$S1_Score_Comb_Final)]
            common_times <- seq(0, 167.5, by = 0.5)
    
            
            # #Combine this into a data-frame:
            alldat <- data.frame("Time" = c(0, common_times, 0, common_times),
                                 "Est" = c(0, pcsm_surv, 0, dm_surv),
                               "Outcome" = c(rep(c("PCSM", "Distant Metastases"), each = (length(common_times)+1))))
            
            dat_score <- data.frame("Time" = c(0, common_times), "Risk" = c(0, pcsm_surv))
            dat_score_dm <- data.frame("Time" = c(0, common_times), "Risk" = c(0, dm_surv))
           
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
            riskten_dm <- dat_score_dm$Risk[which.min(ifelse((120-dat_score_dm$Time) < 0, NA, (120-dat_score_dm$Time)))]
            riskfive <- dat_score$Risk[which.min(ifelse((60-dat_score$Time) < 0, NA, (60-dat_score$Time)))]
            riskfive_dm <- dat_score_dm$Risk[which.min(ifelse((60-dat_score_dm$Time) < 0, NA, (60-dat_score_dm$Time)))]
            
            resultstab <- data.frame("Time" = c("5 Years", "10 Years"), 
                                     "Metastases" = c(paste0(round(riskfive_dm*100, digits = 1), "%"),
                                              paste0(round(riskten_dm*100, digits = 1), "%")),
                                     "PCSM" = c(paste0(round(riskfive*100, digits = 1), "%"),
                                                paste0(round(riskten*100, digits = 1), "%")))
          
            mypred <- round(dat_score$Risk[which.min(ifelse((input$years*12-dat_score$Time) < 0, NA, (input$years*12-dat_score$Time)))]*100, digits=1)
            mypred_dm <- round(dat_score_dm$Risk[which.min(ifelse((input$years*12-dat_score_dm$Time) < 0, NA, (input$years*12-dat_score_dm$Time)))]*100, digits=1)
            mypred_free <- round((100-mypred-mypred_dm), digits = 1)
            
            subdat <- data.frame("Outcome" = c(rep("PCSM", round(riskten*100, digits=0)),
                                              rep("Alive", 100-round(riskten*100, digits=0))), "Test" = 1)
            
            subdat$Outcome <- factor(subdat$Outcome, levels = c("Alive", "PCSM"), ordered = TRUE)
            
            waffle_data <- waffle_iron(subdat, aes_d(group = Outcome), rows = 10) %>% 
              mutate(label = fontawesome('fa-male'))
            
            subdat2 <- data.frame("Outcome" = c(rep("Metastases", round(riskten_dm*100, digits = 0)),
                                               rep("Mets-Free", 100-round(riskten_dm*100, digits=0))), "Test" = 1)
            
            subdat2$Outcome <- factor(subdat2$Outcome, levels = c("Mets-Free", "Metastases"), ordered = TRUE)
            
            waffle_data2 <- waffle_iron(subdat2, aes_d(group = Outcome), rows = 10) %>% 
              mutate(label = fontawesome('fa-male'))
            
            list(alldat = alldat, stagepred = stagepred, riskten = riskten, resultstab = resultstab, mypred = mypred, 
                 patient_char = patient_char, nccn = nccn, waffle_data = waffle_data, waffle_data2 = waffle_data2,
                 riskten_dm = riskten_dm, mypred_dm = mypred_dm, mypred_free = mypred_free)
            
            
        })
       
       output$text0 <- renderText({

        paste0("This patient is  ", model()$patient_char$Age, " years old with clinical ", model()$patient_char$Tstage, " ", 
                model()$patient_char$Nstage, " M0 prostate adenocarcinoma, Gleason ", model()$patient_char$Gleason, " with ",
                model()$patient_char$Pos_cores, "/", model()$patient_char$Tot_cores, " (", model()$patient_char$Pct_cores,
                "%) core biopsies positive, and a PSA of ", model()$patient_char$PSA, " ng/mL.")
                  
       })
       
       output$text0a <- renderText({
          if (model()$patient_char$Nstage == "N0"){
                paste0("This patient is <b>NCCN risk group ", model()$nccn, "<b>.")
          }
       })
       
       output$text1 <- renderText({
        paste0("This patient is grouped in <b>STAR CAP Stage ", model()$stagepred, "<b>.")
       })
       
       output$table1 <- function(){kable(model()$resultstab, col.names = c("Time", "Metastases", "Prostate Cancer Specific Mortality")) %>% 
                       column_spec(column = c(1:3), width = "4cm") %>%
                       kable_styling()}
       
       output$text2 <- renderText({
               number <- ifelse(round(model()$riskten*100, digits=0) < 1, "less than 1", round(model()$riskten*100, digits=0))
               number2 <- ifelse(round(model()$riskten_dm*100, digits=0) < 1, "less than 1", round(model()$riskten_dm*100, digits=0))
               paste0("Of 100 men with prostate cancer like yours, ", number2, " will have developed distant metastases in 10 years, and ",
                      number, " will have died of their prostate cancer in 10 years.")
       })
       
       output$pict1 <- renderPlot({
         
         ggplot(data = model()$waffle_data, aes(x, y, color = group)) + 
           geom_text(aes(label = label), family = 'fontawesome-webfont', size = 8) + 
           coord_equal() + 
           theme_bw() + theme_waffle() + xlab("") + ylab("") + 
           scale_color_manual(name = "Outcome", values = c("gray74", "darkred"), 
                              guide = guide_legend(reverse = TRUE)) + 
           theme(text = element_text(size = 12), legend.position = "top")
       })
       
       output$pict2 <- renderPlot({
         
         ggplot(data = model()$waffle_data2, aes(x, y, color = group)) + 
           geom_text(aes(label = label), family = 'fontawesome-webfont', size = 8) + 
           coord_equal() + 
           theme_bw() + theme_waffle() + xlab("") + ylab("") + 
           scale_color_manual(name = "Outcome", values = c("gray74", "mediumpurple4"), 
                              guide = guide_legend(reverse = TRUE)) + 
           theme(text = element_text(size = 12), legend.position = "top")
       })
       
       output$plot1 <- renderPlot({
               
               ggplot() + geom_step(data = model()$alldat, aes(x = Time, y = Est*100, color = Outcome)) +
                       scale_x_continuous("Years", limits = c(0, 192), breaks = seq(0, 192, by=48), labels = c("0", "4", "8", "12", "16")) +
                       scale_y_continuous("Risk (%)", breaks = c(0, 25, 50, 75, 100)) + coord_cartesian(ylim=c(0, 100)) + theme_bw() + 
                       geom_hline(yintercept = model()$mypred, color = "darkred", linetype = "dashed") + 
                       geom_hline(yintercept = (model()$mypred_dm), color = "mediumpurple4", linetype = "dashed") + 
                       geom_vline(xintercept = input$years*12, linetype = "dashed") + 
                       scale_color_manual(values = c("mediumpurple4", "darkred"))
       })
       
       output$info1 <- renderText({
               paste0("At ", round(input$years, digits = 2), " years, the probability of developing distant metastases is ", model()$mypred_dm, "%. The 
               probability of dying of prostate cancer is ", model()$mypred, "%.")
       })
       
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
