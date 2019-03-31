#--------------------------------------------------------------------------------------------------------------
#   This application was developed based on Snelder's cardiovascular model.
#
#   Yu Fu
#   March 6th, 2019
#--------------------------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(RxODE)
library(ggplot2)
library(rmarkdown)
library(gdata)
library(tools)


server <- function(input, output){
  
  observeEvent(input$titleID0, {
    js$collapse("box0")
  })
  observeEvent(input$titleID1, {
    js$collapse("box1")
  })
  observeEvent(input$titleID2, {
    js$collapse("box2")
  })
  observeEvent(input$titleID3, {
    js$collapse("box3")
  })
  observeEvent(input$titleID4, {
    js$collapse("box4")
  })
  observeEvent(input$titleID5, {
    js$collapse("box5")
  })
  observeEvent(input$titleID6, {
    js$collapse("box6")
  })
  observeEvent(input$titleID7, {
    js$collapse("box7")
  })

  r <- reactive({
    theta1 <- NULL
    theta2 <- NULL
    info1 <- NULL
    info2 <- NULL
    
    pd <- "
    C = A/V1;
    CR_HR = amp_HR*cos(pi*(time+hor_HR)/12)*CR;
    CR_TPR = amp_TPR*cos(pi*(time+hor_TPR)/12)*CR;
    EFF1 = EMAX_1*C/(EC50_1+C);
    EFF2 = EMAX_2*C/(EC50_2+C);
    EFF3 = EMAX_3*C/(EC50_3+C);
    d/dt(HR) = KIN_HR*(1-FB*HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR)*(1+CR_HR)*(1+EFF1)-KOUT_HR*HR;
    d/dt(SV1) = KIN_SV*(1-FB*HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR)*(1+EFF2)-KOUT_SV*SV1;
    d/dt(TPR) = KIN_TPR*(1-FB*HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR)*(1+CR_TPR)*(1+EFF3)-KOUT_TPR*TPR;
    CO = HR*SV1*(1-HR_SV*log(HR/BSL_HR));
    MAP = HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR;"
    
    SLpd <-"
    C = A/V1;
    CR_HR = amp_HR*cos(pi*(time+hor_HR)/12)*CR;
    CR_TPR = amp_TPR*cos(pi*(time+hor_TPR)/12)*CR;
    SLOPE1 = SL1*C**POW;
    SLOPE2 = SL2*C**POW;
    SLOPE3 = SL3*C**POW;
    d/dt(HR) = KIN_HR*(1-FB*HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR)*(1+CR_HR)*(1+SLOPE1)-KOUT_HR*HR;
    d/dt(SV1) = KIN_SV*(1-FB*HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR)*(1+SLOPE2)-KOUT_SV*SV1;
    d/dt(TPR) = KIN_TPR*(1-FB*HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR)*(1+CR_TPR)*(1+SLOPE3)-KOUT_TPR*TPR;
    CO = HR*SV1*(1-HR_SV*log(HR/BSL_HR));
    MAP = HR*SV1*(1-HR_SV*log(HR/BSL_HR))*TPR;"
    
    #--------------------- initial values ----------------------------------
    
    inits1 <- c(C = 0,
               TIME = 0,
               HR  = 310, # bpm
               SV1  = 69/310, # mL / beat
               TPR = 155/69 # mmHg/mL *min
    )
    inits2 <- c(C = 0,
                TIME = 0,
                HR  = 323, # bpm
                SV1  = 129/323, # mL / beat
                TPR = 102/129 # mmHg/mL *min
    )
    inits <- switch(input$rattype, "spontaneously hypertensive rats (SHR)" = inits1, "normotensive Wistar-Kyoto rats (WKY)" = inits2)
    
    #-------------- Mode of Action ----------------------
    
    EMAX_1 = 0
    EMAX_2 = 0
    EMAX_3 = 0
    
    if (input$mode == "Heart Rate") {EMAX_1 = input$emax}
    if (input$mode == "Stroke Volume") {EMAX_2 = input$emax}
    if (input$mode == "Total Peripheral Resistance"){EMAX_3 = input$emax}
    
    #----------------- Circadian Rhythm Switch ----------------------------
    
    CR <- 0
    
    if (input$cr){
      CR <- 1
    }
    
    #------------------ Other Parameters ----------------------------------
    if (input$specie == "Rat"){
    kin1 <- c(BSL_HR = 310,
              KIN_HR = 11.6*310/(1-0.0029*155),
              KIN_SV = 0.126*69/310/(1-0.0029*155),
              KIN_TPR = 3.58*155/69/(1-0.0029*155))
    kin2 <- c(BSL_HR = 323,
              KIN_HR = 11.6*323/(1-0.0029*102),
              KIN_SV = 0.126*129/323/(1-0.0029*102),
              KIN_TPR = 3.58*102/129/(1-0.0029*102))
    
    kin <- switch(input$rattype, "spontaneously hypertensive rats (SHR)" = kin1, "normotensive Wistar-Kyoto rats (WKY)" = kin2)
    
    others <- c(kin,c(
                      FB = 0.0029,    # 1*mmHg-1
                      KOUT_HR = 11.6,  #h-1
                      KOUT_SV = 0.126, #h-1
                      KOUT_TPR = 3.58, #h-1
                      HR_SV = 0.312,
                      hor_HR = 8.73,  #h
                      amp_HR = 0.0918,
                      hor_TPR = 19.3, #h
                      amp_TPR = 0.0918,
                      CR = CR))
    } else {
      others <- c(BSL_HR = 79.7,
                  KIN_HR = 10*79.7/(1-0.0029*110),
                  KIN_SV = 10*1450/79.7/(1-0.0029*110),
                  KIN_TPR = 10*110/1450/(1-0.0029*110),
                  FB = 0.0029,    # 1*mmHg-1
                  KOUT_HR = 10,  #h-1
                  KOUT_SV = 10, #h-1
                  KOUT_TPR = 10, #h-1
                  HR_SV = 0.312,
                  hor_HR = 8.73,  #h
                  amp_HR = 0.0918,
                  hor_TPR = 19.3, #h
                  amp_TPR = 0.0918,
                  CR = CR)
    }
    
    others1 <- c(others,c(EMAX_1=EMAX_1,
                          EC50_1=1000,    #ng mL-1
                          EMAX_2=EMAX_2,
                          EC50_2=1000,    #ng mL-1
                          EMAX_3=EMAX_3,
                          EC50_3=input$ec50    #ng mL-1
                          ))

    #-------------------------------- amount unit ------------------------------------
    
    amount <- switch(input$amountunit, 'mg/kg' = input$amt * 10^6, 'ug/kg' = input$amt * 10^3, 'ng/kg' = input$amt)
    
    
    #-------------------------------- time unit ---------------------------------------
    
    tu <- switch(input$timeunit, hour = 1, day = 24, week = 24*7)
    
    #---------------------------------- dose event ------------------------------------------------
    
    
    if(input$nd > 1){
      ev1 <- eventTable()
      ev1$add.dosing(dose = amount, nbr.doses = input$nd, dosing.interval = input$ii*24)}
    else {
      ev1 <- eventTable()
      ev1$add.dosing(dose = amount, nbr.doses = input$nd)}
    
    ev1$add.sampling(0:(input$obs*tu))
    
    
    #------------------------ Model equations --------------------------
    
    if (input$cmt ==""){
      x1 <- data.frame(time = NA, C = NA, CR_HR = NA,CR_TPR = NA,EFF1 = NA,EFF2 = NA,EFF3 = NA,
                       HR = NA,SV1 = NA,TPR = NA,CO = NA,MAP = NA)[0,]
    } else {
           if (input$cmt == "one-compartmental"){
             pk1 <- "d/dt(A) = -k10*A;"
             pkparams1 <- c(V1=input$V1*1000,
                            k10=input$k10)
           }
           if (input$cmt == "two-compartmental"){
             pk1 <- "d/dt(A) = -(k10 + k12)*A + k21*P1;
                     d/dt(P1) = -k21*P1 + k12*A;"
             pkparams1 <- c(V1=input$V1*1000,
                            k10=input$k10, 
                            k12=input$k12,
                            k21=input$k21)
           }
           if (input$cmt == "three-compartmental"){
             pk1 <- "d/dt(A)  = -(k10 + k12 + k13)*A + k21*P1 +k31*P2;
                     d/dt(P1) = -k21*P1 + k12*A;
                     d/dt(P2) = -k31*P1 + k13*A;"
             pkparams1 <- c(V1=input$V1*1000,
                            k10=input$k10, 
                            k12=input$k12,
                            k13=input$k13,
                            k21=input$k21,
                            k31=input$k31)
           }
      theta1 <- c(others1, pkparams1)
      pkpd1 <- paste(pk1,pd)
      m1 <- RxODE(pkpd1)
      x1 <- solve(m1,theta1,ev1,inits)}

      #---------------- Reference Drug --------------------  
      if (input$plotswitch == FALSE){
        x2 <- data.frame(time = NA, C = NA, CR_HR = NA,CR_TPR = NA,EFF1 = NA,EFF2 = NA,EFF3 = NA,
                         HR = NA,SV1 = NA,TPR = NA,CO = NA,MAP = NA)[0,]
      } 
      else{
        
        #-------------------------------- amount unit 2-------------------------------------
        amount2 <- switch(input$amountunit2, 'mg/kg' = input$amt2 * 10^6, 'ug/kg' = input$amt2 * 10^3, 'ng/kg' = input$amt2)
        #------------------------------------ dose event 2----------------------------------------------
        
        if(input$nd2 > 1){
          ev2 <- eventTable()
          ev2$add.dosing(dose = amount2, nbr.doses = input$nd2, dosing.interval = input$ii2*24)}
        else {
          ev2 <- eventTable()
          ev2$add.dosing(dose = amount2,nbr.doses = input$nd2)}
        
        ev2$add.sampling(0:(input$obs*tu))
        
        #-------------------------- match drug name --------------------------------------
        
        if (input$drugname == "Amiloride"){
          pk2 <- "d/dt(D) = -F1*ka*D;
                  d/dt(L) =  F1*ka*D -klc*L + kcl*A - kle*L;
                  d/dt(A) =           klc*L - kcl*A - kcp*A + kpc*P - kce*A;
                  d/dt(P) =                           kcp*A - kpc*P;"
          pkpd2 <- paste(pk2,pd)
          pkparams2 <- c(ka = 0.086,
                         klc = 0.491,
                         kcl = 0.563,
                         kcp = 0.290,
                         kpc = 0.017,
                         kle = 7.069,
                         kce = 0.042,
                         V1 = 0.202*1000,
                         F1 = 0.9887)
          effparams <- c(EMAX_1=0,
                         EC50_1=1000,    #ng mL-1
                         EMAX_2=1,
                         EC50_2=245,    #ng mL-1
                         EMAX_3=0,
                         EC50_3=1000)    #ng mL-1
        }
        
        if (input$drugname == "Amlodipine"){
          pk2 <- "d/dt(D) = -ka*D;
                  d/dt(A) =  ka*D -k10*A;"
          pkpd2 <- paste(pk2,pd)
          pkparams2 <- c(ka = 0.4, V1 = 32*1000,k10 = 0.23)
          effparams <- c(EMAX_1=0,
                         EC50_1=1000,    #ng mL-1
                         EMAX_2=0,
                         EC50_2=1000,    #ng mL-1
                         EMAX_3=1,
                         EC50_3=82.8)    #ng mL-1
        }
        
        if (input$drugname == "Atropine"){
          pk2 <- "d/dt(A) = -k12*A + k21*P - k10*A;
                  d/dt(P) =  k12*A - k21*P;"
          pkpd2 <- paste(pk2,SLpd)
          pkparams2 <- c(V1 = 3.506/0.325*1000, k10 = 0.043*60, k12 = 0.154*60, k21 = 0.082*60)
          effparams <-c(SL1 = 0.00149, SL2 = 0, SL3 = 0, POW = 1)    #(ng/ml)-1
        }
        
        if (input$drugname == "Enalapril"){
          pk2 <- "d/dt(D) = -F1*ka*D
                  d/dt(A) =  F1*ka*D - VM*A/(KM + A/V1) - k12*A + k21*P;
                  d/dt(P) =                               k12*A - k21*P;"
          pkparams2 <- c(VM = 767*1000, V1 = 0.346*1000, k12 = 1.56, k21 = 2.94, KM = 150*1000, ka = 1.75, F1 = 0.376)
          pkpd2 <- paste(pk2,pd)
          effparams <-c(EMAX_1=0,
                        EC50_1=1000,    #ng mL-1
                        EMAX_2=1,
                        EC50_2=1200,    #ng mL-1
                        EMAX_3=1,
                        EC50_3=1200)    #ng mL-1
        }
        
        if (input$drugname == "Fasudil"){
          pk2 <- "d/dt(A) = -k10*A;"
          pkpd2 <- paste(pk2,pd)
          pkparams2 <- c(V1 = 22.92*1000, k10 = 4.62)
          effparams <-c(EMAX_1=0,
                        EC50_1=1000,    #ng mL-1
                        EMAX_2=0,
                        EC50_2=1000,    #ng mL-1
                        EMAX_3=1,
                        EC50_3=0.172)    #ng mL-1
        }
        
        if (input$drugname == "HCTZ"){
          pk2 <- "d/dt(D) = -F1*ka*D;
                  d/dt(A) =  F1*ka*D - k10*A;"
          pkpd2 <- paste(pk2,pd)
          pkparams2 <- c(V1 = 0.0168*1000, k10 = 0.079, ka = 0.563, F1 = 0.007)
          effparams <-c(EMAX_1=0,
                        EC50_1=1000,    #ng mL-1
                        EMAX_2=1,
                        EC50_2=28900,    #ng mL-1
                        EMAX_3=0,
                        EC50_3=1000)    #ng mL-1
        }
        
        if (input$drugname == "Prazosin"){
          pk2 <- "d/dt(A) = -k10*A;"
          pkpd2 <- paste(pk,SLpd)
          pkparams2 <- c(V1 = 14*2.8*1000,k10 = (0.0927*60/14)*(0.3/2.8)**(-0.25))
          effparams <-c(SL1 = 0, SL2 = 0, SL3 = 0.328, POW = 0.0910)    #ng mL-1
        }
        
        theta2 <- c(others, pkparams2,effparams)
        m2 <- RxODE(pkpd2)
        x2 <- solve(m2,theta2,ev2,inits)
      }
    
    #----------------- plot setting ----------------------      
    pl <- ggplot()+
      theme_bw()+
      xlab(paste0("Time (",input$timeunit,")"))
    
    #-------------------------------------- plotting ---------------------------------------------
    
    File <- input$file1
    
    pklab <- paste0('Concentration (',input$concunit,')')
    
    cu <- switch(input$concunit, 'mg/ml' = 10^6, 'ug/ml' = 10^3, 'ng/ml' = 1) 
    
    pk <- pl + ylab(pklab)
    hr <- pl + ylab('Heart Rate (bpm)')
    co <- pl + ylab('Cardiac Output (ml/min)')
    p  <- pl + ylab('Mean Arterial Pressure (mmHg)')
    
    if (input$plotswitch == FALSE & input$cmt =="" & is.null(File) ){

      pk <- pk + xlim (0,100) + ylim (0,100)
      hr <- hr + xlim (0,100) + ylim (0,100)
      co <- co + xlim (0,100) + ylim (0,100)
      p  <- p + xlim (0,100) + ylim (0,100)
      
    } else{
      
      vc <- c("#001158","#f46e32","#00ad4f")
      names(vc) <- c("x","r","i")
      lc <- c("Investigational Drug",input$drugname,"Input dataset")
      names(lc) <- c("x","r","i")
      vdisp1 <- rep(FALSE,3)
      vdisp2 <- rep(FALSE,3)
      vdisp3 <- rep(FALSE,3)
      vdisp4 <- rep(FALSE,3)
      
      if (input$cmt != "") {
        pk <- pk + geom_line(data = x1, aes(x = time/tu, y = C/cu,color = "x"), size = 1)
        hr <- hr + geom_line(data = x1, aes(x = time/tu, y = HR,  color = "x"), size = 1)
        co <- co + geom_line(data = x1, aes(x = time/tu, y = CO,  color = "x"), size = 1)
        p  <- p  + geom_line(data = x1, aes(x = time/tu, y = MAP, color = "x"), size = 1)
        vdisp1[1] <- TRUE
        vdisp2[1] <- TRUE
        vdisp3[1] <- TRUE
        vdisp4[1] <- TRUE
      }
      if (input$plotswitch == TRUE) {
        pk <- pk + geom_line(data = x2, aes(x = time/tu, y = C/cu,color = "r"), size = 1)
        hr <- hr + geom_line(data = x2, aes(x = time/tu, y = HR,  color = "r"), size = 1)
        co <- co + geom_line(data = x2, aes(x = time/tu, y = CO,  color = "r"), size = 1)
        p  <- p  + geom_line(data = x2, aes(x = time/tu, y = MAP, color = "r"), size = 1)
        vdisp1[2] <- TRUE
        vdisp2[2] <- TRUE
        vdisp3[2] <- TRUE
        vdisp4[2] <- TRUE
      }
      if (is.null(File) == FALSE) {
        if (file_ext(File$name) %in% c("xls","xlsx")) {
          file1 <- read.xls(File$datapath)
        }
        if (file_ext(File$name) == "csv") {
          file1 <- read.csv2(File$datapath)
        }
        
        if ("PK" %in% colnames(file1)) {
          pk <- pk + geom_line(data = file1, aes(x = time,y = PK, color = "i"),size = 1)
          vdisp1[3] <- TRUE
          }
        if ("HR" %in% colnames(file1)) {
          hr <- hr + geom_line(data = file1, aes(x = time,y = HR, color = "i"),size = 1)
          vdisp2[3] <- TRUE
          }
        if ("CO" %in% colnames(file1)) {
          co <- co + geom_line(data = file1, aes(x = time, y = CO, color = "i"),size = 1)
          vdisp3[3] <- TRUE
          }
        if ("MAP" %in% colnames(file1)) {
          p  <- p  + geom_line(data = file1, aes(x = time, y = MAP, color = "i"),size = 1)
          vdisp4[3] <- TRUE
        }
      }
      pk <- pk + scale_colour_manual(values = vc[vdisp1], labels = lc[vdisp1])
      hr <- hr + scale_colour_manual(values = vc[vdisp2], labels = lc[vdisp2])
      co <- co + scale_colour_manual(values = vc[vdisp3], labels = lc[vdisp3])
      p  <- p  + scale_colour_manual(values = vc[vdisp4], labels = lc[vdisp4])
    }
    #--------------------------------- species information --------------------------
    if (input$specie == "Rat"){
      if (input$rattype == "spontaneously hypertensive rats (SHR)"){
        info1 <- "<b>spontaneously hypertensive rats (SHR)</b><br><br>
                  BSL_HR_SHR = 310 beats/min <br>
                  BSL_MAP_SHR = 155 mmHg <br>
                  BSL_CO_SHR = 69 ml/min <br>
                  k<sub>out_HR</sub> = 11.6 h<sup>-1</sup> <br>
                  k<sub>out_SV</sub> = 0.126 h<sup>-1</sup> <br>
                  k<sub>out_TPR</sub> = 3.58 h<sup>-1</sup> <br>
                  FB = 0.0029 mmHg<sup>-1</sup> <br>
                  HR_SV = 0.312 <br>
                  hor<sub>HR</sub> = 8.73 h <br>
                  amp<sub>HR</sub> = 0.0918 <br>
                  hor<sub>TPR</sub> = 19.3 h <br>
                  amp<sub>TPR</sub> = 0.0918 <br><br>
                  <a href='https://bpspubs.onlinelibrary.wiley.com/doi/full/10.1111/bph.12824'>[1] Snelder, N. et al. Br J Pharmacol (2014).</a>"
      }
      if (input$rattype == "normotensive Wistar-Kyoto rats (WKY)"){
        info1 <- "<b>normotensive Wistar-Kyoto rats (WKY)</b><br><br>
                  BSL_HR_WKY = 323 beats/min <br>
                  BSL_MAP_WKY = 102 mmHg <br>
                  BSL_CO_WKY = 129 ml/min <br>
                  k<sub>out_HR</sub> = 11.6 h<sup>-1</sup> <br>
                  k<sub>out_SV</sub> = 0.126 h<sup>-1</sup> <br>
                  k<sub>out_TPR</sub> = 3.58 h<sup>-1</sup> <br>
                  FB = 0.0029 mmHg<sup>-1</sup> <br>
                  HR_SV = 0.312 <br>
                  hor<sub>HR</sub> = 8.73 h <br>
                  amp<sub>HR</sub> = 0.0918 <br>
                  hor<sub>TPR</sub> = 19.3 h <br>
                  amp<sub>TPR</sub> = 0.0918<br><br>
                  <a href='https://bpspubs.onlinelibrary.wiley.com/doi/full/10.1111/bph.12824'>[1] Snelder, N. et al. Br J Pharmacol (2014).</a>"
      }
    }else {
      info1 <- "<b>Beagle Dog</b><br><br>
      BSL_HR = 79.7 beats/min <br>
      BSL_MAP = 110 mmHg <br>
      BSL_CO = 1450 ml/min <br>
      k<sub>out_HR</sub> = 10 h<sup>-1</sup> <br>
      k<sub>out_SV</sub> = 10 h<sup>-1</sup> <br>
      k<sub>out_TPR</sub> = 10 h<sup>-1</sup> <br>
      FB = 0.0029 mmHg<sup>-1</sup> <br>
      HR_SV = 0.312 <br>
      hor<sub>HR</sub> = 8.73 h <br>
      amp<sub>HR</sub> = 0.0918 <br>
      hor<sub>TPR</sub> = 19.3 h <br>
      amp<sub>TPR</sub> = 0.0918<br><br>
      <a href='https://bpspubs.onlinelibrary.wiley.com/doi/full/10.1111/bph.12824'>[1] Snelder, N. et al. Br J Pharmacol (2014).</a><br>
      <a href='https://www.page-meeting.org/?abstract=5953#'>[2] Venkatasubramanian, R. et al. PAGE Poster (2016).</a>"
    }
    #--------------------------------- drug information -----------------------------
    info2 <- switch(input$drugname,
    "Amiloride" = "<b>Amiloride<br><br>
          <img src='Amiloride.png' width = '200'><br><br>
          PK model:</b><br> Two-compartmental model with liver compartment<br>
          ka = 0.086 h<sup>-1</sup> <br> klc = 0.491 h<sup>-1</sup> <br> kcl = 0.563 h<sup>-1</sup> <br> kcp = 0.290 h<sup>-1</sup> <br> 
          kpc = 0.017 h<sup>-1</sup> <br> kle = 7.069 h<sup>-1</sup> <br> kce = 0.042 h<sup>-1</sup> <br> V1 = 0.202 L/kg <br> F1 = 0.9887<br>
          <br><b> PD model: </b> <br> Diuretic with effect on SV <br>
          Emax model with Emax fixed to 1<br>
          EC50 = 245 ng/ml <br>",
    "Amlodipine" = "<b>Amlodipine <br> <br>
          <img src='Amlodipine.png' width = '200'><br><br>
          PK model:</b><br> One-compartmental model<br>
          ka = 0.4 h<sup>-1</sup> <br> V1 = 32 L/kg <br> k10 = 0.23 h<sup>-1</sup> <br>
          <br><b> PD model: </b> <br> Calcium channel blocker with effect on TPR <br>
          Emax model with Emax fixed to 1<br>
          EC50 = 82.8 ng/ml",
    "Atropine" = "<b>Atropine <br> <br>
          <img src='Atropine.png' width = '200'><br><br>
          PK model:</b><br> Two-compartmental model<br>
          V1 = 10.79 L/kg <br> k10 = 2.58 h<sup>-1</sup> <br> k12 = 9.24 h<sup>-1</sup><br> k21 = 4.92 h<sup>-1</sup><br>
          <br><b> PD model: </b> <br> M2 receptor antagonist with effect on HR <br>
          linear model <br>
          SL = 0.00149 (ng/ml)<sup>-1</sup>",
    "Enalapril" = "<b>Enalapril <br> <br> 
          <img src='Enalapril.png' width = '200'><br><br>
          PK model:</b> <br> Two-compartmental model with Michaelis-Menten elimination <br>
          VM = 767 ug/(ml*h)<br> V1 = 0.346 L/kg <br> k12 = 1.56 h<sup>-1</sup><br> k21 = 2.94 h<sup>-1</sup><br>
          KM = 150 ug/ml <br> ka = 1.75 h<sup>-1</sup> <br> F1 = 0.376 h<sup>-1</sup> <br>
          <br><b> PD model: </b> <br> Angiotensin-converting enzyme (ACE) inhibitor with effect on both TPR and SV <br>
          Emax model with Emax fixed to 1<br>
          EC50 = 1200 ng/ml",
    "Fasudil" = "<b>Fasudil<br><br>
          <img src='Fasudil.png' width = '200'><br><br>
          PK model:</b><br> One-compartmental model <br>
          V1 = 22.92 L/kg, k10 = 4.62 h<sup>-1</sup> <br>
          <br><b> PD model: </b> <br> Rho-kinase inhibitor with effect on TPR <br>
          Emax model with Emax fixed to 1<br>
          EC50 = 0.172 ng/ml",
    "HCTZ" = "<b>Hydrochlorothiazide(HCTZ)<br><br>
          <img src='HCTZ.png' width = '200'><br><br>
          PK model:</b><br> One-compartmental model <br>
          V1 = 0.0168 L/kg <br> k10 = 0.079 h<sup>-1</sup> <br> ka = 0.563 h<sup>-1</sup> <br> F1 = 0.007 <br>
          <br><b> PD model: </b><br> Diuretic with effect on SV <br>
          Emax model with Emax fixed to 1<br>
          EC50 = 28900 ng/ml",
    "Prazosin" = "<b>Prazosin<br><br> 
          <img src='Prazosin.png' width = '200'><br><br>
          PK model:</b><br> One-compartmental model <br>
          V1 = 39.2 L/kg <br> k10 = 0.694 h<sup>-1</sup> <br>
          <br><b> PD model: </b><br> Selective &alpha;<sub>1</sub> adrenergic receptor blocker with effect on TPR <br>
          Power model<br>
          SL = 0.328 (ng/ml)<sup>-1</sup> <br>
          POW = 0.091"
    )
 
    return(list(pk = pk, hr = hr, co = co, p = p, theta1 = theta1, theta2 = theta2,info1 = info1, info2 = info2))
  })
  
  #---------------------------- plot setting ----------------------------------
  
  axisset1 <- theme(axis.title.x=element_text(face='bold',size=15),
                   axis.title.y=element_text(face='bold',size=15),
                   axis.text.x=element_text(face='bold',size=10,color='black'),
                   axis.text.y=element_text(face='bold',size=10,color='black'))+
              theme(legend.justification=c(1,1), legend.position=c(0.9,0.9), legend.title=element_blank())
  
  #--------------------------------------- Plots -------------------------------

  output$PK <- renderPlot({
    r()$pk + axisset1
    })

  output$HR<- renderPlot({
    r()$hr + axisset1
  })
  
  output$CO <- renderPlot({
    r()$co + axisset1
  })

  
  output$MAP <- renderPlot({
    r()$p + axisset1
  })
  
  template <- data.frame(time = c(0:3,"..."),PK = c(1:4,"..."),HR = c(2:5,"..."),CO = c(3:6,"..."),MAP = c(4:7,"..."))
  
  observeEvent(input$info1, {
    showModal(modalDialog(
      title = span("Species Information",style = "font-weight:bold"),
      renderUI(HTML(r()$info1))
    ))
  })
  
  observeEvent(input$info2, {
    showModal(modalDialog(
      title = span("Drug Information",style = "font-weight:bold"),
      renderUI({HTML(r()$info2)})
    ))
  })
  
  observeEvent(input$template, {
    showModal(modalDialog(
      title = span("Please follow this template.",style = "font-weight:bold"),
      renderTable(template)
    ))})
  
  output$parameters1 <- renderTable({
    r()$theta1
  },include.rownames = TRUE, include.colnames = FALSE)
  
  output$parameters2 <- renderTable({
    r()$theta2
  },include.rownames = TRUE, include.colnames = FALSE)
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(theta1 = r()$theta1,
                     theta2 = r()$theta2,
                     pk = r()$pk,
                     hr = r()$hr,
                     co = r()$co,
                     p = r()$p)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

}
