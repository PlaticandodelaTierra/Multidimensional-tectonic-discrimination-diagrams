rm(list=ls())  #borra todas las variables que esten asignados
graphics.off() #cierra todas las graficas 
##### Diagramas de discriminaci?n tect?nica log ###
readline('Welcome ! lets generate some diagrams (please push enter to continue)')
Di=readline('Please type Verma_etal_2013_acid')
if(Di=='Verma_etal_2013_acid'){
  A=readline('Did you want the diagrams all together? Yes=(Y),No=(N)')
  if (substr(A,1,1)=='Y'){
    ##############1############  
    x11()
    par(mfrow=c(2,3))
    ##### Datos y funciones ##
    D=read.csv('Verma_etal_2013_acid.csv')
    ### ppm to percentage 
    La=(D[,14]*0.0001)/1 
    Yb=(D[,16]*0.0001)/1
    Sm=(D[,17]*0.0001)/1
    Th=(D[,18]*0.0001)/1
    Nb=(D[,19]*0.0001)/1
    Y=(D[,20]*0.0001)/1
    Zr=(D[,21]*0.0001)/1
    #### Relaciones logaritmicas 1 ##
    Ti_Si=log(D[,4]/D[,3])
    Al_Si=log(D[,5]/D[,3])
    Fe_Si=log(D[,6]/D[,3])
    Feo_Si=log(D[,7]/D[,3])
    Mn_Si=log(D[,8]/D[,3])
    Mg_Si=log(D[,9]/D[,3])
    Ca_Si=log(D[,10]/D[,3])
    Na_Si=log(D[,11]/D[,3])
    K_Si=log(D[,12]/D[,3])
    P_Si=log(D[,13]/D[,3])
    ###Funci?n IA+CA-CR+OI-COL ###
    DF1_AC1=(0.051*Ti_Si)+(0.226*Al_Si)+(-1.77*Fe_Si)+(1.83*Feo_Si)+(-0.065*Mn_Si)+(0.134*Mg_Si)+(0.225*Ca_Si)+(0.742*Na_Si)+(-1.78*K_Si)+(0.146*P_Si)-2.12
    DF2_AC1=(0.130*Ti_Si)+(0.62*Al_Si)+(-0.76*Fe_Si)+(-0.083*Feo_Si)+(-0.147*Mn_Si)+(-0.239*Mg_Si)+(-0.520*Ca_Si)+(2.04*Na_Si)+(-0.164*K_Si)+(0.146*P_Si)+2.65
    ###Funci?n IA-CA-CR+OI ###
    DF1_AC2=(0.130*Ti_Si)+(0.62*Fe_Si)+(-0.76*Feo_Si)+(-0.083*Mn_Si)+(-0.147*Mg_Si)+(-0.239*Ca_Si)+(-0.520*Na_Si)+(2.04*K_Si)+(-0.164*P_Si)+2.65
    DF2_AC2=(-0.045*Ti_Si)+(5.10*Fe_Si)+(-5.15*Feo_Si)+(1.16*Mn_Si)+(-0.253*Mg_Si)+(-0.451*Ca_Si)+(-2.45*Na_Si)+(-1.4*K_Si)+(0.0017*P_Si)-2.98
    ###Funci?n IA-CA-COL ###
    DF1_AC3=(-0.489*Ti_Si)+(2.27*Al_Si)+(0.62*Fe_Si)+(-1.24*Feo_Si)+(-0.91*Mn_Si)+(0.156*Mg_Si)+(-1.23*Na_Si)+(1.15*K_Si)+(0.409*P_Si)-3.22
    DF2_AC3=(0.68*Ti_Si)+(2.24*Al_Si)+(-3.90*Fe_Si)+(3.69*Feo_Si)+(-0.374*Mn_Si)+(0.255*Mg_Si)+(3.04*Na_Si)+(1.16*K_Si)+(-0.226*P_Si)+12.69
    ###Funci?n IA-CR+OI ###
    DF1_AC4=(-0.144*Ti_Si)+(-0.74*Al_Si)+(-0.443*Mn_Si)+(0.075*Ca_Si)+(0.383*Na_Si)+(2.58*K_Si)+(-0.0243*P_Si)+4.29
    DF2_AC4=(-0.87*Ti_Si)+(1.54*Al_Si)+(-0.75*Mn_Si)+(0.023*Ca_Si)+(0.150*Na_Si)+(-0.320*K_Si)+(0.75*P_Si)-2.60
    ###Funci?n CA-CR+OI-COL ###
    DF1_AC5=(-0.0218*Ti_Si)+(1.06*Al_Si)+(-1.65*Fe_Si)+(1.89*Feo_Si)+(-0.296*Mn_Si)+(0.119*Mg_Si)+(0.65*Na_Si)+(-2.43*K_Si)+(0.212*P_Si)-4.33
    DF2_AC5=(-1.08*Ti_Si)+(1.63*Al_Si)+(0.99*Fe_Si)+(-0.80*Feo_Si)+(-0.78*Mn_Si)+(-0.018*Mg_Si)+(-0.223*Na_Si)+(0.65*K_Si)+(0.73*P_Si)-0.92
    ###Funci?n IA+CA-CR+OI-COL ###
    x1=c(1.7,-0.002,6.2,-0.002,-8,-0.002)
    y1=c(8,-0.3,-8,-0.3,-1.8,-0.3)
    plot(DF1_AC1,DF2_AC1,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(4,2,'IA(94%)+CA(96.7%)')
    text(-5,2,'CR(73.9%)+OI(95.4%)')
    text(-5,-5,'Col(75.3%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-CR+OI ###
    x1=c(-8,-0.2,1,-0.2,3.9,-0.2)
    y1=c(-4.8,0.7,8,0.7,-8,0.7)
    plot(DF1_AC2,DF2_AC2,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-3,5,'IA(71.2%)')
    text(-3,-5,'CA(79.5%)')
    text(5,-4,'CR(83.2%)+OI(96.9%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    ###Funci?n IA-CA-COL ###
    x1=c(-8,0.3,0.8,0.3,4.5,0.3)
    y1=c(6.4,-1,-8,-1,8,-1)
    plot(DF1_AC3,DF2_AC3,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,2,'IA(69.1%)')
    text(-1,5,'CA(82.3%)')
    text(6,2,'Col(77.4%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    ###Funci?n IA-CR+OI ###
    x1=c(-2.6,-0.7,-3.7,-0.7,8,-0.7)
    y1=c(-8,0.5,8,0.5,-0.02,0.5)
    plot(DF1_AC4,DF2_AC4,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,3,'IA(85.5%)')
    text(4,-4,'CR(78.9%)+OI(96.9%)')
    text(4,4,'Col(83.1%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n CA-CR+OI-COL ###
    x1=c(1,0.4,8,0.4,-8,0.4)
    y1=c(-8,0.2,8,0.2,2.9,0.2)
    plot(DF1_AC5,DF2_AC5,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(6,0,'CA(88.6%)')
    text(-3,-3,'CR(73.2%)+OI(95.4%)')
    text(-3,5,'Col(74.1%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ##############2###########
    x11()
    par(mfrow=c(2,3))
    #### Relaciones logaritmicas 2 ##
    Mg_Ti=log(D[,9]/D[,4])
    P_Ti=log(D[,13]/D[,4])
    Nb_Ti=log(Nb/D[,4])
    Y_Ti=log(Y/D[,4])
    Zr_Ti=log(Zr/D[,4])
    ###Funci?n IA+CA-CR+OI-COL ###
    DF1_AC6=(-0.091*Mg_Ti)+(-0.228*P_Ti)+(0.73*Nb_Ti)+(-0.237*Y_Ti)+(0.58*Zr_Ti)+4.70
    DF2_AC6=(-0.268*Mg_Ti)+(-1.25*P_Ti)+(-0.476*Nb_Ti)+(0.209*Y_Ti)+(-0.082*Zr_Ti)-3.71
    ###Funci?n IA-CA-CR+OI ###
    DF1_AC7=(-0.018*Mg_Ti)+(-0.025*P_Ti)+(1.06*Nb_Ti)+(-0.53*Y_Ti)+(0.301*Zr_Ti)+4.70
    DF2_AC7=(-0.197*Mg_Ti)+(0.118*P_Ti)+(-0.72*Nb_Ti)+(1.10*Y_Ti)+(0.74*Zr_Ti)+3.70
    ###Funci?n IA-CA-COL ###
    DF1_AC8=(0.248*P_Ti)+(1.18*Nb_Ti)+(-0.86*Y_Ti)+(0.136*Zr_Ti)+3.99
    DF2_AC8=(1.13*P_Ti)+(0.382*Nb_Ti)+(1.13*Y_Ti)+(0.68*Zr_Ti)+7.28
    ###Funci?n IA-CR+OI-COL ###
    DF1_AC9=(0.095*Mg_Ti)+(-0.079*P_Ti)+(1.10*Nb_Ti)+(-0.90*Y_Ti)+(0.333*Zr_Ti)+2.77
    DF2_AC9=(-0.298*Mg_Ti)+(-1.00*P_Ti)+(-0.279*Nb_Ti)+(0.339*Y_Ti)+(0.171*Zr_Ti)-0.80
    ###Funci?n CA-CR+OI-COL ###
    DF1_AC10=(-0.081*Mg_Ti)+(-0.432*P_Ti)+(0.444*Nb_Ti)+(-0.131*Y_Ti)+(0.82*Zr_Ti)+3.73
    DF2_AC10=(-0.341*Mg_Ti)+(-1.11*P_Ti)+(-0.75*Nb_Ti)+(0.271*Y_Ti)+(-0.377*Zr_Ti)-5.42
    ###Funci?n IA+CA-CR+OI-COL ###
    x1=c(-0.07,0.03,-8,0.03,8,0.03)
    y1=c(8,-0.03,-6.1,-0.03,-5.8,-0.03)
    plot(DF1_AC6,DF2_AC6,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,1,'IA(94.3%)+CA(84.6%)')
    text(4,0,'CR(69.7%)+OI(97.5%)')
    text(-2,-5,'Col(76.4%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-CR+OI ###
    x1=c(-8,-0.3,0.6,-0.3,4.9,-0.3)
    y1=c(-6.1,0.8,8,0.8,-8,0.8)
    plot(DF1_AC7,DF2_AC7,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,1,'IA(76.7%)')
    text(-2,-5,'CA(84.6%)')
    text(5,0,'CR(75.2%)+OI(98.8%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-COL ###
    x1=c(-5.62,-0.18,-0.27,-0.18,7.33,-0.18)
    y1=c(-8,0.93,8,0.93,-8,0.93)
    plot(DF1_AC8,DF2_AC8,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,-2,'IA(77.4%)')
    text(1,-5,'CA(82.2%)')
    text(6,2,'Col(75.4%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CR+OI-Col ###
    x1=c(-1.8,-0.7,-4.8,-0.7,8,-0.7)
    y1=c(8,-0.07,-8,-0.07,-1.8,-0.07)
    plot(DF1_AC9,DF2_AC9,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,-3,'IA(85%)')
    text(5,4,'CR(72%)+OI(97.5%)')
    text(-1,-6,'Col(86.7%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n CA-CR+OI-COL ###
    x1=c(-8,-0.2,1.7,-0.2,5.7,-0.2)
    y1=c(-2.7,0.1,8,0.1,-8,0.1)
    plot(DF1_AC10,DF2_AC10,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,4,'CA(93%)')
    text(4,0,'CR(69.3%)+OI(97.5%)')
    text(1,-6,'Col(69.7%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ##############3#######
    x11()
    par(mfrow=c(2,3))
    #### Relaciones logaritmicas 3 ##
    La_Yb=log(D[,14]/D[,16])
    Ce_Yb=log(D[,15]/D[,16])
    Sm_Yb=log(D[,17]/D[,16])
    Nb_Yb=log(D[,19]/D[,16])
    Th_Yb=log(D[,18]/D[,16])
    Y_Yb=log(D[,20]/D[,16])
    Zr_Yb=log(D[,21]/D[,16])
    ###Funci?n IA+CA-CR+OI-COL ###
    DF1_AC11=(-4.99*La_Yb)+(7.81*Ce_Yb)+(-4.33*Sm_Yb)+(0.82*Nb_Yb)+(0.063*Th_Yb)+(0.64*Y_Yb)+(-0.57*Zr_Yb)-9.50
    DF2_AC11=(2.32*La_Yb)+(-3.62*Ce_Yb)+(2.62*Sm_Yb)+(0.25*Nb_Yb)+(0.84*Th_Yb)+(-1.14*Y_Yb)+(-1.27*Zr_Yb)+10.25
    ###Funci?n IA-CA-CR+OI ###
    DF1_AC12=(-5.21*La_Yb)+(6.62*Ce_Yb)+(-3.63*Sm_Yb)+(1.69*Nb_Yb)+(0.33*Th_Yb)+(1.56*Y_Yb)+(-0.49*Zr_Yb)-9.61
    DF2_AC12=(-3.72*La_Yb)+(4.79*Ce_Yb)+(-2.68*Sm_Yb)+(0.16*Nb_Yb)+(-0.50*Th_Yb)+(1.04*Y_Yb)+(-0.34*Zr_Yb)-4.93
    ###Funci?n IA-CA-COL ###
    DF1_AC13=(-0.047*La_Yb)+(1.08*Ce_Yb)+(-0.96*Sm_Yb)+(0.84*Nb_Yb)+(0.59*Th_Yb)+(-0.88*Zr_Yb)-0.73
    DF2_AC13=(-4.07*La_Yb)+(4.74*Ce_Yb)+(-0.077*Sm_Yb)+(-0.23*Nb_Yb)+(0.77*Th_Yb)+(-2.49*Zr_Yb)+5.10
    ###Funci?n IA-CR+OI-COL ###
    DF1_AC14=(0.26*La_Yb)+(1.05*Ce_Yb)+(-1.00*Sm_Yb)+(0.90*Nb_Yb)+(0.54*Th_Yb)+(0.089*Y_Yb)+(-0.62*Zr_Yb)-2.91
    DF2_AC14=(-5.36*La_Yb)+(8.41*Ce_Yb)+(-5.37*Sm_Yb)+(0.48*Nb_Yb)+(-0.41*Th_Yb)+(1.12*Y_Yb)+(0.37*Zr_Yb)-13.95
    ###Funci?n CA-CR+OI-COL ###
    DF1_AC15=(-5.41*La_Yb)+(8.44*Ce_Yb)+(-4.78*Sm_Yb)+(0.78*Nb_Yb)+(-0.079*Th_Yb)+(0.64*Y_Yb)+(-0.26*Zr_Yb)-11.34
    DF2_AC15=(1.68*La_Yb)+(-1.73*Ce_Yb)+(0.52*Sm_Yb)+(0.84*Nb_Yb)+(1.04*Th_Yb)+(-0.98*Y_Yb)+(-1.41*Zr_Yb )+6.09
    ###Funci?n IA+CA-CR+OI-COL ###
    x1=c(-0.1,0.5,-8,0.5,8,0.5)
    y1=c(-8,-0.4,6,-0.4,7.2,-0.4)
    plot(DF1_AC11,DF2_AC11,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,0,'IA(92.6%)+CA(92.6%)')
    text(4,0,'CR(94%)+OI(94)')
    text(0,5,'Col(92.6%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-CR+OI ###
    x1=c(-8,-0.01,1,-0.01,6,-0.01)
    y1=c(-5.4,1.2,8,1.2,-8,1.2)
    plot(DF1_AC12,DF2_AC12,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,2,'IA(86.2%)')
    text(0,-6,'CA(85.5%)')
    text(5,0,'CR(95.7%)+OI(95.7%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-COL ###
    x1=c(-3.6,-0.8,-0.7,-0.8,4.3,-0.8)
    y1=c(-8,2.9,8,2.9,-8,2.9)
    plot(DF1_AC13,DF2_AC13,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,-2,'IA(83.1%)')
    text(0,-5,'CA(71%)')
    text(5,0,'Col(84.8%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CR+OI ###
    x1=c(-5.7,-1.2,-1.8,-1.2,8,-1.2)
    y1=c(8,-0.3,-8,-0.3,3,-0.3)
    plot(DF1_AC14,DF2_AC14,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,3,'IA(91.3%)')
    text(1,5,'CR(97.4%)+OI(97.4%)')
    text(5,-2,'Col(88.2%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n CA-CR+OI-COL ###
    x1=c(1.4,0.4,-8,0.4,5.8,0.4)
    y1=c(-8,-0.3,3.1,-0.3,8,-0.3)
    plot(DF1_AC15,DF2_AC15,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,-4,'CA(74.5%)')
    text(4,-3,'CR(93.1%)+OI(93.1%)')
    text(-2,4,'Col(78.9%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }else if (A=='N'){
    ##### Datos y funciones ##
    D=read.csv('Verma_etal_2013_acid.csv')
    ### ppm to percentage 
    La=(D[,14]*0.0001)/1 
    Yb=(D[,16]*0.0001)/1
    Sm=(D[,17]*0.0001)/1
    Th=(D[,18]*0.0001)/1
    Nb=(D[,19]*0.0001)/1
    Y=(D[,20]*0.0001)/1
    Zr=(D[,21]*0.0001)/1
    #### Relaciones logaritmicas 1 ##
    Ti_Si=log(D[,4]/D[,3])
    Al_Si=log(D[,5]/D[,3])
    Fe_Si=log(D[,6]/D[,3])
    Feo_Si=log(D[,7]/D[,3])
    Mn_Si=log(D[,8]/D[,3])
    Mg_Si=log(D[,9]/D[,3])
    Ca_Si=log(D[,10]/D[,3])
    Na_Si=log(D[,11]/D[,3])
    K_Si=log(D[,12]/D[,3])
    P_Si=log(D[,13]/D[,3])
    ###Funci?n IA+CA-CR+OI-COL ###
    DF1_AC1=(0.051*Ti_Si)+(0.226*Al_Si)+(-1.77*Fe_Si)+(1.83*Feo_Si)+(-0.065*Mn_Si)+(0.134*Mg_Si)+(0.225*Ca_Si)+(0.742*Na_Si)+(-1.78*K_Si)+(0.146*P_Si)-2.12
    DF2_AC1=(0.130*Ti_Si)+(0.62*Al_Si)+(-0.76*Fe_Si)+(-0.083*Feo_Si)+(-0.147*Mn_Si)+(-0.239*Mg_Si)+(-0.520*Ca_Si)+(2.04*Na_Si)+(-0.164*K_Si)+(0.146*P_Si)+2.65
    ###Funci?n IA-CA-CR+OI ###
    DF1_AC2=(0.130*Ti_Si)+(0.62*Fe_Si)+(-0.76*Feo_Si)+(-0.083*Mn_Si)+(-0.147*Mg_Si)+(-0.239*Ca_Si)+(-0.520*Na_Si)+(2.04*K_Si)+(-0.164*P_Si)+2.65
    DF2_AC2=(-0.045*Ti_Si)+(5.10*Fe_Si)+(-5.15*Feo_Si)+(1.16*Mn_Si)+(-0.253*Mg_Si)+(-0.451*Ca_Si)+(-2.45*Na_Si)+(-1.4*K_Si)+(0.0017*P_Si)-2.98
    ###Funci?n IA-CA-COL ###
    DF1_AC3=(-0.489*Ti_Si)+(2.27*Al_Si)+(0.62*Fe_Si)+(-1.24*Feo_Si)+(-0.91*Mn_Si)+(0.156*Mg_Si)+(-1.23*Na_Si)+(1.15*K_Si)+(0.409*P_Si)-3.22
    DF2_AC3=(0.68*Ti_Si)+(2.24*Al_Si)+(-3.90*Fe_Si)+(3.69*Feo_Si)+(-0.374*Mn_Si)+(0.255*Mg_Si)+(3.04*Na_Si)+(1.16*K_Si)+(-0.226*P_Si)+12.69
    ###Funci?n IA-CR+OI ###
    DF1_AC4=(-0.144*Ti_Si)+(-0.74*Al_Si)+(-0.443*Mn_Si)+(0.075*Ca_Si)+(0.383*Na_Si)+(2.58*K_Si)+(-0.0243*P_Si)+4.29
    DF2_AC4=(-0.87*Ti_Si)+(1.54*Al_Si)+(-0.75*Mn_Si)+(0.023*Ca_Si)+(0.150*Na_Si)+(-0.320*K_Si)+(0.75*P_Si)-2.60
    ###Funci?n CA-CR+OI-COL ###
    DF1_AC5=(-0.0218*Ti_Si)+(1.06*Al_Si)+(-1.65*Fe_Si)+(1.89*Feo_Si)+(-0.296*Mn_Si)+(0.119*Mg_Si)+(0.65*Na_Si)+(-2.43*K_Si)+(0.212*P_Si)-4.33
    DF2_AC5=(-1.08*Ti_Si)+(1.63*Al_Si)+(0.99*Fe_Si)+(-0.80*Feo_Si)+(-0.78*Mn_Si)+(-0.018*Mg_Si)+(-0.223*Na_Si)+(0.65*K_Si)+(0.73*P_Si)-0.92
    ###Funci?n IA+CA-CR+OI-COL ###
    x1=c(1.7,-0.002,6.2,-0.002,-8,-0.002)
    y1=c(8,-0.3,-8,-0.3,-1.8,-0.3)
    plot(DF1_AC1,DF2_AC1,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(4,2,'IA(94%)+CA(96.7%)')
    text(-5,2,'CR(73.9%)+OI(95.4%)')
    text(-5,-5,'Col(75.3%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-CR+OI ###
    x1=c(-8,-0.2,1,-0.2,3.9,-0.2)
    y1=c(-4.8,0.7,8,0.7,-8,0.7)
    plot(DF1_AC2,DF2_AC2,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-3,5,'IA(71.2%)')
    text(-3,-5,'CA(79.5%)')
    text(5,-4,'CR(83.2%)+OI(96.9%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    ###Funci?n IA-CA-COL ###
    x1=c(-8,0.3,0.8,0.3,4.5,0.3)
    y1=c(6.4,-1,-8,-1,8,-1)
    plot(DF1_AC3,DF2_AC3,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,2,'IA(69.1%)')
    text(-1,5,'CA(82.3%)')
    text(6,2,'Col(77.4%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    ###Funci?n IA-CR+OI ###
    x1=c(-2.6,-0.7,-3.7,-0.7,8,-0.7)
    y1=c(-8,0.5,8,0.5,-0.02,0.5)
    plot(DF1_AC4,DF2_AC4,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,3,'IA(85.5%)')
    text(4,-4,'CR(78.9%)+OI(96.9%)')
    text(4,4,'Col(83.1%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n CA-CR+OI-COL ###
    x1=c(1,0.4,8,0.4,-8,0.4)
    y1=c(-8,0.2,8,0.2,2.9,0.2)
    plot(DF1_AC5,DF2_AC5,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(6,0,'CA(88.6%)')
    text(-3,-3,'CR(73.2%)+OI(95.4%)')
    text(-3,5,'Col(74.1%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ##############2###########
    #### Relaciones logaritmicas 2 ##
    Mg_Ti=log(D[,9]/D[,4])
    P_Ti=log(D[,13]/D[,4])
    Nb_Ti=log(Nb/D[,4])
    Y_Ti=log(Y/D[,4])
    Zr_Ti=log(Zr/D[,4])
    ###Funci?n IA+CA-CR+OI-COL ###
    DF1_AC6=(-0.091*Mg_Ti)+(-0.228*P_Ti)+(0.73*Nb_Ti)+(-0.237*Y_Ti)+(0.58*Zr_Ti)+4.70
    DF2_AC6=(-0.268*Mg_Ti)+(-1.25*P_Ti)+(-0.476*Nb_Ti)+(0.209*Y_Ti)+(-0.082*Zr_Ti)-3.71
    ###Funci?n IA-CA-CR+OI ###
    DF1_AC7=(-0.018*Mg_Ti)+(-0.025*P_Ti)+(1.06*Nb_Ti)+(-0.53*Y_Ti)+(0.301*Zr_Ti)+4.70
    DF2_AC7=(-0.197*Mg_Ti)+(0.118*P_Ti)+(-0.72*Nb_Ti)+(1.10*Y_Ti)+(0.74*Zr_Ti)+3.70
    ###Funci?n IA-CA-COL ###
    DF1_AC8=(0.248*P_Ti)+(1.18*Nb_Ti)+(-0.86*Y_Ti)+(0.136*Zr_Ti)+3.99
    DF2_AC8=(1.13*P_Ti)+(0.382*Nb_Ti)+(1.13*Y_Ti)+(0.68*Zr_Ti)+7.28
    ###Funci?n IA-CR+OI-COL ###
    DF1_AC9=(0.095*Mg_Ti)+(-0.079*P_Ti)+(1.10*Nb_Ti)+(-0.90*Y_Ti)+(0.333*Zr_Ti)+2.77
    DF2_AC9=(-0.298*Mg_Ti)+(-1.00*P_Ti)+(-0.279*Nb_Ti)+(0.339*Y_Ti)+(0.171*Zr_Ti)-0.80
    ###Funci?n CA-CR+OI-COL ###
    DF1_AC10=(-0.081*Mg_Ti)+(-0.432*P_Ti)+(0.444*Nb_Ti)+(-0.131*Y_Ti)+(0.82*Zr_Ti)+3.73
    DF2_AC10=(-0.341*Mg_Ti)+(-1.11*P_Ti)+(-0.75*Nb_Ti)+(0.271*Y_Ti)+(-0.377*Zr_Ti)-5.42
    ###Funci?n IA+CA-CR+OI-COL ###
    x1=c(-0.07,0.03,-8,0.03,8,0.03)
    y1=c(8,-0.03,-6.1,-0.03,-5.8,-0.03)
    plot(DF1_AC6,DF2_AC6,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,1,'IA(94.3%)+CA(84.6%)')
    text(4,0,'CR(69.7%)+OI(97.5%)')
    text(-2,-5,'Col(76.4%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-CR+OI ###
    x1=c(-8,-0.3,0.6,-0.3,4.9,-0.3)
    y1=c(-6.1,0.8,8,0.8,-8,0.8)
    plot(DF1_AC7,DF2_AC7,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,1,'IA(76.7%)')
    text(-2,-5,'CA(84.6%)')
    text(5,0,'CR(75.2%)+OI(98.8%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-COL ###
    x1=c(-5.62,-0.18,-0.27,-0.18,7.33,-0.18)
    y1=c(-8,0.93,8,0.93,-8,0.93)
    plot(DF1_AC8,DF2_AC8,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,-2,'IA(77.4%)')
    text(1,-5,'CA(82.2%)')
    text(6,2,'Col(75.4%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CR+OI-Col ###
    x1=c(-1.8,-0.7,-4.8,-0.7,8,-0.7)
    y1=c(8,-0.07,-8,-0.07,-1.8,-0.07)
    plot(DF1_AC9,DF2_AC9,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-6,-3,'IA(85%)')
    text(5,4,'CR(72%)+OI(97.5%)')
    text(-1,-6,'Col(86.7%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n CA-CR+OI-COL ###
    x1=c(-8,-0.2,1.7,-0.2,5.7,-0.2)
    y1=c(-2.7,0.1,8,0.1,-8,0.1)
    plot(DF1_AC10,DF2_AC10,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,4,'CA(93%)')
    text(4,0,'CR(69.3%)+OI(97.5%)')
    text(1,-6,'Col(69.7%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ##############3#######
    #### Relaciones logaritmicas 3 ##
    La_Yb=log(D[,14]/D[,16])
    Ce_Yb=log(D[,15]/D[,16])
    Sm_Yb=log(D[,17]/D[,16])
    Nb_Yb=log(D[,19]/D[,16])
    Th_Yb=log(D[,18]/D[,16])
    Y_Yb=log(D[,20]/D[,16])
    Zr_Yb=log(D[,21]/D[,16])
    ###Funci?n IA+CA-CR+OI-COL ###
    DF1_AC11=(-4.99*La_Yb)+(7.81*Ce_Yb)+(-4.33*Sm_Yb)+(0.82*Nb_Yb)+(0.063*Th_Yb)+(0.64*Y_Yb)+(-0.57*Zr_Yb)-9.50
    DF2_AC11=(2.32*La_Yb)+(-3.62*Ce_Yb)+(2.62*Sm_Yb)+(0.25*Nb_Yb)+(0.84*Th_Yb)+(-1.14*Y_Yb)+(-1.27*Zr_Yb)+10.25
    ###Funci?n IA-CA-CR+OI ###
    DF1_AC12=(-5.21*La_Yb)+(6.62*Ce_Yb)+(-3.63*Sm_Yb)+(1.69*Nb_Yb)+(0.33*Th_Yb)+(1.56*Y_Yb)+(-0.49*Zr_Yb)-9.61
    DF2_AC12=(-3.72*La_Yb)+(4.79*Ce_Yb)+(-2.68*Sm_Yb)+(0.16*Nb_Yb)+(-0.50*Th_Yb)+(1.04*Y_Yb)+(-0.34*Zr_Yb)-4.93
    ###Funci?n IA-CA-COL ###
    DF1_AC13=(-0.047*La_Yb)+(1.08*Ce_Yb)+(-0.96*Sm_Yb)+(0.84*Nb_Yb)+(0.59*Th_Yb)+(-0.88*Zr_Yb)-0.73
    DF2_AC13=(-4.07*La_Yb)+(4.74*Ce_Yb)+(-0.077*Sm_Yb)+(-0.23*Nb_Yb)+(0.77*Th_Yb)+(-2.49*Zr_Yb)+5.10
    ###Funci?n IA-CR+OI-COL ###
    DF1_AC14=(0.26*La_Yb)+(1.05*Ce_Yb)+(-1.00*Sm_Yb)+(0.90*Nb_Yb)+(0.54*Th_Yb)+(0.089*Y_Yb)+(-0.62*Zr_Yb)-2.91
    DF2_AC14=(-5.36*La_Yb)+(8.41*Ce_Yb)+(-5.37*Sm_Yb)+(0.48*Nb_Yb)+(-0.41*Th_Yb)+(1.12*Y_Yb)+(0.37*Zr_Yb)-13.95
    ###Funci?n CA-CR+OI-COL ###
    DF1_AC15=(-5.41*La_Yb)+(8.44*Ce_Yb)+(-4.78*Sm_Yb)+(0.78*Nb_Yb)+(-0.079*Th_Yb)+(0.64*Y_Yb)+(-0.26*Zr_Yb)-11.34
    DF2_AC15=(1.68*La_Yb)+(-1.73*Ce_Yb)+(0.52*Sm_Yb)+(0.84*Nb_Yb)+(1.04*Th_Yb)+(-0.98*Y_Yb)+(-1.41*Zr_Yb )+6.09
    ###Funci?n IA+CA-CR+OI-COL ###
    x1=c(-0.1,0.5,-8,0.5,8,0.5)
    y1=c(-8,-0.4,6,-0.4,7.2,-0.4)
    plot(DF1_AC11,DF2_AC11,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,0,'IA(92.6%)+CA(92.6%)')
    text(4,0,'CR(94%)+OI(94)')
    text(0,5,'Col(92.6%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-CR+OI ###
    x1=c(-8,-0.01,1,-0.01,6,-0.01)
    y1=c(-5.4,1.2,8,1.2,-8,1.2)
    plot(DF1_AC12,DF2_AC12,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,2,'IA(86.2%)')
    text(0,-6,'CA(85.5%)')
    text(5,0,'CR(95.7%)+OI(95.7%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CA-COL ###
    x1=c(-3.6,-0.8,-0.7,-0.8,4.3,-0.8)
    y1=c(-8,2.9,8,2.9,-8,2.9)
    plot(DF1_AC13,DF2_AC13,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,-2,'IA(83.1%)')
    text(0,-5,'CA(71%)')
    text(5,0,'Col(84.8%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n IA-CR+OI ###
    x1=c(-5.7,-1.2,-1.8,-1.2,8,-1.2)
    y1=c(8,-0.3,-8,-0.3,3,-0.3)
    plot(DF1_AC14,DF2_AC14,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,3,'IA(91.3%)')
    text(1,5,'CR(97.4%)+OI(97.4%)')
    text(5,-2,'Col(88.2%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ###Funci?n CA-CR+OI-COL ###
    x1=c(1.4,0.4,-8,0.4,5.8,0.4)
    y1=c(-8,-0.3,3.1,-0.3,8,-0.3)
    plot(DF1_AC15,DF2_AC15,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-4,-4,'CA(74.5%)')
    text(4,-3,'CR(93.1%)+OI(93.1%)')
    text(-2,4,'Col(78.9%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }
}
  B=readline('Did you want to generate a report with the DF and log ratios values? Yes=(Y),No=(N)')
  if (B=='Y'){
    X=D[,2]
    x=D[,1] ### Colocar la posici?n de la muestra
    fa=data.frame(D,X,Ti_Si,Al_Si,Fe_Si,Feo_Si,Mn_Si,Mg_Si,Ca_Si,Na_Si,K_Si,P_Si,X,Mg_Ti,P_Ti,Nb_Ti,Y_Ti,Zr_Ti,X,La_Yb,Ce_Yb,Sm_Yb,Nb_Yb,Th_Yb,Y_Yb,Zr_Yb,X,DF1_AC1,DF2_AC1,DF1_AC2,DF2_AC2,DF1_AC3,DF2_AC3,DF1_AC4,DF2_AC4,DF1_AC5,DF2_AC5,DF1_AC6,DF2_AC6,DF1_AC7,DF2_AC7,DF1_AC8,DF2_AC8,DF1_AC9,DF2_AC9,DF1_AC10,DF2_AC10,DF1_AC11,DF2_AC11,DF1_AC12,DF2_AC12,DF1_AC13,DF2_AC13,DF1_AC14,DF2_AC14,DF1_AC15,DF2_AC15)
    write.csv(fa,file='Report_Verma_2013_acid.csv',row.names=T)
  }else if (B=='N'){
    b=print('Thanks, I invite you to generate more diagrams at www.platicandodelatierra.com ')
  }