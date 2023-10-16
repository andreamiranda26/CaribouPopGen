#MakeFigs

setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/Output_local/finaldata") #setwd("C:/Users/Gina/Desktop/2022/ComplexModel_ABM") 
directory = getwd()
outdir = paste(directory, "/figs/", sep = "")

#NOTE: as of 7/5/23, we are using 84% confidence intervals to get an alpha = 0.05
#therefore, CIs will be between 8 and 92
#see https://academic.oup.com/jinsectscience/article/3/1/34/2577125 for more info

{
  #helpful plotting links
  #line types https://r-charts.com/base-r/line-types/
  #axes labels https://r-charts.com/base-r/axes/#axis-labels
  #pch https://r-charts.com/base-r/pch-symbols/
  #legend https://r-charts.com/base-r/legend/
  #making a figure panel https://r-charts.com/base-r/combining-plots/
  #size of png https://stackoverflow.com/questions/8399100/r-plot-size-and-resolution
  #moving axis label closer to axis https://stackoverflow.com/questions/30265728/in-r-base-plot-move-axis-label-closer-to-axis
}

#1=yr, 2=pop size, 3=propmig, 4=He, 5=Ho[driftSNPS], 6=fis, 7=nadult, 8=sxratio, 9=nmig, 10=fst, 11=replicate, 12=paramset, 13=noffspring, 14=fstvsource, 15=fisvsource,
#16=deltaK, 17=propMigSNPs, 18=HoallSNPs, 19=projectname, 20=groupnumb, 21=k, 22=nSNP, 23=miggy, 24=LBhet, 25=LBp, 26=maxage, 27=broodsize, 28=maturity,
#29=years, 30=r0, 31=nSNP.mig, 32=nSNP.cons

{
  #  A = 1 mig per gen
  #  b= 100 @ y=151
  #  c= 25 @y=151,165,181,195
  #  d= 1 mig wheny >= edyr + dur+1
  #  e= 100 @ y=125
  #  f= 25 @ y=125, 140, 155, 170
}

#make panel order by row, with 2 rows and 2 columns
par(mfrow = c(2,2))

par(mar = c(5, 5, 4, 4)+.25)
plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     cex.axis = text.size, cex.lab = text.size)
lines(c(2,4,5,6,7, 8), c(1.3, 1.5, 1.35, 1.52, 1.65,1.7), lwd = 7)
title(ylab = 'Genetic Diversity', line = 1, cex.lab = text.size*3) #1.5
title(xlab = 'Time', line = 2.5, cex.lab = text.size*3)
par(new=TRUE) #merge two plots
lines(c(2,4,5,6,8), c(1.1, 1.15, 1.5, 1.6,1.8), lwd = 7, col = "blue")
axis(4, yaxt = "n")
mtext('Population Divergence', side = 4, line = 2.5, cex = text.size*2.87, col = "blue") #1.2

{
  plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6, 8), c(1.1, 1.2, 1.6, 1.65, 1.7), lwd = 5)
  title(ylab = 'Proportion Migrant \n Ancestry (%)', line = 1, cex.lab = text.size*1.5)
  title(xlab = 'Time', line = 1.5, cex.lab = text.size*2)
  
  plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6,8), c(1.1, 1.15, 1.5, 1.6,1.8), lwd = 5)
  title(ylab = 'Original Population \n Divergence (Fst)', line = 1, cex.lab = text.size*1.5)
  title(xlab = 'Time', line = 1.5, cex.lab = text.size*2)
  
  plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6, 8), c(1.8, 1.7, 1.4, 1.35, 1.3), lwd = 5)
  title(ylab = 'Migrant Source Population \n Divergence (Fst)', line = 1, cex.lab = text.size*1.5)
  title(xlab = 'Time', line = 1.5, cex.lab = text.size*2)
}

par(mfrow = c(1,1)) #go back to default where only one fig per panel

{plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = 'Time', ylab = 'Proportion Migrant \n Ancestry (%)',
      cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6, 8), c(1.1, 1.2, 1.6, 1.65, 1.7), lwd = 5)
  
  plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = 'Time', ylab = 'Original Population \n Divergence (Fst)',
       cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6,8), c(1.1, 1.15, 1.5, 1.6,1.8), lwd = 5)
  
  plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = 'Time', ylab = 'Migrant Source Population \n Divergence (Fst)',
       cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6, 8), c(1.8, 1.7, 1.4, 1.35, 1.3), lwd = 5)
}

par(mfrow = c(1,1)) #go back to default where only one fig per panel


library(colorspace)
library(scales)
library(FSA)
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold") #"springgreen3"

#plot specs
ln.alph <- 0.5
pt.alph <- 1.25
diff <- 0.15
xmin <- 0
xmax <- 350
offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
text.size <- 1.75
pt.cex <- 1.25
lwd <- 4

#poly specs
dens = NULL #c(100, 100, 100, 100, NULL, NA, NULL, NA)
ang = 45
bo = NA #c(NULL, NULL, NULL, NULL, NA, NA, NA, NA) #alpha(gt.cols[col], .8)
alf = c(.7, .7, .7, .7, .7, .7, .7, .7) #.7, .7, .7, .7
lty = c(3,3,3,3,1,1,1,1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Figure 2
{
  p70 = read.table("fin_5.11.23_1LL70_all_summary.csv", header=T, sep=",")
  p30 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
  p10 = read.table("comb_fin_5.11.23_1LL10_all_summary.csv", header=T, sep=",")
  b0 = read.table("fin_6.1.23_nbtl30_all_summary.csv", header=T, sep=",")
  smry = rbind(b0,p10, p30, p70) #p50, 
  
  #make panel order by row, with 2 rows and 2 columns
  par(mfrow = c(2,2))
  
  #1A - population size ~ year, by = IUCN status
  #will want to get total pop size, therefore number adults and babies and numboff
  plot(-20, -20 , xlab="Year", ylab="Population Size", xlim=c(0, 350), ylim=c(0, 1000),
       cex.axis = text.size, cex.lab = text.size) 
  title("A", adj = 0, cex.main = text.size, line = 2)
  col=1
  for(z in unique(smry[,19])){
    zsmry = smry[smry[,19]==z,,drop=FALSE]
    for(b in 1:length(unique(zsmry[,20]))){
      bsmry = zsmry[zsmry[,20]==b,,drop=FALSE]
      for(p in 1:length(unique(smry[,11]))){
        psmry = bsmry[bsmry[,11]==p,,drop=FALSE]
        lines(psmry[,1], psmry[,2], col=gt.cols[col], pch=16)
      }
    }
    col=col+1
  }
  #legend('bottomright', legend = c('critically endangered', 'endangered','vulnerable'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  #legend('bottomright', legend = c('LC', 'CR','EN', 'VU'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  
  #1B - delta H ~ year, by = IUCN status [plus no crash]
  #will want CR at bottom 
  {
    var = 5
    varname = "Heterozygosity"
    title = "Fig 1B"
    range(smry[,var])
    
    ymin <- 0.1 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.3 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("B", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
        
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #1C - fst ~ year, by = IUCN status [plus no crash]
  {
    var = 10
    varname = "Original Population \n Divergence (Fst)"
    title = "Fig 1C"
    range(smry[,var])
    
    ymin <- round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.3 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("C", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #1D - fst vs source ~ year, by IUCN staus [plus no crash]
  {
    var = 14
    varname = "Migrant Source Population \n Divergence (Fst)"
    title = "Fig 1C"
    range(smry[,var])
    
    ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.5 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("D", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 3
{
  #2A - prop mig SNPs (ancestry) ~ year, by = IUCN status
  LL0 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
  LLa = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",") 
  HL0 = read.table("fin_5.23.23_1HL30_all_summary.csv", header=T, sep=",")
  HLa = read.table("fin_5.23.23_1HL3a_all_summary.csv", header=T, sep=",")
  HH0a = read.table("comb_fin_5.10.23_2HH30a_all_summary.csv", header=T, sep=",")
  #HH0a >> manually combined fin_5.10.23_2HH30a_all_summary.csv and cpfin_5.10.23_2HH30a_all_summary.csv
  HH0 = HH0a[HH0a[,23]==0,,drop=FALSE]
  HH0[,19] = "HH0"
  HHa = HH0a[HH0a[,23]!=0,,drop=FALSE]
  HHa[,19] = "HHa"
  
  smry = rbind(LLa, HLa, HHa)
  gt.cols =  c("cyan3", "maroon2", "purple")
  
  {
    var = 17
    varname = "Proportion Migrant\n Ancestry (%)"
    title = "Fig 2A"
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 1 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("A", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = 1)
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('topleft', legend = c('LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #2B - Ho ~ year, by = high and low allele freq, low = grey, high = black, mig = solid, non-mig = dotted
  smry = rbind(LL0, HH0, LLa, HLa, HHa)
  gt.cols =  c("cyan3", "purple", "cyan3", "maroon2", "purple")
  
  dens = NULL #c(100, 100, 100, 100, NULL, NA, NULL, NA)
  ang = 45
  bo = NA #c(NULL, NULL, NULL, NULL, NA, NA, NA, NA) #alpha(gt.cols[col], .8)
  alf = c(.7, .7, .7, .7, .7, .7, .7, .7) #.7, .7, .7, .7
  lty <- c(3,3,1,1,1)
  
  {
    var = 5
    varname = "Heterozygosity"
    title = "Fig 2B"
    range(smry[,var])
    
    ymin <- .1 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- .5 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("B", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('left', legend = c('LL0', 'HL0', 'HH0', 'LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #2C - Fst vs orig ~ year, by = starting allele freqs
  {
    var = 10
    varname = "Original Population \n Divergence (Fst)"
    title = "Fig 2C"
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.4 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("C", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('topleft', legend = c('LL0', 'HL0', 'HH0', 'LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #2D - Fst vs source ~ year, by = starting allele freqs
  {
    var = 14
    varname = "Migrant Source Population \n Divergence (Fst)"
    title = "Fig 2D"
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- .5 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("D", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('LL0', 'HL0', 'HH0', 'LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 4
{
  #3A - Ho ~ year, by = IUCN, mig and no mig
  p70 = read.table("fin_5.11.23_1LL70_all_summary.csv", header=T, sep=",")
  p30 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
  p10 = read.table("comb_fin_5.11.23_1LL10_all_summary.csv", header=T, sep=",")
  b0 = read.table("fin_6.1.23_nbtl30_all_summary.csv", header=T, sep=",")
  p7a = read.table("fin_5.11.23_1LL7a_all_summary.csv", header=T, sep=",")
  p3a = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",")  
  p1a = read.table("comb_fin_5.11.23_1LL1a_all_summary.csv", header=T, sep=",") 
  ba = read.table("fin_6.1.23_nbtl3a_all_summary.csv", header=T, sep=",")
  
  #3A - ancestry ~ year, by = IUCN
  smry = rbind(ba, p1a, p3a, p7a) #p50, 
  {
    var = 17
    varname = "Proportion Migrant\n Ancestry (%)"
    title = "Fig 3A"
    
    ymin <- 0
    ymax <- 1
    
    par(mar = c(4,6,4,4))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n', pin=c(1,1))
    title("A", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #r=1
    #for(z in unique(smry[,19])){
    #  zsmry = smry[smry[,19]==z,,drop=FALSE]
    #  for(b in 1:length(unique(zsmry[,20]))){
    #    bsmry = zsmry[zsmry[,20]==b,,drop=FALSE]
    #    for(p in 1:length(unique(smry[,11]))){
    #      psmry = bsmry[bsmry[,11]==p,,drop=FALSE]
    #      lines(psmry[,1], psmry[,var], col=gt.cols[r], pch=16)
    #    }
    #  }
    #  r=r+1
    #}
    #legend('bottomright', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #3B - H ~ time, IUCN stat
  smry = rbind(b0, p10, p30, p70, ba, p1a, p3a, p7a) #p50, 
  gt.cols <- c("grey", "firebrick3", "darkorange1", "gold", "grey", "firebrick3", "darkorange1", "gold") # "springgreen3"
  {
    var = 5
    varname = "Heterozygosity"
    title = "Fig 3B"
    range(smry[,var])
    
    ymin <- .1 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- .3 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty <- c(3,3,3,3, 1,1,1,1)
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("B", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #3C - orig fst ~ year, by = IUCN
  {
    var = 10
    varname = "Original Population \n Divergence (Fst)"
    title = "Fig 3C"
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.4 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty <- c(3,3,3,3, 1,1,1,1)
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("C", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #3D - source fst ~ year, by = IUCN
  {
    var = 14
    varname = "Migrant Source Population \n Divergence (Fst)"
    title = "Fig 3D"
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.5 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty <- c(3,3,3,3, 1,1,1,1)
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("D", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 5
{
  par(mfcol = c(4,4))
  
  #4A, 4D, 4G, 4J
  p1a = read.table("comb_fin_5.11.23_1LL1a_all_summary.csv", header=T, sep=",") 
  p10 = read.table("comb_fin_5.11.23_1LL10_all_summary.csv", header=T, sep=",")
  p1b = read.table("comb_fin_6.1.23_LL1b_all_summary.csv", header=T, sep=",")  #has 14 pops that crashed!!!
  p1b[,19] = "p1b"
  p1c = read.table("fin_6.1.23_LL1c_all_summary.csv", header=T, sep=",")  #has 3 pops that crashed!!!
  smry = rbind(p10, p1a, p1b, p1c)
  #gt.cols = c("hotpink","orchid3", "springgreen", "blue") #c("black", "chartreuse3", "chocolate3", "goldenrod3")
  
  gt.cols = "firebrick3"
  lty = c(3, 1, 2, 4)
  pch = c(18, 19, 15, 17)
  
  #A
  var = 5
  varname = "Heterozygosity"
  title = "A"
  ymin <- .1
  ymax <- .3
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #E
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "E"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #I
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "I"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #M
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "M"
  ymin <- 0
  ymax <- .5
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  p30 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
  p3a = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",")  
  p3b = read.table("fin_5.10.23_1LL3b_all_summary.csv", header=T, sep=",")
  p3c = read.table("fin_5.10.23_1LL3c_all_summary.csv", header=T, sep=",")
  smry = rbind(p30, p3a, p3b, p3c)
  
  
  gt.cols = "darkorange1"
  lty = c(3, 1, 2, 4)
  pch = c(18, 19, 15, 17)
  
  #B
  var = 5
  varname = "Heterozygosity"
  title = "B"
  ymin <- .1
  ymax <- .3
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #F
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "F"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #J
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "J"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #N
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "N"
  ymin <- 0
  ymax <- .5
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  p7a = read.table("fin_5.11.23_1LL7a_all_summary.csv", header=T, sep=",")
  p70 = read.table("fin_5.11.23_1LL70_all_summary.csv", header=T, sep=",")
  p7b = read.table("fin_6.1.23_LL7b_all_summary.csv", header=T, sep=",")
  p7c = read.table("fin_6.1.23_LL7c_all_summary.csv", header=T, sep=",")
  smry = rbind(p70, p7a, p7b, p7c)
  
  gt.cols = c("goldenrod1")
  lty = c(3, 1, 2, 4)
  pch = c(18, 19, 15, 17)
  
  #C
  var = 5
  varname = "Heterozygosity"
  title = "C"
  ymin <- .1
  ymax <- .3
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #G
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "G"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #K
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "K"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #O
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "O"
  ymin <- 0
  ymax <- .5
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  b0 = read.table("fin_6.1.23_nbtl30_all_summary.csv", header=T, sep=",")
  ba = read.table("fin_6.1.23_nbtl3a_all_summary.csv", header=T, sep=",")
  smry = rbind(b0, ba)
  
  gt.cols = "grey"
  lty = c(3, 1)
  pch = c(18, 19)
  
  #first
  var = 5
  varname = "Heterozygosity"
  title = "D"
  ymin <- .1
  ymax <- .3
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #second
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "H"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #third
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "L"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #fourth
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "P"
  ymin <- 0
  ymax <- .5
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 6
{
  #5A - timing of burst mig rates (B&E) ~ year 
  par(mfcol = c(4,2))
  
  p3b = read.table("fin_5.10.23_1LL3b_all_summary.csv", header=T, sep=",")
  p3e = read.table("cpfin_5.10.23_1LL3e_all_summary.csv", header=T, sep=",")
  smry = rbind(p3b, p3e)
  gt.cols <- c("black", "grey")
  lty = c(1,1)
  
  #A
  var = 5
  varname = "Heterozygosity"
  title = "A"
  ymin <- .1
  ymax <- .3
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #C
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "C"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #E
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "E"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #G
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "G"
  ymin <- 0
  ymax <- .5
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  p3c = read.table("fin_5.10.23_1LL3c_all_summary.csv", header=T, sep=",")
  p3f = read.table("fin_5.10.23_1LL3f_all_summary.csv", header=T, sep=",")
  smry = rbind(p3c, p3f)
  
  #B
  var = 5
  varname = "Heterozygosity"
  title = "B"
  ymin <- .1
  ymax <- .3
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #D
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "D"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #F
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "F"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #H
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "H"
  ymin <- 0
  ymax <- .5
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###SUPPLEMENTAL FIGURES
#Supp Figure 1
#data for when migrants arent preferentially mated ~ migration rate
{
  p30. = read.table("fin_6.1.23_1LL30._all_summary.csv", header=T, sep=",")
  p3a. = read.table("fin_6.1.23_1LL3a._all_summary.csv", header=T, sep=",")  
  p3b. = read.table("fin_6.1.23_1LL3b._all_summary.csv", header=T, sep=",")
  p3c. = read.table("fin_6.1.23_1LL3c._all_summary.csv", header=T, sep=",")
  smry = rbind(p30., p3a., p3b., p3c.)
  
  gt.cols = "darkorange1"
  lty = c(3, 1, 2, 4)
  pch = c(18, 19, 15, 17)
  
  par(mfrow = c(2,2))
  
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "A"
  ymin <- 0
  ymax <- 1
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  var = 5
  varname = "Heterozygosity"
  title = "B"
  ymin <- .1
  ymax <- .3
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "C"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "D"
  ymin <- 0
  ymax <- .5
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}

#Supp Figure 2
#data for the proportion of populations that crashed
{
  p1a = read.table("comb_fin_5.11.23_1LL1a_all_summary.csv", header=T, sep=",") 
  p10 = read.table("comb_fin_5.11.23_1LL10_all_summary.csv", header=T, sep=",")
  p1b = read.table("comb_fin_6.1.23_LL1b_all_summary.csv", header=T, sep=",")  #has 14 pops that crashed!!!
  p1b[,19] = "p1b"
  p1c = read.table("fin_6.1.23_LL1c_all_summary.csv", header=T, sep=",")  #has 3 pops that crashed!!!
  smry = rbind(p10, p1a, p1b, p1c)
  
  par(mfrow = c(1,1)) #go back to default where only one fig per panel
  
  gt.cols = "firebrick3"
  lty = c(3, 1, 2, 4)
  pch = c(18, 19, 15, 17)
  alf = c(1, .8, .6, .4) 
  
  A = table(p10[,1])
  A = as.numeric(A)
  
  B = table(p1a[,1])
  B = as.numeric(B)
  
  C = table(p1b[,1])
  C = as.numeric(C)
  
  D = table(p1c[,1])
  D = as.numeric(D)
  
  var = rbind(A, B, C, D)
  varname = "Proportion of Viable\n Populations (%)"
  title = ""
  ymin <- 0
  ymax <- 1
  
  {
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(0, 350), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(0, .2, .4, .6, .8, 1), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = 151, lty = 2, col = "grey")
    
    col <- 1
    for(m in 1:nrow(var)){
      plt.y = var[m,]/100  #turn into a proportion
      plt.x = c(0:350)
      lines(plt.x, plt.y, col = alpha(gt.cols, alf[col]), lwd = lwd, lty = lty[col])
      col = col+1
    }
    
    legend('bottom', legend = c('no mig', '1 mig/gen', "burst", "pulse"), col = alpha(gt.cols, alf), lty = lty, lwd = lwd, bty = 'n', cex = text.size, text.width = text.size, ncol = 2) #pt.cex = text.size
  }
}

#Supp Figure 3
#Fis, sex ratio, LRS?

p70 = read.table("fin_5.11.23_1LL70_all_summary.csv", header=T, sep=",")
p30 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
p10 = read.table("comb_fin_5.11.23_1LL10_all_summary.csv", header=T, sep=",")
b0 = read.table("fin_6.1.23_nbtl30_all_summary.csv", header=T, sep=",")
p7a = read.table("fin_5.11.23_1LL7a_all_summary.csv", header=T, sep=",")
p3a = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",")  
p1a = read.table("comb_fin_5.11.23_1LL1a_all_summary.csv", header=T, sep=",") 
ba = read.table("fin_6.1.23_nbtl3a_all_summary.csv", header=T, sep=",")

par(mfrow = c(2,2)) #lty = c(3, 1, 2, 4)
#Fis
{
  smry = rbind(b0, p10, p30, p70)
  gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
  lty = c(3,3,3,3)
  {
    var = 6
    varname = "Inbreeding (Fis)"
    title = ""
    range(smry[,var])
    
    ymin <- -.005 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- .005 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("A", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0, lty = 2, col = "black")
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  
  smry = rbind(ba, p1a, p3a, p7a)
  gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
  lty  = c(1,1,1,1)
  {
    var = 6
    varname = "Inbreeding (Fis)"
    title = ""
    range(smry[,var])
    
    ymin <- -.005 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- .005 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("B", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0, lty = 2, col="black") #alpha("black", 0.3)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  smry = rbind(p1b, p3b, p7b)
  gt.cols <- c("firebrick3", "darkorange1", "gold")
  lty = c(2, 2, 2)
  {
    var = 6
    varname = "Inbreeding (Fis)"
    title = ""
    range(smry[,var])
    
    ymin <- -.011 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- .04 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("C", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0, lty = 2, col = "black")
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  
  smry = rbind(p1c, p3c, p7c)
  gt.cols <- c("firebrick3", "darkorange1", "gold")
  lty  = c(4, 4, 4)
  {
    var = 6
    varname = "Inbreeding (Fis)"
    title = ""
    range(smry[,var])
    
    ymin <- -.005 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- .02 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("D", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0, lty = 2, col="black") #alpha("black", 0.3)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}

###Supp Figure 4
#Sex Ratio
{
  smry = rbind(b0, p10, p30, p70)
  gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
  lty = c(3,3,3,3)
  {
    var = 8
    varname = "Sex Ratio"
    title = ""
    range(smry[,var])
    
    ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("A", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0.5, lty = 2, col = "black")
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  smry = rbind(ba, p1a, p3a, p7a)
  gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
  lty  = c(1,1,1,1)
  {
    var = 8
    varname = "Sex Ratio"
    title = ""
    range(smry[,var])
    
    ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("B", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0.5, lty = 2, col="black") #alpha("black", 0.3)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  smry = rbind(p1b, p3b, p7b)
  gt.cols <- c("firebrick3", "darkorange1", "gold")
  lty = c(2, 2, 2)
  {
    var = 8
    varname = "Sex Ratio"
    title = ""
    range(smry[,var])
    
    ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("C", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0.5, lty = 2, col = "black")
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  smry = rbind(p1c, p3c, p7c)
  gt.cols <- c("firebrick3", "darkorange1", "gold")
  lty  = c(4, 4, 4)
  {
    var = 8
    varname = "Sex Ratio"
    title = ""
    range(smry[,var])
    
    ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
    
    par(mar = c(4,6,4,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("D", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    abline(v = c(100,151), lty = 2, col = "grey")
    abline(h = 0.5, lty = 2, col="black") #alpha("black", 0.3)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      #y1<-temp[temp[,1] == orig.xs[1],,]
      #y2<-temp[temp[,1] == orig.xs[2],,]
      #y3<-temp[temp[,1] == orig.xs[3],,]
      #y4<-temp[temp[,1] == orig.xs[4],,]
      #y5<-temp[temp[,1] == orig.xs[5],,]
      #y6<-temp[temp[,1] == orig.xs[6],,]
      #y7<-temp[temp[,1] == orig.xs[7],,]
      #y8<-temp[temp[,1] == orig.xs[8],,]
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        tbl[(d+1),5] = as.character(dat[1,19])
      }
      #TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
      #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
      #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      
      #print(mean(y1[,var]))
      #print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}

###Supp Figure 5
#LRS
{
  r30 = read.table("fin_5.10.23_1LL30_all_repsuc.csv", header=T, sep=",")
  r3a = read.table("fin_5.10.23_1LL3a_all_repsuc.csv", header=T, sep=",")
  r3b = read.table("fin_5.10.23_1LL3b_all_repsuc.csv", header=T, sep=",")
  r3c = read.table("fin_5.10.23_1LL3c_all_repsuc.csv", header=T, sep=",")
  
  r10 = read.table("fin_5.11.23_1LL10_all_repsuc.csv", header=T, sep=",")
  r1a = read.table("comb_fin_5.11.23_1LL1a_all_repsuc.csv", header=T, sep=",")
  r70 = read.table("fin_5.11.23_1LL70_all_repsuc.csv", header=T, sep=",")
  r7a = read.table("fin_5.11.23_1LL7a_all_repsuc.csv", header=T, sep=",")
  
  r1b = read.table("comb_fin_6.1.23_LL1b_all_repsuc.csv", header=T, sep=",")  #has 14 pops that crashed!!!
  r1b[,13] = "r1b"
  r1c = read.table("fin_6.1.23_LL1c_all_repsuc.csv", header=T, sep=",")  #has 3 pops that crashed!!!
  r7b = read.table("fin_6.1.23_LL7b_all_repsuc.csv", header=T, sep=",")
  r7c = read.table("fin_6.1.23_LL7c_all_repsuc.csv", header=T, sep=",")
  
  ra = read.table("fin_6.1.23_nbtl3a_all_repsuc.csv", header=T, sep=",")
  r0 = read.table("fin_6.1.23_nbtl30_all_repsuc.csv", header=T, sep=",")
  
  smry = rbind(r0, r10, r30, r70)
  gt.cols = c("grey","firebrick3", "darkorange1", "gold")
  #smry_new = matrix(nrow = 1, ncol = ncol(smry))
  #colnames(smry_new) <- colnames(smry)
  #for(u in 1:nrow(smry)){
  #  if(is.na(smry[u,3])==FALSE){
  #    rbind(smry_new,smry[u,])
  #  }
  #}
  
  smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
  smry = smry_new
  
  
  dens = NULL #c(100, 100, 100, 100, NULL, NA, NULL, NA)
  ang = 45
  bo = NA #c(NULL, NULL, NULL, NULL, NA, NA, NA, NA) #alpha(gt.cols[col], .8)
  alf = c(.7, .7, .7, .7, .7, .7, .7, .7) #.7, .7, .7, .7
  
  {
    var = 3
    varname = "Lifetime Reproductive \n Success"
    title = ""
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 3 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty = c(3,3,3,3, 1,1,1,1)
    pch = c(18, 19, 15, 17, 18, 19, 15, 17)
    
    par(mar = c(4,6,4,4))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', main = title, xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("A", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    TBL = NULL
    col <- 1
    for(c in unique(smry[,13])){
      print(c)
      temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        #tbl[(d+1),5] = as.character(dat[1,19])
        
        
      }
      TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  
  smry = rbind(ra, r1a, r3a, r7a)
  gt.cols = c("grey","firebrick3", "darkorange1", "gold")
  smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
  smry = smry_new
  
  {
    var = 3
    varname = "Lifetime Reproductive \n Success"
    title = ""
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 3 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty = c(3,3,3,3, 1,1,1,1)
    pch = c(18, 19, 15, 17, 18, 19, 15, 17)
    
    par(mar = c(4,6,4,4))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', main = title, xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("B", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    TBL = NULL
    col <- 1
    for(c in unique(smry[,13])){
      print(c)
      temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        #tbl[(d+1),5] = as.character(dat[1,19])
        
        
      }
      TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  
  smry = rbind(r1b, r3b, r7b)
  gt.cols = c("firebrick3", "darkorange1", "gold")
  smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
  smry = smry_new
  {
    var = 3
    varname = "Lifetime Reproductive \n Success"
    title = ""
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 3 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty = c(3,3,3,3, 1,1,1,1)
    pch = c(18, 19, 15, 17, 18, 19, 15, 17)
    
    par(mar = c(4,6,4,4))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', main = title, xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("C", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    TBL = NULL
    col <- 1
    for(c in unique(smry[,13])){
      print(c)
      temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        #tbl[(d+1),5] = as.character(dat[1,19])
        
        
      }
      TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  smry = rbind(r1c, r3c, r7c)
  gt.cols = c("firebrick3", "darkorange1", "gold")
  smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
  smry = smry_new
  {
    var = 3
    varname = "Lifetime Reproductive \n Success"
    title = ""
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 3 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty = c(3,3,3,3, 1,1,1,1)
    pch = c(18, 19, 15, 17, 18, 19, 15, 17)
    
    par(mar = c(4,6,4,4))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', main = title, xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    title("D", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    TBL = NULL
    col <- 1
    for(c in unique(smry[,13])){
      print(c)
      temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        #tbl[(d+1),5] = as.character(dat[1,19])
        
        
      }
      TBL = rbind(TBL, tbl)
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}

###################################################################################################################
###################################################################################################################
###################################################################################################################

#working polygons:
dens = c(100, 100, 100, 100, NULL, NA, NULL, NA)
ang = 45
bo = c(NULL, NULL, NULL, NULL, NA, NA, NA, NA)
{
  var = 10
  varname = "Original Population \n Divergence (Fst)"
  title = "Fig 1C"
  range(smry[,var])
  
  ymin <- round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.5 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty = c(3,3,3,3, 1,1,1,1)
  pch = c(18, 19, 15, 17, 18, 19, 15, 17)
  
  par(mar = c(4,6,4,4))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', main = title, xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.cex)
    btm = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08)))
    tp = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92))
    polygon(x = c(xs,rev(xs)) , y = c(btm,rev(tp)), density = dens[col], angle = ang, border = alpha(gt.cols[col], .8),
            col = adjustcolor(alpha(gt.cols[col], pt.alph), alpha.f=0.5))
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], .8), lwd = lwd, lty = lty[col])
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = gt.cols[col], code=3, angle=90, length=0.1)
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  
  legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}



#working polygons FOR EVERY YEAR:
dens = NULL #c(100, 100, 100, 100, NULL, NA, NULL, NA)
ang = 45
bo = NA #c(NULL, NULL, NULL, NULL, NA, NA, NA, NA) #alpha(gt.cols[col], .8)
alf = c(.7, .7, .7, .7, .7, .7, .7, .7) #.7, .7, .7, .7
{
  var = 5
  varname = "Heterozygosity"
  title = "Fig 1C"
  range(smry[,var])
  
  ymin <- round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.3 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty = c(3,3,3,3, 1,1,1,1)
  pch = c(18, 19, 15, 17, 18, 19, 15, 17)
  
  par(mar = c(4,6,4,4))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', main = title, xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  TBL = NULL
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      #tbl[(d+1),5] = as.character(dat[1,19])
      
      
    }
    TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    col <- col+1
  }
  
  legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

head(smry)
hold <- matrix(ncol = 6, nrow = nrow(tbl))
for(i in 1:nrow(TBL)){
  hold[i,1] = hold[i,2] - hold[i]
}

subs = matrix(nrow = nrow(tbl), ncol = 4)
subs[,1] = tbl[,1]
for(n in 1:nrow(subs)){
  subs[n,2] = as.numeric(mean_70[n,2]) - as.numeric(mean_nbtl0[n,2])
}

gt.cols <- c("firebrick3", "darkorange1", "gold", "firebrick3", "darkorange1", "gold")
lty = c(3,3,3,1,1,1)
comp = matrix(nrow = 351, ncol = 7)
f = 1
for(e in 1:nrow(comp)){
  comp[e,1]=tbl[e,1]
  comp[e,f+1] = mean_nbtl0[e,2] - mean_10[e,2] 
  comp[e,f+2] = mean_nbtl0[e,2] - mean_30[e,2] 
  comp[e,f+3] = mean_nbtl0[e,2] - mean_70[e,2] 
  comp[e,f+4] = mean_nbtla[e,2] - mean_1a[e,2] 
  comp[e,f+5] = mean_nbtla[e,2] - mean_3a[e,2] 
  comp[e,f+6] = mean_nbtla[e,2] - mean_7a[e,2] 
  
  
}

plot(-1,-1, xlim = c(xmin, xmax), ylim = c(0, .1))
col=1
for(z in 1:(ncol(comp))){
  lines(comp[,1], comp[,z+1], col = gt.cols[col], lwd = lwd, lty = lty[col])
  col=col+1
}

comp = matrix(nrow = 351, ncol = 7)
f = 1
for(e in 1:nrow(comp)){
  comp[e,1]=tbl[e,1]
  if(e<99){
    next
  }else{
    comp[e,f+1] = mean_30[e,2] / mean_30[99,2] 
    comp[e,f+2] = mean_3a[e,2] / mean_3a[99,2]
    comp[e,f+3] = mean_3b[e,2] / mean_3b[99,2] 
    comp[e,f+4] = mean_3c[e,2] / mean_3c[99,2] 
    
  }
}
for(e in 1:nrow(comp)){
  comp[e,1]=tbl[e,1]
  if(e<99){
    next
  }else{
    comp[e,f+1] = mean_H_70[e,2] / mean_H_70[99,2] 
    comp[e,f+2] = mean_H_7a[e,2] / mean_H_7a[99,2]
    comp[e,f+3] = mean_H_7b[e,2] / mean_H_7b[99,2] 
    comp[e,f+4] = mean_H_7c[e,2] / mean_H_7c[99,2] 
    
  }
}

lty = c(3, 1, 2, 4)
pch = c(18, 19, 15, 17)
plot(-1,-1, xlim = c(xmin, xmax), ylim = c(.5, 1.5))
col=1
for(z in 1:(ncol(comp))){
  points(comp[,1], comp[,z+1], col = gt.cols[col], pch=pch[col])
  col=col+1
}

comp_VU = comp
mean_H_30 = mean_30
mean_H_3a = mean_3a
mean_H_3b = mean_3b
mean_H_3c = mean_3c

H_a = cbind(comp[,1], comp_CR[,3], comp_EN[,3], comp_VU[,3])
H_b = cbind(comp[,1], comp_CR[,4], comp_EN[,4], comp_VU[,4])
H_c = cbind(comp[,1], comp_CR[,5], comp_EN[,5], comp_VU[,5])
col=1
for(z in 1:(ncol(H_a))){
  lines(H_a[,1], H_a[,z+1], col = gt.cols[col], lwd = lwd, lty=1)
  col=col+1
}
col=1
for(z in 1:(ncol(H_b))){
  lines(H_b[,1], H_b[,z+1], col = gt.cols[col], lwd = lwd, lty=1)
  col=col+1
}
col=1
for(z in 1:(ncol(H_c))){
  lines(H_c[,1], H_c[,z+1], col = gt.cols[col], lwd = lwd, lty=1)
  col=col+1
}




colnames(comp_CR) <- c("year", "CR_0", "CR_a", "CR_b", "CR_c", "NA", "NA")
colnames(comp_EN) <- c("year", "EN_0", "EN_a", "EN_b", "EN_c", "NA", "NA")
colnames(comp_VU) <- c("year", "VU_0", "VU_a", "VU_b", "VU_c", "NA", "NA")

write.table(comp_CR, paste(directory, "/means&comps/H99-Ht_CR_var.mig.rates.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(comp_EN, paste(directory, "/means&comps/H99-Ht_EN_var.mig.rates.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(comp_VU, paste(directory, "/means&comps/H99-Ht_VU_var.mig.rates.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

colnames(H_a) <- c("year", "CR", "EN", "VU")
colnames(H_b) <- c("year", "CR", "EN", "VU")
colnames(H_c) <- c("year", "CR", "EN", "VU")

write.table(H_a, paste(directory, "/means&comps/H99-Ht_a_var.IUCN.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(H_b, paste(directory, "/means&comps/H99-Ht_b_var.IUCN.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(H_c, paste(directory, "/means&comps/H99-Ht_c_var.IUCN.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

colnames(mean_H_30) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_3a) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_3b) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_3c) <- c("year", "mean", "LQ", "UQ", "NA")

write.table(mean_H_30, paste(directory, "/means&comps/H_30_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_3a, paste(directory, "/means&comps/H_3a_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_3b, paste(directory, "/means&comps/H_3b_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_3c, paste(directory, "/means&comps/H_3c_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

colnames(mean_H_70) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_7a) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_7b) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_7c) <- c("year", "mean", "LQ", "UQ", "NA")

write.table(mean_H_70, paste(directory, "/means&comps/H_70_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_7a, paste(directory, "/means&comps/H_7a_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_7b, paste(directory, "/means&comps/H_7b_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_7c, paste(directory, "/means&comps/H_7c_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)

colnames(mean_H_10) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_1a) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_1b) <- c("year", "mean", "LQ", "UQ", "NA")
colnames(mean_H_1c) <- c("year", "mean", "LQ", "UQ", "NA")

write.table(mean_H_10, paste(directory, "/means&comps/H_10_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_1a, paste(directory, "/means&comps/H_1a_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_1b, paste(directory, "/means&comps/H_1b_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(mean_H_1c, paste(directory, "/means&comps/H_1c_m&qs.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#data for Q1: migration rates
p30 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
p3a = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",")  
p3b = read.table("fin_5.10.23_1LL3b_all_summary.csv", header=T, sep=",")
p3c = read.table("fin_5.10.23_1LL3c_all_summary.csv", header=T, sep=",")
p3d = read.table("fin_5.10.23_1LL3d_all_summary.csv", header=T, sep=",")
p3e = read.table("cpfin_5.10.23_1LL3e_all_summary.csv", header=T, sep=",")
p3f = read.table("fin_5.10.23_1LL3f_all_summary.csv", header=T, sep=",")

#data for Q2: pop crash sizes
p7a = read.table("fin_5.11.23_1LL7a_all_summary.csv", header=T, sep=",")
p70 = read.table("fin_5.11.23_1LL70_all_summary.csv", header=T, sep=",")
p30 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
p3a = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",")  
p50a = read.table("fin_5.11.23_2LL50a_all_summary.csv", header=T, sep=",")
p50 = p50a[p50a[,23]==0,,drop=FALSE]
p5a = p50a[p50a[,23]=="a",,drop=FALSE]
p1a = read.table("comb_fin_5.11.23_1LL1a_all_summary.csv", header=T, sep=",") 
#p1a >> manually combined fin_5.11.23_1LL1a_all_summary.csv, cpfin_5.11.23_1LL1a_all_summary.csv, and ONE RUN of c2fin_5.11.23_1LL1a_all_summary.csv
p10 = read.table("comb_fin_5.11.23_1LL10_all_summary.csv", header=T, sep=",")
#p10 >> manually combined fin_5.11.23_1LL10_all_summary.csv and TEN RUNS of cpfin_5.11.23_1LL10_all_summary.csv
#NOTE THAT IN p10, 13 populations CRASH before hitting y=350!!!!

#data for Q3: allele frequencies
LL0 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
LLa = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",") 
HL0 = read.table("fin_5.23.23_1HL30_all_summary.csv", header=T, sep=",")
HLa = read.table("fin_5.23.23_1HL3a_all_summary.csv", header=T, sep=",")
HH0a = read.table("comb_fin_5.10.23_2HH30a_all_summary.csv", header=T, sep=",")
#HH0a >> manually combined fin_5.10.23_2HH30a_all_summary.csv and cpfin_5.10.23_2HH30a_all_summary.csv
HH0 = HH0a[HH0a[,23]==0,,drop=FALSE]
HH0[,19] = "HH0"
HHa = HH0a[HH0a[,23]!=0,,drop=FALSE]
HHa[,19] = "HHa"

#data for supplemental
#data for various conservation statuses
p1b = read.table("comb_fin_6.1.23_LL1b_all_summary.csv", header=T, sep=",")  #has 14 pops that crashed!!!
#manually combined fin_6.1.23_LL1b_all_summary.csv and cpfin_6.1.23_LL1b_all_summary.csv
p1b[,19] = "p1b"
p1c = read.table("fin_6.1.23_LL1c_all_summary.csv", header=T, sep=",")  #has 3 pops that crashed!!!
p5b = read.table("fin_6.1.23_LL5b_all_summary.csv", header=T, sep=",")
p5c = read.table("comb_fin_6.1.23_LL5c_all_summary.csv", header=T, sep=",")
#manually combined fin_6.1.23_LL5b_all_summary.csv and cpfin_6.1.23_LL5b_all_summary.csv
p7b = read.table("fin_6.1.23_LL7b_all_summary.csv", header=T, sep=",")
p7c = read.table("fin_6.1.23_LL7c_all_summary.csv", header=T, sep=",")
#data for no bottleneck
b0 = read.table("fin_6.1.23_nbtl30_all_summary.csv", header=T, sep=",")
ba = read.table("fin_6.1.23_nbtl3a_all_summary.csv", header=T, sep=",")
#data for when migrants arent preferentially mated
p30. = read.table("fin_6.1.23_1LL30._all_summary.csv", header=T, sep=",")
p3a. = read.table("fin_6.1.23_1LL3a._all_summary.csv", header=T, sep=",")  
p3b. = read.table("fin_6.1.23_1LL3b._all_summary.csv", header=T, sep=",")
p3c. = read.table("fin_6.1.23_1LL3c._all_summary.csv", header=T, sep=",")

smry = rbind(p1b, p3b, p5b, p7b)
smry = rbind(p1c, p3c, p5c, p7c)
smry = rbind(p10, p1a, p1b, p1c)
smry = rbind(p50, p5a, p5b, p5c)
smry = rbind(b0, ba, p30, p3a)
smry = rbind(p70, p7a, p7b, p7c)
smry = rbind(p30.,p3a.,p3b.,p3c.) #,p3d,p3e,p3f
smry = rbind(p1a, p3a, p5a, p7a)
smry = rbind(p10, p30, p70) #p50, 
smry = rbind(LL0, HL0, HH0)
smry = rbind(LLa, HLa, HHa)
smry = rbind(LLa, HLa, HHa, LL0, HL0, HH0)
gt.cols <- c("firebrick3", "darkorange1", "goldenrod1")
gt.cols <- c("#FFC5D0", "#E2D4A8", "#A4DDEF","orchid3", "springgreen", "blue")
gt.cols <- c("black", "#FFC5D0", "#A8E1BF", "#A4DDEF", "hotpink", "springgreen3", "blue")

gt.cols <- c("#FFC5D0", "#E2D4A8", "#A4DDEF", "#E4CBF9")
gt.cols <- c("orchid3", "springgreen", "blue", "hotpink") #"#A8E1BF", 
gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(5, "Pastel1") #ghibli_palette('PonyoLight')[4]
"#C87A8A""#6B9D59""#5F96C2""#909646" "#00A396" "#9189C7"
gt.cols<- c("#C87A8A","#00A396","#9189C7","lightgreen","orange" )
gt.cols<- c("#C87A8A","#00A396","#6B9D59","#AC8C4E")
"orange" "lightgreen""#C87A8A" "#BF826A" "#AC8C4E" "#909646" "#6B9D59" "#39A277" "#00A396" "#169FB1" "#5F96C2" "#9189C7" "#B37EBE" "#C578A8"
"#FFC5D0" "#E2D4A8" "#A8E1BF" "#A4DDEF" "#E4CBF9"

{
  #Data
  {
    #summary_ABM_run.10.12.22_d_all.csv  >> zero migration
    #ABM_run.11.8.22_1a_all_summary.csv  >> 1 mig per generation, starting heterozy in source pop = .4-.6
    #ABM_run.11.7.22_2a_all_summary.csv  >> one migration of 50 indv at year 175, starting heterozy in source pop = .4-.6
    #ABM_run.11.7.22_3a_all_summary.csv  >> three migrations of 25 indv at years 175, 201, 225, starting heterozy in source pop = .4-.6
    #ABM_run.11.8.22_4a_all_summary.csv  >> 1 mig per generation, starting heterozy in source pop = .8-.9
    #ABM_run.11.8.22_5a_all_summary.csv  >> 1 mig per generation, starting heterozy in source pop = .1-.2
    #ABM_run.11.14.22_6a_all_summary.csv >> one migration of 50 indv at year 175, starting heterozy in source pop = .1-.2
    #ABM_run.11.14.22_7a_all_summary.csv >> one migration of 50 indv at year 175, starting heterozy in source pop = .8-.9
    #ABM_run.11.14.22_8a_all_summary.csv >> three migrations of 25 indv at years 175, 201, 225, starting heterozy in source pop = .8-.9
    #ABM_run.11.14.22_9a_all_summary.csv >> three migrations of 25 indv at years 175, 201, 225, starting heterozy in source pop = .1-.2
    
    
    #give each parameter set a unique identifier
    tab0[,12] <- "z"
    tab1[,12] <- "a"
    tab2[,12] <- "b"
    tab3[,12] <- "c"
    tab4[,12] <- "d"
    tab5[,12] <- "e"
    tab6[,12] <- "f"
    tab7[,12] <- "g"
    tab8[,12] <- "h"
    tab9[,12] <- "i"
    
    #note that as written, figs are for FOUR (4) parameter sets -- no mig plus the 3 varieties
    comp1 = rbind(tab0,tab1,tab2,tab3) #Change migration intensities @ .4-.6 het in source pop
    comp2 = rbind(tab0,tab1,tab4,tab5) #change fitness of source pop @ 1 mig per gen
    comp3 = rbind(tab0,tab2,tab6,tab7) #change fitness of source pop @ 1x pulse mig
    comp4 = rbind(tab0,tab3,tab8,tab9) #change fitness of source pop @ 3x pulse mig 
    comp5 = rbind(tab0,tab4,tab7,tab8) #change migration intensities @ .8-.9 het in source pop
    comp6 = rbind(tab0,tab5,tab6,tab9) #change migration intensities @ .1-.2 het in source pop
    
    smry <- tab   #compX
  }
  
  #~~~Avril's Code
  {
    ### set colors
    library(ghibli)
    library(scales)
    library(colorspace)
    gt.cols <- qualitative_hcl(4, "Dark2") #ghibli_palette('PonyoMedium')#[4]
    lt.gt.col <- qualitative_hcl(4, "Pastel1") #ghibli_palette('PonyoLight')[4]
    demoplot(gt.cols, "bar")
    demoplot(lt.gt.col, "bar")
    
    ymin <- 0.2
    ymax <- round(max(smry[,5]), digits = 2)+0.1
    ln.alph <- 0.5
    pt.alph <- 1
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.5, 0, 0.5) #c(-0.2, -0.1, 0, 0.1, 0.2)
    orig.xs <- c(1, 100, 150, 200, 250) #c(1:250) #c(1, 100, 150, 200, 250)
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 2
    
    #tab3[tab3[,1] == 1,,]
    
    ## Observed Het over Time
    plot(0,0, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', main = 'Observed Heterozygosity Over Time', xlab = 'Generation Time', ylab = 'Observed Heterozygosity',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    #axis(2, at = c(-0.1, 0, 0.1, 0.2), cex.axis = text.size)
    axis(1, at = c(0, 100, 150, 200, 250), labels = c('0','100','150','200', '250'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,12])){
      print(c)
      temp <- smry[smry[,12] == c,, drop=FALSE] #separate by parameter set
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #columns <- c(18, 19, 20, 21)
      lines(xs, c(mean(y1[,5]), mean(y2[,5]), mean(y3[,5]), mean(y4[,5]), mean(y5[,5])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
      points(xs, c(mean(y1[,5]), mean(y2[,5]), mean(y3[,5]), mean(y4[,5]), mean(y5[,5])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      arrows(x0 = xs, y0 = c(mean(y1[,5])-sd(y1[,5]), mean(y2[,5])-sd(y2[,5]), mean(y3[,5])-sd(y3[,5]), mean(y4[,5])-sd(y4[,5]), mean(y5[,5])-sd(y5[,5])), 
             y1 = c(mean(y1[,5])+sd(y1[,5]), mean(y2[,5])+sd(y2[,5]), mean(y3[,5])+sd(y3[,5]), mean(y4[,5])+sd(y4[,5]), mean(y5[,5])+sd(y5[,5])), 
             lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      #  for(l in unique(orig.xs)){
      #    column <- columns[l]
      #    ## 95% CIs (inappropriate for large sample sizes)
      #    # arrows(x0 = xs[l], x1 = xs[l], y0 = (mean(temp[,column], na.rm = TRUE) - (sd(temp[,column], na.rm = TRUE)/10*1.96)),
      #    #        y1 = (mean(temp[,column], na.rm = TRUE) + (sd(temp[,column], na.rm = TRUE)/10*1.96)),
      #    #        lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      #  arrows(x0 = OTime[,1], y0 = (OTime[,2])-(sd(OTime[,7])), y1 = (OTime[,2])+(sd(OTime[,7])), lwd = .5, col = "black", code=3, angle=90, length=0.1)
      
      #    
      #    arrows(x0 = xs[l], x1 = xs[l], y0 = quantile(temp[,column], probs = c(0.025,0.975))[1],   #will need to do this for each year of interest
      #           y1 = quantile(temp[,column], probs = c(0.025,0.975))[2],
      #           lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      #  }    
      col <- col+1
    }
    
    legend('top', legend = c('tab1', 'tab2','tab3','tab4'), col = gt.cols, pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = TRUE, x.intersp = 0.7)
  }
  
  ##### Generic plotting code
  #1=yr, 2=pop size, 3=propmig, 4=He, 5=Ho[driftSNPS], 6=fis, 7=nadult, 8=sxratio, 9=nmig, 10=fst, 11=replicate, 12=paramset, 13=noffspring, 14=fstvsource, 15=fisvsource,
  #16=deltaK, 17=propMigSNPs, 18=HoallSNPs, 19=projectname, 20=groupnumb, 21=k, 22=nSNP, 23=miggy, 24=LBhet, 25=LBp, 26=maxage, 27=broodsize, 28=maturity,
  #29=years, 30=r0, 31=nSNP.mig, 32=nSNP.cons
  {
    var = 3
    varname = "observed heterozygosity"
    title = "various starting allele freqs"
    range(smry[,var])
    #if(anyNA(smry[,var]==TRUE)){
    #  hold<- na.omit(smry[,var],drop=FALSE)
    #  smry <- as.matrix(hold)
    #}
    
    ymin <- 0 # round(min(smry[,var]), digits = 2)#-.1
    ymax <- 5 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', main = title, xlab = 'Generation Time', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,13])){
      print(c)
      temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      #columns <- c(18, 19, 20, 21)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
      arrows(x0 = xs, y0 = c(mean(y1[,var])-sd(y1[,var]), mean(y2[,var])-sd(y2[,var]), mean(y3[,var])-sd(y3[,var]), mean(y4[,var])-sd(y4[,var]), mean(y5[,var])-sd(y5[,var]), mean(y6[,var])-sd(y6[,var]), mean(y7[,var])-sd(y7[,var]), mean(y8[,var])-sd(y8[,var])), 
             y1 = c(mean(y1[,var])+sd(y1[,var]), mean(y2[,var])+sd(y2[,var]), mean(y3[,var])+sd(y3[,var]), mean(y4[,var])+sd(y4[,var]), mean(y5[,var])+sd(y5[,var]), mean(y6[,var])+sd(y6[,var]), mean(y7[,var])+sd(y7[,var]), mean(y8[,var])+sd(y8[,var])), 
             lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      #  for(l in unique(orig.xs)){
      #    column <- columns[l]
      #    ## 95% CIs (inappropriate for large sample sizes)
      #    # arrows(x0 = xs[l], x1 = xs[l], y0 = (mean(temp[,column], na.rm = TRUE) - (sd(temp[,column], na.rm = TRUE)/10*1.96)),
      #    #        y1 = (mean(temp[,column], na.rm = TRUE) + (sd(temp[,column], na.rm = TRUE)/10*1.96)),
      #    #        lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      #  arrows(x0 = OTime[,1], y0 = (OTime[,2])-(sd(OTime[,7])), y1 = (OTime[,2])+(sd(OTime[,7])), lwd = .5, col = "black", code=3, angle=90, length=0.1)
      
      #    
      #    arrows(x0 = xs[l], x1 = xs[l], y0 = quantile(temp[,column], probs = c(0.025,0.975))[1],   #will need to do this for each year of interest
      #           y1 = quantile(temp[,column], probs = c(0.025,0.975))[2],
      #           lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
      #  }    
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    
    #legend('topleft', legend = c('mig=0_no btlnk', 'mig=1 mig/gen_no btlnk', 'mig=0_w/btlnk', 'mig=1 mig/genw/btlnk'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
    #legend('topleft', legend = c('mig=0', 'mig=1 mig/gen','mig=100@y=151','mig=25@y=151,165,181,195'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
    #legend('bottomleft', legend = c('critically endangered', 'endangered','threatened','vulnerable'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
    #legend('bottomright', legend = c('low source, low focal', 'high source, low focal','high source, high focal'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
    legend('right', legend = c('LL 1mig', 'HL 1mig','HH 1mig', 'LL no mig', 'HL no mig','HH no mig'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
    #legend('topleft', legend = c('mig=0', 'mig=1 mig/gen','mig=100@y=151','mig=25@y=151,165,181,195', 'mig=1mig@y=151','mig=100@y=125','mig=25 @ y=125,140,155,170'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  ##NEED TO FIGURE OUT A WAY TO DO A POLYGON/GO ACROSS ALL YEARS
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(as.table(table(p10[,1]))/100, main = "no mig", ylab = "proportion pops survive", xlab = "time", col = gt.cols[1], cex.axis = text.size, cex.lab = text.size)
plot(as.table(table(p1a[,1]))/100, main = "1 mig/gen", ylab = "proportion pops survive", xlab = "time", col = gt.cols[2], cex.axis = text.size, cex.lab = text.size)
plot(as.table(table(p1b[,1]))/100, main = "bulk AM", ylab = "proportion pops survive", xlab = "time", col = gt.cols[3], cex.axis = text.size, cex.lab = text.size)
plot(as.table(table(p1c[,1]))/100, main = "freq AM", ylab = "proportion pops survive", xlab = "time", col = gt.cols[4], cex.axis = text.size, cex.lab = text.size)

####Survival Model for crashed populations!
#https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html
#https://rpkgs.datanovia.com/survminer/reference/ggsurvtable.html

surv <- matrix(1,nrow=nrow(smry))
colnames(surv) <- c("Survive")
smry=cbind(smry,surv)
smry[,33] = ifelse(smry[,1]<=350,1,0)

library(coxme)
library(survminer)
library(ggplot2)
library(survival)

run0 <- survfit(Surv(year,Survive)~project, data =smry)
summary(run0)
ggsurvtable(run0, data=smry)

run1 <- coxme(Surv(LatencyZ2360,Survive)~Treatment+(1|FishName)+(Age), data =etho)
summary(run1)

run2 <- coxme(Surv(CumDurZ2,Survive)~Treatment+(1|FishName), data =etho)
summary(run2)

plot1 <- ggsurvplot(data=smry, run0)

plot1 <- ggsurvplot(survfit(run0), data=smry)
{
  #create names for data so taht it is easier to call and change when the data change
  yr       = smry[,1]   #year
  n        = smry[,2]   #population size
  propmig  = smry[,3]   #proportion of pop that are migrants
  He       = smry[,4]   #He
  Ho       = smry[,5]   #Ho
  fis      = smry[,6]   #fis vs initialized pop
  nadult   = smry[,7]   #number of indv that have reached maturity
  sx       = smry[,8]   #sex ratio
  nmig     = smry[,9]   #number of migrants
  fst      = smry[,10]  #fst vs initialized pop
  rep      = smry[,11]  #replicate number
  para     = smry[,19]
  noff     = smry[,13]  #number of offspring produced that year
  fstvs    = smry[,14]  #fst vs the source pop
  fisvs    = smry[,15]  #fis vs the source pop
  col      = 1:length(unique(smry[,19])) #para  #c("#E16A86", "#909800", "#00AD9A", "#9183E6")#smry[,12] #-4
  #What is the best way to plot?? do I put all data in one dataset and then do a loop?
  #or keep all data seperate so that it is easier to do the colors, etc
  
  ############Generic Code
  
  #variable
  plt = smry[,4]
  plt_name = "HE"
  
  plot(-100, -100 , xlab="year", ylab=plt_name, xlim=c(0, max(yr)), ylim=c((min(plt)), (max(plt)))) 
  for(d in unique(smry[,19])){
    da = smry[para==d,,drop=FALSE]
    for(y in unique(da[,20])){ #group
      dat = da[da[,20]==y,,drop=FALSE]
      lines(dat[,1],dat[,6],col=alpha(gt.cols[dat[,19]],0.4),lwd=3.5)
      
    }
  }
  for(d in unique(para)){
    dat = smry[smry[,12]==d,,drop=FALSE]
    avg = aggregate(x = dat[,6], by = list(dat[,1]), FUN = "mean")
    if(d<=9){lines(avg[,1], avg[,2], col=alpha(gt.cols[d],0.99), lwd = 5)} #gt.cols[d]
  }
  
  ############Averages
  plot(-100, -100 , xlab="year", ylab="population size", xlim=c(0, max(yr)), ylim=c(0, max(n))) 
  for(p in unique(para)){
    dat = smry[smry[,19]==p,,drop=FALSE]
    avg = aggregate(x = dat[,2], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col="black", lwd = 3)
  }
  
  plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(0, max(yr)), ylim=c((min(Ho)), (max(Ho)))) #max(yr)
  z=1
  for(d in unique(para)){
    dat = smry[smry[,19]==d,,drop=FALSE]
    avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[z], lwd = 5) #gt.cols[d]
    z<-z+1
  }
  for(d in unique(para)){
    dat = smry[smry[,12]==d,,drop=FALSE]
    avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
    if(d == 1|d==2){
      lines(avg[,1], avg[,2], col="black", lty=2, lwd = 6) 
    }else if(d > 3){
      lines(avg[,1], avg[,2], col="black", lwd = 4) #gt.cols[d]
    }else{next}
    
  }
  for(d in unique(para)){
    dat = smry[smry[,12]==d,,drop=FALSE]
    avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
    if(d == 1|d==2){
      lines(avg[,1], avg[,2], col="grey", lty=2, lwd = 6) 
    }else if(d > 3){
      lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 4) #gt.cols[d]
    }else{next}
    
  }
  for(d in unique(para)){
    dat = smry[smry[,12]==d,,drop=FALSE]
    avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
    if(d > 6){
      lines(avg[,1], avg[,2], col="black", lwd = 4) #gt.cols[d]
    }else{next}
    
  }
  
  
  plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, max(yr)), ylim=c((min(fst)-.01), (max(fst))))
  z=1
  for(d in unique(para)){
    dat = smry[smry[,19]==d,,drop=FALSE]
    avg = aggregate(x = dat[,10], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[z], lwd = 8)
    z=z+1
  }
  
  plot(-100, -100 , xlab="year", ylab="Fst vs source", xlim=c(0, max(yr)), ylim=c(0, max(fstvs)))
  for(d in unique(para)){
    dat = smry[smry[,19]==d,,drop=FALSE]
    avg = aggregate(x = dat[,14], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col="black", lwd = 3)
  }
  
  plot(-100, -100 , xlab="year", ylab="prop migrant SNPs", xlim=c(0, max(yr)), ylim=c(0, max(smry[,17])))
  for(d in unique(para)){
    dat = smry[smry[,19]==d,,drop=FALSE]
    avg = aggregate(x = dat[,17], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col="black", lwd = 3)
  }
  
  plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, max(yr)), ylim=c((min(fis)), (max(fis)))) 
  z=1
  for(d in unique(para)){
    dat = smry[smry[,19]==d,,drop=FALSE]
    avg = aggregate(x = dat[,6], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[z], lwd = 3)
    z=z+1
  }
  
  plot(-100, -100 , xlab="year", ylab="sex ratio", xlim=c(0, max(yr)), ylim=c((min(sx)-.1), (max(sx)+.1)))
  z=1
  for(d in unique(para)){
    dat = smry[smry[,19]==d,,drop=FALSE]
    avg = aggregate(x = dat[,8], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[z], lwd = 3)
    z=z+1
  }
}
######LEGEND
plot(-100, -100 , xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
#miggy.V       = c(0,"a","b","c")  #"a"=one mig per gen, "b"=1xof50@175, "c"=3xpf25@175|201|225  #migration parameter type
legend('center', legend = c('mig=0',
                            'mig=1migpergen_random mates',
                            'mig=1migpergen y>151_randommates', 
                            'mig=0',
                            'mig=1migpergen_migrantspreffered',
                            'mig=1migpergen y>151_migrantspreffered'), 
       col = gt.cols, pch = 19, bty = 'n', cex = 1.5, pt.cex = 3, horiz = FALSE, x.intersp = 0.5)


ne = read.table("fin_5.23.23_1HL30_all_necounts.csv", header=T, sep=",")
head(ne)

allindv = matrix(nrow=nrow(ne), ncol=1)
allindv = ne[,4]+ne[,5]
ne = cbind(ne, allindv)

plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 1000)) 
points(ne[,1], ne[,13], col=gt.cols[col], pch=16)

plot(-100, -100 , xlab="year", ylab="population size", xlim=c(0, 350), ylim=c(0, 1000)) 
r=1
for(z in unique(ne[,11])){
  zne = ne[ne[,11]==z,,drop=FALSE]
  for(b in 1:length(unique(zne[,12]))){
    bne = zne[zne[,12]==b,,drop=FALSE]
    for(p in 1:length(unique(smry[,10]))){
      pne = bne[bne[,10]==p,,drop=FALSE]
      lines(pne[,1], pne[,13], col=gt.cols[r], pch=16)
    }
  }
  r=r+1
}
legend('bottomright', legend = c('critically endangered', 'endangered','vulnerable'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)

#maybe need to add numboff to numb adults and numb babies?



######Points
{
  pm = smry[smry$propmig!=0,,drop=FALSE]
  plot(pm$propmig~pm$Ho,col=gt.cols[col], pch=16)
  #how does the prop or migrants influence the heterozygosity?
  y<-lmer(Ho~propmig + (1|parameterset), data=smry)
  y<-lmer(Ho~propmig+yr*propmig + (1|parameterset), data=smry)
  summary(y)
  #http://127.0.0.1:19921/graphics/plot_zoom_png?width=1147&height=900
  #think about propmig~LRS also -- will need to merge the datasets
  
  png("popsize_overtime.png")
  plot(-100, -100 , xlab="year", ylab="population size", xlim=c(0, 350), ylim=c(0, 1000)) 
  r=1
  for(z in unique(smry[,19])){
    zsmry = smry[smry[,19]==z,,drop=FALSE]
    for(b in 1:length(unique(zsmry[,20]))){
      bsmry = zsmry[zsmry[,20]==b,,drop=FALSE]
      for(p in 1:length(unique(smry[,11]))){
        psmry = bsmry[bsmry[,11]==p,,drop=FALSE]
        lines(psmry[,1], psmry[,2], col=gt.cols[r], pch=16)
      }
    }
    r=r+1
  }
  legend('bottomright', legend = c('critically endangered', 'endangered','vulnerable'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  
  
  points(yr, n, col=gt.cols[col], pch=16) #"firebrick"
  dev.off()
  
  png("Ho_overtime.png")
  plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(0, max(yr)), ylim=c((min(Ho)), (max(Ho)))) 
  points(yr,smry[,18],col=gt.cols[col])
  points(tab5[,1],tab5[,5],col="dodgerblue")
  points(tab0[,1],tab0[,5],col="firebrick")
  points(tab6[,1],tab6[,5],col="gold")
  points(tab9[,1],tab9[,5],col="springgreen")
  legend('top', legend = c('mig=1', 'mig=0','mig=3x','mig=1x'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = TRUE, x.intersp = 0.5)
  legend('bottom', legend = c('mig=1', 'mig=0','mig=1x','mig=3x ALL AT .1-.2'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.5)
  
  dev.off()
  
  for(d in unique(para)){
    dat = smry[smry[,12]==d,,drop=FALSE]
    #avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
    if(d > 3){
      points(dat[,1], dat[,5], col=gt.cols[d]) #gt.cols[d]
    }else{next}
    
  }
  
  #install.packages("scales")
  library(scales)
  plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(0, max(yr)), ylim=c((min(Ho)), (max(Ho)))) 
  for(d in unique(smry[,12])){
    da = smry[para==d,,drop=FALSE]
    for(y in unique(da[,20])){
      dat = da[da[,20]==y,,drop=FALSE]
      if(d <= 9){
        lines(dat[,1],dat[,18],col=alpha(gt.cols[dat[,12]],0.4),lwd=3.5)
      }
      
    }
  }
  for(d in unique(para)){
    dat = smry[smry[,12]==d,,drop=FALSE]
    avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
    if(d<=9){lines(avg[,1], avg[,2], col=alpha(gt.cols[d],0.99), lwd = 5)} #gt.cols[d]
  }
  for(t in unique(tat[,12])){
    tata = tat[tat[,12]==t,,drop=FALSE]
    avg = aggregate(x = tata[,5], by = list(tata[,1]), FUN = "mean")
    if(t==3){lines(avg[,1], avg[,2], col=alpha(ex.cols[tata[,12]],0.99), lwd = 5)}
    #for(p in unique(tata[,11])){
    #  ta = tata[tata[,11]==p,,drop=FALSE]
    #  if(t<4){lines(ta[,1],ta[,18],col=alpha(ex.cols[ta[,12]],0.6),lwd=3)}
    #}
  }
  for(t in unique(ted[,12])){
    teda = ted[ted[,12]==t,,drop=FALSE]
    avg = aggregate(x = teda[,5], by = list(teda[,1]), FUN = "mean")
    if(t<9){lines(avg[,1], avg[,2], col=ad.cols[teda[,12]], lwd = 5)}
    #for(p in unique(tata[,11])){
    #  ta = tata[tata[,11]==p,,drop=FALSE]
    #  if(t<4){lines(ta[,1],ta[,18],col=alpha(ex.cols[ta[,12]],0.6),lwd=3)}
    #}
  }
  
  ex.cols = c("#9189C7","#FFC5D0","#E2D4A8")
  ad.cols = c("purple","hotpink")
  #"#C87A8A","#00A396",
  
  png("fis_overtime.png")
  plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, max(yr)), ylim=c((min(fis)), (max(fis)))) 
  points(yr,fis,col=gt.cols[col])
  points(tab5[,1],tab5[,6],col="dodgerblue")
  points(tab0[,1],tab0[,6],col="firebrick")
  points(tab6[,1],tab6[,6],col="gold")
  points(tab9[,1],tab9[,6],col="springgreen")
  dev.off()
  
  
  png("sexratio_overtime.png")
  plot(-100, -100 , xlab="year", ylab="sex ratio", xlim=c(0, max(yr)), ylim=c((min(sx)-.1), (max(sx)+.1)))
  points(yr,sx,col=gt.cols[col])
  points(tab5[,1],tab5[,8],col="dodgerblue")
  points(tab0[,1],tab0[,8],col="firebrick")
  points(tab6[,1],tab6[,8],col="gold")
  points(tab9[,1],tab9[,8],col="springgreen")
  dev.off()
  
  png("fst_overtime.png")
  plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, max(yr)), ylim=c((min(fst)-.01), (max(fst))))
  points(yr,fst,col=gt.cols[col], pch=16)
  points(tab5[,1],tab5[,10],col="dodgerblue")
  points(tab0[,1],tab0[,10],col="firebrick")
  points(tab6[,1],tab6[,10],col="gold")
  points(tab9[,1],tab9[,10],col="springgreen")
  dev.off()
  
  
  png("noffspring_overtime.png")
  plot(-100, -100 , xlab="year", ylab="number of offpsring produced", xlim=c(0, max(yr)), ylim=c(0, (max(noff))))
  points(yr,noff,col="yellow")
  points(tab5[,1],tab5[,13],col="dodgerblue")
  points(tab0[,1],tab0[,13],col="firebrick")
  points(tab6[,1],tab6[,13],col="gold")
  points(tab9[,1],tab9[,13],col="springgreen")
  dev.off()
  
  png("fstVSource_overtime.png")
  plot(-100, -100 , xlab="year", ylab="Fst vs source", xlim=c(0, max(yr)), ylim=c(0.05, max(fstvs)))
  points(yr,fstvs,col=gt.cols[col])
  points(tab5[,1],tab5[,14],col="dodgerblue")
  points(tab0[,1],tab0[,14],col="firebrick")
  points(tab6[,1],tab6[,14],col="gold")
  points(tab9[,1],tab9[,14],col="springgreen")
  dev.off()
  
  png("fisVSource_overtime.png")
  plot(-100, -100 , xlab="year", ylab="Fis vs source", xlim=c(0, max(yr)), ylim=c(-.5, 1))
  points(yr,fisvs,col="green")
  points(tab5[,1],tab5[,15],col="dodgerblue")
  points(tab0[,1],tab0[,15],col="firebrick")
  points(tab6[,1],tab6[,15],col="gold")
  points(tab9[,1],tab9[,15],col="springgreen")
  dev.off()
  
  plot(-100, -100 , xlab="year", ylab="prop migrant SNPs", xlim=c(0, max(yr)), ylim=c(0, 1))
  points(yr,smry[,17],col=gt.cols[col],pch=16)
  
  plot(-100, -100 , xlab="Ho drift SNPs", ylab="Ho all SNPS", xlim=c(0, 1), ylim=c(0, 1))
  points(Ho,smry[,18],col=gt.cols[col])
  abline(a=0,b=1)
}

########################################################################
########################################################################
#REPRODUCTIVE SUCCESS
########################################################################

#NOTE--- as of 1.13.23, migrants ARE included in LRS values 

r30 = read.table("fin_5.10.23_1LL30_all_repsuc.csv", header=T, sep=",")
r3a = read.table("fin_5.10.23_1LL3a_all_repsuc.csv", header=T, sep=",")
r3b = read.table("fin_5.10.23_1LL3b_all_repsuc.csv", header=T, sep=",")
r3c = read.table("fin_5.10.23_1LL3c_all_repsuc.csv", header=T, sep=",")
r3d = read.table("fin_5.10.23_1LL3d_all_repsuc.csv", header=T, sep=",")
r3e = read.table("fin_5.10.23_1LL3e_all_repsuc.csv", header=T, sep=",")
r3f = read.table("fin_5.10.23_1LL3f_all_repsuc.csv", header=T, sep=",")

r10 = read.table("fin_5.11.23_1LL10_all_repsuc.csv", header=T, sep=",")
r1a = read.table("comb_fin_5.11.23_1LL1a_all_repsuc.csv", header=T, sep=",")
r50 = read.table("fin_5.11.23_1LL50_all_repsuc.csv", header=T, sep=",")
r5a = read.table("fin_5.11.23_1LL5a_all_repsuc.csv", header=T, sep=",")
r70 = read.table("fin_5.11.23_1LL70_all_repsuc.csv", header=T, sep=",")
r7a = read.table("fin_5.11.23_1LL7a_all_repsuc.csv", header=T, sep=",")

rep[is.na(rep)] <- 0
red[is.na(red)] <- 0
rev[is.na(rev)] <- 0

{
  #ABG, CDH, EFI
  #rep = 1,2,3
  #red = 4,5,6
  #rev = 7,8
  
  #give each parameter set a unique identifier
  rep[,10] = rep[,10]+3
  table(red[,10])
  
  rev[,10] = rev[,10]+6
  table(rev[,10])
}
rep = rbind(r30, r3a, r3b, r3c)

library(colorspace)
gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(6, "Pastel1") #ghibli_palette('PonyoLight')[4]

{
  #het1<- rep[rep[,10]<=4,,drop=FALSE]
  #het2<- rep[(rep[,10]>4)&(rep[,10]<9),,drop=FALSE]
  #het2<- rep[rep[,10]>=5,,drop=FALSE]
  
  library(ghibli)
  library(scales)
  gt.cols <- ghibli_palette('PonyoMedium')#[4]
  lt.gt.col <- ghibli_palette('PonyoLight')[4]
  library(colorspace)
  gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
  lt.gt.col <- qualitative_hcl(6, "Pastel1") #ghibli_palette('PonyoLight')[4]
  
  col = rep[,13] #-4  #color by parameter
  para = rep[,13]
  yr  = rep[,1]   #year
  n   = rep[,2]   #n born
  LRS = rep[,3]   #mean LRS
  SD  = rep[,4]   #SD of LRS
  LRSf= rep[,5]   #female LRS
  LRSm= rep[,6]   #male LRS
  RRS = rep[,7]   #mean RRS
  SDR = rep[,8]   #SD of RRS
  replic = rep[,9]   #replicate
  LRSmig = rep[,11] #LRS of migrants
  LRSnat = rep[,12] #LRS of natives
  
  #Averages
  plot(-100, -100 , xlab="year", ylab="LRS", xlim=c(min(yr), max(yr)), ylim=c(0, 4)) #
  a=1
  for(d in unique(para)){
    dat = rep[rep[,13]==d,,drop=FALSE]
    avg = aggregate(x = dat[,3], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[a], lwd = 3)
    a<-a+1
  }
  
  #Relative repro success
  rel = matrix(nrow=nrow(rep),ncol=1)
  rel = rep[,12]/rep[,11]
  rep = cbind(rep,rel)
  
  plot(-100, -100 , xlab="year", ylab="LRSnatives/LRSmigrants", xlim=c(min(yr), max(yr)), ylim=c(0, 5)) #
  for(d in unique(para)){
    dat = rep[rep[,10]==d,,drop=FALSE]
    avg = aggregate(x = dat[,15], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
  }
  
  plot(-100, -100 , xlab="year", ylab="RRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(RRS)))
  for(d in unique(para)){
    dat = rep[rep[,10]==d,,drop=FALSE]
    avg = aggregate(x = dat[,7], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
  }
  
  plot(-100, -100 , xlab="year", ylab="nborn", xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
  for(d in unique(para)){
    dat = rep[rep[,10]==d,,drop=FALSE]
    avg = aggregate(x = dat[,2], by = list(dat[,1]), FUN = "mean")
    lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
  }
  
  plot(-100, -100 , xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
  legend('top', legend = c('mig=0',
                           'mig=1migpergen',
                           'mig=1migpergen y<100 & >151', 
                           'mig=100@yr151',
                           'mig=25@yr151,165,181,195',
                           'mig=1migpergen y>151',
                           'mig=100@yr125',
                           'mig=25@yr125,140,155,170',
                           'another'), 
         col = ("black"), pch = 19, bty = 'n', cex = 1.75, pt.cex = 5, horiz = FALSE, x.intersp = 0.5)
  #miggy.V       = c(0,"a","b","c")  #"a"=one mig per gen, "b"=1xof50@175, "c"=3xpf25@175|201|225  #migration parameter type
  legend('center', legend = c('mig=1migpergen_NOAllee', 'mig=1migpergen_WAllee','mig=1migpergen_NOAllee', 'mig=0_WAllee','mig=1migpergen_WAllee','mig=0', 'mig=100@yr 175,201,225','mig=1mpg y <100 & >150','mig=2migpergen y>150'), col = gt.cols[1:6], pch = 19, bty = 'n', cex = 1.75, pt.cex = 2, horiz = FALSE, x.intersp = 0.5)
  
  ####POints
  {
    plot(-100, -100 , xlab="year", ylab="LRS", xlim=c(min(yr), max(yr)), ylim=c(0, 3))#max(LRS)
    points(yr, LRS, col=gt.cols[col], pch=19)
    points(rep8[,1],rep8[,3], col="dodgerblue")
    points(rep9[,1],rep9[,3], col="firebrick")
    
    plot(-100, -100 , xlab="year", ylab="nborn", xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
    points(yr, n, col=gt.cols[col])
    points(rep8[,1],rep8[,2], col="dodgerblue")
    points(rep9[,1],rep9[,2], col="firebrick")
    
    plot(-100, -100 , xlab="year", ylab="RRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(RRS)))
    points(yr, RRS, col=gt.cols[col], pch=19)
    points(rep8[,1],rep8[,7], col="dodgerblue")
    points(rep9[,1],rep9[,7], col="firebrick")
    
    plot(-100, -100 , xlab="year", ylab="LRS of females", xlim=c(min(yr), max(yr)), ylim=c(0, max(LRSf)))
    points(yr, LRSf, col=gt.cols[col])
    
    plot(-100, -100 , xlab="year", ylab="LRS of males", xlim=c(min(yr), max(yr)), ylim=c(0, max(LRSm)))
    points(yr, LRSm, col=gt.cols[col], pch=16)
    
    plot(-100, -100 , xlab="year", ylab="SD of LRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(SD)))
    points(yr, SD, col=gt.cols[col])
    
    plot(-100, -100 , xlab="year", ylab="SD of RRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(SDR)))
    points(yr, SDR, col=gt.cols[col])
    
    plot(-100, -100 , xlab="year", ylab="LRS of migrants", xlim=c(min(yr), max(yr)), ylim=c(0, 10)) #max(LRSmig)
    points(yr, LRSmig, col=gt.cols[col])
    
    plot(-100, -100 , xlab="year", ylab="LRS of natives", xlim=c(min(yr), max(yr)), ylim=c(0, 5)) #max(LRSnat)
    points(yr, LRSnat, col=gt.cols[col])
    
    plot(-100, -100 , xlab="year", ylab="LRS of migrants/natives", xlim=c(min(yr), max(yr)), ylim=c(0, 5)) #max(LRSnat)
    points(yr, LRSmig/LRSmig, col=gt.cols[col], pch=16)
  }
}
##### Generic plotting code
{
  #1=yr, 2=nborn, 3=meanLRS, 4=SD, 5=LRSFemale, 6=LRSmale, 7=meanRRS, 8=SDRRS, 9=replicate, 10=parameterset, 11=LRSmigrants, 12=LRSnatives, 13=project, 14=group
  var = 7
  
  range(rep[,var])
  if(anyNA(rep[,var]==TRUE)){
    hold<- na.omit(rep)
    rep <- hold
  }
  
  gt.cols <- qualitative_hcl(8, "Dark2") #ghibli_palette('PonyoMedium')#[4]
  lt.gt.col <- qualitative_hcl(8, "Pastel1")
  
  
  ymin <- round(min(rep[,var]), digits = 2)#-.1
  ymax <- round(max(rep[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.5, 0, 0.5, 0.1) #c(-0.2, -0.1, 0, 0.1, 0.2)
  orig.xs <- c(1, 100, 150, 201, 250, 300, 345) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 2
  
  ## make plot
  plot(0,0, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', main = 'RRS with maxage=3', xlab = 'Generation Time', ylab = 'RRS',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  #axis(2, at = c(-0.1, 0, 0.1, 0.2), cex.axis = text.size)
  axis(1, at = c(0, 100, 150, 200, 250, 300, 350), labels = c('0','100','150','200', '250', '300', '350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(rep[,10])){
    #print(c)
    temp <- rep[rep[,10] == c,, drop=FALSE] #separate by parameter set
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #columns <- c(18, 19, 20, 21)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    arrows(x0 = xs, y0 = c(mean(y1[,var])-sd(y1[,var]), mean(y2[,var])-sd(y2[,var]), mean(y3[,var])-sd(y3[,var]), mean(y4[,var])-sd(y4[,var]), mean(y5[,var])-sd(y5[,var]), mean(y6[,var])-sd(y6[,var]), mean(y7[,var])-sd(y7[,var])), 
           y1 = c(mean(y1[,var])+sd(y1[,var]), mean(y2[,var])+sd(y2[,var]), mean(y3[,var])+sd(y3[,var]), mean(y4[,var])+sd(y4[,var]), mean(y5[,var])+sd(y5[,var]), mean(y6[,var])+sd(y6[,var]), mean(y7[,var])+sd(y7[,var])), 
           lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    #  for(l in unique(orig.xs)){
    #    column <- columns[l]
    #    ## 95% CIs (inappropriate for large sample sizes)
    #    # arrows(x0 = xs[l], x1 = xs[l], y0 = (mean(temp[,column], na.rm = TRUE) - (sd(temp[,column], na.rm = TRUE)/10*1.96)),
    #    #        y1 = (mean(temp[,column], na.rm = TRUE) + (sd(temp[,column], na.rm = TRUE)/10*1.96)),
    #    #        lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    #  arrows(x0 = OTime[,1], y0 = (OTime[,2])-(sd(OTime[,7])), y1 = (OTime[,2])+(sd(OTime[,7])), lwd = .5, col = "black", code=3, angle=90, length=0.1)
    
    #    
    #    arrows(x0 = xs[l], x1 = xs[l], y0 = quantile(temp[,column], probs = c(0.025,0.975))[1],   #will need to do this for each year of interest
    #           y1 = quantile(temp[,column], probs = c(0.025,0.975))[2],
    #           lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    #  }    
    col <- col+1
  }
  
  legend('topleft', legend = c('mig=0', 'mig=3x,25indv@y=175','high source het','low source het'), col = gt.cols, pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.2)
  
  ##NEED TO FIGURE OUT A WAY TO DO A POLYGON/GO ACROSS ALL YEARS
}

#############################################################################3
#############################################################################
n30 = read.table("fin_5.10.23_1LL30_all_necounts.csv", header=T, sep=",")
n3a = read.table("fin_5.10.23_1LL3a_all_necounts.csv", header=T, sep=",")
n3b = read.table("fin_5.10.23_1LL3b_all_necounts.csv", header=T, sep=",")
n3c = read.table("fin_5.10.23_1LL3c_all_necounts.csv", header=T, sep=",")

ne = rbind(n30,n3a,n3b,n3c)

library(colorspace)
gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(6, "Pastel1") #ghibli_palette('PonyoLight')[4]

col = ne[,9] 

{
  library(lme4)
  y<-lmer(Ho~propmig + (1|parameterset), data=smry)
  
  max(ne[,3])
  
  plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 200))
  points(ne[,1], ne[,2], col="pink")  #effective moms
  points(ne[,1], ne[,3], col="blue")  #effective dads
  
  plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 600))
  points(ne[,1], ne[,6], col="pink")  #possible moms
  points(ne[,1], ne[,7], col="blue")  #possible dads
  
  plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 30))
  points(ne[,1], ne[,8], col="pink")  #effective migrants
  
  for(i in 1:nrow(ne)){
    NE <- (4*ne[i,2]*ne[i,3])/(ne[i,2]+ne[i,3])           #Ne = 4*number males*number females/number males+number females
    ne[i,10] <- NE
  }
  plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 300))
  points(ne[,1], ne[,10], col="pink")
  points(ne[,1], ne[,8], col="purple")
  
  for(j in 1:nrow(ne)){
    FST <- 1/(1+(4*ne[j,10]*ne[j,8]))                     #1/(1+4*effective pop size*[effective] migrants) 
    ne[j,11] <- FST
  }
  plot(-100, -100 , xlab="year", ylab="1/1+(4*Ne*Nm)", xlim=c(0, 350), ylim=c(0, .03))
  points(ne[,1], ne[,11], col= gt.cols[col])
  
  
  
  smry2 <- smry[smry[,12]==2,,drop=FALSE]
  smry2<-smry2[smry2[,1]!=0,,drop=FALSE]
  ne2 <- ne[ne[,9]==2,,drop=FALSE]
  plot(-100, -100 , xlab="year", ylab="FST", xlim=c(0, 350), ylim=c(0, .02))
  points(ne2[,1], ne2[,11], col= "orange")
  
  
  for(e in 1:nrow(ne)){
    RAT <- ne[e,10]/ne[e,5]                                #ratio Ne/Nc == small Ne relative to Nc (that is, small Ne/Nc ratio) will lose gene diversity more quickly 
    ne[e,12] <- RAT
  }
  plot(-100, -100 , xlab="year", ylab="ratio Ne/Nc", xlim=c(0, 350), ylim=c(0, 1))
  points(ne[,1], ne[,12], col= gt.cols[col])
  
  
  
  bb[,12]=2
  cc[,12]=3
  smry = rbind(aa,bb,cc)
  smry = smry[smry[,1]!=0,,drop=FALSE]
  data = cbind(ne, smry)
  head(data)
  
  plot(-100, -100 , xlab="Fst", ylab="Ne = 4*number males*number females/number males+number females", xlim=c(0, 275), ylim=c(0, 1))
  plot(data[,10]~data[,22])  #Ne = 4*number males*number females/number males+number females   ~  Fst (calc in sim)
  plot(data[,11]~data[,22])  #1/(1+4*effective pop size*[effective] migrants) 
  plot(data[,12]~data[,22])    #ratio Ne/Nc    <-- interestinggg
}

#############################################################################
##LMM
#############################################################################
y99 <- smry[smry[,1]==99,,drop=FALSE]
y150 <- smry[smry[,1]==150,,drop=FALSE]
y151 <- smry[smry[,1]==151,,drop=FALSE]
y350 <- smry[smry[,1]==350,,drop=FALSE]

#1=yr, 2=pop size, 3=propmig, 4=He, 5=Ho[driftSNPS], 6=fis, 7=nadult, 8=sxratio, 9=nmig, 10=fst, 11=replicate, 12=paramset, 13=noffspring, 14=fstvsource, 15=fisvsource,
#16=deltaK, 17=propMigSNPs, 18=HoallSNPs, 19=projectname, 20=groupnumb, 21=k, 22=nSNP, 23=miggy, 24=LBhet, 25=LBp, 26=maxage, 27=broodsize, 28=maturity,
#29=years, 30=r0, 31=nSNP.mig, 32=nSNP.cons

library(lme4)
reg1 <- lmer(y99[,5]~y99[,17] + y99[,19] + (1|y99[,20]), data=y99)
summary(reg1)