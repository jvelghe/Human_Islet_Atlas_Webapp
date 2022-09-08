library(shiny)
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
library(ggplot2) 
library(ggrepel) 
library(hdf5r) 
library(ggdendro) 
library(gridExtra) 
library(shinydashboard)
hisletconf = readRDS("hisletconf.rds")
hisletdef  = readRDS("hisletdef.rds")
hisletgene = readRDS("hisletgene.rds")
hisletmeta = readRDS("hisletmeta.rds")



endoconf = readRDS("endoconf.rds")
endodef  = readRDS("endodef.rds")
endogene = readRDS("endogene.rds")
endometa = readRDS("endometa.rds")



alphaconf = readRDS("alphaconf.rds")
alphadef  = readRDS("alphadef.rds")
alphagene = readRDS("alphagene.rds")
alphameta = readRDS("alphameta.rds")



betaconf = readRDS("betaconf.rds")
betadef  = readRDS("betadef.rds")
betagene = readRDS("betagene.rds")
betameta = readRDS("betameta.rds")



deltaconf = readRDS("deltaconf.rds")
deltadef  = readRDS("deltadef.rds")
deltagene = readRDS("deltagene.rds")
deltameta = readRDS("deltameta.rds")



dpconf = readRDS("dpconf.rds")
dpdef  = readRDS("dpdef.rds")
dpgene = readRDS("dpgene.rds")
dpmeta = readRDS("dpmeta.rds")



### Useful stuff 
# Colour palette 
cList = list(c("#f0f0f0", "#3700ff"), 
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF", 
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)], 
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C", 
               "#2C728E","#3B528B","#472D7B","#440154")) 
names(cList) = c("Purple Gradient", "Blue-Yellow-Red", "Yellow-Green-Purple") 
 
# Panel sizes 
pList = c("400px", "600px", "800px") 
names(pList) = c("Small", "Medium", "Large") 
pList2 = c("500px", "700px", "900px") 
names(pList2) = c("Small", "Medium", "Large") 
pList3 = c("600px", "800px", "1000px") 
names(pList3) = c("Small", "Medium", "Large") 
sList = c(18,24,30) 
names(sList) = c("Small", "Medium", "Large") 
lList = c(5,6,7) 
names(lList) = c("Small", "Medium", "Large") 
 
# Function to extract legend 
g_legend <- function(a.gplot){  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")  
  legend <- tmp$grobs[[leg]]  
  legend 
}  
 
# Plot theme 
sctheme <- function(base_size = 24, XYval = TRUE, Xang = 0, XjusH = 0.5){ 
  oupTheme = theme( 
    text =             element_text(size = base_size, family = "Helvetica"), 
    panel.background = element_rect(fill = "white", colour = NA), 
    axis.line =   element_line(colour = "black"), 
    axis.ticks =  element_line(colour = "black", size = base_size / 20), 
    axis.title =  element_text(face = "bold"), 
    axis.text =   element_text(size = base_size), 
    axis.text.x = element_text(angle = Xang, hjust = XjusH), 
    legend.position = "bottom", 
    legend.key =      element_rect(colour = NA, fill = NA) 
  ) 
  if(!XYval){ 
    oupTheme = oupTheme + theme( 
      axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
      axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
  } 
  return(oupTheme) 
} 
 
### Common plotting functions 
# Plot cell information on dimred 
scDRcell <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt, inplab){ 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, 
                       inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),  
                   with = FALSE] 
  colnames(ggData) = c("X", "Y", "val", "sub") 
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
  bgCells = FALSE 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    bgCells = TRUE 
    ggData2 = ggData[!sub %in% inpsub2] 
    ggData = ggData[sub %in% inpsub2] 
  } 
  if(inpord == "Max-1st"){ 
    ggData = ggData[order(val)] 
  } else if(inpord == "Min-1st"){ 
    ggData = ggData[order(-val)] 
  } else if(inpord == "Random"){ 
    ggData = ggData[sample(nrow(ggData))] 
  } 
  
  # Do factoring if required 
  if(!is.na(inpConf[UI == inp1]$fCL)){ 
    ggCol = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]] 
    names(ggCol) = levels(ggData$val) 
    ggLvl = levels(ggData$val)[levels(ggData$val) %in% unique(ggData$val)] 
    ggData$val = factor(ggData$val, levels = ggLvl) 
    ggCol = ggCol[ggLvl] 
  } 
 
  # Actual ggplot 
  ggOut = ggplot(ggData, aes(X, Y, color = val)) 
  if(bgCells){ 
    ggOut = ggOut + 
      geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16) 
  } 
  ggOut = ggOut + 
    geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) + 
    sctheme(base_size = sList[inpfsz], XYval = inptxt) 
  if(is.na(inpConf[UI == inp1]$fCL)){ 
    ggOut = ggOut + scale_color_gradientn("", colours = cList[[inpcol]]) + 
      guides(color = guide_colorbar(barwidth = 15)) 
  } else { 
    sListX = min(nchar(paste0(levels(ggData$val), collapse = "")), 200) 
    sListX = 0.75 * (sList - (1.5 * floor(sListX/50))) 
    ggOut = ggOut + scale_color_manual("", values = ggCol) + 
      guides(color = guide_legend(override.aes = list(size = 5),  
                                  nrow = inpConf[UI == inp1]$fRow)) + 
      theme(legend.text = element_text(size = sListX[inpfsz])) 
    if(inplab){ 
      ggData3 = ggData[, .(X = mean(X), Y = mean(Y)), by = "val"] 
      lListX = min(nchar(paste0(ggData3$val, collapse = "")), 200) 
      lListX = lList - (0.25 * floor(lListX/50)) 
      ggOut = ggOut + 
        geom_text_repel(data = ggData3, aes(X, Y, label = val), 
                        color = "grey10", bg.color = "grey95", bg.r = 0.15, 
                        size = lListX[inpfsz], seed = 42) 
    } 
  } 
  if(inpasp == "Square") { 
    ggOut = ggOut + coord_fixed(ratio = rat) 
  } else if(inpasp == "Fixed") { 
    ggOut = ggOut + coord_fixed() 
  } 
  return(ggOut) 
} 
 
scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, 
                    inpH5, inpGene, inpsplt){ 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID), 
                   with = FALSE] 
  colnames(ggData) = c("group", "sub") 
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  h5file$close_all() 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Split inp1 if necessary 
  if(is.na(inpConf[UI == inp1]$fCL)){ 
    if(inpsplt == "Quartile"){nBk = 4} 
    if(inpsplt == "Decile"){nBk = 10} 
    ggData$group = cut(ggData$group, breaks = nBk) 
  } 
  
  # Actual data.table 
  ggData$express = FALSE 
  ggData[val2 > 0]$express = TRUE 
  ggData1 = ggData[express == TRUE, .(nExpress = .N), by = "group"] 
  ggData = ggData[, .(nCells = .N), by = "group"] 
  ggData = ggData1[ggData, on = "group"] 
  ggData = ggData[, c("group", "nCells", "nExpress"), with = FALSE] 
  ggData[is.na(nExpress)]$nExpress = 0 
  ggData$pctExpress = 100 * ggData$nExpress / ggData$nCells 
  ggData = ggData[order(group)] 
  colnames(ggData)[3] = paste0(colnames(ggData)[3], "_", inp2) 
  return(ggData) 
} 
# Plot gene expression on dimred 
scDRgene <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2, 
                     inpH5, inpGene, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt){ 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, 
                       inpConf[UI == inpsub1]$ID),  
                   with = FALSE] 
  colnames(ggData) = c("X", "Y", "sub") 
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
  
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val = h5data$read(args = list(inpGene[inp1], quote(expr=))) 
  ggData[val < 0]$val = 0 
  h5file$close_all() 
  bgCells = FALSE 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    bgCells = TRUE 
    ggData2 = ggData[!sub %in% inpsub2] 
    ggData = ggData[sub %in% inpsub2] 
  } 
  if(inpord == "Max-1st"){ 
    ggData = ggData[order(val)] 
  } else if(inpord == "Min-1st"){ 
    ggData = ggData[order(-val)] 
  } else if(inpord == "Random"){ 
    ggData = ggData[sample(nrow(ggData))] 
  } 
   
  # Actual ggplot 
  ggOut = ggplot(ggData, aes(X, Y, color = val)) 
  if(bgCells){ 
    ggOut = ggOut + 
      geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16) 
  } 
  ggOut = ggOut + 
    geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) + 
    sctheme(base_size = sList[inpfsz], XYval = inptxt) +  
    scale_color_gradientn(inp1, colours = cList[[inpcol]]) + 
      guides(color = guide_colorbar(barwidth = 15)) 
  if(inpasp == "Square") { 
    ggOut = ggOut + coord_fixed(ratio = rat) 
  } else if(inpasp == "Fixed") { 
    ggOut = ggOut + coord_fixed() 
  } 
  return(ggOut) 
} 
 
# Plot gene coexpression on dimred 
bilinear <- function(x,y,xy,Q11,Q21,Q12,Q22){ 
  oup = (xy-x)*(xy-y)*Q11 + x*(xy-y)*Q21 + (xy-x)*y*Q12 + x*y*Q22 
  oup = oup / (xy*xy) 
  return(oup) 
} 
scDRcoex <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inp2, 
                     inpsub1, inpsub2, inpH5, inpGene, 
                     inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt){ 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, 
                       inpConf[UI == inpsub1]$ID),  
                   with = FALSE] 
  colnames(ggData) = c("X", "Y", "sub") 
  rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y)) 
  
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val1 = h5data$read(args = list(inpGene[inp1], quote(expr=))) 
  ggData[val1 < 0]$val1 = 0 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  h5file$close_all() 
  bgCells = FALSE 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    bgCells = TRUE 
    ggData2 = ggData[!sub %in% inpsub2] 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Generate coex color palette 
  cInp = strsplit(inpcol, "; ")[[1]] 
  if(cInp[1] == "Red (Gene1)"){ 
    c10 = c(255,0,0) 
  } else if(cInp[1] == "Orange (Gene1)"){ 
    c10 = c(255,140,0) 
  } else { 
    c10 = c(0,255,0) 
  } 
  if(cInp[2] == "Green (Gene2)"){ 
    c01 = c(0,255,0) 
  } else { 
    c01 = c(0,0,255) 
  } 
  c00 = c(217,217,217) ; c11 = c10 + c01 
  nGrid = 12; nPadLow = 7; nPadHigh = 1; nTot = nGrid + nPadLow + nPadHigh
  gg = data.table(v1 = rep(0:nTot,nTot+1), v2 = sort(rep(0:nTot,nTot+1))) 
  gg$vv1 = gg$v1 - nPadLow ; gg[vv1 < 0]$vv1 = 0; gg[vv1 > nGrid]$vv1 = nGrid 
  gg$vv2 = gg$v2 - nPadLow ; gg[vv2 < 0]$vv2 = 0; gg[vv2 > nGrid]$vv2 = nGrid 
  gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1]) 
  gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2]) 
  gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3]) 
  gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255) 
  gg = gg[, c("v1", "v2", "cMix")] 
  
  # Map colours 
  ggData$v1 = round(nTot * ggData$val1 / max(ggData$val1)) 
  ggData$v2 = round(nTot * ggData$val2 / max(ggData$val2)) 
  ggData$v0 = ggData$v1 + ggData$v2 
  ggData = gg[ggData, on = c("v1", "v2")] 
  if(inpord == "Max-1st"){ 
    ggData = ggData[order(v0)] 
  } else if(inpord == "Min-1st"){ 
    ggData = ggData[order(-v0)] 
  } else if(inpord == "Random"){ 
    ggData = ggData[sample(nrow(ggData))] 
  } 
  
  # Actual ggplot 
  ggOut = ggplot(ggData, aes(X, Y)) 
  if(bgCells){ 
    ggOut = ggOut + 
      geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16) 
  } 
  ggOut = ggOut + 
    geom_point(size = inpsiz, shape = 20, color = ggData$cMix) + 
    xlab(inpdrX) + ylab(inpdrY) + 
    sctheme(base_size = sList[inpfsz], XYval = inptxt) + 
    scale_color_gradientn(inp1, colours = cList[[1]]) + 
    guides(color = guide_colorbar(barwidth = 15)) 
  if(inpasp == "Square") { 
    ggOut = ggOut + coord_fixed(ratio = rat) 
  } else if(inpasp == "Fixed") { 
    ggOut = ggOut + coord_fixed() 
  } 
  return(ggOut) 
} 
 
scDRcoexLeg <- function(inp1, inp2, inpcol, inpfsz){ 
  # Generate coex color palette 
  cInp = strsplit(inpcol, "; ")[[1]] 
  if(cInp[1] == "Red (Gene1)"){ 
    c10 = c(255,0,0) 
  } else if(cInp[1] == "Orange (Gene1)"){ 
    c10 = c(255,140,0) 
  } else { 
    c10 = c(0,255,0) 
  } 
  if(cInp[2] == "Green (Gene2)"){ 
    c01 = c(0,255,0) 
  } else { 
    c01 = c(0,0,255) 
  } 
  c00 = c(217,217,217) ; c11 = c10 + c01 
  nGrid = 12; nPadLow = 7; nPadHigh = 1; nTot = nGrid + nPadLow + nPadHigh
  gg = data.table(v1 = rep(0:nTot,nTot+1), v2 = sort(rep(0:nTot,nTot+1))) 
  gg$vv1 = gg$v1 - nPadLow ; gg[vv1 < 0]$vv1 = 0; gg[vv1 > nGrid]$vv1 = nGrid 
  gg$vv2 = gg$v2 - nPadLow ; gg[vv2 < 0]$vv2 = 0; gg[vv2 > nGrid]$vv2 = nGrid 
  gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1]) 
  gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2]) 
  gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3]) 
  gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255) 
  gg = gg[, c("v1", "v2", "cMix")] 
  
  # Actual ggplot 
  ggOut = ggplot(gg, aes(v1, v2)) + 
    geom_tile(fill = gg$cMix) + 
    xlab(inp1) + ylab(inp2) + coord_fixed(ratio = 1) + 
    scale_x_continuous(breaks = c(0, nTot), label = c("low", "high")) + 
    scale_y_continuous(breaks = c(0, nTot), label = c("low", "high")) + 
    sctheme(base_size = sList[inpfsz], XYval = TRUE) 
  return(ggOut) 
} 
 
scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2, 
                        inpsub1, inpsub2, inpH5, inpGene){ 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE] 
  colnames(ggData) = c("sub") 
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val1 = h5data$read(args = list(inpGene[inp1], quote(expr=))) 
  ggData[val1 < 0]$val1 = 0 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  h5file$close_all() 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Actual data.table 
  ggData$express = "none" 
  ggData[val1 > 0]$express = inp1 
  ggData[val2 > 0]$express = inp2 
  ggData[val1 > 0 & val2 > 0]$express = "both" 
  ggData$express = factor(ggData$express, levels = unique(c("both", inp1, inp2, "none"))) 
  ggData = ggData[, .(nCells = .N), by = "express"] 
  ggData$percent = 100 * ggData$nCells / sum(ggData$nCells) 
  ggData = ggData[order(express)] 
  colnames(ggData)[1] = "expression > 0"
  return(ggData) 
} 
 
# Plot violin / boxplot 
scVioBox <- function(inpConf, inpMeta, inp1, inp2, inpH5, inpGene, 
                     inptyp, inppts, inpsiz, inpfsz){ 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[grp == TRUE]$ID),  
                   with = FALSE] 
  colnames(ggData)[1] = c("X") 
  
  # Load in either cell meta or gene expr
  if(inp2 %in% inpConf$UI){ 
    ggData$val = inpMeta[[inpConf[UI == inp2]$ID]] 
  } else { 
    h5file <- H5File$new(inpH5, mode = "r") 
    h5data <- h5file[["grp"]][["data"]] 
    ggData$val = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
    ggData[val < 0]$val = 0 
    set.seed(42) 
    tmpNoise = rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000 
    ggData$val = ggData$val + tmpNoise 
    h5file$close_all() 
  } 
  
  # Do factoring 
  ggCol = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]] 
  names(ggCol) = levels(ggData$X) 
  ggLvl = levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)] 
  ggData$X = factor(ggData$X, levels = ggLvl) 
  ggCol = ggCol[ggLvl] 
  
  # Actual ggplot 
  if(inptyp == "violin"){ 
    ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_violin(scale = "width") 
  } else { 
    ggOut = ggplot(ggData, aes(X, val, fill = X)) + geom_boxplot() 
  } 
  if(inppts){ 
    ggOut = ggOut + geom_jitter(size = inpsiz, shape = 16) 
  } 
  ggOut = ggOut + xlab(inp1) + ylab(inp2) + 
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +  
    scale_fill_manual("", values = ggCol) +
    theme(legend.position = "none")
  return(ggOut) 
} 
 
# Plot proportion plot 
scProp <- function(inpConf, inpMeta, inp1, inp2,  
                   inptyp, inpflp, inpfsz){ 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inp2]$ID),  
                   with = FALSE] 
  colnames(ggData) = c("X", "grp") 
  ggData = ggData[, .(nCells = .N), by = c("X", "grp")] 
  ggData = ggData[, {tot = sum(nCells) 
                      .SD[,.(pctCells = 100 * sum(nCells) / tot, 
                             nCells = nCells), by = "grp"]}, by = "X"] 
  
  # Do factoring 
  ggCol = strsplit(inpConf[UI == inp2]$fCL, "\\|")[[1]] 
  names(ggCol) = levels(ggData$grp) 
  ggLvl = levels(ggData$grp)[levels(ggData$grp) %in% unique(ggData$grp)] 
  ggData$grp = factor(ggData$grp, levels = ggLvl) 
  ggCol = ggCol[ggLvl] 
  
  # Actual ggplot 
  if(inptyp == "Proportion"){ 
    ggOut = ggplot(ggData, aes(X, pctCells, fill = grp)) + 
      geom_col() + ylab("Cell Proportion (%)") 
  } else { 
    ggOut = ggplot(ggData, aes(X, nCells, fill = grp)) + 
      geom_col() + ylab("Number of Cells") 
  } 
  if(inpflp){ 
    ggOut = ggOut + coord_flip() 
  } 
  ggOut = ggOut + xlab(inp1) + 
    sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +  
    scale_fill_manual("", values = ggCol) + 
    theme(legend.position = "right") 
  return(ggOut) 
} 
 
# Get gene list 
scGeneList <- function(inp, inpGene){ 
  geneList = data.table(gene = unique(trimws(strsplit(inp, ",|;|
")[[1]])), 
                        present = TRUE) 
  geneList[!gene %in% names(inpGene)]$present = FALSE 
  return(geneList) 
} 
 
# Plot gene expression bubbleplot / heatmap 
scBubbHeat <- function(inpConf, inpMeta, inp, inpGrp, inpPlt,  
                       inpH5, inpGene, inpScl, inpRow, inpCol,  
                       inpcols, inpfsz, save = FALSE){ 
  # Identify genes that are in our dataset 
  geneList = scGeneList(inp, inpGene) 
  geneList = geneList[present == TRUE] 
  shiny::validate(need(nrow(geneList) <= 50, "More than 50 genes to plot! Please reduce the gene list!")) 
  shiny::validate(need(nrow(geneList) > 1, "Please input at least 2 genes to plot!")) 
   
  # Prepare ggData 
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData = data.table() 
  for(iGene in geneList$gene){ 
    tmp = inpMeta[, c("sampleID", inpConf[grp == TRUE]$ID), with = FALSE] 
    tmp$grpBy = inpMeta[[inpConf[UI == inpGrp]$ID]] 
    tmp$geneName = iGene 
    tmp$val = h5data$read(args = list(inpGene[iGene], quote(expr=))) 
    ggData = rbindlist(list(ggData, tmp)) 
  } 
  h5file$close_all() 
   
  # Aggregate 
  ggData$val = expm1(ggData$val) 
  ggData = ggData[, .(val = mean(val), prop = sum(val>0) / length(sampleID)), 
                  by = c("geneName", "grpBy")] 
  ggData$val = log1p(ggData$val) 
   
  # Scale if required 
  colRange = range(ggData$val) 
  if(inpScl){ 
    ggData[, val:= scale(val), keyby = "geneName"] 
    colRange = c(-max(abs(range(ggData$val))), max(abs(range(ggData$val)))) 
  } 
   
  # hclust row/col if necessary 
  ggMat = dcast.data.table(ggData, geneName~grpBy, value.var = "val") 
  tmp = ggMat$geneName 
  ggMat = as.matrix(ggMat[, -1]) 
  rownames(ggMat) = tmp 
  if(inpRow){ 
    hcRow = dendro_data(as.dendrogram(hclust(dist(ggMat)))) 
    ggRow = ggplot() + coord_flip() + 
      geom_segment(data = hcRow$segments, aes(x=x,y=y,xend=xend,yend=yend)) + 
      scale_y_continuous(breaks = rep(0, uniqueN(ggData$grpBy)), 
                         labels = unique(ggData$grpBy), expand = c(0, 0)) + 
      scale_x_continuous(breaks = seq_along(hcRow$labels$label), 
                         labels = hcRow$labels$label, expand = c(0, 0.5)) + 
      sctheme(base_size = sList[inpfsz]) + 
      theme(axis.title = element_blank(), axis.line = element_blank(), 
            axis.ticks = element_blank(), axis.text.y = element_blank(), 
            axis.text.x = element_text(color="white", angle = 45, hjust = 1)) 
    ggData$geneName = factor(ggData$geneName, levels = hcRow$labels$label) 
  } else { 
    ggData$geneName = factor(ggData$geneName, levels = rev(geneList$gene)) 
  } 
  if(inpCol){ 
    hcCol = dendro_data(as.dendrogram(hclust(dist(t(ggMat))))) 
    ggCol = ggplot() + 
      geom_segment(data = hcCol$segments, aes(x=x,y=y,xend=xend,yend=yend)) + 
      scale_x_continuous(breaks = seq_along(hcCol$labels$label), 
                         labels = hcCol$labels$label, expand = c(0.05, 0)) + 
      scale_y_continuous(breaks = rep(0, uniqueN(ggData$geneName)), 
                         labels = unique(ggData$geneName), expand=c(0,0)) + 
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) + 
      theme(axis.title = element_blank(), axis.line = element_blank(), 
            axis.ticks = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_text(color = "white")) 
    ggData$grpBy = factor(ggData$grpBy, levels = hcCol$labels$label) 
  } 
   
  # Actual plot according to plottype 
  if(inpPlt == "Bubbleplot"){ 
    # Bubbleplot 
    ggOut = ggplot(ggData, aes(grpBy, geneName, color = val, size = prop)) + 
      geom_point() +  
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) +  
      scale_x_discrete(expand = c(0.05, 0)) +  
      scale_y_discrete(expand = c(0, 0.5)) + 
      scale_size_continuous("proportion", range = c(0, 8), 
                            limits = c(0, 1), breaks = c(0.00,0.25,0.50,0.75,1.00)) + 
      scale_color_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) + 
      guides(color = guide_colorbar(barwidth = 15)) + 
      theme(axis.title = element_blank(), legend.box = "vertical") 
  } else { 
    # Heatmap 
    ggOut = ggplot(ggData, aes(grpBy, geneName, fill = val)) + 
      geom_tile() +  
      sctheme(base_size = sList[inpfsz], Xang = 45, XjusH = 1) + 
      scale_x_discrete(expand = c(0.05, 0)) +  
      scale_y_discrete(expand = c(0, 0.5)) + 
      scale_fill_gradientn("expression", limits = colRange, colours = cList[[inpcols]]) + 
      guides(fill = guide_colorbar(barwidth = 15)) + 
      theme(axis.title = element_blank()) 
  } 
     
  # Final tidy 
  ggLeg = g_legend(ggOut) 
  ggOut = ggOut + theme(legend.position = "none") 
  if(!save){ 
    if(inpRow & inpCol){ggOut =  
      grid.arrange(ggOut, ggLeg, ggCol, ggRow, widths = c(7,1), heights = c(1,7,2),  
                   layout_matrix = rbind(c(3,NA),c(1,4),c(2,NA)))  
    } else if(inpRow){ggOut =  
      grid.arrange(ggOut, ggLeg, ggRow, widths = c(7,1), heights = c(7,2),  
                   layout_matrix = rbind(c(1,3),c(2,NA)))  
    } else if(inpCol){ggOut =  
      grid.arrange(ggOut, ggLeg, ggCol, heights = c(1,7,2),  
                   layout_matrix = rbind(c(3),c(1),c(2)))  
    } else {ggOut =  
      grid.arrange(ggOut, ggLeg, heights = c(7,2),  
                   layout_matrix = rbind(c(1),c(2)))  
    }  
  } else { 
    if(inpRow & inpCol){ggOut =  
      arrangeGrob(ggOut, ggLeg, ggCol, ggRow, widths = c(7,1), heights = c(1,7,2),  
                  layout_matrix = rbind(c(3,NA),c(1,4),c(2,NA)))  
    } else if(inpRow){ggOut =  
      arrangeGrob(ggOut, ggLeg, ggRow, widths = c(7,1), heights = c(7,2),  
                  layout_matrix = rbind(c(1,3),c(2,NA)))  
    } else if(inpCol){ggOut =  
      arrangeGrob(ggOut, ggLeg, ggCol, heights = c(1,7,2),  
                  layout_matrix = rbind(c(3),c(1),c(2)))  
    } else {ggOut =  
      arrangeGrob(ggOut, ggLeg, heights = c(7,2),  
                  layout_matrix = rbind(c(1),c(2)))  
    }  
  } 
  return(ggOut) 
} 
 
 
 
 
 
### Start server code 
shinyServer(function(input, output, session) { 
  ### For all tags and Server-side selectize 
  observe_helpers() 
   optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  updateSelectizeInput(session, "hisleta1inp2", choices = sort(names(hisletgene)), server = TRUE, 
                       selected = hisletdef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "hisleta3inp1", choices = sort(names(hisletgene)), server = TRUE, 
                       selected = hisletdef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "hisleta3inp2", choices = sort(names(hisletgene)), server = TRUE, 
                       selected = hisletdef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "hisletb2inp1", choices = sort(names(hisletgene)), server = TRUE, 
                       selected = hisletdef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "hisletb2inp2", choices = sort(names(hisletgene)), server = TRUE, 
                       selected = hisletdef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "hisletc1inp2", server = TRUE, 
                       choices = c(hisletconf[is.na(fID)]$UI,sort(names(hisletgene))), 
                       selected = hisletconf[is.na(fID)]$UI[1], options = list( 
                         maxOptions = length(hisletconf[is.na(fID)]$UI) + 3, 
                         create = TRUE, persist = TRUE, render = I(optCrt))) 
 
  observeEvent(input$link_to_navbarpage_hislet,{
    updateNavbarPage(session, "page", "Hislet")
  })
  observeEvent(input$link_to_navbar_endo, {
    updateNavbarPage(session, "page", "Endo")
  })
  
  ### Plots for tab a1 
  output$hisleta1sub1.ui <- renderUI({ 
    sub = strsplit(hisletconf[UI == input$hisleta1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("hisleta1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$hisleta1oup1 <- renderPlot({ 
    scDRcell(hisletconf, hisletmeta, input$hisleta1drX, input$hisleta1drY, input$hisleta1inp1,  
             input$hisleta1sub1, input$hisleta1sub2, 
             input$hisleta1siz, input$hisleta1col1, input$hisleta1ord1, 
             input$hisleta1fsz, input$hisleta1asp, input$hisleta1txt, input$hisleta1lab1) 
  }) 
  output$hisleta1oup1.ui <- renderUI({ 
    plotOutput("hisleta1oup1", height = pList[input$hisleta1psz]) 
  }) 
  output$hisleta1oup1.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta1drX,"_",input$hisleta1drY,"_",  
                                   input$hisleta1inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisleta1oup1.h, width = input$hisleta1oup1.w, useDingbats = FALSE, 
      plot = scDRcell(hisletconf, hisletmeta, input$hisleta1drX, input$hisleta1drY, input$hisleta1inp1,   
                      input$hisleta1sub1, input$hisleta1sub2, 
                      input$hisleta1siz, input$hisleta1col1, input$hisleta1ord1,  
                      input$hisleta1fsz, input$hisleta1asp, input$hisleta1txt, input$hisleta1lab1) ) 
  }) 
  output$hisleta1oup1.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta1drX,"_",input$hisleta1drY,"_",  
                                   input$hisleta1inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisleta1oup1.h, width = input$hisleta1oup1.w, 
      plot = scDRcell(hisletconf, hisletmeta, input$hisleta1drX, input$hisleta1drY, input$hisleta1inp1,   
                      input$hisleta1sub1, input$hisleta1sub2, 
                      input$hisleta1siz, input$hisleta1col1, input$hisleta1ord1,  
                      input$hisleta1fsz, input$hisleta1asp, input$hisleta1txt, input$hisleta1lab1) ) 
  }) 
  output$hisleta1.dt <- renderDataTable({ 
    ggData = scDRnum(hisletconf, hisletmeta, input$hisleta1inp1, input$hisleta1inp2, 
                     input$hisleta1sub1, input$hisleta1sub2, 
                     "hisletgexpr.h5", hisletgene, input$hisleta1splt) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("pctExpress"), digits = 2) 
  }) 
   
  output$hisleta1oup2 <- renderPlot({ 
    scDRgene(hisletconf, hisletmeta, input$hisleta1drX, input$hisleta1drY, input$hisleta1inp2,  
             input$hisleta1sub1, input$hisleta1sub2, 
             "hisletgexpr.h5", hisletgene, 
             input$hisleta1siz, input$hisleta1col2, input$hisleta1ord2, 
             input$hisleta1fsz, input$hisleta1asp, input$hisleta1txt) 
  }) 
  output$hisleta1oup2.ui <- renderUI({ 
    plotOutput("hisleta1oup2", height = pList[input$hisleta1psz]) 
  }) 
  output$hisleta1oup2.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta1drX,"_",input$hisleta1drY,"_",  
                                   input$hisleta1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisleta1oup2.h, width = input$hisleta1oup2.w, useDingbats = FALSE, 
      plot = scDRgene(hisletconf, hisletmeta, input$hisleta1drX, input$hisleta1drY, input$hisleta1inp2,  
                      input$hisleta1sub1, input$hisleta1sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisleta1siz, input$hisleta1col2, input$hisleta1ord2, 
                      input$hisleta1fsz, input$hisleta1asp, input$hisleta1txt) ) 
  }) 
  output$hisleta1oup2.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta1drX,"_",input$hisleta1drY,"_",  
                                   input$hisleta1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisleta1oup2.h, width = input$hisleta1oup2.w, 
      plot = scDRgene(hisletconf, hisletmeta, input$hisleta1drX, input$hisleta1drY, input$hisleta1inp2,  
                      input$hisleta1sub1, input$hisleta1sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisleta1siz, input$hisleta1col2, input$hisleta1ord2, 
                      input$hisleta1fsz, input$hisleta1asp, input$hisleta1txt) ) 
  }) 
   
   
  ### Plots for tab a2 
  output$hisleta2sub1.ui <- renderUI({ 
    sub = strsplit(hisletconf[UI == input$hisleta2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("hisleta2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$hisleta2oup1 <- renderPlot({ 
    scDRcell(hisletconf, hisletmeta, input$hisleta2drX, input$hisleta2drY, input$hisleta2inp1,  
             input$hisleta2sub1, input$hisleta2sub2, 
             input$hisleta2siz, input$hisleta2col1, input$hisleta2ord1, 
             input$hisleta2fsz, input$hisleta2asp, input$hisleta2txt, input$hisleta2lab1) 
  }) 
  output$hisleta2oup1.ui <- renderUI({ 
    plotOutput("hisleta2oup1", height = pList[input$hisleta2psz]) 
  }) 
  output$hisleta2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta2drX,"_",input$hisleta2drY,"_",  
                                   input$hisleta2inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisleta2oup1.h, width = input$hisleta2oup1.w, useDingbats = FALSE, 
      plot = scDRcell(hisletconf, hisletmeta, input$hisleta2drX, input$hisleta2drY, input$hisleta2inp1,   
                      input$hisleta2sub1, input$hisleta2sub2, 
                      input$hisleta2siz, input$hisleta2col1, input$hisleta2ord1,  
                      input$hisleta2fsz, input$hisleta2asp, input$hisleta2txt, input$hisleta2lab1) ) 
  }) 
  output$hisleta2oup1.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta2drX,"_",input$hisleta2drY,"_",  
                                   input$hisleta2inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisleta2oup1.h, width = input$hisleta2oup1.w, 
      plot = scDRcell(hisletconf, hisletmeta, input$hisleta2drX, input$hisleta2drY, input$hisleta2inp1,   
                      input$hisleta2sub1, input$hisleta2sub2, 
                      input$hisleta2siz, input$hisleta2col1, input$hisleta2ord1,  
                      input$hisleta2fsz, input$hisleta2asp, input$hisleta2txt, input$hisleta2lab1) ) 
  }) 
   
  output$hisleta2oup2 <- renderPlot({ 
    scDRcell(hisletconf, hisletmeta, input$hisleta2drX, input$hisleta2drY, input$hisleta2inp2,  
             input$hisleta2sub1, input$hisleta2sub2, 
             input$hisleta2siz, input$hisleta2col2, input$hisleta2ord2, 
             input$hisleta2fsz, input$hisleta2asp, input$hisleta2txt, input$hisleta2lab2) 
  }) 
  output$hisleta2oup2.ui <- renderUI({ 
    plotOutput("hisleta2oup2", height = pList[input$hisleta2psz]) 
  }) 
  output$hisleta2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta2drX,"_",input$hisleta2drY,"_",  
                                   input$hisleta2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisleta2oup2.h, width = input$hisleta2oup2.w, useDingbats = FALSE, 
      plot = scDRcell(hisletconf, hisletmeta, input$hisleta2drX, input$hisleta2drY, input$hisleta2inp2,   
                      input$hisleta2sub1, input$hisleta2sub2, 
                      input$hisleta2siz, input$hisleta2col2, input$hisleta2ord2,  
                      input$hisleta2fsz, input$hisleta2asp, input$hisleta2txt, input$hisleta2lab2) ) 
  }) 
  output$hisleta2oup2.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta2drX,"_",input$hisleta2drY,"_",  
                                   input$hisleta2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisleta2oup2.h, width = input$hisleta2oup2.w, 
      plot = scDRcell(hisletconf, hisletmeta, input$hisleta2drX, input$hisleta2drY, input$hisleta2inp2,   
                      input$hisleta2sub1, input$hisleta2sub2, 
                      input$hisleta2siz, input$hisleta2col2, input$hisleta2ord2,  
                      input$hisleta2fsz, input$hisleta2asp, input$hisleta2txt, input$hisleta2lab2) ) 
  }) 
   
   
  ### Plots for tab a3 
  output$hisleta3sub1.ui <- renderUI({ 
    sub = strsplit(hisletconf[UI == input$hisleta3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("hisleta3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$hisleta3oup1 <- renderPlot({ 
    scDRgene(hisletconf, hisletmeta, input$hisleta3drX, input$hisleta3drY, input$hisleta3inp1,  
             input$hisleta3sub1, input$hisleta3sub2, 
             "hisletgexpr.h5", hisletgene, 
             input$hisleta3siz, input$hisleta3col1, input$hisleta3ord1, 
             input$hisleta3fsz, input$hisleta3asp, input$hisleta3txt) 
  }) 
  output$hisleta3oup1.ui <- renderUI({ 
    plotOutput("hisleta3oup1", height = pList[input$hisleta3psz]) 
  }) 
  output$hisleta3oup1.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta3drX,"_",input$hisleta3drY,"_",  
                                   input$hisleta3inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisleta3oup1.h, width = input$hisleta3oup1.w, useDingbats = FALSE, 
      plot = scDRgene(hisletconf, hisletmeta, input$hisleta3drX, input$hisleta3drY, input$hisleta3inp1,  
                      input$hisleta3sub1, input$hisleta3sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisleta3siz, input$hisleta3col1, input$hisleta3ord1, 
                      input$hisleta3fsz, input$hisleta3asp, input$hisleta3txt) ) 
  }) 
  output$hisleta3oup1.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta3drX,"_",input$hisleta3drY,"_",  
                                   input$hisleta3inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisleta3oup1.h, width = input$hisleta3oup1.w, 
      plot = scDRgene(hisletconf, hisletmeta, input$hisleta3drX, input$hisleta3drY, input$hisleta3inp1,  
                      input$hisleta3sub1, input$hisleta3sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisleta3siz, input$hisleta3col1, input$hisleta3ord1, 
                      input$hisleta3fsz, input$hisleta3asp, input$hisleta3txt) ) 
  }) 
   
  output$hisleta3oup2 <- renderPlot({ 
    scDRgene(hisletconf, hisletmeta, input$hisleta3drX, input$hisleta3drY, input$hisleta3inp2,  
             input$hisleta3sub1, input$hisleta3sub2, 
             "hisletgexpr.h5", hisletgene, 
             input$hisleta3siz, input$hisleta3col2, input$hisleta3ord2, 
             input$hisleta3fsz, input$hisleta3asp, input$hisleta3txt) 
  }) 
  output$hisleta3oup2.ui <- renderUI({ 
    plotOutput("hisleta3oup2", height = pList[input$hisleta3psz]) 
  }) 
  output$hisleta3oup2.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta3drX,"_",input$hisleta3drY,"_",  
                                   input$hisleta3inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisleta3oup2.h, width = input$hisleta3oup2.w, useDingbats = FALSE, 
      plot = scDRgene(hisletconf, hisletmeta, input$hisleta3drX, input$hisleta3drY, input$hisleta3inp2,  
                      input$hisleta3sub1, input$hisleta3sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisleta3siz, input$hisleta3col2, input$hisleta3ord2, 
                      input$hisleta3fsz, input$hisleta3asp, input$hisleta3txt) ) 
  }) 
  output$hisleta3oup2.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisleta3drX,"_",input$hisleta3drY,"_",  
                                   input$hisleta3inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisleta3oup2.h, width = input$hisleta3oup2.w, 
      plot = scDRgene(hisletconf, hisletmeta, input$hisleta3drX, input$hisleta3drY, input$hisleta3inp2,  
                      input$hisleta3sub1, input$hisleta3sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisleta3siz, input$hisleta3col2, input$hisleta3ord2, 
                      input$hisleta3fsz, input$hisleta3asp, input$hisleta3txt) ) 
  }) 
     
   
  ### Plots for tab b2 
  output$hisletb2sub1.ui <- renderUI({ 
    sub = strsplit(hisletconf[UI == input$hisletb2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("hisletb2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$hisletb2oup1 <- renderPlot({ 
    scDRcoex(hisletconf, hisletmeta, input$hisletb2drX, input$hisletb2drY,   
             input$hisletb2inp1, input$hisletb2inp2, input$hisletb2sub1, input$hisletb2sub2, 
             "hisletgexpr.h5", hisletgene, 
             input$hisletb2siz, input$hisletb2col1, input$hisletb2ord1, 
             input$hisletb2fsz, input$hisletb2asp, input$hisletb2txt) 
  }) 
  output$hisletb2oup1.ui <- renderUI({ 
    plotOutput("hisletb2oup1", height = pList2[input$hisletb2psz]) 
  }) 
  output$hisletb2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletb2drX,"_",input$hisletb2drY,"_",  
                                    input$hisletb2inp1,"_",input$hisletb2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisletb2oup1.h, width = input$hisletb2oup1.w, useDingbats = FALSE, 
      plot = scDRcoex(hisletconf, hisletmeta, input$hisletb2drX, input$hisletb2drY,  
                      input$hisletb2inp1, input$hisletb2inp2, input$hisletb2sub1, input$hisletb2sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisletb2siz, input$hisletb2col1, input$hisletb2ord1, 
                      input$hisletb2fsz, input$hisletb2asp, input$hisletb2txt) ) 
  }) 
  output$hisletb2oup1.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletb2drX,"_",input$hisletb2drY,"_",  
                                    input$hisletb2inp1,"_",input$hisletb2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisletb2oup1.h, width = input$hisletb2oup1.w, 
      plot = scDRcoex(hisletconf, hisletmeta, input$hisletb2drX, input$hisletb2drY,  
                      input$hisletb2inp1, input$hisletb2inp2, input$hisletb2sub1, input$hisletb2sub2, 
                      "hisletgexpr.h5", hisletgene, 
                      input$hisletb2siz, input$hisletb2col1, input$hisletb2ord1, 
                      input$hisletb2fsz, input$hisletb2asp, input$hisletb2txt) ) 
  }) 
  output$hisletb2oup2 <- renderPlot({ 
    scDRcoexLeg(input$hisletb2inp1, input$hisletb2inp2, input$hisletb2col1, input$hisletb2fsz) 
  }) 
  output$hisletb2oup2.ui <- renderUI({ 
    plotOutput("hisletb2oup2", height = "300px") 
  }) 
  output$hisletb2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletb2drX,"_",input$hisletb2drY,"_",  
                                    input$hisletb2inp1,"_",input$hisletb2inp2,"_leg.pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = 3, width = 4, useDingbats = FALSE, 
      plot = scDRcoexLeg(input$hisletb2inp1, input$hisletb2inp2, input$hisletb2col1, input$hisletb2fsz) ) 
  }) 
  output$hisletb2oup2.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletb2drX,"_",input$hisletb2drY,"_",  
                                    input$hisletb2inp1,"_",input$hisletb2inp2,"_leg.png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = 3, width = 4, 
      plot = scDRcoexLeg(input$hisletb2inp1, input$hisletb2inp2, input$hisletb2col1, input$hisletb2fsz) ) 
  }) 
  output$hisletb2.dt <- renderDataTable({ 
    ggData = scDRcoexNum(hisletconf, hisletmeta, input$hisletb2inp1, input$hisletb2inp2, 
                         input$hisletb2sub1, input$hisletb2sub2, "hisletgexpr.h5", hisletgene) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
     
   
  ### Plots for tab c1 
  output$hisletc1oup <- renderPlot({ 
    scVioBox(hisletconf, hisletmeta, input$hisletc1inp1, input$hisletc1inp2,  
             "hisletgexpr.h5", hisletgene, input$hisletc1typ, input$hisletc1pts, 
             input$hisletc1siz, input$hisletc1fsz) 
  }) 
  output$hisletc1oup.ui <- renderUI({ 
    plotOutput("hisletc1oup", height = pList2[input$hisletc1psz]) 
  }) 
  output$hisletc1oup.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletc1typ,"_",input$hisletc1inp1,"_",  
                                   input$hisletc1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisletc1oup.h, width = input$hisletc1oup.w, useDingbats = FALSE, 
      plot = scVioBox(hisletconf, hisletmeta, input$hisletc1inp1, input$hisletc1inp2,  
                      "hisletgexpr.h5", hisletgene, input$hisletc1typ, input$hisletc1pts, 
                      input$hisletc1siz, input$hisletc1fsz) ) 
  }) 
  output$hisletc1oup.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletc1typ,"_",input$hisletc1inp1,"_",  
                                   input$hisletc1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisletc1oup.h, width = input$hisletc1oup.w, 
      plot = scVioBox(hisletconf, hisletmeta, input$hisletc1inp1, input$hisletc1inp2,  
                      "hisletgexpr.h5", hisletgene, input$hisletc1typ, input$hisletc1pts, 
                      input$hisletc1siz, input$hisletc1fsz) ) 
  }) 
     
   
### Plots for tab c2 
output$hisletc2oup <- renderPlot({ 
  scProp(hisletconf, hisletmeta, input$hisletc2inp1, input$hisletc2inp2,  
         input$hisletc2typ, input$hisletc2flp, input$hisletc2fsz) 
}) 
output$hisletc2oup.ui <- renderUI({ 
  plotOutput("hisletc2oup", height = pList2[input$hisletc2psz]) 
}) 
output$hisletc2oup.pdf <- downloadHandler( 
  filename = function() { paste0("hislet",input$hisletc2typ,"_",input$hisletc2inp1,"_",  
                                 input$hisletc2inp2,".pdf") }, 
  content = function(file) { ggsave( 
    file, device = "pdf", height = input$hisletc2oup.h, width = input$hisletc2oup.w, useDingbats = FALSE, 
    plot = scProp(hisletconf, hisletmeta, input$hisletc2inp1, input$hisletc2inp2,  
                  input$hisletc2typ, input$hisletc2flp, input$hisletc2fsz) ) 
  }) 
output$hisletc2oup.png <- downloadHandler( 
  filename = function() { paste0("hislet",input$hisletc2typ,"_",input$hisletc2inp1,"_",  
                                 input$hisletc2inp2,".png") }, 
  content = function(file) { ggsave( 
    file, device = "png", height = input$hisletc2oup.h, width = input$hisletc2oup.w, 
    plot = scProp(hisletconf, hisletmeta, input$hisletc2inp1, input$hisletc2inp2,  
                  input$hisletc2typ, input$hisletc2flp, input$hisletc2fsz) ) 
  }) 
     
   
  ### Plots for tab d1 
  output$hisletd1oupTxt <- renderUI({ 
    geneList = scGeneList(input$hisletd1inp, hisletgene) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  output$hisletd1oup <- renderPlot({ 
    scBubbHeat(hisletconf, hisletmeta, input$hisletd1inp, input$hisletd1grp, input$hisletd1plt, 
               "hisletgexpr.h5", hisletgene, 
               input$hisletd1scl, input$hisletd1row, input$hisletd1col, 
               input$hisletd1cols, input$hisletd1fsz) 
  }) 
  output$hisletd1oup.ui <- renderUI({ 
    plotOutput("hisletd1oup", height = pList3[input$hisletd1psz]) 
  }) 
  output$hisletd1oup.pdf <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletd1plt,"_",input$hisletd1grp,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$hisletd1oup.h, width = input$hisletd1oup.w, 
      plot = scBubbHeat(hisletconf, hisletmeta, input$hisletd1inp, input$hisletd1grp, input$hisletd1plt, 
                        "hisletgexpr.h5", hisletgene, 
                        input$hisletd1scl, input$hisletd1row, input$hisletd1col, 
                        input$hisletd1cols, input$hisletd1fsz, save = TRUE) ) 
  }) 
  output$hisletd1oup.png <- downloadHandler( 
    filename = function() { paste0("hislet",input$hisletd1plt,"_",input$hisletd1grp,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$hisletd1oup.h, width = input$hisletd1oup.w, 
      plot = scBubbHeat(hisletconf, hisletmeta, input$hisletd1inp, input$hisletd1grp, input$hisletd1plt, 
                        "hisletgexpr.h5", hisletgene, 
                        input$hisletd1scl, input$hisletd1row, input$hisletd1col, 
                        input$hisletd1cols, input$hisletd1fsz, save = TRUE) ) 
  }) 
   
   
   optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  updateSelectizeInput(session, "endoa1inp2", choices = sort(names(endogene)), server = TRUE, 
                       selected = endodef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "endoa3inp1", choices = sort(names(endogene)), server = TRUE, 
                       selected = endodef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "endoa3inp2", choices = sort(names(endogene)), server = TRUE, 
                       selected = endodef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "endob2inp1", choices = sort(names(endogene)), server = TRUE, 
                       selected = endodef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "endob2inp2", choices = sort(names(endogene)), server = TRUE, 
                       selected = endodef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "endoc1inp2", server = TRUE, 
                       choices = c(endoconf[is.na(fID)]$UI,sort(names(endogene))), 
                       selected = endoconf[is.na(fID)]$UI[1], options = list( 
                         maxOptions = length(endoconf[is.na(fID)]$UI) + 3, 
                         create = TRUE, persist = TRUE, render = I(optCrt))) 
 
 
  ### Plots for tab a1 
  output$endoa1sub1.ui <- renderUI({ 
    sub = strsplit(endoconf[UI == input$endoa1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("endoa1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$endoa1oup1 <- renderPlot({ 
    scDRcell(endoconf, endometa, input$endoa1drX, input$endoa1drY, input$endoa1inp1,  
             input$endoa1sub1, input$endoa1sub2, 
             input$endoa1siz, input$endoa1col1, input$endoa1ord1, 
             input$endoa1fsz, input$endoa1asp, input$endoa1txt, input$endoa1lab1) 
  }) 
  output$endoa1oup1.ui <- renderUI({ 
    plotOutput("endoa1oup1", height = pList[input$endoa1psz]) 
  }) 
  output$endoa1oup1.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa1drX,"_",input$endoa1drY,"_",  
                                   input$endoa1inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endoa1oup1.h, width = input$endoa1oup1.w, useDingbats = FALSE, 
      plot = scDRcell(endoconf, endometa, input$endoa1drX, input$endoa1drY, input$endoa1inp1,   
                      input$endoa1sub1, input$endoa1sub2, 
                      input$endoa1siz, input$endoa1col1, input$endoa1ord1,  
                      input$endoa1fsz, input$endoa1asp, input$endoa1txt, input$endoa1lab1) ) 
  }) 
  output$endoa1oup1.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa1drX,"_",input$endoa1drY,"_",  
                                   input$endoa1inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endoa1oup1.h, width = input$endoa1oup1.w, 
      plot = scDRcell(endoconf, endometa, input$endoa1drX, input$endoa1drY, input$endoa1inp1,   
                      input$endoa1sub1, input$endoa1sub2, 
                      input$endoa1siz, input$endoa1col1, input$endoa1ord1,  
                      input$endoa1fsz, input$endoa1asp, input$endoa1txt, input$endoa1lab1) ) 
  }) 
  output$endoa1.dt <- renderDataTable({ 
    ggData = scDRnum(endoconf, endometa, input$endoa1inp1, input$endoa1inp2, 
                     input$endoa1sub1, input$endoa1sub2, 
                     "endogexpr.h5", endogene, input$endoa1splt) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("pctExpress"), digits = 2) 
  }) 
   
  output$endoa1oup2 <- renderPlot({ 
    scDRgene(endoconf, endometa, input$endoa1drX, input$endoa1drY, input$endoa1inp2,  
             input$endoa1sub1, input$endoa1sub2, 
             "endogexpr.h5", endogene, 
             input$endoa1siz, input$endoa1col2, input$endoa1ord2, 
             input$endoa1fsz, input$endoa1asp, input$endoa1txt) 
  }) 
  output$endoa1oup2.ui <- renderUI({ 
    plotOutput("endoa1oup2", height = pList[input$endoa1psz]) 
  }) 
  output$endoa1oup2.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa1drX,"_",input$endoa1drY,"_",  
                                   input$endoa1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endoa1oup2.h, width = input$endoa1oup2.w, useDingbats = FALSE, 
      plot = scDRgene(endoconf, endometa, input$endoa1drX, input$endoa1drY, input$endoa1inp2,  
                      input$endoa1sub1, input$endoa1sub2, 
                      "endogexpr.h5", endogene, 
                      input$endoa1siz, input$endoa1col2, input$endoa1ord2, 
                      input$endoa1fsz, input$endoa1asp, input$endoa1txt) ) 
  }) 
  output$endoa1oup2.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa1drX,"_",input$endoa1drY,"_",  
                                   input$endoa1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endoa1oup2.h, width = input$endoa1oup2.w, 
      plot = scDRgene(endoconf, endometa, input$endoa1drX, input$endoa1drY, input$endoa1inp2,  
                      input$endoa1sub1, input$endoa1sub2, 
                      "endogexpr.h5", endogene, 
                      input$endoa1siz, input$endoa1col2, input$endoa1ord2, 
                      input$endoa1fsz, input$endoa1asp, input$endoa1txt) ) 
  }) 
   
   
  ### Plots for tab a2 
  output$endoa2sub1.ui <- renderUI({ 
    sub = strsplit(endoconf[UI == input$endoa2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("endoa2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$endoa2oup1 <- renderPlot({ 
    scDRcell(endoconf, endometa, input$endoa2drX, input$endoa2drY, input$endoa2inp1,  
             input$endoa2sub1, input$endoa2sub2, 
             input$endoa2siz, input$endoa2col1, input$endoa2ord1, 
             input$endoa2fsz, input$endoa2asp, input$endoa2txt, input$endoa2lab1) 
  }) 
  output$endoa2oup1.ui <- renderUI({ 
    plotOutput("endoa2oup1", height = pList[input$endoa2psz]) 
  }) 
  output$endoa2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa2drX,"_",input$endoa2drY,"_",  
                                   input$endoa2inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endoa2oup1.h, width = input$endoa2oup1.w, useDingbats = FALSE, 
      plot = scDRcell(endoconf, endometa, input$endoa2drX, input$endoa2drY, input$endoa2inp1,   
                      input$endoa2sub1, input$endoa2sub2, 
                      input$endoa2siz, input$endoa2col1, input$endoa2ord1,  
                      input$endoa2fsz, input$endoa2asp, input$endoa2txt, input$endoa2lab1) ) 
  }) 
  output$endoa2oup1.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa2drX,"_",input$endoa2drY,"_",  
                                   input$endoa2inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endoa2oup1.h, width = input$endoa2oup1.w, 
      plot = scDRcell(endoconf, endometa, input$endoa2drX, input$endoa2drY, input$endoa2inp1,   
                      input$endoa2sub1, input$endoa2sub2, 
                      input$endoa2siz, input$endoa2col1, input$endoa2ord1,  
                      input$endoa2fsz, input$endoa2asp, input$endoa2txt, input$endoa2lab1) ) 
  }) 
   
  output$endoa2oup2 <- renderPlot({ 
    scDRcell(endoconf, endometa, input$endoa2drX, input$endoa2drY, input$endoa2inp2,  
             input$endoa2sub1, input$endoa2sub2, 
             input$endoa2siz, input$endoa2col2, input$endoa2ord2, 
             input$endoa2fsz, input$endoa2asp, input$endoa2txt, input$endoa2lab2) 
  }) 
  output$endoa2oup2.ui <- renderUI({ 
    plotOutput("endoa2oup2", height = pList[input$endoa2psz]) 
  }) 
  output$endoa2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa2drX,"_",input$endoa2drY,"_",  
                                   input$endoa2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endoa2oup2.h, width = input$endoa2oup2.w, useDingbats = FALSE, 
      plot = scDRcell(endoconf, endometa, input$endoa2drX, input$endoa2drY, input$endoa2inp2,   
                      input$endoa2sub1, input$endoa2sub2, 
                      input$endoa2siz, input$endoa2col2, input$endoa2ord2,  
                      input$endoa2fsz, input$endoa2asp, input$endoa2txt, input$endoa2lab2) ) 
  }) 
  output$endoa2oup2.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa2drX,"_",input$endoa2drY,"_",  
                                   input$endoa2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endoa2oup2.h, width = input$endoa2oup2.w, 
      plot = scDRcell(endoconf, endometa, input$endoa2drX, input$endoa2drY, input$endoa2inp2,   
                      input$endoa2sub1, input$endoa2sub2, 
                      input$endoa2siz, input$endoa2col2, input$endoa2ord2,  
                      input$endoa2fsz, input$endoa2asp, input$endoa2txt, input$endoa2lab2) ) 
  }) 
   
   
  ### Plots for tab a3 
  output$endoa3sub1.ui <- renderUI({ 
    sub = strsplit(endoconf[UI == input$endoa3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("endoa3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$endoa3oup1 <- renderPlot({ 
    scDRgene(endoconf, endometa, input$endoa3drX, input$endoa3drY, input$endoa3inp1,  
             input$endoa3sub1, input$endoa3sub2, 
             "endogexpr.h5", endogene, 
             input$endoa3siz, input$endoa3col1, input$endoa3ord1, 
             input$endoa3fsz, input$endoa3asp, input$endoa3txt) 
  }) 
  output$endoa3oup1.ui <- renderUI({ 
    plotOutput("endoa3oup1", height = pList[input$endoa3psz]) 
  }) 
  output$endoa3oup1.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa3drX,"_",input$endoa3drY,"_",  
                                   input$endoa3inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endoa3oup1.h, width = input$endoa3oup1.w, useDingbats = FALSE, 
      plot = scDRgene(endoconf, endometa, input$endoa3drX, input$endoa3drY, input$endoa3inp1,  
                      input$endoa3sub1, input$endoa3sub2, 
                      "endogexpr.h5", endogene, 
                      input$endoa3siz, input$endoa3col1, input$endoa3ord1, 
                      input$endoa3fsz, input$endoa3asp, input$endoa3txt) ) 
  }) 
  output$endoa3oup1.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa3drX,"_",input$endoa3drY,"_",  
                                   input$endoa3inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endoa3oup1.h, width = input$endoa3oup1.w, 
      plot = scDRgene(endoconf, endometa, input$endoa3drX, input$endoa3drY, input$endoa3inp1,  
                      input$endoa3sub1, input$endoa3sub2, 
                      "endogexpr.h5", endogene, 
                      input$endoa3siz, input$endoa3col1, input$endoa3ord1, 
                      input$endoa3fsz, input$endoa3asp, input$endoa3txt) ) 
  }) 
   
  output$endoa3oup2 <- renderPlot({ 
    scDRgene(endoconf, endometa, input$endoa3drX, input$endoa3drY, input$endoa3inp2,  
             input$endoa3sub1, input$endoa3sub2, 
             "endogexpr.h5", endogene, 
             input$endoa3siz, input$endoa3col2, input$endoa3ord2, 
             input$endoa3fsz, input$endoa3asp, input$endoa3txt) 
  }) 
  output$endoa3oup2.ui <- renderUI({ 
    plotOutput("endoa3oup2", height = pList[input$endoa3psz]) 
  }) 
  output$endoa3oup2.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa3drX,"_",input$endoa3drY,"_",  
                                   input$endoa3inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endoa3oup2.h, width = input$endoa3oup2.w, useDingbats = FALSE, 
      plot = scDRgene(endoconf, endometa, input$endoa3drX, input$endoa3drY, input$endoa3inp2,  
                      input$endoa3sub1, input$endoa3sub2, 
                      "endogexpr.h5", endogene, 
                      input$endoa3siz, input$endoa3col2, input$endoa3ord2, 
                      input$endoa3fsz, input$endoa3asp, input$endoa3txt) ) 
  }) 
  output$endoa3oup2.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endoa3drX,"_",input$endoa3drY,"_",  
                                   input$endoa3inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endoa3oup2.h, width = input$endoa3oup2.w, 
      plot = scDRgene(endoconf, endometa, input$endoa3drX, input$endoa3drY, input$endoa3inp2,  
                      input$endoa3sub1, input$endoa3sub2, 
                      "endogexpr.h5", endogene, 
                      input$endoa3siz, input$endoa3col2, input$endoa3ord2, 
                      input$endoa3fsz, input$endoa3asp, input$endoa3txt) ) 
  }) 
     
   
  ### Plots for tab b2 
  output$endob2sub1.ui <- renderUI({ 
    sub = strsplit(endoconf[UI == input$endob2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("endob2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$endob2oup1 <- renderPlot({ 
    scDRcoex(endoconf, endometa, input$endob2drX, input$endob2drY,   
             input$endob2inp1, input$endob2inp2, input$endob2sub1, input$endob2sub2, 
             "endogexpr.h5", endogene, 
             input$endob2siz, input$endob2col1, input$endob2ord1, 
             input$endob2fsz, input$endob2asp, input$endob2txt) 
  }) 
  output$endob2oup1.ui <- renderUI({ 
    plotOutput("endob2oup1", height = pList2[input$endob2psz]) 
  }) 
  output$endob2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endob2drX,"_",input$endob2drY,"_",  
                                    input$endob2inp1,"_",input$endob2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endob2oup1.h, width = input$endob2oup1.w, useDingbats = FALSE, 
      plot = scDRcoex(endoconf, endometa, input$endob2drX, input$endob2drY,  
                      input$endob2inp1, input$endob2inp2, input$endob2sub1, input$endob2sub2, 
                      "endogexpr.h5", endogene, 
                      input$endob2siz, input$endob2col1, input$endob2ord1, 
                      input$endob2fsz, input$endob2asp, input$endob2txt) ) 
  }) 
  output$endob2oup1.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endob2drX,"_",input$endob2drY,"_",  
                                    input$endob2inp1,"_",input$endob2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endob2oup1.h, width = input$endob2oup1.w, 
      plot = scDRcoex(endoconf, endometa, input$endob2drX, input$endob2drY,  
                      input$endob2inp1, input$endob2inp2, input$endob2sub1, input$endob2sub2, 
                      "endogexpr.h5", endogene, 
                      input$endob2siz, input$endob2col1, input$endob2ord1, 
                      input$endob2fsz, input$endob2asp, input$endob2txt) ) 
  }) 
  output$endob2oup2 <- renderPlot({ 
    scDRcoexLeg(input$endob2inp1, input$endob2inp2, input$endob2col1, input$endob2fsz) 
  }) 
  output$endob2oup2.ui <- renderUI({ 
    plotOutput("endob2oup2", height = "300px") 
  }) 
  output$endob2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endob2drX,"_",input$endob2drY,"_",  
                                    input$endob2inp1,"_",input$endob2inp2,"_leg.pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = 3, width = 4, useDingbats = FALSE, 
      plot = scDRcoexLeg(input$endob2inp1, input$endob2inp2, input$endob2col1, input$endob2fsz) ) 
  }) 
  output$endob2oup2.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endob2drX,"_",input$endob2drY,"_",  
                                    input$endob2inp1,"_",input$endob2inp2,"_leg.png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = 3, width = 4, 
      plot = scDRcoexLeg(input$endob2inp1, input$endob2inp2, input$endob2col1, input$endob2fsz) ) 
  }) 
  output$endob2.dt <- renderDataTable({ 
    ggData = scDRcoexNum(endoconf, endometa, input$endob2inp1, input$endob2inp2, 
                         input$endob2sub1, input$endob2sub2, "endogexpr.h5", endogene) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
     
   
  ### Plots for tab c1 
  output$endoc1oup <- renderPlot({ 
    scVioBox(endoconf, endometa, input$endoc1inp1, input$endoc1inp2,  
             "endogexpr.h5", endogene, input$endoc1typ, input$endoc1pts, 
             input$endoc1siz, input$endoc1fsz) 
  }) 
  output$endoc1oup.ui <- renderUI({ 
    plotOutput("endoc1oup", height = pList2[input$endoc1psz]) 
  }) 
  output$endoc1oup.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endoc1typ,"_",input$endoc1inp1,"_",  
                                   input$endoc1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endoc1oup.h, width = input$endoc1oup.w, useDingbats = FALSE, 
      plot = scVioBox(endoconf, endometa, input$endoc1inp1, input$endoc1inp2,  
                      "endogexpr.h5", endogene, input$endoc1typ, input$endoc1pts, 
                      input$endoc1siz, input$endoc1fsz) ) 
  }) 
  output$endoc1oup.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endoc1typ,"_",input$endoc1inp1,"_",  
                                   input$endoc1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endoc1oup.h, width = input$endoc1oup.w, 
      plot = scVioBox(endoconf, endometa, input$endoc1inp1, input$endoc1inp2,  
                      "endogexpr.h5", endogene, input$endoc1typ, input$endoc1pts, 
                      input$endoc1siz, input$endoc1fsz) ) 
  }) 
     
   
### Plots for tab c2 
output$endoc2oup <- renderPlot({ 
  scProp(endoconf, endometa, input$endoc2inp1, input$endoc2inp2,  
         input$endoc2typ, input$endoc2flp, input$endoc2fsz) 
}) 
output$endoc2oup.ui <- renderUI({ 
  plotOutput("endoc2oup", height = pList2[input$endoc2psz]) 
}) 
output$endoc2oup.pdf <- downloadHandler( 
  filename = function() { paste0("endo",input$endoc2typ,"_",input$endoc2inp1,"_",  
                                 input$endoc2inp2,".pdf") }, 
  content = function(file) { ggsave( 
    file, device = "pdf", height = input$endoc2oup.h, width = input$endoc2oup.w, useDingbats = FALSE, 
    plot = scProp(endoconf, endometa, input$endoc2inp1, input$endoc2inp2,  
                  input$endoc2typ, input$endoc2flp, input$endoc2fsz) ) 
  }) 
output$endoc2oup.png <- downloadHandler( 
  filename = function() { paste0("endo",input$endoc2typ,"_",input$endoc2inp1,"_",  
                                 input$endoc2inp2,".png") }, 
  content = function(file) { ggsave( 
    file, device = "png", height = input$endoc2oup.h, width = input$endoc2oup.w, 
    plot = scProp(endoconf, endometa, input$endoc2inp1, input$endoc2inp2,  
                  input$endoc2typ, input$endoc2flp, input$endoc2fsz) ) 
  }) 
     
   
  ### Plots for tab d1 
  output$endod1oupTxt <- renderUI({ 
    geneList = scGeneList(input$endod1inp, endogene) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  output$endod1oup <- renderPlot({ 
    scBubbHeat(endoconf, endometa, input$endod1inp, input$endod1grp, input$endod1plt, 
               "endogexpr.h5", endogene, 
               input$endod1scl, input$endod1row, input$endod1col, 
               input$endod1cols, input$endod1fsz) 
  }) 
  output$endod1oup.ui <- renderUI({ 
    plotOutput("endod1oup", height = pList3[input$endod1psz]) 
  }) 
  output$endod1oup.pdf <- downloadHandler( 
    filename = function() { paste0("endo",input$endod1plt,"_",input$endod1grp,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$endod1oup.h, width = input$endod1oup.w, 
      plot = scBubbHeat(endoconf, endometa, input$endod1inp, input$endod1grp, input$endod1plt, 
                        "endogexpr.h5", endogene, 
                        input$endod1scl, input$endod1row, input$endod1col, 
                        input$endod1cols, input$endod1fsz, save = TRUE) ) 
  }) 
  output$endod1oup.png <- downloadHandler( 
    filename = function() { paste0("endo",input$endod1plt,"_",input$endod1grp,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$endod1oup.h, width = input$endod1oup.w, 
      plot = scBubbHeat(endoconf, endometa, input$endod1inp, input$endod1grp, input$endod1plt, 
                        "endogexpr.h5", endogene, 
                        input$endod1scl, input$endod1row, input$endod1col, 
                        input$endod1cols, input$endod1fsz, save = TRUE) ) 
  }) 
   
   
   optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  updateSelectizeInput(session, "alphaa1inp2", choices = sort(names(alphagene)), server = TRUE, 
                       selected = alphadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "alphaa3inp1", choices = sort(names(alphagene)), server = TRUE, 
                       selected = alphadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "alphaa3inp2", choices = sort(names(alphagene)), server = TRUE, 
                       selected = alphadef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "alphab2inp1", choices = sort(names(alphagene)), server = TRUE, 
                       selected = alphadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "alphab2inp2", choices = sort(names(alphagene)), server = TRUE, 
                       selected = alphadef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "alphac1inp2", server = TRUE, 
                       choices = c(alphaconf[is.na(fID)]$UI,sort(names(alphagene))), 
                       selected = alphaconf[is.na(fID)]$UI[1], options = list( 
                         maxOptions = length(alphaconf[is.na(fID)]$UI) + 3, 
                         create = TRUE, persist = TRUE, render = I(optCrt))) 
 
 
  ### Plots for tab a1 
  output$alphaa1sub1.ui <- renderUI({ 
    sub = strsplit(alphaconf[UI == input$alphaa1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("alphaa1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$alphaa1oup1 <- renderPlot({ 
    scDRcell(alphaconf, alphameta, input$alphaa1drX, input$alphaa1drY, input$alphaa1inp1,  
             input$alphaa1sub1, input$alphaa1sub2, 
             input$alphaa1siz, input$alphaa1col1, input$alphaa1ord1, 
             input$alphaa1fsz, input$alphaa1asp, input$alphaa1txt, input$alphaa1lab1) 
  }) 
  output$alphaa1oup1.ui <- renderUI({ 
    plotOutput("alphaa1oup1", height = pList[input$alphaa1psz]) 
  }) 
  output$alphaa1oup1.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa1drX,"_",input$alphaa1drY,"_",  
                                   input$alphaa1inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphaa1oup1.h, width = input$alphaa1oup1.w, useDingbats = FALSE, 
      plot = scDRcell(alphaconf, alphameta, input$alphaa1drX, input$alphaa1drY, input$alphaa1inp1,   
                      input$alphaa1sub1, input$alphaa1sub2, 
                      input$alphaa1siz, input$alphaa1col1, input$alphaa1ord1,  
                      input$alphaa1fsz, input$alphaa1asp, input$alphaa1txt, input$alphaa1lab1) ) 
  }) 
  output$alphaa1oup1.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa1drX,"_",input$alphaa1drY,"_",  
                                   input$alphaa1inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphaa1oup1.h, width = input$alphaa1oup1.w, 
      plot = scDRcell(alphaconf, alphameta, input$alphaa1drX, input$alphaa1drY, input$alphaa1inp1,   
                      input$alphaa1sub1, input$alphaa1sub2, 
                      input$alphaa1siz, input$alphaa1col1, input$alphaa1ord1,  
                      input$alphaa1fsz, input$alphaa1asp, input$alphaa1txt, input$alphaa1lab1) ) 
  }) 
  output$alphaa1.dt <- renderDataTable({ 
    ggData = scDRnum(alphaconf, alphameta, input$alphaa1inp1, input$alphaa1inp2, 
                     input$alphaa1sub1, input$alphaa1sub2, 
                     "alphagexpr.h5", alphagene, input$alphaa1splt) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("pctExpress"), digits = 2) 
  }) 
   
  output$alphaa1oup2 <- renderPlot({ 
    scDRgene(alphaconf, alphameta, input$alphaa1drX, input$alphaa1drY, input$alphaa1inp2,  
             input$alphaa1sub1, input$alphaa1sub2, 
             "alphagexpr.h5", alphagene, 
             input$alphaa1siz, input$alphaa1col2, input$alphaa1ord2, 
             input$alphaa1fsz, input$alphaa1asp, input$alphaa1txt) 
  }) 
  output$alphaa1oup2.ui <- renderUI({ 
    plotOutput("alphaa1oup2", height = pList[input$alphaa1psz]) 
  }) 
  output$alphaa1oup2.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa1drX,"_",input$alphaa1drY,"_",  
                                   input$alphaa1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphaa1oup2.h, width = input$alphaa1oup2.w, useDingbats = FALSE, 
      plot = scDRgene(alphaconf, alphameta, input$alphaa1drX, input$alphaa1drY, input$alphaa1inp2,  
                      input$alphaa1sub1, input$alphaa1sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphaa1siz, input$alphaa1col2, input$alphaa1ord2, 
                      input$alphaa1fsz, input$alphaa1asp, input$alphaa1txt) ) 
  }) 
  output$alphaa1oup2.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa1drX,"_",input$alphaa1drY,"_",  
                                   input$alphaa1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphaa1oup2.h, width = input$alphaa1oup2.w, 
      plot = scDRgene(alphaconf, alphameta, input$alphaa1drX, input$alphaa1drY, input$alphaa1inp2,  
                      input$alphaa1sub1, input$alphaa1sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphaa1siz, input$alphaa1col2, input$alphaa1ord2, 
                      input$alphaa1fsz, input$alphaa1asp, input$alphaa1txt) ) 
  }) 
   
   
  ### Plots for tab a2 
  output$alphaa2sub1.ui <- renderUI({ 
    sub = strsplit(alphaconf[UI == input$alphaa2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("alphaa2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$alphaa2oup1 <- renderPlot({ 
    scDRcell(alphaconf, alphameta, input$alphaa2drX, input$alphaa2drY, input$alphaa2inp1,  
             input$alphaa2sub1, input$alphaa2sub2, 
             input$alphaa2siz, input$alphaa2col1, input$alphaa2ord1, 
             input$alphaa2fsz, input$alphaa2asp, input$alphaa2txt, input$alphaa2lab1) 
  }) 
  output$alphaa2oup1.ui <- renderUI({ 
    plotOutput("alphaa2oup1", height = pList[input$alphaa2psz]) 
  }) 
  output$alphaa2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa2drX,"_",input$alphaa2drY,"_",  
                                   input$alphaa2inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphaa2oup1.h, width = input$alphaa2oup1.w, useDingbats = FALSE, 
      plot = scDRcell(alphaconf, alphameta, input$alphaa2drX, input$alphaa2drY, input$alphaa2inp1,   
                      input$alphaa2sub1, input$alphaa2sub2, 
                      input$alphaa2siz, input$alphaa2col1, input$alphaa2ord1,  
                      input$alphaa2fsz, input$alphaa2asp, input$alphaa2txt, input$alphaa2lab1) ) 
  }) 
  output$alphaa2oup1.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa2drX,"_",input$alphaa2drY,"_",  
                                   input$alphaa2inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphaa2oup1.h, width = input$alphaa2oup1.w, 
      plot = scDRcell(alphaconf, alphameta, input$alphaa2drX, input$alphaa2drY, input$alphaa2inp1,   
                      input$alphaa2sub1, input$alphaa2sub2, 
                      input$alphaa2siz, input$alphaa2col1, input$alphaa2ord1,  
                      input$alphaa2fsz, input$alphaa2asp, input$alphaa2txt, input$alphaa2lab1) ) 
  }) 
   
  output$alphaa2oup2 <- renderPlot({ 
    scDRcell(alphaconf, alphameta, input$alphaa2drX, input$alphaa2drY, input$alphaa2inp2,  
             input$alphaa2sub1, input$alphaa2sub2, 
             input$alphaa2siz, input$alphaa2col2, input$alphaa2ord2, 
             input$alphaa2fsz, input$alphaa2asp, input$alphaa2txt, input$alphaa2lab2) 
  }) 
  output$alphaa2oup2.ui <- renderUI({ 
    plotOutput("alphaa2oup2", height = pList[input$alphaa2psz]) 
  }) 
  output$alphaa2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa2drX,"_",input$alphaa2drY,"_",  
                                   input$alphaa2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphaa2oup2.h, width = input$alphaa2oup2.w, useDingbats = FALSE, 
      plot = scDRcell(alphaconf, alphameta, input$alphaa2drX, input$alphaa2drY, input$alphaa2inp2,   
                      input$alphaa2sub1, input$alphaa2sub2, 
                      input$alphaa2siz, input$alphaa2col2, input$alphaa2ord2,  
                      input$alphaa2fsz, input$alphaa2asp, input$alphaa2txt, input$alphaa2lab2) ) 
  }) 
  output$alphaa2oup2.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa2drX,"_",input$alphaa2drY,"_",  
                                   input$alphaa2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphaa2oup2.h, width = input$alphaa2oup2.w, 
      plot = scDRcell(alphaconf, alphameta, input$alphaa2drX, input$alphaa2drY, input$alphaa2inp2,   
                      input$alphaa2sub1, input$alphaa2sub2, 
                      input$alphaa2siz, input$alphaa2col2, input$alphaa2ord2,  
                      input$alphaa2fsz, input$alphaa2asp, input$alphaa2txt, input$alphaa2lab2) ) 
  }) 
   
   
  ### Plots for tab a3 
  output$alphaa3sub1.ui <- renderUI({ 
    sub = strsplit(alphaconf[UI == input$alphaa3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("alphaa3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$alphaa3oup1 <- renderPlot({ 
    scDRgene(alphaconf, alphameta, input$alphaa3drX, input$alphaa3drY, input$alphaa3inp1,  
             input$alphaa3sub1, input$alphaa3sub2, 
             "alphagexpr.h5", alphagene, 
             input$alphaa3siz, input$alphaa3col1, input$alphaa3ord1, 
             input$alphaa3fsz, input$alphaa3asp, input$alphaa3txt) 
  }) 
  output$alphaa3oup1.ui <- renderUI({ 
    plotOutput("alphaa3oup1", height = pList[input$alphaa3psz]) 
  }) 
  output$alphaa3oup1.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa3drX,"_",input$alphaa3drY,"_",  
                                   input$alphaa3inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphaa3oup1.h, width = input$alphaa3oup1.w, useDingbats = FALSE, 
      plot = scDRgene(alphaconf, alphameta, input$alphaa3drX, input$alphaa3drY, input$alphaa3inp1,  
                      input$alphaa3sub1, input$alphaa3sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphaa3siz, input$alphaa3col1, input$alphaa3ord1, 
                      input$alphaa3fsz, input$alphaa3asp, input$alphaa3txt) ) 
  }) 
  output$alphaa3oup1.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa3drX,"_",input$alphaa3drY,"_",  
                                   input$alphaa3inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphaa3oup1.h, width = input$alphaa3oup1.w, 
      plot = scDRgene(alphaconf, alphameta, input$alphaa3drX, input$alphaa3drY, input$alphaa3inp1,  
                      input$alphaa3sub1, input$alphaa3sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphaa3siz, input$alphaa3col1, input$alphaa3ord1, 
                      input$alphaa3fsz, input$alphaa3asp, input$alphaa3txt) ) 
  }) 
   
  output$alphaa3oup2 <- renderPlot({ 
    scDRgene(alphaconf, alphameta, input$alphaa3drX, input$alphaa3drY, input$alphaa3inp2,  
             input$alphaa3sub1, input$alphaa3sub2, 
             "alphagexpr.h5", alphagene, 
             input$alphaa3siz, input$alphaa3col2, input$alphaa3ord2, 
             input$alphaa3fsz, input$alphaa3asp, input$alphaa3txt) 
  }) 
  output$alphaa3oup2.ui <- renderUI({ 
    plotOutput("alphaa3oup2", height = pList[input$alphaa3psz]) 
  }) 
  output$alphaa3oup2.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa3drX,"_",input$alphaa3drY,"_",  
                                   input$alphaa3inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphaa3oup2.h, width = input$alphaa3oup2.w, useDingbats = FALSE, 
      plot = scDRgene(alphaconf, alphameta, input$alphaa3drX, input$alphaa3drY, input$alphaa3inp2,  
                      input$alphaa3sub1, input$alphaa3sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphaa3siz, input$alphaa3col2, input$alphaa3ord2, 
                      input$alphaa3fsz, input$alphaa3asp, input$alphaa3txt) ) 
  }) 
  output$alphaa3oup2.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphaa3drX,"_",input$alphaa3drY,"_",  
                                   input$alphaa3inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphaa3oup2.h, width = input$alphaa3oup2.w, 
      plot = scDRgene(alphaconf, alphameta, input$alphaa3drX, input$alphaa3drY, input$alphaa3inp2,  
                      input$alphaa3sub1, input$alphaa3sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphaa3siz, input$alphaa3col2, input$alphaa3ord2, 
                      input$alphaa3fsz, input$alphaa3asp, input$alphaa3txt) ) 
  }) 
     
   
  ### Plots for tab b2 
  output$alphab2sub1.ui <- renderUI({ 
    sub = strsplit(alphaconf[UI == input$alphab2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("alphab2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$alphab2oup1 <- renderPlot({ 
    scDRcoex(alphaconf, alphameta, input$alphab2drX, input$alphab2drY,   
             input$alphab2inp1, input$alphab2inp2, input$alphab2sub1, input$alphab2sub2, 
             "alphagexpr.h5", alphagene, 
             input$alphab2siz, input$alphab2col1, input$alphab2ord1, 
             input$alphab2fsz, input$alphab2asp, input$alphab2txt) 
  }) 
  output$alphab2oup1.ui <- renderUI({ 
    plotOutput("alphab2oup1", height = pList2[input$alphab2psz]) 
  }) 
  output$alphab2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphab2drX,"_",input$alphab2drY,"_",  
                                    input$alphab2inp1,"_",input$alphab2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphab2oup1.h, width = input$alphab2oup1.w, useDingbats = FALSE, 
      plot = scDRcoex(alphaconf, alphameta, input$alphab2drX, input$alphab2drY,  
                      input$alphab2inp1, input$alphab2inp2, input$alphab2sub1, input$alphab2sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphab2siz, input$alphab2col1, input$alphab2ord1, 
                      input$alphab2fsz, input$alphab2asp, input$alphab2txt) ) 
  }) 
  output$alphab2oup1.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphab2drX,"_",input$alphab2drY,"_",  
                                    input$alphab2inp1,"_",input$alphab2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphab2oup1.h, width = input$alphab2oup1.w, 
      plot = scDRcoex(alphaconf, alphameta, input$alphab2drX, input$alphab2drY,  
                      input$alphab2inp1, input$alphab2inp2, input$alphab2sub1, input$alphab2sub2, 
                      "alphagexpr.h5", alphagene, 
                      input$alphab2siz, input$alphab2col1, input$alphab2ord1, 
                      input$alphab2fsz, input$alphab2asp, input$alphab2txt) ) 
  }) 
  output$alphab2oup2 <- renderPlot({ 
    scDRcoexLeg(input$alphab2inp1, input$alphab2inp2, input$alphab2col1, input$alphab2fsz) 
  }) 
  output$alphab2oup2.ui <- renderUI({ 
    plotOutput("alphab2oup2", height = "300px") 
  }) 
  output$alphab2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphab2drX,"_",input$alphab2drY,"_",  
                                    input$alphab2inp1,"_",input$alphab2inp2,"_leg.pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = 3, width = 4, useDingbats = FALSE, 
      plot = scDRcoexLeg(input$alphab2inp1, input$alphab2inp2, input$alphab2col1, input$alphab2fsz) ) 
  }) 
  output$alphab2oup2.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphab2drX,"_",input$alphab2drY,"_",  
                                    input$alphab2inp1,"_",input$alphab2inp2,"_leg.png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = 3, width = 4, 
      plot = scDRcoexLeg(input$alphab2inp1, input$alphab2inp2, input$alphab2col1, input$alphab2fsz) ) 
  }) 
  output$alphab2.dt <- renderDataTable({ 
    ggData = scDRcoexNum(alphaconf, alphameta, input$alphab2inp1, input$alphab2inp2, 
                         input$alphab2sub1, input$alphab2sub2, "alphagexpr.h5", alphagene) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
     
   
  ### Plots for tab c1 
  output$alphac1oup <- renderPlot({ 
    scVioBox(alphaconf, alphameta, input$alphac1inp1, input$alphac1inp2,  
             "alphagexpr.h5", alphagene, input$alphac1typ, input$alphac1pts, 
             input$alphac1siz, input$alphac1fsz) 
  }) 
  output$alphac1oup.ui <- renderUI({ 
    plotOutput("alphac1oup", height = pList2[input$alphac1psz]) 
  }) 
  output$alphac1oup.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphac1typ,"_",input$alphac1inp1,"_",  
                                   input$alphac1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphac1oup.h, width = input$alphac1oup.w, useDingbats = FALSE, 
      plot = scVioBox(alphaconf, alphameta, input$alphac1inp1, input$alphac1inp2,  
                      "alphagexpr.h5", alphagene, input$alphac1typ, input$alphac1pts, 
                      input$alphac1siz, input$alphac1fsz) ) 
  }) 
  output$alphac1oup.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphac1typ,"_",input$alphac1inp1,"_",  
                                   input$alphac1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphac1oup.h, width = input$alphac1oup.w, 
      plot = scVioBox(alphaconf, alphameta, input$alphac1inp1, input$alphac1inp2,  
                      "alphagexpr.h5", alphagene, input$alphac1typ, input$alphac1pts, 
                      input$alphac1siz, input$alphac1fsz) ) 
  }) 
     
   
### Plots for tab c2 
output$alphac2oup <- renderPlot({ 
  scProp(alphaconf, alphameta, input$alphac2inp1, input$alphac2inp2,  
         input$alphac2typ, input$alphac2flp, input$alphac2fsz) 
}) 
output$alphac2oup.ui <- renderUI({ 
  plotOutput("alphac2oup", height = pList2[input$alphac2psz]) 
}) 
output$alphac2oup.pdf <- downloadHandler( 
  filename = function() { paste0("alpha",input$alphac2typ,"_",input$alphac2inp1,"_",  
                                 input$alphac2inp2,".pdf") }, 
  content = function(file) { ggsave( 
    file, device = "pdf", height = input$alphac2oup.h, width = input$alphac2oup.w, useDingbats = FALSE, 
    plot = scProp(alphaconf, alphameta, input$alphac2inp1, input$alphac2inp2,  
                  input$alphac2typ, input$alphac2flp, input$alphac2fsz) ) 
  }) 
output$alphac2oup.png <- downloadHandler( 
  filename = function() { paste0("alpha",input$alphac2typ,"_",input$alphac2inp1,"_",  
                                 input$alphac2inp2,".png") }, 
  content = function(file) { ggsave( 
    file, device = "png", height = input$alphac2oup.h, width = input$alphac2oup.w, 
    plot = scProp(alphaconf, alphameta, input$alphac2inp1, input$alphac2inp2,  
                  input$alphac2typ, input$alphac2flp, input$alphac2fsz) ) 
  }) 
     
   
  ### Plots for tab d1 
  output$alphad1oupTxt <- renderUI({ 
    geneList = scGeneList(input$alphad1inp, alphagene) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  output$alphad1oup <- renderPlot({ 
    scBubbHeat(alphaconf, alphameta, input$alphad1inp, input$alphad1grp, input$alphad1plt, 
               "alphagexpr.h5", alphagene, 
               input$alphad1scl, input$alphad1row, input$alphad1col, 
               input$alphad1cols, input$alphad1fsz) 
  }) 
  output$alphad1oup.ui <- renderUI({ 
    plotOutput("alphad1oup", height = pList3[input$alphad1psz]) 
  }) 
  output$alphad1oup.pdf <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphad1plt,"_",input$alphad1grp,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$alphad1oup.h, width = input$alphad1oup.w, 
      plot = scBubbHeat(alphaconf, alphameta, input$alphad1inp, input$alphad1grp, input$alphad1plt, 
                        "alphagexpr.h5", alphagene, 
                        input$alphad1scl, input$alphad1row, input$alphad1col, 
                        input$alphad1cols, input$alphad1fsz, save = TRUE) ) 
  }) 
  output$alphad1oup.png <- downloadHandler( 
    filename = function() { paste0("alpha",input$alphad1plt,"_",input$alphad1grp,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$alphad1oup.h, width = input$alphad1oup.w, 
      plot = scBubbHeat(alphaconf, alphameta, input$alphad1inp, input$alphad1grp, input$alphad1plt, 
                        "alphagexpr.h5", alphagene, 
                        input$alphad1scl, input$alphad1row, input$alphad1col, 
                        input$alphad1cols, input$alphad1fsz, save = TRUE) ) 
  }) 
   
   
   optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  updateSelectizeInput(session, "betaa1inp2", choices = sort(names(betagene)), server = TRUE, 
                       selected = betadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "betaa3inp1", choices = sort(names(betagene)), server = TRUE, 
                       selected = betadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "betaa3inp2", choices = sort(names(betagene)), server = TRUE, 
                       selected = betadef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "betab2inp1", choices = sort(names(betagene)), server = TRUE, 
                       selected = betadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "betab2inp2", choices = sort(names(betagene)), server = TRUE, 
                       selected = betadef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "betac1inp2", server = TRUE, 
                       choices = c(betaconf[is.na(fID)]$UI,sort(names(betagene))), 
                       selected = betaconf[is.na(fID)]$UI[1], options = list( 
                         maxOptions = length(betaconf[is.na(fID)]$UI) + 3, 
                         create = TRUE, persist = TRUE, render = I(optCrt))) 
 
 
  ### Plots for tab a1 
  output$betaa1sub1.ui <- renderUI({ 
    sub = strsplit(betaconf[UI == input$betaa1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("betaa1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$betaa1oup1 <- renderPlot({ 
    scDRcell(betaconf, betameta, input$betaa1drX, input$betaa1drY, input$betaa1inp1,  
             input$betaa1sub1, input$betaa1sub2, 
             input$betaa1siz, input$betaa1col1, input$betaa1ord1, 
             input$betaa1fsz, input$betaa1asp, input$betaa1txt, input$betaa1lab1) 
  }) 
  output$betaa1oup1.ui <- renderUI({ 
    plotOutput("betaa1oup1", height = pList[input$betaa1psz]) 
  }) 
  output$betaa1oup1.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa1drX,"_",input$betaa1drY,"_",  
                                   input$betaa1inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betaa1oup1.h, width = input$betaa1oup1.w, useDingbats = FALSE, 
      plot = scDRcell(betaconf, betameta, input$betaa1drX, input$betaa1drY, input$betaa1inp1,   
                      input$betaa1sub1, input$betaa1sub2, 
                      input$betaa1siz, input$betaa1col1, input$betaa1ord1,  
                      input$betaa1fsz, input$betaa1asp, input$betaa1txt, input$betaa1lab1) ) 
  }) 
  output$betaa1oup1.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa1drX,"_",input$betaa1drY,"_",  
                                   input$betaa1inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betaa1oup1.h, width = input$betaa1oup1.w, 
      plot = scDRcell(betaconf, betameta, input$betaa1drX, input$betaa1drY, input$betaa1inp1,   
                      input$betaa1sub1, input$betaa1sub2, 
                      input$betaa1siz, input$betaa1col1, input$betaa1ord1,  
                      input$betaa1fsz, input$betaa1asp, input$betaa1txt, input$betaa1lab1) ) 
  }) 
  output$betaa1.dt <- renderDataTable({ 
    ggData = scDRnum(betaconf, betameta, input$betaa1inp1, input$betaa1inp2, 
                     input$betaa1sub1, input$betaa1sub2, 
                     "betagexpr.h5", betagene, input$betaa1splt) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("pctExpress"), digits = 2) 
  }) 
   
  output$betaa1oup2 <- renderPlot({ 
    scDRgene(betaconf, betameta, input$betaa1drX, input$betaa1drY, input$betaa1inp2,  
             input$betaa1sub1, input$betaa1sub2, 
             "betagexpr.h5", betagene, 
             input$betaa1siz, input$betaa1col2, input$betaa1ord2, 
             input$betaa1fsz, input$betaa1asp, input$betaa1txt) 
  }) 
  output$betaa1oup2.ui <- renderUI({ 
    plotOutput("betaa1oup2", height = pList[input$betaa1psz]) 
  }) 
  output$betaa1oup2.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa1drX,"_",input$betaa1drY,"_",  
                                   input$betaa1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betaa1oup2.h, width = input$betaa1oup2.w, useDingbats = FALSE, 
      plot = scDRgene(betaconf, betameta, input$betaa1drX, input$betaa1drY, input$betaa1inp2,  
                      input$betaa1sub1, input$betaa1sub2, 
                      "betagexpr.h5", betagene, 
                      input$betaa1siz, input$betaa1col2, input$betaa1ord2, 
                      input$betaa1fsz, input$betaa1asp, input$betaa1txt) ) 
  }) 
  output$betaa1oup2.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa1drX,"_",input$betaa1drY,"_",  
                                   input$betaa1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betaa1oup2.h, width = input$betaa1oup2.w, 
      plot = scDRgene(betaconf, betameta, input$betaa1drX, input$betaa1drY, input$betaa1inp2,  
                      input$betaa1sub1, input$betaa1sub2, 
                      "betagexpr.h5", betagene, 
                      input$betaa1siz, input$betaa1col2, input$betaa1ord2, 
                      input$betaa1fsz, input$betaa1asp, input$betaa1txt) ) 
  }) 
   
   
  ### Plots for tab a2 
  output$betaa2sub1.ui <- renderUI({ 
    sub = strsplit(betaconf[UI == input$betaa2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("betaa2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$betaa2oup1 <- renderPlot({ 
    scDRcell(betaconf, betameta, input$betaa2drX, input$betaa2drY, input$betaa2inp1,  
             input$betaa2sub1, input$betaa2sub2, 
             input$betaa2siz, input$betaa2col1, input$betaa2ord1, 
             input$betaa2fsz, input$betaa2asp, input$betaa2txt, input$betaa2lab1) 
  }) 
  output$betaa2oup1.ui <- renderUI({ 
    plotOutput("betaa2oup1", height = pList[input$betaa2psz]) 
  }) 
  output$betaa2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa2drX,"_",input$betaa2drY,"_",  
                                   input$betaa2inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betaa2oup1.h, width = input$betaa2oup1.w, useDingbats = FALSE, 
      plot = scDRcell(betaconf, betameta, input$betaa2drX, input$betaa2drY, input$betaa2inp1,   
                      input$betaa2sub1, input$betaa2sub2, 
                      input$betaa2siz, input$betaa2col1, input$betaa2ord1,  
                      input$betaa2fsz, input$betaa2asp, input$betaa2txt, input$betaa2lab1) ) 
  }) 
  output$betaa2oup1.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa2drX,"_",input$betaa2drY,"_",  
                                   input$betaa2inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betaa2oup1.h, width = input$betaa2oup1.w, 
      plot = scDRcell(betaconf, betameta, input$betaa2drX, input$betaa2drY, input$betaa2inp1,   
                      input$betaa2sub1, input$betaa2sub2, 
                      input$betaa2siz, input$betaa2col1, input$betaa2ord1,  
                      input$betaa2fsz, input$betaa2asp, input$betaa2txt, input$betaa2lab1) ) 
  }) 
   
  output$betaa2oup2 <- renderPlot({ 
    scDRcell(betaconf, betameta, input$betaa2drX, input$betaa2drY, input$betaa2inp2,  
             input$betaa2sub1, input$betaa2sub2, 
             input$betaa2siz, input$betaa2col2, input$betaa2ord2, 
             input$betaa2fsz, input$betaa2asp, input$betaa2txt, input$betaa2lab2) 
  }) 
  output$betaa2oup2.ui <- renderUI({ 
    plotOutput("betaa2oup2", height = pList[input$betaa2psz]) 
  }) 
  output$betaa2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa2drX,"_",input$betaa2drY,"_",  
                                   input$betaa2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betaa2oup2.h, width = input$betaa2oup2.w, useDingbats = FALSE, 
      plot = scDRcell(betaconf, betameta, input$betaa2drX, input$betaa2drY, input$betaa2inp2,   
                      input$betaa2sub1, input$betaa2sub2, 
                      input$betaa2siz, input$betaa2col2, input$betaa2ord2,  
                      input$betaa2fsz, input$betaa2asp, input$betaa2txt, input$betaa2lab2) ) 
  }) 
  output$betaa2oup2.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa2drX,"_",input$betaa2drY,"_",  
                                   input$betaa2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betaa2oup2.h, width = input$betaa2oup2.w, 
      plot = scDRcell(betaconf, betameta, input$betaa2drX, input$betaa2drY, input$betaa2inp2,   
                      input$betaa2sub1, input$betaa2sub2, 
                      input$betaa2siz, input$betaa2col2, input$betaa2ord2,  
                      input$betaa2fsz, input$betaa2asp, input$betaa2txt, input$betaa2lab2) ) 
  }) 
   
   
  ### Plots for tab a3 
  output$betaa3sub1.ui <- renderUI({ 
    sub = strsplit(betaconf[UI == input$betaa3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("betaa3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$betaa3oup1 <- renderPlot({ 
    scDRgene(betaconf, betameta, input$betaa3drX, input$betaa3drY, input$betaa3inp1,  
             input$betaa3sub1, input$betaa3sub2, 
             "betagexpr.h5", betagene, 
             input$betaa3siz, input$betaa3col1, input$betaa3ord1, 
             input$betaa3fsz, input$betaa3asp, input$betaa3txt) 
  }) 
  output$betaa3oup1.ui <- renderUI({ 
    plotOutput("betaa3oup1", height = pList[input$betaa3psz]) 
  }) 
  output$betaa3oup1.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa3drX,"_",input$betaa3drY,"_",  
                                   input$betaa3inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betaa3oup1.h, width = input$betaa3oup1.w, useDingbats = FALSE, 
      plot = scDRgene(betaconf, betameta, input$betaa3drX, input$betaa3drY, input$betaa3inp1,  
                      input$betaa3sub1, input$betaa3sub2, 
                      "betagexpr.h5", betagene, 
                      input$betaa3siz, input$betaa3col1, input$betaa3ord1, 
                      input$betaa3fsz, input$betaa3asp, input$betaa3txt) ) 
  }) 
  output$betaa3oup1.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa3drX,"_",input$betaa3drY,"_",  
                                   input$betaa3inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betaa3oup1.h, width = input$betaa3oup1.w, 
      plot = scDRgene(betaconf, betameta, input$betaa3drX, input$betaa3drY, input$betaa3inp1,  
                      input$betaa3sub1, input$betaa3sub2, 
                      "betagexpr.h5", betagene, 
                      input$betaa3siz, input$betaa3col1, input$betaa3ord1, 
                      input$betaa3fsz, input$betaa3asp, input$betaa3txt) ) 
  }) 
   
  output$betaa3oup2 <- renderPlot({ 
    scDRgene(betaconf, betameta, input$betaa3drX, input$betaa3drY, input$betaa3inp2,  
             input$betaa3sub1, input$betaa3sub2, 
             "betagexpr.h5", betagene, 
             input$betaa3siz, input$betaa3col2, input$betaa3ord2, 
             input$betaa3fsz, input$betaa3asp, input$betaa3txt) 
  }) 
  output$betaa3oup2.ui <- renderUI({ 
    plotOutput("betaa3oup2", height = pList[input$betaa3psz]) 
  }) 
  output$betaa3oup2.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa3drX,"_",input$betaa3drY,"_",  
                                   input$betaa3inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betaa3oup2.h, width = input$betaa3oup2.w, useDingbats = FALSE, 
      plot = scDRgene(betaconf, betameta, input$betaa3drX, input$betaa3drY, input$betaa3inp2,  
                      input$betaa3sub1, input$betaa3sub2, 
                      "betagexpr.h5", betagene, 
                      input$betaa3siz, input$betaa3col2, input$betaa3ord2, 
                      input$betaa3fsz, input$betaa3asp, input$betaa3txt) ) 
  }) 
  output$betaa3oup2.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betaa3drX,"_",input$betaa3drY,"_",  
                                   input$betaa3inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betaa3oup2.h, width = input$betaa3oup2.w, 
      plot = scDRgene(betaconf, betameta, input$betaa3drX, input$betaa3drY, input$betaa3inp2,  
                      input$betaa3sub1, input$betaa3sub2, 
                      "betagexpr.h5", betagene, 
                      input$betaa3siz, input$betaa3col2, input$betaa3ord2, 
                      input$betaa3fsz, input$betaa3asp, input$betaa3txt) ) 
  }) 
     
   
  ### Plots for tab b2 
  output$betab2sub1.ui <- renderUI({ 
    sub = strsplit(betaconf[UI == input$betab2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("betab2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$betab2oup1 <- renderPlot({ 
    scDRcoex(betaconf, betameta, input$betab2drX, input$betab2drY,   
             input$betab2inp1, input$betab2inp2, input$betab2sub1, input$betab2sub2, 
             "betagexpr.h5", betagene, 
             input$betab2siz, input$betab2col1, input$betab2ord1, 
             input$betab2fsz, input$betab2asp, input$betab2txt) 
  }) 
  output$betab2oup1.ui <- renderUI({ 
    plotOutput("betab2oup1", height = pList2[input$betab2psz]) 
  }) 
  output$betab2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betab2drX,"_",input$betab2drY,"_",  
                                    input$betab2inp1,"_",input$betab2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betab2oup1.h, width = input$betab2oup1.w, useDingbats = FALSE, 
      plot = scDRcoex(betaconf, betameta, input$betab2drX, input$betab2drY,  
                      input$betab2inp1, input$betab2inp2, input$betab2sub1, input$betab2sub2, 
                      "betagexpr.h5", betagene, 
                      input$betab2siz, input$betab2col1, input$betab2ord1, 
                      input$betab2fsz, input$betab2asp, input$betab2txt) ) 
  }) 
  output$betab2oup1.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betab2drX,"_",input$betab2drY,"_",  
                                    input$betab2inp1,"_",input$betab2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betab2oup1.h, width = input$betab2oup1.w, 
      plot = scDRcoex(betaconf, betameta, input$betab2drX, input$betab2drY,  
                      input$betab2inp1, input$betab2inp2, input$betab2sub1, input$betab2sub2, 
                      "betagexpr.h5", betagene, 
                      input$betab2siz, input$betab2col1, input$betab2ord1, 
                      input$betab2fsz, input$betab2asp, input$betab2txt) ) 
  }) 
  output$betab2oup2 <- renderPlot({ 
    scDRcoexLeg(input$betab2inp1, input$betab2inp2, input$betab2col1, input$betab2fsz) 
  }) 
  output$betab2oup2.ui <- renderUI({ 
    plotOutput("betab2oup2", height = "300px") 
  }) 
  output$betab2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betab2drX,"_",input$betab2drY,"_",  
                                    input$betab2inp1,"_",input$betab2inp2,"_leg.pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = 3, width = 4, useDingbats = FALSE, 
      plot = scDRcoexLeg(input$betab2inp1, input$betab2inp2, input$betab2col1, input$betab2fsz) ) 
  }) 
  output$betab2oup2.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betab2drX,"_",input$betab2drY,"_",  
                                    input$betab2inp1,"_",input$betab2inp2,"_leg.png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = 3, width = 4, 
      plot = scDRcoexLeg(input$betab2inp1, input$betab2inp2, input$betab2col1, input$betab2fsz) ) 
  }) 
  output$betab2.dt <- renderDataTable({ 
    ggData = scDRcoexNum(betaconf, betameta, input$betab2inp1, input$betab2inp2, 
                         input$betab2sub1, input$betab2sub2, "betagexpr.h5", betagene) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
     
   
  ### Plots for tab c1 
  output$betac1oup <- renderPlot({ 
    scVioBox(betaconf, betameta, input$betac1inp1, input$betac1inp2,  
             "betagexpr.h5", betagene, input$betac1typ, input$betac1pts, 
             input$betac1siz, input$betac1fsz) 
  }) 
  output$betac1oup.ui <- renderUI({ 
    plotOutput("betac1oup", height = pList2[input$betac1psz]) 
  }) 
  output$betac1oup.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betac1typ,"_",input$betac1inp1,"_",  
                                   input$betac1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betac1oup.h, width = input$betac1oup.w, useDingbats = FALSE, 
      plot = scVioBox(betaconf, betameta, input$betac1inp1, input$betac1inp2,  
                      "betagexpr.h5", betagene, input$betac1typ, input$betac1pts, 
                      input$betac1siz, input$betac1fsz) ) 
  }) 
  output$betac1oup.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betac1typ,"_",input$betac1inp1,"_",  
                                   input$betac1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betac1oup.h, width = input$betac1oup.w, 
      plot = scVioBox(betaconf, betameta, input$betac1inp1, input$betac1inp2,  
                      "betagexpr.h5", betagene, input$betac1typ, input$betac1pts, 
                      input$betac1siz, input$betac1fsz) ) 
  }) 
     
   
### Plots for tab c2 
output$betac2oup <- renderPlot({ 
  scProp(betaconf, betameta, input$betac2inp1, input$betac2inp2,  
         input$betac2typ, input$betac2flp, input$betac2fsz) 
}) 
output$betac2oup.ui <- renderUI({ 
  plotOutput("betac2oup", height = pList2[input$betac2psz]) 
}) 
output$betac2oup.pdf <- downloadHandler( 
  filename = function() { paste0("beta",input$betac2typ,"_",input$betac2inp1,"_",  
                                 input$betac2inp2,".pdf") }, 
  content = function(file) { ggsave( 
    file, device = "pdf", height = input$betac2oup.h, width = input$betac2oup.w, useDingbats = FALSE, 
    plot = scProp(betaconf, betameta, input$betac2inp1, input$betac2inp2,  
                  input$betac2typ, input$betac2flp, input$betac2fsz) ) 
  }) 
output$betac2oup.png <- downloadHandler( 
  filename = function() { paste0("beta",input$betac2typ,"_",input$betac2inp1,"_",  
                                 input$betac2inp2,".png") }, 
  content = function(file) { ggsave( 
    file, device = "png", height = input$betac2oup.h, width = input$betac2oup.w, 
    plot = scProp(betaconf, betameta, input$betac2inp1, input$betac2inp2,  
                  input$betac2typ, input$betac2flp, input$betac2fsz) ) 
  }) 
     
   
  ### Plots for tab d1 
  output$betad1oupTxt <- renderUI({ 
    geneList = scGeneList(input$betad1inp, betagene) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  output$betad1oup <- renderPlot({ 
    scBubbHeat(betaconf, betameta, input$betad1inp, input$betad1grp, input$betad1plt, 
               "betagexpr.h5", betagene, 
               input$betad1scl, input$betad1row, input$betad1col, 
               input$betad1cols, input$betad1fsz) 
  }) 
  output$betad1oup.ui <- renderUI({ 
    plotOutput("betad1oup", height = pList3[input$betad1psz]) 
  }) 
  output$betad1oup.pdf <- downloadHandler( 
    filename = function() { paste0("beta",input$betad1plt,"_",input$betad1grp,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$betad1oup.h, width = input$betad1oup.w, 
      plot = scBubbHeat(betaconf, betameta, input$betad1inp, input$betad1grp, input$betad1plt, 
                        "betagexpr.h5", betagene, 
                        input$betad1scl, input$betad1row, input$betad1col, 
                        input$betad1cols, input$betad1fsz, save = TRUE) ) 
  }) 
  output$betad1oup.png <- downloadHandler( 
    filename = function() { paste0("beta",input$betad1plt,"_",input$betad1grp,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$betad1oup.h, width = input$betad1oup.w, 
      plot = scBubbHeat(betaconf, betameta, input$betad1inp, input$betad1grp, input$betad1plt, 
                        "betagexpr.h5", betagene, 
                        input$betad1scl, input$betad1row, input$betad1col, 
                        input$betad1cols, input$betad1fsz, save = TRUE) ) 
  }) 
   
   
   optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  updateSelectizeInput(session, "deltaa1inp2", choices = sort(names(deltagene)), server = TRUE, 
                       selected = deltadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "deltaa3inp1", choices = sort(names(deltagene)), server = TRUE, 
                       selected = deltadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "deltaa3inp2", choices = sort(names(deltagene)), server = TRUE, 
                       selected = deltadef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "deltab2inp1", choices = sort(names(deltagene)), server = TRUE, 
                       selected = deltadef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "deltab2inp2", choices = sort(names(deltagene)), server = TRUE, 
                       selected = deltadef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "deltac1inp2", server = TRUE, 
                       choices = c(deltaconf[is.na(fID)]$UI,sort(names(deltagene))), 
                       selected = deltaconf[is.na(fID)]$UI[1], options = list( 
                         maxOptions = length(deltaconf[is.na(fID)]$UI) + 3, 
                         create = TRUE, persist = TRUE, render = I(optCrt))) 
 
 
  ### Plots for tab a1 
  output$deltaa1sub1.ui <- renderUI({ 
    sub = strsplit(deltaconf[UI == input$deltaa1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("deltaa1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$deltaa1oup1 <- renderPlot({ 
    scDRcell(deltaconf, deltameta, input$deltaa1drX, input$deltaa1drY, input$deltaa1inp1,  
             input$deltaa1sub1, input$deltaa1sub2, 
             input$deltaa1siz, input$deltaa1col1, input$deltaa1ord1, 
             input$deltaa1fsz, input$deltaa1asp, input$deltaa1txt, input$deltaa1lab1) 
  }) 
  output$deltaa1oup1.ui <- renderUI({ 
    plotOutput("deltaa1oup1", height = pList[input$deltaa1psz]) 
  }) 
  output$deltaa1oup1.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa1drX,"_",input$deltaa1drY,"_",  
                                   input$deltaa1inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltaa1oup1.h, width = input$deltaa1oup1.w, useDingbats = FALSE, 
      plot = scDRcell(deltaconf, deltameta, input$deltaa1drX, input$deltaa1drY, input$deltaa1inp1,   
                      input$deltaa1sub1, input$deltaa1sub2, 
                      input$deltaa1siz, input$deltaa1col1, input$deltaa1ord1,  
                      input$deltaa1fsz, input$deltaa1asp, input$deltaa1txt, input$deltaa1lab1) ) 
  }) 
  output$deltaa1oup1.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa1drX,"_",input$deltaa1drY,"_",  
                                   input$deltaa1inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltaa1oup1.h, width = input$deltaa1oup1.w, 
      plot = scDRcell(deltaconf, deltameta, input$deltaa1drX, input$deltaa1drY, input$deltaa1inp1,   
                      input$deltaa1sub1, input$deltaa1sub2, 
                      input$deltaa1siz, input$deltaa1col1, input$deltaa1ord1,  
                      input$deltaa1fsz, input$deltaa1asp, input$deltaa1txt, input$deltaa1lab1) ) 
  }) 
  output$deltaa1.dt <- renderDataTable({ 
    ggData = scDRnum(deltaconf, deltameta, input$deltaa1inp1, input$deltaa1inp2, 
                     input$deltaa1sub1, input$deltaa1sub2, 
                     "deltagexpr.h5", deltagene, input$deltaa1splt) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("pctExpress"), digits = 2) 
  }) 
   
  output$deltaa1oup2 <- renderPlot({ 
    scDRgene(deltaconf, deltameta, input$deltaa1drX, input$deltaa1drY, input$deltaa1inp2,  
             input$deltaa1sub1, input$deltaa1sub2, 
             "deltagexpr.h5", deltagene, 
             input$deltaa1siz, input$deltaa1col2, input$deltaa1ord2, 
             input$deltaa1fsz, input$deltaa1asp, input$deltaa1txt) 
  }) 
  output$deltaa1oup2.ui <- renderUI({ 
    plotOutput("deltaa1oup2", height = pList[input$deltaa1psz]) 
  }) 
  output$deltaa1oup2.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa1drX,"_",input$deltaa1drY,"_",  
                                   input$deltaa1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltaa1oup2.h, width = input$deltaa1oup2.w, useDingbats = FALSE, 
      plot = scDRgene(deltaconf, deltameta, input$deltaa1drX, input$deltaa1drY, input$deltaa1inp2,  
                      input$deltaa1sub1, input$deltaa1sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltaa1siz, input$deltaa1col2, input$deltaa1ord2, 
                      input$deltaa1fsz, input$deltaa1asp, input$deltaa1txt) ) 
  }) 
  output$deltaa1oup2.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa1drX,"_",input$deltaa1drY,"_",  
                                   input$deltaa1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltaa1oup2.h, width = input$deltaa1oup2.w, 
      plot = scDRgene(deltaconf, deltameta, input$deltaa1drX, input$deltaa1drY, input$deltaa1inp2,  
                      input$deltaa1sub1, input$deltaa1sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltaa1siz, input$deltaa1col2, input$deltaa1ord2, 
                      input$deltaa1fsz, input$deltaa1asp, input$deltaa1txt) ) 
  }) 
   
   
  ### Plots for tab a2 
  output$deltaa2sub1.ui <- renderUI({ 
    sub = strsplit(deltaconf[UI == input$deltaa2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("deltaa2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$deltaa2oup1 <- renderPlot({ 
    scDRcell(deltaconf, deltameta, input$deltaa2drX, input$deltaa2drY, input$deltaa2inp1,  
             input$deltaa2sub1, input$deltaa2sub2, 
             input$deltaa2siz, input$deltaa2col1, input$deltaa2ord1, 
             input$deltaa2fsz, input$deltaa2asp, input$deltaa2txt, input$deltaa2lab1) 
  }) 
  output$deltaa2oup1.ui <- renderUI({ 
    plotOutput("deltaa2oup1", height = pList[input$deltaa2psz]) 
  }) 
  output$deltaa2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa2drX,"_",input$deltaa2drY,"_",  
                                   input$deltaa2inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltaa2oup1.h, width = input$deltaa2oup1.w, useDingbats = FALSE, 
      plot = scDRcell(deltaconf, deltameta, input$deltaa2drX, input$deltaa2drY, input$deltaa2inp1,   
                      input$deltaa2sub1, input$deltaa2sub2, 
                      input$deltaa2siz, input$deltaa2col1, input$deltaa2ord1,  
                      input$deltaa2fsz, input$deltaa2asp, input$deltaa2txt, input$deltaa2lab1) ) 
  }) 
  output$deltaa2oup1.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa2drX,"_",input$deltaa2drY,"_",  
                                   input$deltaa2inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltaa2oup1.h, width = input$deltaa2oup1.w, 
      plot = scDRcell(deltaconf, deltameta, input$deltaa2drX, input$deltaa2drY, input$deltaa2inp1,   
                      input$deltaa2sub1, input$deltaa2sub2, 
                      input$deltaa2siz, input$deltaa2col1, input$deltaa2ord1,  
                      input$deltaa2fsz, input$deltaa2asp, input$deltaa2txt, input$deltaa2lab1) ) 
  }) 
   
  output$deltaa2oup2 <- renderPlot({ 
    scDRcell(deltaconf, deltameta, input$deltaa2drX, input$deltaa2drY, input$deltaa2inp2,  
             input$deltaa2sub1, input$deltaa2sub2, 
             input$deltaa2siz, input$deltaa2col2, input$deltaa2ord2, 
             input$deltaa2fsz, input$deltaa2asp, input$deltaa2txt, input$deltaa2lab2) 
  }) 
  output$deltaa2oup2.ui <- renderUI({ 
    plotOutput("deltaa2oup2", height = pList[input$deltaa2psz]) 
  }) 
  output$deltaa2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa2drX,"_",input$deltaa2drY,"_",  
                                   input$deltaa2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltaa2oup2.h, width = input$deltaa2oup2.w, useDingbats = FALSE, 
      plot = scDRcell(deltaconf, deltameta, input$deltaa2drX, input$deltaa2drY, input$deltaa2inp2,   
                      input$deltaa2sub1, input$deltaa2sub2, 
                      input$deltaa2siz, input$deltaa2col2, input$deltaa2ord2,  
                      input$deltaa2fsz, input$deltaa2asp, input$deltaa2txt, input$deltaa2lab2) ) 
  }) 
  output$deltaa2oup2.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa2drX,"_",input$deltaa2drY,"_",  
                                   input$deltaa2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltaa2oup2.h, width = input$deltaa2oup2.w, 
      plot = scDRcell(deltaconf, deltameta, input$deltaa2drX, input$deltaa2drY, input$deltaa2inp2,   
                      input$deltaa2sub1, input$deltaa2sub2, 
                      input$deltaa2siz, input$deltaa2col2, input$deltaa2ord2,  
                      input$deltaa2fsz, input$deltaa2asp, input$deltaa2txt, input$deltaa2lab2) ) 
  }) 
   
   
  ### Plots for tab a3 
  output$deltaa3sub1.ui <- renderUI({ 
    sub = strsplit(deltaconf[UI == input$deltaa3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("deltaa3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$deltaa3oup1 <- renderPlot({ 
    scDRgene(deltaconf, deltameta, input$deltaa3drX, input$deltaa3drY, input$deltaa3inp1,  
             input$deltaa3sub1, input$deltaa3sub2, 
             "deltagexpr.h5", deltagene, 
             input$deltaa3siz, input$deltaa3col1, input$deltaa3ord1, 
             input$deltaa3fsz, input$deltaa3asp, input$deltaa3txt) 
  }) 
  output$deltaa3oup1.ui <- renderUI({ 
    plotOutput("deltaa3oup1", height = pList[input$deltaa3psz]) 
  }) 
  output$deltaa3oup1.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa3drX,"_",input$deltaa3drY,"_",  
                                   input$deltaa3inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltaa3oup1.h, width = input$deltaa3oup1.w, useDingbats = FALSE, 
      plot = scDRgene(deltaconf, deltameta, input$deltaa3drX, input$deltaa3drY, input$deltaa3inp1,  
                      input$deltaa3sub1, input$deltaa3sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltaa3siz, input$deltaa3col1, input$deltaa3ord1, 
                      input$deltaa3fsz, input$deltaa3asp, input$deltaa3txt) ) 
  }) 
  output$deltaa3oup1.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa3drX,"_",input$deltaa3drY,"_",  
                                   input$deltaa3inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltaa3oup1.h, width = input$deltaa3oup1.w, 
      plot = scDRgene(deltaconf, deltameta, input$deltaa3drX, input$deltaa3drY, input$deltaa3inp1,  
                      input$deltaa3sub1, input$deltaa3sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltaa3siz, input$deltaa3col1, input$deltaa3ord1, 
                      input$deltaa3fsz, input$deltaa3asp, input$deltaa3txt) ) 
  }) 
   
  output$deltaa3oup2 <- renderPlot({ 
    scDRgene(deltaconf, deltameta, input$deltaa3drX, input$deltaa3drY, input$deltaa3inp2,  
             input$deltaa3sub1, input$deltaa3sub2, 
             "deltagexpr.h5", deltagene, 
             input$deltaa3siz, input$deltaa3col2, input$deltaa3ord2, 
             input$deltaa3fsz, input$deltaa3asp, input$deltaa3txt) 
  }) 
  output$deltaa3oup2.ui <- renderUI({ 
    plotOutput("deltaa3oup2", height = pList[input$deltaa3psz]) 
  }) 
  output$deltaa3oup2.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa3drX,"_",input$deltaa3drY,"_",  
                                   input$deltaa3inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltaa3oup2.h, width = input$deltaa3oup2.w, useDingbats = FALSE, 
      plot = scDRgene(deltaconf, deltameta, input$deltaa3drX, input$deltaa3drY, input$deltaa3inp2,  
                      input$deltaa3sub1, input$deltaa3sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltaa3siz, input$deltaa3col2, input$deltaa3ord2, 
                      input$deltaa3fsz, input$deltaa3asp, input$deltaa3txt) ) 
  }) 
  output$deltaa3oup2.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltaa3drX,"_",input$deltaa3drY,"_",  
                                   input$deltaa3inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltaa3oup2.h, width = input$deltaa3oup2.w, 
      plot = scDRgene(deltaconf, deltameta, input$deltaa3drX, input$deltaa3drY, input$deltaa3inp2,  
                      input$deltaa3sub1, input$deltaa3sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltaa3siz, input$deltaa3col2, input$deltaa3ord2, 
                      input$deltaa3fsz, input$deltaa3asp, input$deltaa3txt) ) 
  }) 
     
   
  ### Plots for tab b2 
  output$deltab2sub1.ui <- renderUI({ 
    sub = strsplit(deltaconf[UI == input$deltab2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("deltab2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$deltab2oup1 <- renderPlot({ 
    scDRcoex(deltaconf, deltameta, input$deltab2drX, input$deltab2drY,   
             input$deltab2inp1, input$deltab2inp2, input$deltab2sub1, input$deltab2sub2, 
             "deltagexpr.h5", deltagene, 
             input$deltab2siz, input$deltab2col1, input$deltab2ord1, 
             input$deltab2fsz, input$deltab2asp, input$deltab2txt) 
  }) 
  output$deltab2oup1.ui <- renderUI({ 
    plotOutput("deltab2oup1", height = pList2[input$deltab2psz]) 
  }) 
  output$deltab2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltab2drX,"_",input$deltab2drY,"_",  
                                    input$deltab2inp1,"_",input$deltab2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltab2oup1.h, width = input$deltab2oup1.w, useDingbats = FALSE, 
      plot = scDRcoex(deltaconf, deltameta, input$deltab2drX, input$deltab2drY,  
                      input$deltab2inp1, input$deltab2inp2, input$deltab2sub1, input$deltab2sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltab2siz, input$deltab2col1, input$deltab2ord1, 
                      input$deltab2fsz, input$deltab2asp, input$deltab2txt) ) 
  }) 
  output$deltab2oup1.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltab2drX,"_",input$deltab2drY,"_",  
                                    input$deltab2inp1,"_",input$deltab2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltab2oup1.h, width = input$deltab2oup1.w, 
      plot = scDRcoex(deltaconf, deltameta, input$deltab2drX, input$deltab2drY,  
                      input$deltab2inp1, input$deltab2inp2, input$deltab2sub1, input$deltab2sub2, 
                      "deltagexpr.h5", deltagene, 
                      input$deltab2siz, input$deltab2col1, input$deltab2ord1, 
                      input$deltab2fsz, input$deltab2asp, input$deltab2txt) ) 
  }) 
  output$deltab2oup2 <- renderPlot({ 
    scDRcoexLeg(input$deltab2inp1, input$deltab2inp2, input$deltab2col1, input$deltab2fsz) 
  }) 
  output$deltab2oup2.ui <- renderUI({ 
    plotOutput("deltab2oup2", height = "300px") 
  }) 
  output$deltab2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltab2drX,"_",input$deltab2drY,"_",  
                                    input$deltab2inp1,"_",input$deltab2inp2,"_leg.pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = 3, width = 4, useDingbats = FALSE, 
      plot = scDRcoexLeg(input$deltab2inp1, input$deltab2inp2, input$deltab2col1, input$deltab2fsz) ) 
  }) 
  output$deltab2oup2.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltab2drX,"_",input$deltab2drY,"_",  
                                    input$deltab2inp1,"_",input$deltab2inp2,"_leg.png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = 3, width = 4, 
      plot = scDRcoexLeg(input$deltab2inp1, input$deltab2inp2, input$deltab2col1, input$deltab2fsz) ) 
  }) 
  output$deltab2.dt <- renderDataTable({ 
    ggData = scDRcoexNum(deltaconf, deltameta, input$deltab2inp1, input$deltab2inp2, 
                         input$deltab2sub1, input$deltab2sub2, "deltagexpr.h5", deltagene) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
     
   
  ### Plots for tab c1 
  output$deltac1oup <- renderPlot({ 
    scVioBox(deltaconf, deltameta, input$deltac1inp1, input$deltac1inp2,  
             "deltagexpr.h5", deltagene, input$deltac1typ, input$deltac1pts, 
             input$deltac1siz, input$deltac1fsz) 
  }) 
  output$deltac1oup.ui <- renderUI({ 
    plotOutput("deltac1oup", height = pList2[input$deltac1psz]) 
  }) 
  output$deltac1oup.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltac1typ,"_",input$deltac1inp1,"_",  
                                   input$deltac1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltac1oup.h, width = input$deltac1oup.w, useDingbats = FALSE, 
      plot = scVioBox(deltaconf, deltameta, input$deltac1inp1, input$deltac1inp2,  
                      "deltagexpr.h5", deltagene, input$deltac1typ, input$deltac1pts, 
                      input$deltac1siz, input$deltac1fsz) ) 
  }) 
  output$deltac1oup.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltac1typ,"_",input$deltac1inp1,"_",  
                                   input$deltac1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltac1oup.h, width = input$deltac1oup.w, 
      plot = scVioBox(deltaconf, deltameta, input$deltac1inp1, input$deltac1inp2,  
                      "deltagexpr.h5", deltagene, input$deltac1typ, input$deltac1pts, 
                      input$deltac1siz, input$deltac1fsz) ) 
  }) 
     
   
### Plots for tab c2 
output$deltac2oup <- renderPlot({ 
  scProp(deltaconf, deltameta, input$deltac2inp1, input$deltac2inp2,  
         input$deltac2typ, input$deltac2flp, input$deltac2fsz) 
}) 
output$deltac2oup.ui <- renderUI({ 
  plotOutput("deltac2oup", height = pList2[input$deltac2psz]) 
}) 
output$deltac2oup.pdf <- downloadHandler( 
  filename = function() { paste0("delta",input$deltac2typ,"_",input$deltac2inp1,"_",  
                                 input$deltac2inp2,".pdf") }, 
  content = function(file) { ggsave( 
    file, device = "pdf", height = input$deltac2oup.h, width = input$deltac2oup.w, useDingbats = FALSE, 
    plot = scProp(deltaconf, deltameta, input$deltac2inp1, input$deltac2inp2,  
                  input$deltac2typ, input$deltac2flp, input$deltac2fsz) ) 
  }) 
output$deltac2oup.png <- downloadHandler( 
  filename = function() { paste0("delta",input$deltac2typ,"_",input$deltac2inp1,"_",  
                                 input$deltac2inp2,".png") }, 
  content = function(file) { ggsave( 
    file, device = "png", height = input$deltac2oup.h, width = input$deltac2oup.w, 
    plot = scProp(deltaconf, deltameta, input$deltac2inp1, input$deltac2inp2,  
                  input$deltac2typ, input$deltac2flp, input$deltac2fsz) ) 
  }) 
     
   
  ### Plots for tab d1 
  output$deltad1oupTxt <- renderUI({ 
    geneList = scGeneList(input$deltad1inp, deltagene) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  output$deltad1oup <- renderPlot({ 
    scBubbHeat(deltaconf, deltameta, input$deltad1inp, input$deltad1grp, input$deltad1plt, 
               "deltagexpr.h5", deltagene, 
               input$deltad1scl, input$deltad1row, input$deltad1col, 
               input$deltad1cols, input$deltad1fsz) 
  }) 
  output$deltad1oup.ui <- renderUI({ 
    plotOutput("deltad1oup", height = pList3[input$deltad1psz]) 
  }) 
  output$deltad1oup.pdf <- downloadHandler( 
    filename = function() { paste0("delta",input$deltad1plt,"_",input$deltad1grp,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$deltad1oup.h, width = input$deltad1oup.w, 
      plot = scBubbHeat(deltaconf, deltameta, input$deltad1inp, input$deltad1grp, input$deltad1plt, 
                        "deltagexpr.h5", deltagene, 
                        input$deltad1scl, input$deltad1row, input$deltad1col, 
                        input$deltad1cols, input$deltad1fsz, save = TRUE) ) 
  }) 
  output$deltad1oup.png <- downloadHandler( 
    filename = function() { paste0("delta",input$deltad1plt,"_",input$deltad1grp,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$deltad1oup.h, width = input$deltad1oup.w, 
      plot = scBubbHeat(deltaconf, deltameta, input$deltad1inp, input$deltad1grp, input$deltad1plt, 
                        "deltagexpr.h5", deltagene, 
                        input$deltad1scl, input$deltad1row, input$deltad1col, 
                        input$deltad1cols, input$deltad1fsz, save = TRUE) ) 
  }) 
   
   
   optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }" 
  updateSelectizeInput(session, "dpa1inp2", choices = sort(names(dpgene)), server = TRUE, 
                       selected = dpdef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "dpa3inp1", choices = sort(names(dpgene)), server = TRUE, 
                       selected = dpdef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "dpa3inp2", choices = sort(names(dpgene)), server = TRUE, 
                       selected = dpdef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "dpb2inp1", choices = sort(names(dpgene)), server = TRUE, 
                       selected = dpdef$gene1, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "dpb2inp2", choices = sort(names(dpgene)), server = TRUE, 
                       selected = dpdef$gene2, options = list( 
                         maxOptions = 6, create = TRUE, persist = TRUE, render = I(optCrt))) 
  updateSelectizeInput(session, "dpc1inp2", server = TRUE, 
                       choices = c(dpconf[is.na(fID)]$UI,sort(names(dpgene))), 
                       selected = dpconf[is.na(fID)]$UI[1], options = list( 
                         maxOptions = length(dpconf[is.na(fID)]$UI) + 3, 
                         create = TRUE, persist = TRUE, render = I(optCrt))) 
 
 
  ### Plots for tab a1 
  output$dpa1sub1.ui <- renderUI({ 
    sub = strsplit(dpconf[UI == input$dpa1sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("dpa1sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$dpa1oup1 <- renderPlot({ 
    scDRcell(dpconf, dpmeta, input$dpa1drX, input$dpa1drY, input$dpa1inp1,  
             input$dpa1sub1, input$dpa1sub2, 
             input$dpa1siz, input$dpa1col1, input$dpa1ord1, 
             input$dpa1fsz, input$dpa1asp, input$dpa1txt, input$dpa1lab1) 
  }) 
  output$dpa1oup1.ui <- renderUI({ 
    plotOutput("dpa1oup1", height = pList[input$dpa1psz]) 
  }) 
  output$dpa1oup1.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa1drX,"_",input$dpa1drY,"_",  
                                   input$dpa1inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpa1oup1.h, width = input$dpa1oup1.w, useDingbats = FALSE, 
      plot = scDRcell(dpconf, dpmeta, input$dpa1drX, input$dpa1drY, input$dpa1inp1,   
                      input$dpa1sub1, input$dpa1sub2, 
                      input$dpa1siz, input$dpa1col1, input$dpa1ord1,  
                      input$dpa1fsz, input$dpa1asp, input$dpa1txt, input$dpa1lab1) ) 
  }) 
  output$dpa1oup1.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa1drX,"_",input$dpa1drY,"_",  
                                   input$dpa1inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpa1oup1.h, width = input$dpa1oup1.w, 
      plot = scDRcell(dpconf, dpmeta, input$dpa1drX, input$dpa1drY, input$dpa1inp1,   
                      input$dpa1sub1, input$dpa1sub2, 
                      input$dpa1siz, input$dpa1col1, input$dpa1ord1,  
                      input$dpa1fsz, input$dpa1asp, input$dpa1txt, input$dpa1lab1) ) 
  }) 
  output$dpa1.dt <- renderDataTable({ 
    ggData = scDRnum(dpconf, dpmeta, input$dpa1inp1, input$dpa1inp2, 
                     input$dpa1sub1, input$dpa1sub2, 
                     "dpgexpr.h5", dpgene, input$dpa1splt) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("pctExpress"), digits = 2) 
  }) 
   
  output$dpa1oup2 <- renderPlot({ 
    scDRgene(dpconf, dpmeta, input$dpa1drX, input$dpa1drY, input$dpa1inp2,  
             input$dpa1sub1, input$dpa1sub2, 
             "dpgexpr.h5", dpgene, 
             input$dpa1siz, input$dpa1col2, input$dpa1ord2, 
             input$dpa1fsz, input$dpa1asp, input$dpa1txt) 
  }) 
  output$dpa1oup2.ui <- renderUI({ 
    plotOutput("dpa1oup2", height = pList[input$dpa1psz]) 
  }) 
  output$dpa1oup2.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa1drX,"_",input$dpa1drY,"_",  
                                   input$dpa1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpa1oup2.h, width = input$dpa1oup2.w, useDingbats = FALSE, 
      plot = scDRgene(dpconf, dpmeta, input$dpa1drX, input$dpa1drY, input$dpa1inp2,  
                      input$dpa1sub1, input$dpa1sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpa1siz, input$dpa1col2, input$dpa1ord2, 
                      input$dpa1fsz, input$dpa1asp, input$dpa1txt) ) 
  }) 
  output$dpa1oup2.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa1drX,"_",input$dpa1drY,"_",  
                                   input$dpa1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpa1oup2.h, width = input$dpa1oup2.w, 
      plot = scDRgene(dpconf, dpmeta, input$dpa1drX, input$dpa1drY, input$dpa1inp2,  
                      input$dpa1sub1, input$dpa1sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpa1siz, input$dpa1col2, input$dpa1ord2, 
                      input$dpa1fsz, input$dpa1asp, input$dpa1txt) ) 
  }) 
   
   
  ### Plots for tab a2 
  output$dpa2sub1.ui <- renderUI({ 
    sub = strsplit(dpconf[UI == input$dpa2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("dpa2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$dpa2oup1 <- renderPlot({ 
    scDRcell(dpconf, dpmeta, input$dpa2drX, input$dpa2drY, input$dpa2inp1,  
             input$dpa2sub1, input$dpa2sub2, 
             input$dpa2siz, input$dpa2col1, input$dpa2ord1, 
             input$dpa2fsz, input$dpa2asp, input$dpa2txt, input$dpa2lab1) 
  }) 
  output$dpa2oup1.ui <- renderUI({ 
    plotOutput("dpa2oup1", height = pList[input$dpa2psz]) 
  }) 
  output$dpa2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa2drX,"_",input$dpa2drY,"_",  
                                   input$dpa2inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpa2oup1.h, width = input$dpa2oup1.w, useDingbats = FALSE, 
      plot = scDRcell(dpconf, dpmeta, input$dpa2drX, input$dpa2drY, input$dpa2inp1,   
                      input$dpa2sub1, input$dpa2sub2, 
                      input$dpa2siz, input$dpa2col1, input$dpa2ord1,  
                      input$dpa2fsz, input$dpa2asp, input$dpa2txt, input$dpa2lab1) ) 
  }) 
  output$dpa2oup1.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa2drX,"_",input$dpa2drY,"_",  
                                   input$dpa2inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpa2oup1.h, width = input$dpa2oup1.w, 
      plot = scDRcell(dpconf, dpmeta, input$dpa2drX, input$dpa2drY, input$dpa2inp1,   
                      input$dpa2sub1, input$dpa2sub2, 
                      input$dpa2siz, input$dpa2col1, input$dpa2ord1,  
                      input$dpa2fsz, input$dpa2asp, input$dpa2txt, input$dpa2lab1) ) 
  }) 
   
  output$dpa2oup2 <- renderPlot({ 
    scDRcell(dpconf, dpmeta, input$dpa2drX, input$dpa2drY, input$dpa2inp2,  
             input$dpa2sub1, input$dpa2sub2, 
             input$dpa2siz, input$dpa2col2, input$dpa2ord2, 
             input$dpa2fsz, input$dpa2asp, input$dpa2txt, input$dpa2lab2) 
  }) 
  output$dpa2oup2.ui <- renderUI({ 
    plotOutput("dpa2oup2", height = pList[input$dpa2psz]) 
  }) 
  output$dpa2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa2drX,"_",input$dpa2drY,"_",  
                                   input$dpa2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpa2oup2.h, width = input$dpa2oup2.w, useDingbats = FALSE, 
      plot = scDRcell(dpconf, dpmeta, input$dpa2drX, input$dpa2drY, input$dpa2inp2,   
                      input$dpa2sub1, input$dpa2sub2, 
                      input$dpa2siz, input$dpa2col2, input$dpa2ord2,  
                      input$dpa2fsz, input$dpa2asp, input$dpa2txt, input$dpa2lab2) ) 
  }) 
  output$dpa2oup2.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa2drX,"_",input$dpa2drY,"_",  
                                   input$dpa2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpa2oup2.h, width = input$dpa2oup2.w, 
      plot = scDRcell(dpconf, dpmeta, input$dpa2drX, input$dpa2drY, input$dpa2inp2,   
                      input$dpa2sub1, input$dpa2sub2, 
                      input$dpa2siz, input$dpa2col2, input$dpa2ord2,  
                      input$dpa2fsz, input$dpa2asp, input$dpa2txt, input$dpa2lab2) ) 
  }) 
   
   
  ### Plots for tab a3 
  output$dpa3sub1.ui <- renderUI({ 
    sub = strsplit(dpconf[UI == input$dpa3sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("dpa3sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$dpa3oup1 <- renderPlot({ 
    scDRgene(dpconf, dpmeta, input$dpa3drX, input$dpa3drY, input$dpa3inp1,  
             input$dpa3sub1, input$dpa3sub2, 
             "dpgexpr.h5", dpgene, 
             input$dpa3siz, input$dpa3col1, input$dpa3ord1, 
             input$dpa3fsz, input$dpa3asp, input$dpa3txt) 
  }) 
  output$dpa3oup1.ui <- renderUI({ 
    plotOutput("dpa3oup1", height = pList[input$dpa3psz]) 
  }) 
  output$dpa3oup1.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa3drX,"_",input$dpa3drY,"_",  
                                   input$dpa3inp1,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpa3oup1.h, width = input$dpa3oup1.w, useDingbats = FALSE, 
      plot = scDRgene(dpconf, dpmeta, input$dpa3drX, input$dpa3drY, input$dpa3inp1,  
                      input$dpa3sub1, input$dpa3sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpa3siz, input$dpa3col1, input$dpa3ord1, 
                      input$dpa3fsz, input$dpa3asp, input$dpa3txt) ) 
  }) 
  output$dpa3oup1.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa3drX,"_",input$dpa3drY,"_",  
                                   input$dpa3inp1,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpa3oup1.h, width = input$dpa3oup1.w, 
      plot = scDRgene(dpconf, dpmeta, input$dpa3drX, input$dpa3drY, input$dpa3inp1,  
                      input$dpa3sub1, input$dpa3sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpa3siz, input$dpa3col1, input$dpa3ord1, 
                      input$dpa3fsz, input$dpa3asp, input$dpa3txt) ) 
  }) 
   
  output$dpa3oup2 <- renderPlot({ 
    scDRgene(dpconf, dpmeta, input$dpa3drX, input$dpa3drY, input$dpa3inp2,  
             input$dpa3sub1, input$dpa3sub2, 
             "dpgexpr.h5", dpgene, 
             input$dpa3siz, input$dpa3col2, input$dpa3ord2, 
             input$dpa3fsz, input$dpa3asp, input$dpa3txt) 
  }) 
  output$dpa3oup2.ui <- renderUI({ 
    plotOutput("dpa3oup2", height = pList[input$dpa3psz]) 
  }) 
  output$dpa3oup2.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa3drX,"_",input$dpa3drY,"_",  
                                   input$dpa3inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpa3oup2.h, width = input$dpa3oup2.w, useDingbats = FALSE, 
      plot = scDRgene(dpconf, dpmeta, input$dpa3drX, input$dpa3drY, input$dpa3inp2,  
                      input$dpa3sub1, input$dpa3sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpa3siz, input$dpa3col2, input$dpa3ord2, 
                      input$dpa3fsz, input$dpa3asp, input$dpa3txt) ) 
  }) 
  output$dpa3oup2.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpa3drX,"_",input$dpa3drY,"_",  
                                   input$dpa3inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpa3oup2.h, width = input$dpa3oup2.w, 
      plot = scDRgene(dpconf, dpmeta, input$dpa3drX, input$dpa3drY, input$dpa3inp2,  
                      input$dpa3sub1, input$dpa3sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpa3siz, input$dpa3col2, input$dpa3ord2, 
                      input$dpa3fsz, input$dpa3asp, input$dpa3txt) ) 
  }) 
     
   
  ### Plots for tab b2 
  output$dpb2sub1.ui <- renderUI({ 
    sub = strsplit(dpconf[UI == input$dpb2sub1]$fID, "\\|")[[1]] 
    checkboxGroupInput("dpb2sub2", "Select which cells to show", inline = TRUE, 
                       choices = sub, selected = sub) 
  }) 
  output$dpb2oup1 <- renderPlot({ 
    scDRcoex(dpconf, dpmeta, input$dpb2drX, input$dpb2drY,   
             input$dpb2inp1, input$dpb2inp2, input$dpb2sub1, input$dpb2sub2, 
             "dpgexpr.h5", dpgene, 
             input$dpb2siz, input$dpb2col1, input$dpb2ord1, 
             input$dpb2fsz, input$dpb2asp, input$dpb2txt) 
  }) 
  output$dpb2oup1.ui <- renderUI({ 
    plotOutput("dpb2oup1", height = pList2[input$dpb2psz]) 
  }) 
  output$dpb2oup1.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpb2drX,"_",input$dpb2drY,"_",  
                                    input$dpb2inp1,"_",input$dpb2inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpb2oup1.h, width = input$dpb2oup1.w, useDingbats = FALSE, 
      plot = scDRcoex(dpconf, dpmeta, input$dpb2drX, input$dpb2drY,  
                      input$dpb2inp1, input$dpb2inp2, input$dpb2sub1, input$dpb2sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpb2siz, input$dpb2col1, input$dpb2ord1, 
                      input$dpb2fsz, input$dpb2asp, input$dpb2txt) ) 
  }) 
  output$dpb2oup1.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpb2drX,"_",input$dpb2drY,"_",  
                                    input$dpb2inp1,"_",input$dpb2inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpb2oup1.h, width = input$dpb2oup1.w, 
      plot = scDRcoex(dpconf, dpmeta, input$dpb2drX, input$dpb2drY,  
                      input$dpb2inp1, input$dpb2inp2, input$dpb2sub1, input$dpb2sub2, 
                      "dpgexpr.h5", dpgene, 
                      input$dpb2siz, input$dpb2col1, input$dpb2ord1, 
                      input$dpb2fsz, input$dpb2asp, input$dpb2txt) ) 
  }) 
  output$dpb2oup2 <- renderPlot({ 
    scDRcoexLeg(input$dpb2inp1, input$dpb2inp2, input$dpb2col1, input$dpb2fsz) 
  }) 
  output$dpb2oup2.ui <- renderUI({ 
    plotOutput("dpb2oup2", height = "300px") 
  }) 
  output$dpb2oup2.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpb2drX,"_",input$dpb2drY,"_",  
                                    input$dpb2inp1,"_",input$dpb2inp2,"_leg.pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = 3, width = 4, useDingbats = FALSE, 
      plot = scDRcoexLeg(input$dpb2inp1, input$dpb2inp2, input$dpb2col1, input$dpb2fsz) ) 
  }) 
  output$dpb2oup2.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpb2drX,"_",input$dpb2drY,"_",  
                                    input$dpb2inp1,"_",input$dpb2inp2,"_leg.png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = 3, width = 4, 
      plot = scDRcoexLeg(input$dpb2inp1, input$dpb2inp2, input$dpb2col1, input$dpb2fsz) ) 
  }) 
  output$dpb2.dt <- renderDataTable({ 
    ggData = scDRcoexNum(dpconf, dpmeta, input$dpb2inp1, input$dpb2inp2, 
                         input$dpb2sub1, input$dpb2sub2, "dpgexpr.h5", dpgene) 
    datatable(ggData, rownames = FALSE, extensions = "Buttons", 
              options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
      formatRound(columns = c("percent"), digits = 2) 
  }) 
     
   
  ### Plots for tab c1 
  output$dpc1oup <- renderPlot({ 
    scVioBox(dpconf, dpmeta, input$dpc1inp1, input$dpc1inp2,  
             "dpgexpr.h5", dpgene, input$dpc1typ, input$dpc1pts, 
             input$dpc1siz, input$dpc1fsz) 
  }) 
  output$dpc1oup.ui <- renderUI({ 
    plotOutput("dpc1oup", height = pList2[input$dpc1psz]) 
  }) 
  output$dpc1oup.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpc1typ,"_",input$dpc1inp1,"_",  
                                   input$dpc1inp2,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpc1oup.h, width = input$dpc1oup.w, useDingbats = FALSE, 
      plot = scVioBox(dpconf, dpmeta, input$dpc1inp1, input$dpc1inp2,  
                      "dpgexpr.h5", dpgene, input$dpc1typ, input$dpc1pts, 
                      input$dpc1siz, input$dpc1fsz) ) 
  }) 
  output$dpc1oup.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpc1typ,"_",input$dpc1inp1,"_",  
                                   input$dpc1inp2,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpc1oup.h, width = input$dpc1oup.w, 
      plot = scVioBox(dpconf, dpmeta, input$dpc1inp1, input$dpc1inp2,  
                      "dpgexpr.h5", dpgene, input$dpc1typ, input$dpc1pts, 
                      input$dpc1siz, input$dpc1fsz) ) 
  }) 
     
   
### Plots for tab c2 
output$dpc2oup <- renderPlot({ 
  scProp(dpconf, dpmeta, input$dpc2inp1, input$dpc2inp2,  
         input$dpc2typ, input$dpc2flp, input$dpc2fsz) 
}) 
output$dpc2oup.ui <- renderUI({ 
  plotOutput("dpc2oup", height = pList2[input$dpc2psz]) 
}) 
output$dpc2oup.pdf <- downloadHandler( 
  filename = function() { paste0("dp",input$dpc2typ,"_",input$dpc2inp1,"_",  
                                 input$dpc2inp2,".pdf") }, 
  content = function(file) { ggsave( 
    file, device = "pdf", height = input$dpc2oup.h, width = input$dpc2oup.w, useDingbats = FALSE, 
    plot = scProp(dpconf, dpmeta, input$dpc2inp1, input$dpc2inp2,  
                  input$dpc2typ, input$dpc2flp, input$dpc2fsz) ) 
  }) 
output$dpc2oup.png <- downloadHandler( 
  filename = function() { paste0("dp",input$dpc2typ,"_",input$dpc2inp1,"_",  
                                 input$dpc2inp2,".png") }, 
  content = function(file) { ggsave( 
    file, device = "png", height = input$dpc2oup.h, width = input$dpc2oup.w, 
    plot = scProp(dpconf, dpmeta, input$dpc2inp1, input$dpc2inp2,  
                  input$dpc2typ, input$dpc2flp, input$dpc2fsz) ) 
  }) 
     
   
  ### Plots for tab d1 
  output$dpd1oupTxt <- renderUI({ 
    geneList = scGeneList(input$dpd1inp, dpgene) 
    if(nrow(geneList) > 50){ 
      HTML("More than 50 input genes! Please reduce the gene list!") 
    } else { 
      oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted") 
      if(nrow(geneList[present == FALSE]) > 0){ 
        oup = paste0(oup, "<br/>", 
                     nrow(geneList[present == FALSE]), " genes not found (", 
                     paste0(geneList[present == FALSE]$gene, collapse = ", "), ")") 
      } 
      HTML(oup) 
    } 
  }) 
  output$dpd1oup <- renderPlot({ 
    scBubbHeat(dpconf, dpmeta, input$dpd1inp, input$dpd1grp, input$dpd1plt, 
               "dpgexpr.h5", dpgene, 
               input$dpd1scl, input$dpd1row, input$dpd1col, 
               input$dpd1cols, input$dpd1fsz) 
  }) 
  output$dpd1oup.ui <- renderUI({ 
    plotOutput("dpd1oup", height = pList3[input$dpd1psz]) 
  }) 
  output$dpd1oup.pdf <- downloadHandler( 
    filename = function() { paste0("dp",input$dpd1plt,"_",input$dpd1grp,".pdf") }, 
    content = function(file) { ggsave( 
      file, device = "pdf", height = input$dpd1oup.h, width = input$dpd1oup.w, 
      plot = scBubbHeat(dpconf, dpmeta, input$dpd1inp, input$dpd1grp, input$dpd1plt, 
                        "dpgexpr.h5", dpgene, 
                        input$dpd1scl, input$dpd1row, input$dpd1col, 
                        input$dpd1cols, input$dpd1fsz, save = TRUE) ) 
  }) 
  output$dpd1oup.png <- downloadHandler( 
    filename = function() { paste0("dp",input$dpd1plt,"_",input$dpd1grp,".png") }, 
    content = function(file) { ggsave( 
      file, device = "png", height = input$dpd1oup.h, width = input$dpd1oup.w, 
      plot = scBubbHeat(dpconf, dpmeta, input$dpd1inp, input$dpd1grp, input$dpd1plt, 
                        "dpgexpr.h5", dpgene, 
                        input$dpd1scl, input$dpd1row, input$dpd1col, 
                        input$dpd1cols, input$dpd1fsz, save = TRUE) ) 
  }) 
   
   
   
}) 
 
 
 
 