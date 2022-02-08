### Load libraries ###
library(ggplot2)
library(dplyr)
library(ggpubr)

## Funcion for simulation ##

#str.cell.n - starting cell number
#ittr - number of itterations(how many times to repeat)
#tp_mean - mean transition probability table
#tp_sd - standard deviatinon table of mean transition probabilities
#nsc2_c,rg_c ... a4_c - number of starting cells in each population
#str.prolif - proloferation rate of starting cells(nsc1)
#nsc2.prolif - proloferation rate of nsc2 cells
#rg.proloif - proloferation rate of rg cells

tp_processing <- function(str.cell.n = 1000,ittr =10,tp_mean,tp_sd,
                          nsc2_c = 0,rg_c = 0,n1_c = 0,n2_c = 0,n3_c = 0,n4_c = 0,
                          n5_c = 0,n6_c = 0,a1_c = 0,a2_c = 0,a3_c = 0,a4_c = 0,
                          str.prolif = 1,nsc2.prolif = 1,rg.prolif = 1){
  
  nsc1_c <- str.cell.n

  
  cy = 0
  repeat
  {
    ##nsc1
    
    aa <- matrix(nrow= 13,ncol = length(1:nsc1_c)) #empty matrix
    
    #using rnorm function to generate probabilities from mean and sd for every cell
    #negative probabilities are set to zero
    for (i in 1:nsc1_c) { a <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,3]%>%unlist,tp_sd[v,3]%>%unlist)
      if(j<0){j=0}
      a[v] <- j
    }
    #using rmultinom function transition is calculated from probabilities
    if(sum(a) == 0){aa[,i] <- a}
    if(sum(a) > 0){aa[,i] <- rmultinom(1,1, prob = a)}  
    }
    #adding proliferation
    aa[1,] <- aa[1,] * str.prolif
    ##nsc2
    bb <- matrix(nrow= 13,ncol = length(1:nsc2_c))
    
    for (i in 1:nsc2_c) { b <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,4]%>%unlist,tp_sd[v,4]%>%unlist)
      if(j<0){ j = 0}
      b[v] <- j
    }
    if(sum(b) == 0){bb[,i] <- b}
    if(sum(b) > 0){bb[,i] <- rmultinom(1,1, prob = b) } 
    }  
    bb[2,] <- bb[2,] * nsc2.prolif
    ##rg
    cc <- matrix(nrow= 13,ncol = length(1:rg_c))
    
    for (i in 1:rg_c) {   c <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,5]%>%unlist,tp_sd[v,5]%>%unlist)
      if(j<0){j=0}
      c[v] <- j
    }
    
    if(sum(c) == 0){cc[,i] <- c}
    if(sum(c) > 0){cc[,i] <- rmultinom(1,1, prob = c)}  
    } 
    cc[3,] <- cc[3,] * rg.prolif
    ##n1
    dd <- matrix(nrow= 13,ncol = length(1:n1_c))
    
    for (i in 1:n1_c) { d <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,6]%>%unlist,tp_sd[v,6]%>%unlist)
      if(j<0){j=0}
      d[v] <- j
    }
    if(sum(d) == 0){dd[,i] <- d}
    if(sum(d) > 0){dd[,i] <- rmultinom(1,1, prob = d)}  
    } 
    
    ##n2
    ee <- matrix(nrow= 13,ncol = length(1:n2_c))
    
    for (i in 1:n2_c) {e <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,7]%>%unlist,tp_sd[v,7]%>%unlist)
      if(j<0){j=0}
      e[v] <- j
    }
    if(sum(e) == 0){ee[,i] <- e}
    if(sum(e) > 0){ ee[,i] <- rmultinom(1,1, prob = e)}  
    } 
    
    ##n3
    ff <- matrix(nrow= 13,ncol = length(1:n3_c))
    
    for (i in 1:n3_c) {f <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,8]%>%unlist,tp_sd[v,8]%>%unlist)
      if(j<0){j=0}
      f[v] <- j
    }
    if(sum(f) == 0){ff[,i] <- f}
    if(sum(f) > 0){ff[,i] <- rmultinom(1,1, prob = f)}  
    }
    
    ##n4
    gg <- matrix(nrow= 13,ncol = length(1:n4_c))
    
    for (i in 1:n4_c) {g <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,9]%>%unlist,tp_sd[v,9]%>%unlist)
      if(j<0){j=0}
      g[v] <- j
    }
    if(sum(g) == 0){gg[,i] <- g}
    if(sum(g) > 0){ gg[,i] <- rmultinom(1,1, prob = g)}  
    }  
    
    ##n5  
    hh <- matrix(nrow= 13,ncol = length(1:n5_c))
    
    for (i in 1:n5_c) {  h <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,10]%>%unlist,tp_sd[v,10]%>%unlist)
      if(j<0){j=0}
      h[v] <- j
    }
    if(sum(h) == 0){hh[,i] <- h}
    if(sum(h) > 0){hh[,i] <- rmultinom(1,1, prob = h) } 
    }  
    
    ##n6
    kk <- matrix(nrow= 13,ncol = length(1:n6_c))
    
    for (i in 1:n6_c) { k <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,11]%>%unlist,tp_sd[v,11]%>%unlist)
      if(j<0){j=0}
      k[v] <- j
    }
    if(sum(k) == 0){kk[,i] <- k}
    if(sum(k) > 0){kk[,i] <- rmultinom(1,1, prob = k)  }
    }  
    
    ##a1
    ll <- matrix(nrow= 13,ncol = length(1:a1_c))
    
    for (i in 1:a1_c) { l <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,12]%>%unlist,tp_sd[v,12]%>%unlist)
      if(j<0){j=0}
      l[v] <- j
    }
    if(sum(l) == 0){ll[,i] <- l}
    if(sum(l) > 0){ll[,i] <- rmultinom(1,1, prob = l)  }
    } 
    
    ##a2
    mm <- matrix(nrow= 13,ncol = length(1:a2_c))
    
    for (i in 1:a2_c) {m <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,13]%>%unlist,tp_sd[v,13]%>%unlist)
      if(j<0){j=0}
      m[v] <- j
    }
    if(sum(m) == 0){mm[,i] <- m}
    if(sum(m) > 0){mm[,i] <- rmultinom(1,1, prob = m)  }
    } 
    
    ##a3
    nn <- matrix(nrow= 13,ncol = length(1:a3_c))
    
    for (i in 1:a3_c) {n <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,14]%>%unlist,tp_sd[v,14]%>%unlist)
      if(j<0){j=0}
      n[v] <- j
    }
    if(sum(n) == 0){nn[,i] <- n}
    if(sum(n) > 0){nn[,i] <- rmultinom(1,1, prob = n)  }
    }  
    
    ##a4  
    oo <- matrix(nrow= 13,ncol = length(1:a4_c))
    
    for (i in 1:a4_c) { o <- c()
    for (v in 1:13) {
      j <- rnorm(1,tp_mean[v,15]%>%unlist,tp_sd[v,15]%>%unlist)
      if(j<0){j=0}
      o[v] <- j
    }
    if(sum(o) == 0){oo[,i] <- o}
    if(sum(o) > 0){oo[,i] <- rmultinom(1,1, prob = o)  }
    }  
    
    
    x <- cbind(rowSums(aa),rowSums(bb),rowSums(cc),rowSums(dd),rowSums(ee),
               rowSums(ff), rowSums(gg),rowSums(hh),rowSums(kk),rowSums(ll),
               rowSums(mm),rowSums(nn),rowSums(oo))
    
    #updating cell counts after each cycle
    nsc1_c <- rowSums(x,na.rm = T)[1] 
    nsc2_c <- rowSums(x,na.rm = T)[2]
    rg_c <- rowSums(x,na.rm = T)[3]
    n1_c <- rowSums(x,na.rm = T)[4]
    n2_c <- rowSums(x,na.rm = T)[5]
    n3_c <- rowSums(x,na.rm = T)[6]
    n4_c <- rowSums(x,na.rm = T)[7]
    n5_c <- rowSums(x,na.rm = T)[8]
    n6_c <- rowSums(x,na.rm = T)[9]
    a1_c <- rowSums(x,na.rm = T)[10]  
    a2_c <- rowSums(x,na.rm = T)[11]
    a3_c <- rowSums(x,na.rm = T)[12]
    a4_c <- rowSums(x,na.rm = T)[13]  
    
    cy = cy+1 
    if(ittr == cy){
      break
    }
    
  }
  
  
  print(rbind(nsc1_c,nsc2_c,rg_c,n1_c,n2_c,n3_c,n4_c,n5_c,n6_c,a1_c,a2_c,a3_c,a4_c))
}



### Calculating cell counts after 10 itterations ###
#control
set.seed(1)
cell.numb_c <- tp_processing(str.cell.n = 42330,ittr = 10,tp_mean = tp_mean_7,tp_sd = tp_sd_7,
                             str.prolif = 1)

cell.numb_c <- as.data.frame(cell.numb_c)
cell.numb_c <- data.frame(count = cell.numb_c$V1,name = rownames(cell.numb_c))

#TBI
set.seed(1)
cell.numb_t <- tp_processing(str.cell.n = 57670,ittr = 10,tp_mean = tp_mean_7[14:26,],tp_sd = tp_sd_7[14:26,],
                             str.prolif = 1,rg.prolif = 1,nsc2.prolif = 1)

cell.numb_t <- as.data.frame(cell.numb_t)
cell.numb_t <- data.frame(count = cell.numb_t$V1,name = rownames(cell.numb_t))

# calculating proportions
#control
vvet_c <- cell.numb_c$count/(cell.numb_c$count + cell.numb_t$count)
#TBI
vvet_t <- cell.numb_t$count/(cell.numb_c$count + cell.numb_t$count)

cell.numb <- data.frame(exp = c(rep("Sham",times = 13),rep("TBI",times = 13)),
                        rbind(cell.numb_c,cell.numb_t),prop =  c(vvet_c,vvet_t))

#maiking predicted barplot
pp <- ggbarplot(cell.numb, x = "name", y = "prop",xlab = "",ylab = "Predicted %",
                fill = "exp",colour = c("#F35E5A ","#17B3B7"),  
                position = position_stack()) + labs(fill = "") + 
  scale_y_continuous(breaks = c(0,0.5767,1),labels = c("0.00","57.67","100.00")) +
  scale_x_discrete(labels = c("NSC-stage1","NSC-stage2","RG-like","N-stage1",
                              "N-stage2","N-stage3","N-stage4","N-stage5","N-stage6",
                              "A-stage1","A-stage2","A-stage3","A-stage4")) + 
  geom_abline(slope = 0,intercept = 0.5767,size =1.5,linetype="dashed" ) + 
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(colour = "black"))


pp
