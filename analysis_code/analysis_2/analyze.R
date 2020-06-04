

library(metafor)
library(weightr)
library(PublicationBias)
library(MetaUtility)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(readxl)
library(testthat)
library(ggplot2)
library(tidyr)
library(robumeta)
library(here)

############################ PREP ############################
 
# working directories containing Kvarven's data, where results will be written, and containing helper fns
data.dir = here("data/analysis_2")
res.dir = here("results/analysis_2")
code.dir = here("analysis_code/analysis_2")

# # here() points to wrong directories on MM's machine
# # hard-code dirs instead:
# data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Molly's Kvarven comment/data/analysis_2"
# res.dir = "~/Dropbox/Personal computer/Independent studies/2020/Molly's Kvarven comment/results/analysis_2"
# code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Molly's Kvarven comment/analysis_code/analysis_2"






setwd(code.dir)
source("helper.R")

# should we overwrite existing results and plots?
overwrite.res = TRUE
overwrite.plots = TRUE

if ( overwrite.res == TRUE & exists("res") ) {
  message("Deleting previous results")
  rm(res)
}


############################ READ IN AND ANALYZE EACH META ############################

# Kvarven's results containing replications
setwd(data.dir)
agg = read_xls("Dataset.xls")
# remove blank rows
agg = agg[ !is.na(agg$metaanalysis), ]
# make merger variable with just first author's last name
agg$meta = unlist( lapply( agg$metaanalysis, FUN = function(x) strsplit( x, " " )[[1]][1] ) )

# list all meta-analysis datasets
setwd(data.dir)
setwd("Meta")
files = list.files()[ grepl( pattern = ".csv", x = list.files() ) ]

# separation character different for different meta-analyses
sep.vec = rep( ";", length(files) )
sep.vec[1] = ","
sep.vec[10] = ","

# initialize list of p-value plots
pvals = list()
# initialize results dataframe
names = c( 'meta',
           "orig.name",
           
           'Mhat.rep',
           
           'Mhat.naive',
           'Mhat.naive.lo',
           'Mhat.naive.hi',
           'tau',
           'Mhat.worst',
           'Mhat.worst.lo',
           'Mhat.worst.hi',
           'Mhat.worst.error',
           
           'k.nonaffirm', 
           'k.affirm',
           
           'Sval.0',
           'Sval.CI.0',
           'Sval.error',
           
           'Sval.rep',
           'Sval.rep.error',
           
           'Pdisaffirm.ratio',
           
           'Phat.below',
           "Phat.below.lo",
           "Phat.below.hi",
           "Phat.below.error" )

# 15 meta-analyses
res = as.data.frame( matrix( NA, nrow = 15, ncol = length(names) ) )
names(res) = names


##### Read In and Analyze Each Meta #####

for ( i in 1:length(files) ) {
  
  cat( paste("\n ****Starting", files[i] ) )
  
  setwd(data.dir)
  setwd("Meta")
  
  # d and var are always the first 2 columns, even though they are named
  #  differently
  # EXCEPT Schimmack, which uses Cohen's q and has cols in different order
  if ( files[i] != "Schimmack.csv" ) {
    d = read.csv( files[i], sep = sep.vec[i] )[,1:2]
    print( names(d) )  # as a sanity check
    # standardize names
    names(d) = c("d", "var")
  } 
  
  if ( files[i] == "Schimmack.csv" ) {
    d = read.csv( files[i], sep = ";" )
    d = d %>% select( q, Var.q. )
    # not actually Cohen's d, but irrelevant for the analyses we're going to do
    names(d) = c("d", "var")
  }
  
  # add unique merger ID for the meta-analysis
  meta = str_remove_all( files[i], ".csv" )
  res$meta[i] = meta
  
  # merge in replication result
  res$Mhat.rep[i] = agg$replication_s[ agg$meta == meta ]
  
  # merge in original study name
  res$orig.name[i] = agg$original[ agg$meta == meta ]
  
  ##### Naive Meta-Analysis ######
  m0 = rma.uni( yi = d,
                vi = var,
                data = d,
                method = "REML",
                knha = TRUE )
  res$Mhat.naive[i] = m0$b
  res$Mhat.naive.hi[i] = m0$ci.ub
  res$Mhat.naive.lo[i] = m0$ci.lb
  res$tau[i] = sqrt(m0$tau2)
  
  # sanity check: Kvarven's reported meta-analytic point estimate vs. ours
  a = round( as.numeric(m0$b), 2 )
  b = round( agg$meta_s[ agg$meta == meta ], 2 )
  if ( abs(a-b) > 0.01 ) warning( paste( meta, ": reported was ", b, " but mine was ", a, sep = "" ) )
  cat("\n")
  
  ##### Contour-Enhanced Funnel Plots ######
  if ( overwrite.plots == TRUE ) {
    setwd(res.dir)
    setwd("Funnels")
    pdf( file = paste( "funnel_", meta, sep = "" ) )
    funnel.rma(m0,
               level = c(0.95),
               legend = TRUE,
               main = meta,
               refline = 0)
    dev.off()
  }


  ##### Plot the P-Values #####
  
  # calculate one-sided p-values
  d$pval.one = ( 1 - pnorm( ( d$d / sqrt(d$var) ) ) )
  d$pval.two = 2 * ( 1 - pnorm( ( abs(d$d) / sqrt(d$var) ) ) )
  d$affirm = (d$pval.two <= 0.05) & (d$d > 0)
  
  res$k.affirm[i] = sum( d$affirm )
  res$k.nonaffirm[i] = sum( d$affirm == 0 )
  
  res$Pdisaffirm.ratio[i] = round( mean( d$pval.two[!d$affirm] > 0.975 ) / (0.025 / 0.975), 2 )
  
  if ( overwrite.plots == TRUE ) {
    # seems like no evidence of 2-tailed selection visually
    pval.plot = ggplot( data = d,
                        aes( x = pval.one ) ) +
      geom_histogram() + 
      theme_classic() +
      geom_vline(xintercept = 0.025, color = "red", lty = 2) +
      geom_vline(xintercept = 0.975, color = "red", lty = 2) +
      ggtitle(meta)
    pvals[[i]] = pval.plot
  }

  
  ##### SAPB ######
  library(PublicationBias)
  
  # to shift Mhat to 0
  tryCatch( {
    sval.0 = svalue( yi = d$d,
                     vi = d$var,
                     q = 0,
                     model = "robust" )
    
    # as character because might be "Not possible" or ">200"
    res$Sval.0[i] = as.character( sval.0$sval.est )
    res$Sval.CI.0[i] = as.character( sval.0$sval.ci )
    
  }, error = function(err) {
    res$Sval.0[i] <<- NA
    res$Sval.CI.0[i] <<- NA
    res$Sval.error[i] = err$message
  } )
  
  # to shift Mhat to match the replication estimate
  tryCatch( {
    sval.rep = svalue( yi = d$d,
                     vi = d$var,
                     q = res$Mhat.rep[i],
                     model = "robust" )
    
    res$Sval.rep[i] = as.character(sval.rep$sval.est)
    
  }, error = function(err) {
    res$Sval.rep[i] <<- NA
    res$Sval.rep.error[i] <<- err$message
  } )
  

  
  ##### Worst-Case Meta-Analysis ######
  temp = d[ d$affirm == 0, ]

  if ( nrow(temp) > 0 ) {
    # non-robust version (not best practice)
    # meta.worst = rma.uni( yi = d,
    #                       vi = var, 
    #                       data = temp,
    #                       method = "REML",
    #                       knha = TRUE )
    # 
    # res$Mhat.worst[i] = meta.worst$b
    # res$Mhat.worst.hi[i] = meta.worst$ci.ub
    # res$Mhat.worst.lo[i] = meta.worst$ci.lb
    
    # robust meta-analysis
    meta.worst = robu( d ~ 1, 
                       data = temp, 
                       studynum = 1:nrow(temp),
                       var.eff.size = var )
    
    res$Mhat.worst[i] = meta.worst$b.r
    res$Mhat.worst.hi[i] = meta.worst$reg_table$CI.U
    res$Mhat.worst.lo[i] = meta.worst$reg_table$CI.L
    
  } else {
    res$Mhat.worst.error[i] = "No nonaffirmatives"
  }
  
  ##### Proportion of Meta-Analysis True Effects Below Replication Pooled Point Estimate #####
  
  if ( m0$tau2 > 0 & nrow(d) >= 10 ) {
    Phat = prop_stronger( q = res$Mhat.rep[i],
                          tail = "below",
                          dat = d,
                          yi.name = "d",
                          vi.name = "var" )
    res$Phat.below[i] = Phat$est
    res$Phat.below.lo[i] = Phat$lo
    res$Phat.below.hi[i] = Phat$hi
    
    cat( paste("\n", Phat$est) )
    
  } else {
    res$Phat.below.error[i] = "t2 is zero or k<10"
  }
  
  cat( paste("\n ****Done", files[i] ) )
}


##### Write P-Values Plots #####
library(gridExtra)
nCol = floor(sqrt( length(pvals)))
do.call("grid.arrange", c(pvals, ncol=nCol))
# save manually via Export

##### Write Results #####
setwd(res.dir)
write.csv(res, "results_by_meta.csv")


############################ WRITE KEY STATS TO OWN FILE ############################

# confirm that all have Mhat.naive > 0, so no reversal of signs needed
table(res$Mhat.naive>0)

# among metas for which Mhat.rep < Mhat, proportion for which it's possible to
#  shift Mhat to Mhat.rep

# ** for 61% of the 13 metas for which Mhat.rep < Mhat and >0 nonaffirmatives,
#  cannot shift Mhat to Mhat.rep
# ** only 1 of 13 can shift to 0!
res$include = ( res$Mhat.rep < res$Mhat.naive ) & ( res$k.nonaffirm > 0 )

( t = res %>% filter( include == TRUE ) %>%
    summarise( n = n(),
               
               # can't shift to Mhat.rep
               k1 = sum( Mhat.worst > Mhat.rep ),
               P1 = mean( Mhat.worst > Mhat.rep ), 
               
               # can't shift to 0
               k2 = sum( Mhat.worst > 0 ),
               P2 = mean( Mhat.worst > 0 ) )  ) # can't shift to 0

update_result_csv( name = "Perc cannot shift to Mhat.rep",
                   value = t$P1 )
update_result_csv( name = "k cannot shift to Mhat.rep",
                   value = t$k1 )
update_result_csv( name = "Perc cannot shift to 0",
                   value = t$P2 )
update_result_csv( name = "k cannot shift to 0",
                   value = t$k2 )

update_result_csv( name = "Mean perc nonaffirm (all metas)",
                   value = mean( res$k.nonaffirm / (res$k.affirm + res$k.nonaffirm) ) )




phatb = res$Phat.below[ !is.na(res$Phat.below) ]

update_result_csv( name = "Median Phat.below",
                   value = median( phatb ),
                   print = TRUE )

update_result_csv( name = "Perc Phat.below > .25",
                   value = mean(phatb > .25),
                   print = TRUE )




############################ MAKE FIGURE ############################

##### Read Results Back In #####
setwd(res.dir)
res = read.csv("results_by_meta.csv")

res$Phat.string = "Not estimable"
res$Phat.string[ !is.na(res$Phat.below) ] = paste( round(100 * res$Phat.below[ !is.na(res$Phat.below) ], 0),
                                                 "% [",
                                                 round(100 * res$Phat.below.lo[ !is.na(res$Phat.below) ], 0),
                                                 "%, ",
                                                 round(100 * res$Phat.below.hi[ !is.na(res$Phat.below) ], 0),
                                                 "%]",
                                                 sep="")
res$Phat.string[ !is.na(res$Phat.below) & is.na(res$Phat.below.lo) ] = paste( round(100 * res$Phat.below[ !is.na(res$Phat.below) & is.na(res$Phat.below.lo) ], 0),
                                                                              "%",
                                                                              sep="")

# make plotting dataframe and rename for plotting joy
dp = res %>% select( "orig.name",
                     "meta",
                     "MLR" = "Mhat.rep",
                     "Worst-case meta-analysis" = "Mhat.worst",
                     "Mhat.worst.lo",
                     "Mhat.worst.hi",
                     "Naïve meta-analysis" = "Mhat.naive",
                     "Phat.string")

dp$worst.ind = NA
dp$worst.ind[ dp$`Worst-case meta-analysis` > dp$`MLR` ] = "Worst-case meta-analysis estimate \nstill exceeds MLR estimate"
dp$worst.ind[ dp$`Worst-case meta-analysis` <= dp$`MLR` ] = "Worst-case meta-analysis estimate\ndoes not exceed MLR estimate\n"

# remove the one with NA
dp = dp[ !is.na(dp$worst.ind), ]
droplevels(dp)

# order by naive estimate size
correct.order = dp[ order(dp$`Naïve meta-analysis`), ]$orig.name


# put in long form for plotting joy
dpl = gather( dp,
              key = "var",
              value = "est",
              "MLR",
              "Worst-case meta-analysis",
              # "Mhat.worst.lo",
              # "Mhat.worst.hi",
              "Naïve meta-analysis" )
# avoid repeating values
dpl$Mhat.worst.hi[ dpl$var != "Worst-case meta-analysis" ] = NA
dpl$Mhat.worst.lo[ dpl$var != "Worst-case meta-analysis" ] = NA

# sanity check
dpl[ dpl$meta == "Belle", ]

# force ordering of y-axis
dpl$orig.name = factor(dpl$orig.name, levels = correct.order)


# decide how to scale x-axis
x.lim = c( -.5, 1.4 )
x.breaks = seq( x.lim[1], x.lim[2], .1 )

# get the correctly-ordered Phat strings for the RHS of plot
Phat.strings = dp[ order(dp$`Naïve meta-analysis`), ]$Phat.string
dp[ order(dp$`Naïve meta-analysis`), ]$orig.name  # sanity check: should match ordering of y-axis

##### Make Plot ######

colors = c("black", "orange")

shapes = c(1, 2, 124)


# # without annotations
# ggplot( data = dpl,
#         aes( x = est,
#              y = meta,
#              color = worst.ind,
#              shape = var ) ) +
#   theme_bw() +
# 
#   geom_vline( xintercept = 0,
#               color = "gray",
#               lty = 2,
#               lwd = 1.2 ) +
# 
#   geom_errorbarh( aes( xmin = Mhat.worst.lo,
#                        xmax = Mhat.worst.hi,
#                        height = 0 ) ) +
#   geom_point( size = 4 ) +
# 
#   scale_color_manual( values = colors,
#                       name = "" ) +
#   scale_shape_manual(values = shapes, 
#                      name = "") +
#   
#   scale_x_continuous( limits = x.lim,
#                       breaks = x.breaks ) + 
#   
#   xlab("Pooled point estimate") +
#   ylab("Meta-analysis name") 


# with annotations
p = ggplot( data = dpl,
            aes( x = est,
                 y = orig.name,
                 color = worst.ind,
                 shape = var ) ) +
  theme_classic() +
  
  geom_vline( xintercept = 0,
              color = "gray",
              lty = 1,
              lwd = 1.1 ) +
  
  geom_errorbarh( aes( xmin = Mhat.worst.lo,
                       xmax = Mhat.worst.hi,
                       height = 0 ) ) +
  geom_point( size = 4 ) +
  
  scale_color_manual( values = colors,
                      name = "" ) +
  scale_shape_manual(values = shapes, 
                     name = "") +
  
  scale_x_continuous( limits = x.lim,
                      breaks = x.breaks ) + 
  
  annotate( "text",
            x = 1.2,
            y = 1:nrow(dp),
            label = Phat.strings,
            size = 3 ) +
  
  xlab("Estimated mean") +
  ylab("Original study ") 

p

setwd(res.dir)
ggsave( filename = "forest.pdf",
        plot = p,
        width = 11,
        height = 8,
        units = "in")


############################ SANITY CHECK ############################

##### Sanity Check: Manually Reproduce Key Estimates for One of Them #####

# randomly choose one of them, but not Schimmack
( file = sample( files[ !files == "Schimmack.csv" ] )[1] )
meta = strsplit(file, "[.]")[[1]][1]

setwd(data.dir)
setwd("Meta")
d = read.csv( file, sep = sep.vec[ which(files == file) ] )[,1:2]
names(d) = c("d", "var")

# confirm k.affirm and k.nonaffirm
d$affirm = (d$d > 0) & ( abs( d$d / sqrt(d$v) ) >= qnorm(.975) )
table(d$affirm)

expect_equal( res$k.affirm[ res$meta == meta ],
              sum(d$affirm) ) 
expect_equal( res$k.nonaffirm[ res$meta == meta ],
              sum(d$affirm == 0) ) 

# confirm results of worst-case meta-analysis
worst = rma.uni( yi = d,
                 vi = var, 
                 data = d[ d$affirm == FALSE, ],
                 method = "REML",
                 knha = TRUE )

expect_equal( res$Mhat.worst[ res$meta == meta ],
              as.numeric(worst$b),
              tol = 0.001 ) 


##### Sanity Check: Worst-Case Estimates vs. S-Values #####
# just the 13 for analysis
temp = res[ res$include == TRUE, ]

table( temp$Sval.0[ temp$Mhat.worst > 0 ] )

# the minor disagreement is again because PublicationBias uses robust meta-analysis
#  vs. my worst-case uses REML
table( temp$Sval.rep[ temp$Mhat.worst > temp$Mhat.rep ] )


##### Sanity Check: Phat below #####

# get the replication estimate straight from the file
rep = agg$replication_s[ agg$meta == meta ]

prop_stronger( q = rep,
               tail = "below",
               dat = d,
               yi.name = "d",
               vi.name = "var" )

# I checked this against the forest plot


