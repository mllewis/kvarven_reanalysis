

############################ PREP ############################
 
# working directories containing Kvarven's data, where results will be written, and containing helper fns
data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Molly's Kvarven response/Data - Upload/Meta"
res.dir = "~/Dropbox/Personal computer/Independent studies/2020/Molly's Kvarven response/Results from R"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Molly's Kvarven response/Code"


library(metafor)
library(weightr)
library(PublicationBias)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(readxl)
library(testthat)

setwd(code.dir)
source("helper.R")

# should we overwrite existing results and plots?
overwrite.res = TRUE
overwrite.plots = FALSE

if ( overwrite.res == TRUE ) {
  rm(res)
}


############################ READ IN AND ANALYZE EACH META ############################

# Kvarven's results containing replications
setwd("~/Dropbox/Personal computer/Independent studies/2020/Molly's Kvarven response/Data - Upload")
agg = read_xls("Dataset.xls")
# remove blank rows
agg = agg[ !is.na(agg$metaanalysis), ]
# make merger variable with just first author's last name
agg$meta = unlist( lapply( agg$metaanalysis, FUN = function(x) strsplit( x, " " )[[1]][1] ) )

# list all meta-analysis datasets
setwd(data.dir)
files = list.files(pattern = "*.csv") 

# separation character different for different meta-analyses
sep.vec = rep( ";", length(files) )
sep.vec[1] = ","
sep.vec[10] = ","

# initialize list of p-value plots
pvals = list()

# initialize results dataframe
names = c( 'meta',
           
           'Mhat.rep',
           
           'Mhat.naive',
           'Mhat.naive.lo',
           'Mhat.naive.hi',
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
           
           'Pdisaffirm.ratio' )

res = as.data.frame( matrix( NA, nrow = 15, ncol = length(names) ) )
names(res) = names


##### Read In and Analyze Each Meta #####

for ( i in 1:length(files) ) {
  
  cat( paste("\n Starting", files[i] ) )
  
  setwd(data.dir)
  
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
  
  
  ##### Naive Meta-Analysis ######
  m0 = rma.uni( yi = d,
                vi = var,
                data = d,
                method = "REML",
                knha = TRUE )
  res$Mhat.naive[i] = m0$b
  res$Mhat.naive.hi[i] = m0$ci.ub
  res$Mhat.naive.lo[i] = m0$ci.lb
  
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
    meta.worst = rma.uni( yi = d,
                          vi = var, 
                          data = temp,
                          method = "REML",
                          knha = TRUE )
    
    res$Mhat.worst[i] = meta.worst$b
    res$Mhat.worst.hi[i] = meta.worst$ci.ub
    res$Mhat.worst.lo[i] = meta.worst$ci.lb
  } else {
    res$Mhat.worst.error[i] = "No nonaffirmatives"
  }
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
                   value = mean( res$k.nonaffirm / (res$k.affirm + res$k.nonaffirm) ),
                   print = TRUE )


############################ SANITY CHECK ############################

##### Sanity Check: Manually Reproduce Key Estimates for One of Them #####

# randomly choose one of them, but not Schimmack
( file = sample( files[ !files == "Schimmack.csv" ] )[1] )
meta = strsplit(file, "[.]")[[1]][1]

setwd(data.dir)
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


