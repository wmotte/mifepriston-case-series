#!/usr/bin/env Rscript
#
# Determine Cred. Interval and Bayes Factor for for PCL data (n-of-1)
#
# Wim Otte (w.m.otte@umcutrecht.nl), 19 June 2023.
#
################################################################################

library( 'brms' )           # for modeling (v.2.17.0)
library( 'ggplot2' )        # for plotting
library( 'tidybayes' )      # for add_predicted_draws()
library( 'dplyr' )          # for %>% functionality

###
# Get data
##
get_data <- function()
{
    # read first tab sheet
    infile <- 'out.01.plot/data_combined.csv'   
    df <- read.csv( infile, row.names = 1 )
    
    # remove intervention [only keep baseline and post-intervention groups]
    df_dich <- df[ df$group %in% c( 'baseline', 'post-intervention' ), ]
    
    return( df_dich )
}

### 
# Helper function plot.
##
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

################################################################################
# END FUNCTIONS
################################################################################

# set seed
set.seed( 123 )

# output dir
outdir <- 'out.02.model'
dir.create( outdir, showWarnings = FALSE )

# get data
head( df <- get_data() )

########### NEGATIVE BINOMIAL / POISSON / BINOMIAL #############

cases <- c( 'Tim', 'Linda', 'Matthew' )

for( case in cases )
{
    data <- df[ df$case == case, ]   

    ## model without group effect
    model_nb_null <- brms::brm( y ~ 1, data = data, family = negbinomial(), save_pars = save_pars( all = TRUE ) )
        
    ## model with group effect
    model_nb_group <- brms::brm( y ~ group, data = data, family = negbinomial(), save_pars = save_pars( all = TRUE ) )
    
    # estimated Bayes factor ratio
    print( bf_nb_group_over_null <- brms::bayes_factor( model_nb_group, model_nb_null ) )
    
    # write BF to file
    write.csv( round( bf_nb_group_over_null$bf, 2 ), file = paste0( outdir, '/bf__', case, '.csv' ) )
    
    # posterior samples
    post <- as_draws_df( model_nb_group )
    
    # get delta 'baseline ~ follow-up' in original units
    post$delta <- exp( post$b_Intercept + post$b_grouppostMintervention ) - exp( post$b_Intercept )
    
    mean <- mean(post$delta )
    cis <- quantile(post$delta, c( 0.025, 0.975 ) )
     
    output_data <- data.frame( mean = mean, ci_low = cis[ 1 ], ci_high = cis[ 2 ] )
    
    # write BF to file
    write.csv( output_data, file = paste0( outdir, '/mean_and_95CI__', case, '.csv' ) )
        
    # write posterior data to disk
    write.csv( post, file = gzfile( paste0( outdir, '/posteriors.csv.gz' ) ) )
    
    # plot [c.f., http://mjskay.github.io/tidybayes/index.html]
    p_post <- df %>%
        tidybayes::add_predicted_draws( model_nb_group ) %>%
        ggplot( aes( x = day, y = y ) ) +
        stat_lineribbon( aes( y = .prediction ), .width = c( 0.25, 0.50, 0.75, 0.95 ), color = "#08519C" ) +
        geom_point( data = data, size = 2, alpha = 0.5 ) +
        scale_fill_brewer() +
        ylab( "Total PCL-5 (posterior predictive intervals)" ) +
        xlab( 'Time (days)' ) +
        scale_x_continuous( breaks = number_ticks( 30 ) ) +
        scale_y_continuous( breaks = number_ticks( 20 ) ) +
        theme_classic() +
        coord_cartesian( expand = FALSE, xlim = c( 0.5, max( df$day) + 0.5 ), ylim = c( 0, 80 ) ) +
        theme( legend.position = 'top', strip.text.x = element_text( face = 'bold' ), 
               strip.text.y = element_text( face = 'bold' ) )
    
    # save
    ggsave( plot = p_post, file = paste0( outdir, '/plot_posteriors__', case, '.png' ), dpi = 300, height = 5, width = 8 )
}
