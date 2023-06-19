#!/usr/bin/env Rscript
#
# Visualize n-of-1 trial data for PCL data (n-of-1)
#
# Wim Otte (w.m.otte@umcutrecht.nl), 19 June 2023
#
################################################################################
library( 'ggplot2' )        # for plotting
library( 'readxl' )         # for Excel input reading

###
# Get data for single case
##
get_data <- function( case = 1 )
{
    # read first tab sheet
    infile <- paste0( 'data/case', case, '.xlsx' )
    df <- data.frame( readxl::read_excel( infile ) )
    df$case <- paste0( 'case-', case )
    return( df )
}

### 
# Helper function plot.
##
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.01.plot'
dir.create( outdir, showWarnings = FALSE )

# get data
df1 <- get_data( 1 )
df2 <- get_data( 2 )
df3 <- get_data( 3 )

# combine
df <- rbind( df1, rbind( df2, df3 ) )
levels( df$case <- as.factor( df$case ) )
levels( df$case ) <- c( 'Tim', 'Linda', 'Matthew' )

# blue + orange
custom_colours <- c( "#56B4E9", "#E69F00", "#E34A33" )

####################################
### V1 - with two interventions ####

# plot
p <- ggplot( data = df, aes( x = day, y = y, group = group, colour = group, fill = group ) ) + 
    ggfx::with_shadow( geom_point( alpha = 0.8 ), x_offset = 1, y_offset = 1, colour = 'gray30' ) +
    geom_smooth( method = 'lm', formula = 'y ~ 1', se = FALSE, alpha = 0.5 ) +
    geom_point() +
    ylab( "Total PCL-5 (mean)" ) +
    xlab( 'Time (days)' ) +
    scale_x_continuous( breaks = number_ticks( 20 ) ) +
    scale_y_continuous( breaks = number_ticks( 10 ) ) +
    scale_color_manual( values = custom_colours ) +
    scale_fill_manual( values = custom_colours ) +
    theme_classic() +
    facet_wrap( ~case, ncol = 1 ) +
    coord_cartesian( expand = FALSE, xlim = c( 0.5, max( df$day) + 0.5 ), ylim = c( -2, 80 ) ) +
    theme( legend.position = 'top', 
           legend.title = element_blank(),
           strip.text.x = element_text( face = 'bold' ), 
           strip.text.y = element_text( face = 'bold' ) )

# save
ggsave( plot = p, file = paste0( outdir, '/plot_mean_PCL-5.png' ), dpi = 900, height = 7, width = 4 )

# save raw data
write.csv( df, file = paste0( outdir, '/data_combined.csv' ), quote = TRUE )
