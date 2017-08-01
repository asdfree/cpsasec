if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

cpsasec_cat <-
	get_catalog( "cpsasec" ,
		output_dir = file.path( getwd() ) )

# sample 25% of the records
which_records <- sample( seq( nrow( cpsasec_cat ) ) , round( nrow( cpsasec_cat ) * 0.25 ) )

# always sample year == 2016
cpsasec_cat <- unique( rbind( cpsasec_cat[ which_records , ] , subset( cpsasec_cat , year == 2016 ) ) )

lodown( "cpsasec" , cpsasec_cat )
