if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

cpsasec_cat <-
	get_catalog( "cpsasec" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( cpsasec_cat ) ) / ceiling( nrow( cpsasec_cat ) / 12 ) )

cpsasec_cat <- unique( rbind( cpsasec_cat[ record_categories == this_sample_break , ] , cpsasec_cat[ cpsasec_cat$year == 2016 , ] ) )

lodown( "cpsasec" , cpsasec_cat )
