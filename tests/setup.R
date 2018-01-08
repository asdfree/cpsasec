if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )
cpsasec_cat <- get_catalog( "cpsasec" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( cpsasec_cat ) ) / ceiling( nrow( cpsasec_cat ) / 12 ) )
cpsasec_cat <- cpsasec_cat[ record_categories == this_sample_break , ]
lodown( "cpsasec" , cpsasec_cat )
if( any( cpsasec_cat$year == 2016 ) ){
library(lodown)
# examine all available CPSASEC microdata files
cpsasec_cat <-
	get_catalog( "cpsasec" ,
		output_dir = file.path( getwd() ) )

# 2016 only
cpsasec_cat <- subset( cpsasec_cat , year == 2016 )
# download the microdata to your local computer


options( survey.replicates.mse = TRUE )

library(survey)

cpsasec_df <- 
	readRDS( file.path( getwd() , "2016 cps asec.rds" ) )

variables_to_keep <-
	c( 'a_maritl' , 'gestfips' , 'a_sex' , 'ptotval' , 'moop' , 'a_age' , 'htotval' , 
	'one' , 'a_exprrp' , 'marsupwt' , 
	grep( "pwwgt" , names( cpsasec_df ) , value = TRUE ) )
	
cpsasec_df <- cpsasec_df[ variables_to_keep ] ; gc()
	
cpsasec_design <-
	svrepdesign(
		weights = ~ marsupwt ,
		repweights = "pwwgt[1-9]" ,
		type = "Fay" ,
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		data = cpsasec_df ,
		combined.weights = TRUE
	)
cpsasec_design <- 
	update( 
		cpsasec_design , 

		a_maritl = 
			factor( 
				a_maritl , 
				labels = 
					c( 
						"married - civilian spouse present" ,
						"married - AF spouse present" ,
						"married - spouse absent" ,
						"widowed" ,
						"divorced" , 
						"separated" , 
						"never married"
					)
			) ,
			
		state_name =
			factor(
				gestfips ,
				levels = 
					c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 
					11L, 12L, 13L, 15L, 16L, 17L, 18L, 
					19L, 20L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 30L, 31L, 32L, 
					33L, 34L, 35L, 36L, 37L, 38L, 39L, 
					40L, 41L, 42L, 44L, 45L, 46L, 47L, 
					48L, 49L, 50L, 51L, 53L, 54L, 55L, 
					56L) ,
				labels =
					c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
					"Colorado", "Connecticut", "Delaware", "District of Columbia", 
					"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
					"Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
					"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
					"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
					"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
					"Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
					"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
					"Washington", "West Virginia", "Wisconsin", "Wyoming")
			) ,

		male = as.numeric( a_sex == 1 )
	)
sum( weights( cpsasec_design , "sampling" ) != 0 )

svyby( ~ one , ~ state_name , cpsasec_design , unwtd.count )
svytotal( ~ one , cpsasec_design )

svyby( ~ one , ~ state_name , cpsasec_design , svytotal )
svymean( ~ ptotval , cpsasec_design )

svyby( ~ ptotval , ~ state_name , cpsasec_design , svymean )
svymean( ~ a_maritl , cpsasec_design )

svyby( ~ a_maritl , ~ state_name , cpsasec_design , svymean )
svytotal( ~ ptotval , cpsasec_design )

svyby( ~ ptotval , ~ state_name , cpsasec_design , svytotal )
svytotal( ~ a_maritl , cpsasec_design )

svyby( ~ a_maritl , ~ state_name , cpsasec_design , svytotal )
svyquantile( ~ ptotval , cpsasec_design , 0.5 )

svyby( 
	~ ptotval , 
	~ state_name , 
	cpsasec_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ moop , 
	denominator = ~ ptotval , 
	cpsasec_design 
)
sub_cpsasec_design <- subset( cpsasec_design , a_age %in% 18:64 )
svymean( ~ ptotval , sub_cpsasec_design )
this_result <- svymean( ~ ptotval , cpsasec_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ ptotval , 
		~ state_name , 
		cpsasec_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( cpsasec_design )
svyvar( ~ ptotval , cpsasec_design )
# SRS without replacement
svymean( ~ ptotval , cpsasec_design , deff = TRUE )

# SRS with replacement
svymean( ~ ptotval , cpsasec_design , deff = "replace" )
svyciprop( ~ male , cpsasec_design ,
	method = "likelihood" )
svyttest( ptotval ~ male , cpsasec_design )
svychisq( 
	~ male + a_maritl , 
	cpsasec_design 
)
glm_result <- 
	svyglm( 
		ptotval ~ male + a_maritl , 
		cpsasec_design 
	)

summary( glm_result )
library(convey)
cpsasec_design <- convey_prep( cpsasec_design )

sub_cpsasec_design <- 
	subset( 
		cpsasec_design , 
		a_exprrp %in% 1:2
	)

svygini( ~ htotval , sub_cpsasec_design )

}
