source('./validation.R')

countries = c(
	'Argentina',
	'Haiti',
	'United_States',
	'Brazil',
	'Colombia',
	'Mexico',
	'Dominican_Republic',
	'Nicaragua',
	'Ecuador',
	'Panama',
	'El_Salvador',
	'Puerto_Rico',
	'France',
	'Guatemala',
	'USVI'
)

for (country in countries){
	places <- read.csv(Sys.glob(paste('../', country, '/*Places.csv', sep='')), header=TRUE)
	guide <- read.csv(Sys.glob(paste('../', country, '/*Guide.csv', sep='')), header=TRUE)

	dirs <- list.dirs(path=paste('../', country, sep=''))
	dataDirs <- dirs[grepl("*data", dirs)]

	for(dir in dataDirs){
		files = Sys.glob(paste(dir, '/*.csv', sep=''))
		for(dataFile in files){
			dataFile <- read.csv(dataFile, header=TRUE)
			if(!validate(dataFile, guide, places)){
				stop('Validation Failed!')
			}
		}
	}
}
