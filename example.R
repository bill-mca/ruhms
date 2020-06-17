shr <- read.csv('2020-06-17_molonglo_sites.csv')

colnames(shr)[1] <- 'site.code'

shr <- shr[!is.na(shr[[1]]), ]

shr[order(shr[['distance.to.bottom']]), ]

shr <- shr[order(shr[['distance.to.bottom']]), ]

