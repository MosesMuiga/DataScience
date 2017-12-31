NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]
sumNEI <- NEI[fips=='24510', lapply(.SD, sum, na.rm = TRUE)
                , .SDcols = c("Emissions")
                , by = year]

png(filename='plot2.png')

barplot(totalNEI[, Emissions]
        , names = sumNEI[, year]
        , xlab = "Years", ylab = "Emissions"
        , main = "Emissions over the Years")

dev.off()