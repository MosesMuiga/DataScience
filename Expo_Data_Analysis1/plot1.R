NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")]

sumNEI <- NEI[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("Emissions"), by = year]

png(filename='plot1.png')

barplot(sumNEI[, Emissions]
        , names = totalNEI[, year]
        , xlab = "Years", ylab = "Emissions"
        , main = "Emissions over the Years")

dev.off()