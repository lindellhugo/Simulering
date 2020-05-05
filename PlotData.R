Plotting <- function(baseline, bond, helicopter, permanent, title, legend_pos = "none", usePercent = FALSE) {
    baseline_color <- "black"
    bond_color <- "red"
    helicopter_color <- "green"
    permanent_color <- "yellow"
    texts <- c("Baseline", "Bond financed", "Helicopter") #, "Permanent")
    baseline_lty <- 1
    bond_lty <- 1
    helicopter_lty <- 2
    permanent_lty <- 3
    years <- 1:length(baseline) - 1
    ymin <- min(baseline, bond, helicopter)
    ymax <- max(baseline, bond, helicopter)
    scaleFactor = 1
    ylabel = ""
    if (usePercent) {
        scaleFactor = 100
        ylabel = "%"
    }

    plot(years, scaleFactor * baseline, type = "l", lty = baseline_lty, col = baseline_color, main = title, xlab = "Year", ylab = ylabel,
    ylim = scaleFactor * c(ymin, ymax))
    lines(years, scaleFactor * bond, lty = bond_lty, col = bond_color)
    lines(years, scaleFactor * helicopter, col = helicopter_color, lty = helicopter_lty)
    #lines(years, scaleFactor *permanent, col = permanent_color, lty = permanent_lty)

    

    if (legend_pos != "none") {
        legend(legend_pos, legend = texts,
       col = c(baseline_color, bond_color, helicopter_color),
       lty = c(baseline_lty, bond_lty, helicopter_lty), cex = 0.8)
    }
}

PlotData <- function(result_baseline, result_bond, result_helicopter, result_permanent, parameters) {
    titles = c("Output gap", "Inflation", "Nominal interest rate", "OMO/GDP", "Monetary base/GDP", "Dept/GDP", "Transfers/GDP")
    lengendpos = c("bottomright", "bottomright", "bottomright", "bottomright", "bottomleft", "bottomleft", "bottomright")
    usePercent = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)

    par(mfrow = c(3, 2))
    for (index in 1:6) {
        title <- titles[index]
        pos <- lengendpos[index]
        Plotting(result_baseline[[index]], result_bond[[index]], result_helicopter[[index]], result_permanent[[index]], title, pos, usePercent[index])
    }

    for (index in 1:7) {
        title <- titles[index]
        pos <- lengendpos[index]
        filename <- gsub("/", "_", title)
        filename = paste0("Images/", filename, ".png")
        png(filename, width = 640, height = 480)
        Plotting(result_baseline[[index]], result_bond[[index]], result_helicopter[[index]], result_permanent[[index]], title, pos, usePercent[index])
        dev.off()
    }

    
}