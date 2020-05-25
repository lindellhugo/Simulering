Plotting <- function(baseline, bond, helicopter, permanent, title, texts, legend_pos = "none", usePercent = FALSE) {
    baseline_color <- "black"
    bond_color <- "red"
    helicopter_color <- "green"
    permanent_color <- "yellow"
    baseline_lty <- 1
    bond_lty <- 1
    helicopter_lty <- 2
    permanent_lty <- 3
    years <- 1:length(baseline) - 1
    if (length(helicopter) > 0) {
        ymin <- min(baseline, bond, helicopter)
        ymax <- max(baseline, bond, helicopter)
    } else {
        ymin <- min(baseline, bond)
        ymax <- max(baseline, bond)
    }
    
    scaleFactor = 1
    ylabel = ""
    if (usePercent) {
        scaleFactor = 100
        ylabel = "%"
    }

    plot(years, scaleFactor * baseline, type = "l", lty = baseline_lty, col = baseline_color, main = title, xlab = "Year", ylab = ylabel,
    ylim = scaleFactor * c(ymin, ymax))
    lines(years, scaleFactor * bond, lty = bond_lty, col = bond_color)
    if (length(helicopter) > 0) {
        lines(years, scaleFactor * helicopter, col = helicopter_color, lty = helicopter_lty)
    }
    #lines(years, scaleFactor *permanent, col = permanent_color, lty = permanent_lty)

    

    if (legend_pos != "none") {
        legend(legend_pos, legend = texts,
       col = c(baseline_color, bond_color, helicopter_color),
       lty = c(baseline_lty, bond_lty, helicopter_lty), cex = 0.8)
    }
}

PlotData <- function(result_baseline, result_bond, result_helicopter, result_permanent, parameters) {
        titles = c("Output gap", "Inflation", "Nominal interest rate", "OMO/GDP", "Monetary base/GDP", "Debt/GDP", "Transfers/GDP", "Real interest rate")
        lengendpos = c("bottomright", "bottomleft", "topleft", "bottomright", "bottomleft", "bottomleft", "bottomright", "bottomright")
    usePercent = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
texts <- c("Baseline", "Bond financed", "Helicopter") #, "Permanent")
    par(mfrow = c(3, 2))
    for (index in 1:6) {
        
        title <- titles[index]
        pos <- lengendpos[index]
        if (index != 1) {
            pos <- "none"
        }
        Plotting(result_baseline[[index]], result_bond[[index]], result_helicopter[[index]], result_permanent[[index]], title, texts, pos, usePercent[index])
    }

    for (index in 1:8) {
        title <- titles[index]
        pos <- lengendpos[index]
        filename <- gsub("/", "_", title)
        filename = paste0("Images/", filename, ".png")
        png(filename, width = 600, height = 300)
        Plotting(result_baseline[[index]], result_bond[[index]], result_helicopter[[index]], result_permanent[[index]], title, texts, pos, usePercent[index])
        dev.off()
    }

    
}