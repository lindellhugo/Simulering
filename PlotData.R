Plotting <- function(baseline, bond, helicopter, permanent, title, legend_pos = "none") {
    baseline_color <- "black"
    bond_color <- "red"
    helicopter_color <- "green"
    permanent_color <- "yellow"
    texts <- c("Baseline", "Bond financed", "Helicopter", "Permanent")
    baseline_lty <- 1
    bond_lty <- 1
    helicopter_lty <- 2
    permanent_lty <- 3
    years <- 1:length(baseline) - 1
    ymin <- min(baseline, bond, helicopter)
    ymax <- max(baseline, bond, helicopter)

    plot(years, baseline, type = "l", lty = baseline_lty, col = baseline_color, main = title, xlab = "Year", ylab = "",
    ylim = c(ymin, ymax))
    lines(years, bond, lty = bond_lty, col = bond_color)
    lines(years, helicopter, col = helicopter_color, lty = helicopter_lty)
    #lines(years, permanent, col = permanent_color, lty = permanent_lty)
    if (legend_pos != "none") {
        legend(legend_pos, legend = texts,
       col = c(baseline_color, bond_color, helicopter_color, permanent_color),
       lty = c(baseline_lty, bond_lty, helicopter_lty, permanent_lty), cex = 0.8)
    }
}

PlotData <- function(baseline, bond, helicopter, permanent, title, legend_pos = "none") {
    filename <- gsub("/", "_",title)
    filename = paste0("Images/", filename, ".png")
    png(filename)
    Plotting(baseline, bond, helicopter, permanent, title, legend_pos)
    dev.off()
    #Plotting(baseline, bond, helicopter, permanent, title, legend_pos)
}