PlotData <- function(baseline, bond, helicopter, title, legend_pos = "none") {
    baseline_color <- "black"
    bond_color <- "red"
    helicopter_color <- "green"
    baseline_lty <- 1
    bond_lty <- 1
    helicopter_lty <- 2
    years <- 1:length(baseline) - 1
    ymin <- min(baseline, bond, helicopter)
    ymax <- max(baseline, bond, helicopter)

    plot(years, baseline, type="l",lty=baseline_lty, col = baseline_color, main = title, xlab = "Year", ylab = "",
    ylim = c(ymin, ymax))
    lines(years, bond, lty = bond_lty, col = bond_color)
    lines(years, helicopter, col = helicopter_color, lty = helicopter_lty)
    if (legend_pos != "none") {
        legend(legend_pos, legend = c("Baseline", "Bond financed", "Helicopter"),
       col = c(baseline_color, bond_color, helicopter_color),
       lty = c(baseline_lty, bond_lty, helicopter_lty), cex = 0.8)
    }

}