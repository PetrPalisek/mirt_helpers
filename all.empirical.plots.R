all.empirical.plots <- function(model, plot.titles = NULL) {
  require(mirt)
  if (is.null(plot.titles)) {
    for (i in 1:ncol(model@Data$data)) {
      plot(mirt::itemfit(model, p.adjust = "BH", na.rm = TRUE, empirical.plot = i))
    }
  }
  
  else if (plot.titles == "data") {
    col_names <- colnames(model@Data$data)
    for (i in 1:ncol(model@Data$data)) {
      p <- mirt::itemfit(model, p.adjust = "BH", na.rm = TRUE, empirical.plot = i)
      p$main <- col_names[i]
      print(p)
    }
  }
}
