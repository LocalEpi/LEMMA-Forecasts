# --------------------------------------------------------------------------------
#   Functions to create overviews/maps:
#   1. CreateOverview
#   2. RtMap
# --------------------------------------------------------------------------------


#' @title Create overview from results
#' @description write me!
#' @param lemma.set write me!
#' @export
CreateOverview <- function(lemma.set) {
  GetProjectionPlot1 <- function(projection, data.type, inputs, title1) {
    obs.data <- inputs$obs.data[, .(date, conf = get(paste0(data.type, ".conf")), pui = get(paste0(data.type, ".pui")))]
    if (all(is.na(obs.data$conf))) return(NULL)
    projection.dt <- projection[, c("date", data.type), with = F]
    names(projection.dt)[2] <- "proj"
    dt.plot <- merge(obs.data, projection.dt, all = T, by = "date")

    max.date <- obs.data[!is.na(conf), max(date)] + 14
    min.date <- as.Date("2020/11/1") #max.date - 90

    obs.size <- 1 #1.5
    frac.pui <- inputs$frac_pui[name == data.type, mu]
    lb <- "Confirmed"
    ub <- paste0("Confirmed + ", 100 * frac.pui, "%PUI")
    dt.plot[, upper := conf + frac.pui * pui]

    dt.plot <- dt.plot[date >= min.date & date <= max.date]
    gg <- ggplot(dt.plot, aes(x=date)) +
      theme_light() +
      geom_line(aes(y = proj, color = "Median"))

    if (all(is.na(dt.plot$upper))) {
      gg <- gg + geom_point(aes(y=conf, color=lb), size = obs.size, na.rm = T)
    } else {
      gg <- gg + geom_point(aes(y=upper, color=ub), size = obs.size, na.rm = T)
    }

    over.aes <- list(linetype = c(1, 0))

    gg <- gg +
      xlab("") +
      ylab(LEMMA:::GetYLabel(data.type)) +
      labs(title = title1, caption = "localepi.github.io/LEMMA") +
      scale_color_manual("", values = c("blue", "palegreen4", "red4"), breaks = c("Median", lb, ub)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %d", expand = expansion()) +
      guides(color = guide_legend("", order = 1, override.aes = over.aes)) +
      theme(legend.position = "bottom", plot.margin = margin(0.1, 0.2, 0, 0.1, "in"),
            axis.text.x=element_text(hjust = 0.7))
    return(gg)
  }

  county.rt <- data.table(county = county.set)
  pdf("Forecasts/StateOverview.pdf", width = 11, height = 8.5)
  for (i in seq_along(county.set)) {
    lemma1 <- lemma.set[[i]]
    rt1 <- lemma1$projection[date == (lemma1$inputs$obs.data[, max(date)] - 14), rt]
    county.rt[i, Rt := rt1]
    title1 <- paste0(county.set[i], " Rt = ", sprintf("%2.2f", rt1))
    gg1 <- GetProjectionPlot1(lemma1$projection, "hosp", lemma1$inputs, title1)
    gg2 <- GetProjectionPlot1(lemma1$projection, "cases", lemma1$inputs, title1)
    print(ggpubr::ggarrange(gg1, gg2, nrow = 2))
  }
  dev.off()
  return(county.rt)
}


#' @title Create Rt map results
#' @description write me!
#' @param max.date write me!
#' @param county.rt write me!
#' @export
RtMap <- function(max.date, county.rt) {
  datestr <- as.character(max.date - 14)
  filestr <- paste0("Map/Rt_map_", datestr)
  grDevices::pdf(file = paste0(filestr, ".pdf"), width = 9.350, height = 7.225)
  county.rt[, subregion := tolower(county)]
  ca <- map_data("county", "california")
  ca <- as.data.table(ca)
  ca <- merge(ca, county.rt, by = "subregion", all.x = T)
  ca[, Rt := pmin(1.5, Rt)]
  ca[, Rt := pmax(0.8, Rt)]
  print(ggplot(ca, aes(long, lat)) + geom_polygon(aes(group = group, fill = Rt), colour = "black") + ggthemes::theme_map() + scale_fill_gradient(low = "green2", high = "red", limits = c(0.8, 1.5)) + ggtitle(datestr))
  dev.off()

  bay.area <- c("San Francisco", "San Mateo",  "Alameda", "Contra Costa", "Santa Clara", "Marin")
  print(county.rt[county %in% bay.area, .(county, Re = round(Rt, 2))])

  prev.file <- paste0("Map/Rt_map_", as.character(max.date - 15), ".csv")
  if (file.exists(prev.file)) {
    prev.rt <- fread(file = prev.file)
    setnames(prev.rt, "Rt", "previousRt")
    county.rt <- merge(county.rt, prev.rt, all = T)
    county.rt[, Rt_change := Rt - previousRt]
  } else {
    county.rt[, Rt_change := NA_real_]
  }

  write.csv(county.rt[!is.na(Rt), .(county, Rt = round(Rt, 2), Rt_change = round(Rt_change, 2))], file = paste0(filestr, ".csv"), row.names = F)
  invisible(NULL)
}

