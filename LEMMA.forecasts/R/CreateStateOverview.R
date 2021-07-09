# --------------------------------------------------------------------------------
#   Functions to create overviews/maps:
#   1. CreateOverview
#   2. RtMap
# --------------------------------------------------------------------------------


#' @title Create overview from results
#' @param lemma.set list of results from running \code{\link[LEMMA.forecasts]{RunOneCounty}} on multiple counties
#' @param writedir a character string giving a directory to write to
#' @export
CreateOverview <- function(lemma.set, writedir = NULL) {
  GetProjectionPlot1 <- function(projection, data.type, inputs, title1, writedir = NULL) {
    obs.data <- inputs$obs.data[, .(date, conf = get(paste0(data.type, ".conf")), pui = get(paste0(data.type, ".pui")))]
    if (all(is.na(obs.data$conf))) return(NULL)
    projection.dt <- projection[, c("date", data.type), with = F]
    names(projection.dt)[2] <- "proj"
    dt.plot <- merge(obs.data, projection.dt, all = T, by = "date")

    max.date <- obs.data[!is.na(conf), max(date)] + 14
    min.date <- as.Date("2021/3/1") #max.date - 90

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
      coord_cartesian(ylim = c(0, NA), expand = F) +
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

  if (is.null(writedir)) {
    pdf("Forecasts/StateOverview.pdf", width = 11, height = 8.5)
  } else {
    pdf(file = file.path(writedir,"StateOverview.pdf"), width = 11, height = 8.5)
  }

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
#' @param max.date maximum date of data
#' @param county.rt `data.table` object produced by \code{\link[LEMMA.forecasts]{CreateOverview}}
#' @param writedir a character string giving a directory to write to
#' @export
RtMap <- function(max.date, county.rt, writedir = NULL) {
  datestr <- as.character(max.date - 14)

  if (is.null(writedir)) {
    filestr <- paste0("Map/Rt_map_", datestr)
    grDevices::pdf(file = paste0(filestr, ".pdf"), width = 9.350, height = 7.225)
  } else {
    filestr <- paste0("Rt_map_", datestr)
    grDevices::pdf(file = file.path(writedir, paste0(filestr, ".pdf")), width = 9.350, height = 7.225)
  }

  county.rt[, subregion := tolower(county)]
  ca <- map_data("county", "california")
  ca <- as.data.table(ca)
  ca <- merge(ca, county.rt, by = "subregion", all.x = T)
  ca[, Rt := pmin(1.5, Rt)]
  ca[, Rt := pmax(0.8, Rt)]
  print(ggplot(ca, aes(long, lat)) + geom_polygon(aes(group = group, fill = Rt), colour = "black") + ggthemes::theme_map() + scale_fill_gradient(low = "green2", high = "red", limits = c(0.8, 1.5)) + ggtitle(datestr))
  dev.off()

  # only write the csv if running assuming user is specifying the working directory for R (internal use)
  if (is.null(writedir)) {
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
  }

  invisible(NULL)
}


AggregateState <- function(county.set, county.pop, max.date, path, postfix) {
  covered_pop_frac <- sum(county.pop[county.set]) / sum(county.pop)
  scale <- 1 / covered_pop_frac
  total_proj <- NULL
  for (county1 in county.set) {
    cur_frac <- county.pop[county1] / sum(county.pop[county.set])
    proj <- as.data.table(read_excel(paste0(path, "/", county1, postfix, ".xlsx")))
    proj[, date := as.Date(date)]
    proj[, rt := rt * cur_frac]
    proj[, seroprev := seroprev * cur_frac]
    if (is.null(total_proj)) {
      total_proj <- proj
    } else {
      stopifnot(all.equal(proj$date, total_proj$date))
      for (i in setdiff(names(total_proj), "date")) {
        if (i %in% c("rt", "seroprev")) {
          temp <- proj[[i]] #don't scale these, using weighted avg
        } else {
          temp <- proj[[i]] * scale #scale up a little
        }
        total_proj[[i]] <- total_proj[[i]] + temp
      }
    }
  }
  openxlsx::write.xlsx(list(projection = total_proj), file = paste0(path, "/California", postfix, ".xlsx"))

  dt <- merge(total_proj, county.dt[, .(hosp.conf = sum(hosp.conf), icu.conf = sum(icu.conf), cases.conf = sum(cases.conf)), by = "date"], all.x = T)
  print(ggplot(dt, aes(x = date)) + geom_line(aes(y = hosp)) + geom_point(aes(y = hosp.conf)) + ggtitle(postfix))
  print(ggplot(dt[abs(date - max.date) < 60], aes(x = date)) + geom_line(aes(y = hosp)) + geom_point(aes(y = hosp.conf)) + ggtitle(postfix))

  print(ggplot(dt, aes(x = date)) + geom_line(aes(y = cases)) + geom_point(aes(y = cases.conf)) + ggtitle(postfix))
  print(ggplot(dt[abs(date - max.date) < 60], aes(x = date)) + geom_line(aes(y = cases)) + geom_point(aes(y = cases.conf)) + ggtitle(postfix))

  print(ggplot(dt, aes(x = date)) + geom_line(aes(y = icu)) + geom_point(aes(y = icu.conf)) + ggtitle(postfix))
  print(ggplot(dt[abs(date - max.date) < 60], aes(x = date)) + geom_line(aes(y = icu)) + geom_point(aes(y = icu.conf)) + ggtitle(postfix))

}


