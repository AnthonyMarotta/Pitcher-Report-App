# Pitching Report — Shiny app
# Place tracking CSV(s) in this folder; select date and pitcher; preview and download PDF.

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

# Folder that contains this script — CSV files are read from here.
PITCHER_APP_HOME <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }
  wd <- normalizePath(getwd(), winslash = "/")
  if (file.exists(file.path(wd, "app.R"))) {
    return(wd)
  }
  if (file.exists(file.path(wd, "Pitcher Report App.R"))) {
    return(wd)
  }
  wd
})

app_dir <- function() {
  PITCHER_APP_HOME
}

list_local_csvs <- function(dir = app_dir()) {
  files <- list.files(dir, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(character(0))
  }
  files[order(basename(files))]
}

has_csv_upload <- function(upload) {
  !is.null(upload) &&
    !is.null(upload$datapath) &&
    length(upload$datapath) > 0 &&
    nzchar(upload$datapath[1]) &&
    file.exists(upload$datapath[1])
}

GAME_DATE_ALL <- "All"

is_all_dates_selected <- function(selected) {
  if (is.null(selected) || length(selected) == 0) {
    return(FALSE)
  }
  GAME_DATE_ALL %in% as.character(selected)
}

game_date_choices <- function(dates) {
  dates <- sort(unique(dates[!is.na(dates)]))
  if (length(dates) == 0) {
    return(character(0))
  }
  labels <- format(dates, "%m/%d/%Y")
  values <- format(dates, "%Y-%m-%d")
  names(values) <- labels
  c("All" = GAME_DATE_ALL, values)
}

coerce_selected_game_dates <- function(selected, available_dates) {
  available_dates <- sort(unique(available_dates[!is.na(available_dates)]))
  if (is_all_dates_selected(selected)) {
    return(available_dates)
  }
  if (is.null(selected) || length(selected) == 0) {
    return(as.Date(character(0)))
  }
  sel <- selected[as.character(selected) != GAME_DATE_ALL]
  if (length(sel) == 0) {
    return(as.Date(character(0)))
  }
  parsed <- safe_date(sel)
  intersect(parsed[!is.na(parsed)], available_dates)
}

resolve_selected_dates <- function(selected, available_dates) {
  coerce_selected_game_dates(selected, available_dates)
}

filter_df_by_game_dates <- function(df, selected, available_dates = NULL) {
  if (is.null(available_dates)) {
    available_dates <- df$Date
  }
  dates <- resolve_selected_dates(selected, available_dates)
  if (length(dates) == 0) {
    return(df[FALSE, , drop = FALSE])
  }
  df %>% filter(Date %in% dates)
}

preserve_game_date_selection <- function(current, available_dates) {
  available_dates <- sort(unique(available_dates[!is.na(available_dates)]))
  if (length(available_dates) == 0) {
    return(character(0))
  }
  if (is_all_dates_selected(current)) {
    return(GAME_DATE_ALL)
  }
  if (is.null(current) || length(current) == 0) {
    return(format(available_dates[1], "%Y-%m-%d"))
  }
  valid <- coerce_selected_game_dates(current, available_dates)
  if (length(valid) > 0) {
    return(format(valid, "%Y-%m-%d"))
  }
  format(available_dates[1], "%Y-%m-%d")
}

game_date_status_text <- function(selected) {
  if (is_all_dates_selected(selected)) {
    return("All dates")
  }
  sel <- selected[as.character(selected) != GAME_DATE_ALL]
  date_vec <- safe_date(sel)
  date_vec <- date_vec[!is.na(date_vec)]
  if (length(date_vec) == 1) {
    return(format(date_vec[1], "%B %d, %Y"))
  }
  "Multiple dates"
}

safe_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    x_num <- x[is.finite(x)]
    if (length(x_num) > 0) {
      med <- median(x_num, na.rm = TRUE)
      if (med > 20000) {
        return(as.Date(x, origin = "1899-12-30"))
      }
      if (med > 1000) {
        return(as.Date(x, origin = "1970-01-01"))
      }
    }
  }
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  # Mixed formats are common (e.g. some rows "2026-03-25" and others "2/19/26").
  # Parse iteratively so early matches don't prevent later formats.
  out <- rep(as.Date(NA), length(x_chr))
  remaining <- is.na(out) & !is.na(x_chr)
  possible_formats <- c("%Y-%m-%d", "%m/%d/%y", "%m/%d/%Y", "%m-%d-%Y", "%m-%d-%y", "%Y%m%d")
  for (fmt in possible_formats) {
    if (!any(remaining)) break
    parsed <- as.Date(x_chr[remaining], format = fmt)
    ok <- !is.na(parsed)
    if (any(ok)) {
      idx <- which(remaining)[ok]
      out[idx] <- parsed[ok]
      remaining[idx] <- FALSE
    }
  }
  if (any(remaining)) {
    parsed <- suppressWarnings(as.Date(x_chr[remaining]))
    out[which(remaining)] <- parsed
  }
  out
}

format_pitcher_name <- function(pitcher) {
  parts <- strsplit(pitcher, ", ", fixed = TRUE)[[1]]
  if (length(parts) >= 2) {
    paste(parts[2], parts[1])
  } else {
    pitcher
  }
}

guess_opponent <- function(pitcher_data, pitcher_team) {
  if ("HomeTeam" %in% names(pitcher_data) && "AwayTeam" %in% names(pitcher_data)) {
    ht <- unique(stats::na.omit(pitcher_data$HomeTeam))[1]
    at <- unique(stats::na.omit(pitcher_data$AwayTeam))[1]
    if (!is.na(pitcher_team) && nzchar(pitcher_team)) {
      if (identical(pitcher_team, ht)) {
        return(as.character(at))
      }
      if (identical(pitcher_team, at)) {
        return(as.character(ht))
      }
    }
  }
  bt <- unique(stats::na.omit(pitcher_data$BatterTeam))
  pt <- unique(stats::na.omit(pitcher_data$PitcherTeam))[1]
  opps <- setdiff(bt, pt)
  if (length(opps) > 0) {
    return(as.character(opps[1]))
  }
  "Opponent"
}

# ---- Pitch report constants ----
pitch_type_colors <- c(
  "Fastball" = "#B22222",
  "Curveball" = "#5CACEE",
  "Slider" = "#E6C200",
  "ChangeUp" = "#458B00",
  "Sinker" = "#CD6600",
  "Cutter" = "#2F4F4F",
  "Splitter" = "#DAA520"
)

tm_bg <- "#F5F5F5"
tm_axis_col <- "black"
report_header_green <- "navyblue"

sz_xmin <- -0.83
sz_xmax <- 0.83
sz_ymin <- 1.479
sz_ymax <- 3.5
sz_x_breaks <- seq(sz_xmin, sz_xmax, length.out = 4)
sz_y_breaks <- seq(sz_ymin, sz_ymax, length.out = 4)

mv_break_xrange <- 70
mv_break_yrange <- 66
rel_xrange <- 10
rel_yrange <- 7.15
movement_rel_coord_ratio_mv <- 1.08
movement_rel_coord_ratio_rel <- movement_rel_coord_ratio_mv *
  (mv_break_yrange / mv_break_xrange) / (rel_yrange / rel_xrange)

mv_rel_plot_margin <- margin(6, 6, 4, 6)
mv_rel_legend_theme <- theme(
  legend.position = "bottom",
  legend.title = element_text(size = 9, face = "bold"),
  legend.text = element_text(size = 8),
  legend.margin = margin(t = 4, b = 0),
  legend.box.spacing = unit(2, "pt")
)
mv_rel_fill_guide <- guide_legend(nrow = 1, byrow = TRUE)

pitch_types_fastball <- c("Fastball", "Sinker")
pitch_types_breaking <- c("Slider", "Cutter", "Curveball")
pitch_types_offspeed <- c("ChangeUp")

location_coord_ratio <- 1.22
location_ymin <- 1
location_ymax <- 4.05

attach_title_to_table_grob <- function(tg, label,
                                       gp = gpar(fontsize = 12, fontface = "bold", col = "black", fontfamily = "sans")) {
  tit <- textGrob(label, x = unit(0.5, "npc"), y = unit(0.45, "npc"), just = "center", gp = gp)
  tit_h <- grobHeight(tit) + unit(4, "pt")
  nc <- length(tg$widths)
  tg2 <- gtable::gtable_add_rows(tg, heights = tit_h, pos = 0)
  gtable::gtable_add_grob(tg2, tit, t = 1, l = 1, r = nc, b = 1, z = Inf, clip = "off")
}

strike_zone_nine_box_layers <- list(
  geom_segment(
    x = sz_x_breaks[2], xend = sz_x_breaks[2], y = sz_ymin, yend = sz_ymax,
    inherit.aes = FALSE, color = "grey35", linewidth = 0.35
  ),
  geom_segment(
    x = sz_x_breaks[3], xend = sz_x_breaks[3], y = sz_ymin, yend = sz_ymax,
    inherit.aes = FALSE, color = "grey35", linewidth = 0.35
  ),
  geom_segment(
    x = sz_xmin, xend = sz_xmax, y = sz_y_breaks[2], yend = sz_y_breaks[2],
    inherit.aes = FALSE, color = "grey35", linewidth = 0.35
  ),
  geom_segment(
    x = sz_xmin, xend = sz_xmax, y = sz_y_breaks[3], yend = sz_y_breaks[3],
    inherit.aes = FALSE, color = "grey35", linewidth = 0.35
  )
)

hull_polygons <- function(df, xcol, ycol, grp = "TaggedPitchType") {
  if (!nrow(df)) {
    return(df[FALSE, ])
  }
  xr <- df[[xcol]]
  yr <- df[[ycol]]
  ok <- !is.na(xr) & !is.na(yr)
  df <- df[ok, , drop = FALSE]
  if (!nrow(df)) {
    return(df[FALSE, ])
  }
  dplyr::group_by(df, .data[[grp]]) %>%
    dplyr::group_modify(~ {
      d <- .x
      if (nrow(d) < 3L) {
        return(d[0, , drop = FALSE])
      }
      ih <- tryCatch(
        grDevices::chull(d[[xcol]], d[[ycol]]),
        error = function(e) integer()
      )
      if (length(ih) < 3L) {
        return(d[0, , drop = FALSE])
      }
      d <- d[ih, , drop = FALSE]
      rbind(d, d[1L, , drop = FALSE])
    }) %>%
    dplyr::ungroup()
}

strike_zone_location_plot <- function(df, title, show_legend = TRUE) {
  sz_common_scales <- list(
    scale_x_continuous(limits = c(-1.8, 1.8), expand = c(0, 0)),
    scale_y_continuous(limits = c(location_ymin, location_ymax), expand = c(0, 0)),
    coord_fixed(ratio = location_coord_ratio, expand = FALSE, clip = "off")
  )
  sz_theme <- theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 9, hjust = 0.5, margin = margin(b = 2))
  )
  zone_outline <- geom_rect(
    xmin = sz_xmin, xmax = sz_xmax, ymin = sz_ymin, ymax = sz_ymax,
    inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.6
  )
  if (nrow(df) == 0) {
    p <- ggplot() +
      zone_outline +
      strike_zone_nine_box_layers +
      annotate("text", x = 0, y = 2.5, label = "No pitches", size = 3.5) +
      labs(title = title, x = " ", y = " ") +
      theme_minimal() +
      sz_theme +
      sz_common_scales
    return(p)
  }
  p <- ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)) +
    zone_outline +
    strike_zone_nine_box_layers +
    geom_point(shape = 21, size = 2.2, stroke = 0.35, color = "black", alpha = 0.75, na.rm = TRUE) +
    scale_fill_manual(values = pitch_type_colors, drop = FALSE) +
    labs(title = title, x = " ", y = " ", fill = "Pitch Type") +
    theme_minimal() +
    sz_theme +
    sz_common_scales
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  p
}

prepare_tracking_data <- function(data) {
  if (!"Date" %in% names(data) && "GameDate" %in% names(data)) {
    data$Date <- data$GameDate
  }
  data$Date <- safe_date(data$Date)

  if ("TaggedPitchType" %in% names(data)) {
    data <- data %>%
      filter(TaggedPitchType != "Undefined") %>%
      mutate(
        TaggedPitchType = case_when(
          TaggedPitchType %in% c("Sinker", "FourSeamFastBall") ~ "Fastball",
          TaggedPitchType == "Splitter" ~ "ChangeUp",
          TaggedPitchType == "Sweeper" ~ "Slider",
          TRUE ~ TaggedPitchType
        )
      )
  }
  data
}

calculate_pitcher_game_statistics <- function(data_filtered) {
  data_filtered <- data_filtered %>%
    mutate(
      InStrikeZone = PlateLocSide >= sz_xmin & PlateLocSide <= sz_xmax &
        PlateLocHeight >= sz_ymin & PlateLocHeight <= sz_ymax,
      Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
      Chase = ifelse(!InStrikeZone & Swing, 1, 0)
    )

  first_pitch_strikes <- data_filtered %>%
    filter(
      PitchofPA == 1 &
        PitchCall %in% c(
          "FoulBallNotFieldable", "FoulBallFieldable",
          "InPlay", "StrikeCalled", "StrikeSwinging"
        )
    )

  first_pitch_count <- nrow(data_filtered %>% filter(PitchofPA == 1))
  first_pitch_strike_percentage <- if (first_pitch_count > 0) {
    round(nrow(first_pitch_strikes) / first_pitch_count * 100, 1)
  } else {
    NA_real_
  }

  earned_run_group_cols <- if ("GameID" %in% names(data_filtered) && any(!is.na(data_filtered$GameID))) {
    c("GameID", "Inning")
  } else {
    "Inning"
  }

  earned_run_data <- data_filtered %>%
    group_by(across(all_of(earned_run_group_cols))) %>%
    mutate(
      ErrorOccurred = any(PlayResult == "Error"),
      RunWithTwoOuts = RunsScored > 0 & Outs == 2,
      UnearnedFlag = ErrorOccurred & RunWithTwoOuts
    ) %>%
    ungroup()

  ER <- earned_run_data %>%
    filter(RunsScored > 0) %>%
    filter(!UnearnedFlag) %>%
    summarise(EarnedRuns = sum(RunsScored, na.rm = TRUE)) %>%
    pull(EarnedRuns)
  if (length(ER) == 0) {
    ER <- 0
  }

  data_filtered <- data_filtered %>%
    group_by(Pitcher, Batter) %>%
    mutate(AtBatID = cumsum(PitchofPA == 1)) %>%
    ungroup()

  two_thirds_data <- data_filtered %>%
    group_by(Pitcher, Batter, AtBatID) %>%
    summarise(
      StrikeCount = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable") & PitchofPA <= 3, na.rm = TRUE),
      OutRecorded = sum(PlayResult == "Out" & PitchofPA <= 3, na.rm = TRUE),
      TwoStrikesOrOut = ifelse(StrikeCount >= 2 | OutRecorded >= 1, 1, 0),
      .groups = "drop"
    ) %>%
    summarise(
      TotalAtBats = dplyr::n(),
      SuccessfulOutcomes = sum(TwoStrikesOrOut),
      `2/3%` = if (TotalAtBats > 0) round(SuccessfulOutcomes / TotalAtBats * 100, 1) else NA_real_,
      .groups = "drop"
    )

  hits <- sum(data_filtered$PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE)
  ab <- sum(
    (data_filtered$PitchCall %in% c("InPlay") | data_filtered$KorBB == "Strikeout") &
      !data_filtered$KorBB %in% c("Walk") &
      !data_filtered$PitchCall %in% c("HitByPitch") &
      !data_filtered$PlayResult %in% c("Sacrifice"),
    na.rm = TRUE
  )
  so <- sum(data_filtered$KorBB == "Strikeout", na.rm = TRUE)
  bb <- sum(data_filtered$KorBB == "Walk", na.rm = TRUE)
  pa <- sum(
    data_filtered$PitchCall %in% c("InPlay", "HitByPitch") | data_filtered$KorBB %in% c("Strikeout", "Walk"),
    na.rm = TRUE
  )
  ip <- round(
    (sum(as.numeric(data_filtered$OutsOnPlay), na.rm = TRUE) + sum(data_filtered$KorBB == "Strikeout", na.rm = TRUE)) / 3,
    2
  )

  statistics <- data_filtered %>%
    summarise(
      Pitches = length(PitchCall),
      PA = pa,
      AB = ab,
      IP = ip,
      R = sum(RunsScored, na.rm = TRUE),
      H = hits,
      `1B` = sum(PlayResult == "Single", na.rm = TRUE),
      `2B` = sum(PlayResult == "Double", na.rm = TRUE),
      `3B` = sum(PlayResult == "Triple", na.rm = TRUE),
      HR = sum(PlayResult == "HomeRun", na.rm = TRUE),
      SO = so,
      BB = bb,
      HBP = sum(PitchCall == "HitByPitch", na.rm = TRUE),
      BAA = if (ab > 0) round(hits / ab, 3) else NA_real_,
      WHIP = if (ip > 0) round((bb + hits) / ip, 2) else NA_real_,
      `K/BB` = if (bb > 0) round(so / bb, 2) else NA_real_,
      `FPS%` = first_pitch_strike_percentage,
      `K%` = if (pa > 0) round((so / pa) * 100, 1) else NA_real_,
      `BB%` = if (pa > 0) round((bb / pa) * 100, 1) else NA_real_
    ) %>%
    mutate(`2/3%` = two_thirds_data$`2/3%`)

  statistics
}

build_pitcher_report_pdf <- function(file, pitcher_data, pitcher, opponent = NULL) {
  if (nrow(pitcher_data) == 0) {
    stop("No rows for this pitcher and date.")
  }

  formatted_name <- format_pitcher_name(pitcher)
  # If multiple dates are selected, omit date from the header/subtitle area (easiest).
  date_values <- sort(unique(pitcher_data$Date[!is.na(pitcher_data$Date)]))
  date_label <- if (length(date_values) == 1) format(date_values[1], "%m/%d/%Y") else ""
  pitcher_team <- unique(stats::na.omit(pitcher_data$PitcherTeam))[1]
  if (is.null(opponent) || !nzchar(opponent)) {
    opponent <- guess_opponent(pitcher_data, pitcher_team)
  }

  mv_hull <- hull_polygons(pitcher_data, "HorzBreak", "InducedVertBreak")
  mv_centroids <- pitcher_data %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      HorzBreak = mean(HorzBreak, na.rm = TRUE),
      InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
      .groups = "drop"
    )

  movement_chart <- ggplot(pitcher_data, aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType))
  if (nrow(mv_hull)) {
    movement_chart <- movement_chart +
      geom_polygon(
        data = mv_hull,
        aes(x = HorzBreak, y = InducedVertBreak, group = TaggedPitchType),
        inherit.aes = FALSE, fill = "grey82", alpha = 0.32, color = NA
      )
  }
  movement_chart <- movement_chart +
    geom_hline(yintercept = 0, linetype = "dashed", color = tm_axis_col, linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = tm_axis_col, linewidth = 0.35) +
    geom_point(
      data = mv_centroids, aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType),
      shape = 21, size = 16, stroke = 0.35, color = "black", alpha = 0.22, na.rm = TRUE
    ) +
    geom_point(shape = 21, size = 2.6, stroke = 0.35, color = "black", alpha = 0.95, na.rm = TRUE) +
    scale_x_continuous(limits = c(-35, 35), breaks = seq(-35, 35, by = 5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-36, 30), breaks = seq(-35, 30, by = 5), expand = c(0, 0)) +
    coord_fixed(ratio = movement_rel_coord_ratio_mv, expand = FALSE, clip = "off") +
    labs(x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)", fill = "Pitch Type", title = NULL) +
    scale_fill_manual(values = pitch_type_colors, guide = mv_rel_fill_guide) +
    theme_minimal(base_size = 11) +
    mv_rel_legend_theme +
    theme(
      plot.background = element_rect(fill = tm_bg, color = NA),
      panel.background = element_rect(fill = tm_bg, color = NA),
      panel.grid = element_blank(),
      axis.title = element_text(size = 9, color = "grey25"),
      axis.text = element_text(size = 8, color = "grey35"),
      plot.margin = mv_rel_plot_margin
    )

  rel_hull <- hull_polygons(pitcher_data, "RelSide", "RelHeight")

  release_chart <- ggplot(pitcher_data, aes(x = RelSide, y = RelHeight, fill = TaggedPitchType))
  if (nrow(rel_hull)) {
    release_chart <- release_chart +
      geom_polygon(
        data = rel_hull,
        aes(x = RelSide, y = RelHeight, group = TaggedPitchType),
        inherit.aes = FALSE, fill = "grey82", alpha = 0.32, color = NA
      )
  }
  release_chart <- release_chart +
    geom_vline(xintercept = 0, linetype = "dashed", color = tm_axis_col, linewidth = 0.35) +
    geom_point(shape = 21, size = 2.6, stroke = 0.35, color = "black", alpha = 0.95, na.rm = TRUE) +
    scale_x_continuous(limits = c(-5, 5), breaks = -5:5, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 7.15), breaks = 0:7, expand = c(0, 0)) +
    coord_fixed(ratio = movement_rel_coord_ratio_rel, expand = FALSE, clip = "off") +
    labs(
      x = "Horizontal Release (ft)",
      y = "Vertical Release (ft)",
      fill = "Pitch Type",
      title = NULL
    ) +
    scale_fill_manual(values = pitch_type_colors, guide = mv_rel_fill_guide) +
    theme_minimal(base_size = 11) +
    mv_rel_legend_theme +
    theme(
      plot.background = element_rect(fill = tm_bg, color = NA),
      panel.background = element_rect(fill = tm_bg, color = NA),
      panel.grid = element_blank(),
      axis.title = element_text(size = 9, color = "grey25"),
      axis.text = element_text(size = 8, color = "grey35"),
      plot.margin = mv_rel_plot_margin
    )

  pitch_loc_df <- pitcher_data %>%
    mutate(
      Strikes_pre = suppressWarnings(as.integer(Strikes)),
      InStrikeZone = PlateLocSide >= sz_xmin & PlateLocSide <= sz_xmax &
        PlateLocHeight >= sz_ymin & PlateLocHeight <= sz_ymax,
      Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay")
    )

  loc_fastball <- strike_zone_location_plot(
    pitch_loc_df %>% filter(TaggedPitchType %in% pitch_types_fastball),
    "PITCH DISTRIBUTION (FB)",
    show_legend = TRUE
  )
  loc_breaking <- strike_zone_location_plot(
    pitch_loc_df %>% filter(TaggedPitchType %in% pitch_types_breaking),
    "PITCH DISTRIBUTION (BB)",
    show_legend = TRUE
  )
  loc_offspeed <- strike_zone_location_plot(
    pitch_loc_df %>% filter(TaggedPitchType %in% pitch_types_offspeed),
    "PITCH DISTRIBUTION (OS)",
    show_legend = TRUE
  )
  loc_whiffs <- strike_zone_location_plot(
    pitch_loc_df %>% filter(PitchCall == "StrikeSwinging"),
    "WHIFFS",
    show_legend = TRUE
  )
  loc_chase <- strike_zone_location_plot(
    pitch_loc_df %>% filter(!InStrikeZone & Swing),
    "CHASE",
    show_legend = TRUE
  )
  loc_hits <- strike_zone_location_plot(
    pitch_loc_df %>%
      filter(
        PitchCall == "InPlay",
        PlayResult %in% c("Single", "Double", "Triple", "HomeRun")
      ),
    "HITS",
    show_legend = TRUE
  )
  loc_outs <- strike_zone_location_plot(
    pitch_loc_df %>% filter(PitchCall == "InPlay", PlayResult == "Out"),
    "OUTS",
    show_legend = TRUE
  )
  loc_2strike <- strike_zone_location_plot(
    pitch_loc_df %>% filter(Strikes_pre == 2L),
    "PITCH DISTRIBUTION (2K)",
    show_legend = TRUE
  )

  pitch_metrics_table <- pitcher_data %>%
    group_by(TaggedPitchType) %>%
    summarise(
      Pitches = length(TaggedPitchType),
      `Avg Velocity` = round(mean(RelSpeed, na.rm = TRUE), 1),
      `Max Velocity` = round(max(RelSpeed, na.rm = TRUE), 1),
      `Avg Spin Rate` = round(mean(SpinRate, na.rm = TRUE), 1),
      `Max Spin Rate` = round(max(SpinRate, na.rm = TRUE), 1),
      `iVB` = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      `Horz Break` = round(mean(HorzBreak, na.rm = TRUE), 1),
      `Rel Height` = round(mean(RelHeight, na.rm = TRUE), 1),
      `Rel Side` = round(mean(RelSide, na.rm = TRUE), 1),
      `Ext` = round(mean(Extension, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(Pitches)) %>%
    dplyr::rename_with(~ "Pitch Type", .cols = 1)

  data_filtered <- pitcher_data %>%
    mutate(
      InStrikeZone = PlateLocSide >= sz_xmin & PlateLocSide <= sz_xmax &
        PlateLocHeight >= sz_ymin & PlateLocHeight <= sz_ymax,
      Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
      Chase = ifelse(!InStrikeZone & Swing, 1, 0),
      ZSwing = ifelse(InStrikeZone & Swing, 1, 0),
      HardHitCheck = ifelse(!is.na(ExitSpeed) & ExitSpeed >= 95 & ExitSpeed <= 120 & PitchCall == "InPlay", 1, 0)
    )

  total_pitches <- nrow(data_filtered)

  statistics <- data_filtered %>%
    group_by(TaggedPitchType) %>%
    summarise(
      Pitches = length(TaggedPitchType),
      Strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"), na.rm = TRUE),
      Whiffs = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
      Swing = sum(Swing, na.rm = TRUE),
      GroundBall = sum(TaggedHitType == "GroundBall", na.rm = TRUE),
      FlyBall = sum(TaggedHitType == "FlyBall", na.rm = TRUE),
      TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive"), na.rm = TRUE),
      InStrikeZone = sum(InStrikeZone, na.rm = TRUE),
      Chase = sum(Chase, na.rm = TRUE),
      ZSwingCount = sum(ZSwing, na.rm = TRUE),
      SO = sum(KorBB == "Strikeout", na.rm = TRUE),
      BB = sum(KorBB == "Walk", na.rm = TRUE),
      HBP = sum(PitchCall == "HitByPitch", na.rm = TRUE),
      AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !PitchCall %in% c("HitByPitch") & !KorBB %in% c("Walk") & !PlayResult %in% c("Sacrifice"), na.rm = TRUE),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE),
      `HardHit%` = round(sum(HardHitCheck, na.rm = TRUE) / sum(PitchCall == "InPlay", na.rm = TRUE) * 100, 1),
      ZWhiffs = sum(PitchCall == "StrikeSwinging" & InStrikeZone, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Swing%` = round(Swing / Pitches * 100, 1),
      `Whiff%` = round(Whiffs / Swing * 100, 1),
      `Chase%` = round(Chase / Swing * 100, 1),
      `Strike%` = round(Strikes / Pitches * 100, 1),
      `Strikeout%` = round(SO / AB * 100, 1),
      `Walk%` = round(BB / AB * 100, 1),
      `GroundBall%` = round(GroundBall / TotalBallsInPlay * 100, 1),
      `FlyBall%` = round(FlyBall / TotalBallsInPlay * 100, 1),
      `Zone%` = round(ifelse(Pitches > 0, InStrikeZone / Pitches * 100, 0), 1),
      `Zone Whiff%` = round(ifelse(ZSwingCount > 0, ZWhiffs / ZSwingCount * 100, 0), 1),
      `ZSwing%` = round(ifelse(InStrikeZone > 0, ZSwingCount / InStrikeZone * 100, 0), 1),
      BAA = round(H / AB, 3),
      `Pitch%` = round(Pitches / total_pitches * 100, 1)
    ) %>%
    select(TaggedPitchType, Pitches, `Pitch%`, `Swing%`, `Whiff%`, `Chase%`, `Strike%`, `Zone%`, `GroundBall%`, `FlyBall%`, `HardHit%`, BAA) %>%
    arrange(desc(Pitches))

  all_row <- data_filtered %>%
    summarise(
      TaggedPitchType = "All",
      Pitches = length(Swing),
      Swing = sum(Swing, na.rm = TRUE),
      Whiffs = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
      Chase = sum(Chase, na.rm = TRUE),
      Strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"), na.rm = TRUE),
      GroundBall = sum(TaggedHitType == "GroundBall", na.rm = TRUE),
      FlyBall = sum(TaggedHitType == "FlyBall", na.rm = TRUE),
      TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive"), na.rm = TRUE),
      InStrikeZone = sum(InStrikeZone, na.rm = TRUE),
      ZSwingCount = sum(ZSwing, na.rm = TRUE),
      ZWhiffs = sum(PitchCall == "StrikeSwinging" & InStrikeZone, na.rm = TRUE),
      AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !PitchCall %in% c("HitByPitch") & !KorBB %in% c("Walk") & !PlayResult %in% c("Sacrifice"), na.rm = TRUE),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE),
      SO = sum(KorBB == "Strikeout", na.rm = TRUE),
      BB = sum(KorBB == "Walk", na.rm = TRUE),
      `HardHit%` = round(sum(HardHitCheck, na.rm = TRUE) / sum(PitchCall == "InPlay", na.rm = TRUE) * 100, 1)
    ) %>%
    mutate(
      `Pitch%` = 100,
      `Swing%` = round(Swing / Pitches * 100, 1),
      `Whiff%` = round(Whiffs / Swing * 100, 1),
      `Chase%` = round(Chase / Swing * 100, 1),
      `Strike%` = round(Strikes / Pitches * 100, 1),
      `GroundBall%` = round(GroundBall / TotalBallsInPlay * 100, 1),
      `FlyBall%` = round(FlyBall / TotalBallsInPlay * 100, 1),
      `Zone%` = round(ifelse(Pitches > 0, InStrikeZone / Pitches * 100, 0), 1),
      BAA = round(H / AB, 3)
    ) %>%
    select(TaggedPitchType, Pitches, `Pitch%`, `Swing%`, `Whiff%`, `Chase%`, `Strike%`, `Zone%`, `GroundBall%`, `FlyBall%`, `HardHit%`, BAA)

  performance_table <- bind_rows(statistics, all_row) %>%
    dplyr::rename_with(~ "Pitch Type", .cols = 1)

  statistics_table <- calculate_pitcher_game_statistics(pitcher_data)

  title_grob <- textGrob(
    paste0(
      toupper(formatted_name),
      " — POST GAME PITCHING REPORT",
      if (nzchar(date_label)) paste0(" — ", date_label) else ""
    ),
    x = 0.5, y = 0.5, just = "center",
    gp = gpar(fontsize = 15, fontface = "bold", col = "black", fontfamily = "sans")
  )

  fancy_theme <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 8, fontface = "plain", fontfamily = "sans"),
      bg_params = list(fill = "white", col = "grey80"),
      padding = unit(c(2, 3), "mm")
    ),
    colhead = list(
      fg_params = list(fontsize = 8, fontface = "bold", col = "white"),
      bg_params = list(fill = report_header_green, col = "grey75"),
      padding = unit(c(1.25, 3.5), "mm")
    )
  )

  pitch_metrics_grob <- tableGrob(pitch_metrics_table, rows = NULL, theme = fancy_theme)
  statistics_grob <- tableGrob(statistics_table, rows = NULL, theme = fancy_theme)

  performance_font_size <- if (nrow(performance_table) > 5) 6 else 8

  performance_theme_dynamic <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = performance_font_size, fontface = "plain", fontfamily = "sans"),
      bg_params = list(fill = "white", col = "grey80"),
      padding = unit(c(2, 3), "mm")
    ),
    colhead = list(
      fg_params = list(fontsize = performance_font_size + 1, fontface = "bold", col = "white"),
      bg_params = list(fill = report_header_green, col = "grey75"),
      padding = unit(c(1.25, 3.5), "mm")
    )
  )

  performance_grob <- tableGrob(performance_table, rows = NULL, theme = performance_theme_dynamic)

  statistics_grob_with_title <- attach_title_to_table_grob(statistics_grob, "GAME STATISTICS")
  pitch_metrics_grob_with_title <- attach_title_to_table_grob(pitch_metrics_grob, "PITCH METRICS")
  performance_grob_with_title <- attach_title_to_table_grob(performance_grob, "PITCH PERFORMANCE")

  combined_tables_grob <- arrangeGrob(
    grobs = list(statistics_grob_with_title, pitch_metrics_grob_with_title, performance_grob_with_title),
    nrow = 3,
    heights = c(0.85, 1.15, 1.25),
    padding = unit(0, "pt")
  )

  fancy_plot_theme <- theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, "cm")
  )

  plot_margin_sm <- theme(plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm"))

  loc_fastball <- loc_fastball + fancy_plot_theme
  loc_breaking <- loc_breaking + fancy_plot_theme
  loc_offspeed <- loc_offspeed + fancy_plot_theme
  loc_whiffs <- loc_whiffs + fancy_plot_theme
  loc_chase <- loc_chase + fancy_plot_theme
  loc_hits <- loc_hits + fancy_plot_theme
  loc_outs <- loc_outs + fancy_plot_theme
  loc_2strike <- loc_2strike + fancy_plot_theme

  location_maps_panel <- arrangeGrob(
    arrangeGrob(
      loc_fastball + plot_margin_sm,
      loc_breaking + plot_margin_sm,
      loc_offspeed + plot_margin_sm,
      ncol = 3
    ),
    arrangeGrob(
      loc_whiffs + plot_margin_sm,
      loc_chase + plot_margin_sm,
      loc_hits + plot_margin_sm,
      ncol = 3
    ),
    arrangeGrob(
      loc_outs + plot_margin_sm,
      loc_2strike + plot_margin_sm,
      ncol = 2
    ),
    nrow = 3,
    heights = c(1, 1, 0.9)
  )

  movement_release_panel <- arrangeGrob(
    movement_chart,
    release_chart,
    ncol = 2,
    widths = c(1, 1)
  )

  report_layout <- arrangeGrob(
    grobs = list(
      title_grob,
      combined_tables_grob,
      movement_release_panel,
      location_maps_panel
    ),
    nrow = 4,
    heights = c(0.07, 1.55, 1.3, 2.25)
  )

  pdf(file, width = 11, height = 21, family = "Helvetica", onefile = TRUE)
  on.exit(dev.off(), add = TRUE)
  grid.draw(report_layout)

  invisible(file)
}

safe_pdf_name <- function(name) {
  gsub("[^A-Za-z0-9._ -]+", "", name)
}

stat_val <- function(st, col, default = 0L) {
  if (is.null(st) || !col %in% names(st) || nrow(st) == 0) {
    return(default)
  }
  v <- st[[col]][[1]]
  if (length(v) == 0 || is.na(v)) default else v
}

# Same column order as GAME STATISTICS in the PDF report
GAME_STAT_COLS <- c(
  "Pitches", "PA", "AB", "IP", "R", "H", "1B", "2B", "3B", "HR",
  "SO", "BB", "HBP", "BAA", "WHIP", "K/BB", "FPS%", "K%", "BB%", "2/3%"
)
GAME_STAT_RATE_COLS <- c("IP", "BAA", "WHIP", "K/BB", "FPS%", "K%", "BB%", "2/3%")

game_stat_kpi <- function(value, label) {
  div(
    class = "kpi",
    div(class = "kpi-value", value),
    div(class = "kpi-label", label)
  )
}

build_game_stats_ui <- function(st = NULL) {
  fmt_rate <- function(x, digits = 3) {
    if (length(x) == 0 || is.na(x) || !is.finite(x)) {
      return("—")
    }
    if (digits == 3) {
      format(round(x, 3), nsmall = 3, trim = TRUE)
    } else {
      format(round(x, digits), nsmall = digits, trim = TRUE)
    }
  }

  cells <- lapply(GAME_STAT_COLS, function(col) {
    if (is.null(st)) {
      val <- "—"
    } else if (col == "IP") {
      val <- fmt_rate(stat_val(st, col, NA_real_), 2)
    } else if (col %in% c("BAA")) {
      val <- fmt_rate(stat_val(st, col, NA_real_), 3)
    } else if (col %in% c("WHIP", "K/BB")) {
      val <- fmt_rate(stat_val(st, col, NA_real_), 2)
    } else if (col %in% GAME_STAT_RATE_COLS) {
      val <- fmt_rate(stat_val(st, col, NA_real_), 1)
    } else {
      val <- as.character(stat_val(st, col))
    }
    game_stat_kpi(val, col)
  })

  div(class = "game-line-wrap", div(class = "kpi-grid game-line-grid", cells))
}

# ---- Shiny UI ----
ui <- fluidPage(
  class = "pitcher-app-page",
  useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&family=DM+Sans:ital,opsz,wght@0,9..40,400;0,9..40,500;0,9..40,600;0,9..40,700;1,9..40,400&display=swap"
    ),
    tags$style(HTML("
      :root {
        --bg: #eef1f8;
        --surface: #ffffff;
        --surface-2: #f8faff;
        --border: #e2e8f3;
        --text: #1e293b;
        --muted: #64748b;
        --navy: #1e3a8a;
        --navy-dark: #172554;
        --accent: #dc2626;
        --accent-soft: #fee2e2;
        --gold: #f59e0b;
        --shadow: 0 8px 30px rgba(15, 23, 42, 0.08);
      }
      body.dark-mode {
        --bg: #0f1419;
        --surface: #1a2332;
        --surface-2: #141c28;
        --border: #2d3a52;
        --text: #e8edf7;
        --muted: #94a3b8;
        --navy: #3b5bdb;
        --navy-dark: #1e3a8a;
        --accent-soft: #3f1d1d;
        --shadow: 0 8px 30px rgba(0, 0, 0, 0.35);
      }
      body {
        background: var(--bg);
        color: var(--text);
        font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        margin: 0;
      }
      .app-wrap { max-width: 1480px; margin: 0 auto; padding: 0 18px 28px; }
      .hero {
        background: linear-gradient(135deg, var(--navy-dark) 0%, var(--navy) 55%, #2563eb 100%);
        border-radius: 18px;
        padding: 28px 32px 24px;
        margin: 18px 0 20px;
        box-shadow: var(--shadow);
        position: relative;
        overflow: hidden;
      }
      .hero::after {
        content: '';
        position: absolute;
        right: -40px;
        top: -60px;
        width: 220px;
        height: 220px;
        border-radius: 50%;
        background: rgba(255,255,255,0.06);
      }
      .hero h1 {
        font-family: 'Bebas Neue', sans-serif;
        font-size: 52px;
        letter-spacing: 0.04em;
        color: #fff;
        margin: 0 0 4px;
        line-height: 1;
      }
      .hero p {
        color: #dbeafe;
        margin: 0;
        font-size: 15px;
        font-weight: 500;
      }
      .hero-actions {
        margin-top: 16px;
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
      }
      #theme_toggle {
        background: rgba(255,255,255,0.12);
        border: 1px solid rgba(255,255,255,0.25);
        color: #fff;
        border-radius: 10px;
        font-weight: 600;
        padding: 8px 16px;
      }
      #theme_toggle:hover { background: rgba(255,255,255,0.2); }
      .layout { display: flex; gap: 18px; align-items: flex-start; }
      @media (max-width: 1100px) { .layout { flex-direction: column; } }
      .sidebar {
        flex: 0 0 340px;
        max-width: 100%;
      }
      .main-panel { flex: 1; min-width: 0; }
      .card {
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: 16px;
        box-shadow: var(--shadow);
        padding: 18px;
        margin-bottom: 16px;
      }
      .card-title {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.1em;
        text-transform: uppercase;
        color: var(--muted);
        margin: 0 0 14px;
      }
      .filter-block {
        background: var(--surface-2);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 14px;
        margin-bottom: 12px;
      }
      .control-label {
        font-size: 10px !important;
        font-weight: 700 !important;
        letter-spacing: 0.08em !important;
        text-transform: uppercase !important;
        color: var(--muted) !important;
        margin-bottom: 6px !important;
      }
      .selectize-input, .form-control, .shiny-input-container select {
        border-radius: 10px !important;
        border: 1px solid var(--border) !important;
        min-height: 42px !important;
        background: var(--surface) !important;
        color: var(--text) !important;
        box-shadow: 0 1px 2px rgba(15,23,42,0.04) !important;
      }
      .selectize-dropdown {
        border-radius: 10px !important;
        border-color: var(--border) !important;
      }
      .csv-status {
        font-size: 12px;
        color: var(--muted);
        margin-top: 8px;
        padding: 8px 10px;
        background: var(--surface-2);
        border-radius: 8px;
        border-left: 3px solid var(--navy);
      }
      .kpi-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 10px;
        margin-top: 4px;
      }
      .kpi {
        background: var(--surface-2);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 12px 10px;
        text-align: center;
      }
      .kpi-value {
        font-size: 22px;
        font-weight: 700;
        color: var(--navy);
        line-height: 1.1;
      }
      body.dark-mode .kpi-value { color: #93c5fd; }
      .kpi-label {
        font-size: 10px;
        font-weight: 600;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        color: var(--muted);
        margin-top: 4px;
      }
      .game-line-wrap { margin-top: 4px; }
      .game-line-grid {
        grid-template-columns: repeat(4, 1fr);
      }
      .game-line-grid .kpi-value { font-size: 18px; }
      #download_report {
        width: 100%;
        margin-top: 8px;
        padding: 14px 18px;
        font-size: 15px;
        font-weight: 700;
        letter-spacing: 0.02em;
        border-radius: 12px;
        border: none;
        background: linear-gradient(135deg, #dc2626 0%, #b91c1c 100%);
        color: #fff;
        box-shadow: 0 4px 14px rgba(220, 38, 38, 0.35);
      }
      #download_report:hover {
        background: linear-gradient(135deg, #ef4444 0%, #dc2626 100%);
        color: #fff;
      }
      .status-banner {
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: 12px;
        padding: 16px 20px;
        border-radius: 14px;
        background: linear-gradient(90deg, var(--surface-2) 0%, var(--surface) 100%);
        border: 1px solid var(--border);
        margin-bottom: 16px;
      }
      .status-banner.ready {
        border-color: #93c5fd;
        background: linear-gradient(90deg, #eff6ff 0%, #ffffff 100%);
      }
      body.dark-mode .status-banner.ready {
        background: linear-gradient(90deg, #1e293b 0%, #1a2332 100%);
        border-color: #3b82f6;
      }
      .status-title {
        font-size: 20px;
        font-weight: 700;
        margin: 0;
        color: var(--text);
      }
      .status-meta {
        font-size: 14px;
        color: var(--muted);
        margin: 4px 0 0;
      }
      .status-link a {
        display: inline-block;
        padding: 10px 18px;
        border-radius: 10px;
        background: var(--navy);
        color: #fff !important;
        font-weight: 600;
        text-decoration: none;
        font-size: 13px;
      }
      .status-link a:hover { background: var(--navy-dark); }
      .status-empty {
        color: var(--muted);
        font-size: 15px;
        padding: 20px 0;
        text-align: center;
      }
      .preview-card { padding: 12px; }
      .report-frame {
        width: 100%;
        height: 1180px;
        border: 1px solid var(--border);
        border-radius: 12px;
        background: #fff;
        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.5);
      }
      .shiny-file-input { margin-bottom: 0; }
      body.dark-mode .card,
      body.dark-mode .filter-block,
      body.dark-mode .kpi,
      body.dark-mode .csv-status {
        background: var(--surface) !important;
        border-color: var(--border) !important;
        color: var(--text) !important;
      }
      body.dark-mode .selectize-input,
      body.dark-mode .form-control {
        background: var(--surface-2) !important;
        color: var(--text) !important;
        border-color: var(--border) !important;
      }
      body.dark-mode .report-frame { background: #0f172a; }
    "))
  ),
  div(
    class = "app-wrap",
    div(
      class = "hero",
      h1("Pitching Report"),
      p("Build post-game pitching reports by date and pitcher — preview and download PDF"),
      div(
        class = "hero-actions",
        actionButton("theme_toggle", "Toggle dark mode", icon = NULL)
      )
    ),
    div(
      class = "layout",
      div(
        class = "sidebar",
        div(
          class = "card",
          div(class = "card-title", "Data source"),
          selectInput("local_csv", "CSV in app folder", choices = c("(no CSV found)" = "")),
          fileInput("csv_upload", "Or upload CSV", accept = c(".csv")),
          div(class = "csv-status", textOutput("csv_status"))
        ),
        div(
          class = "card",
          div(class = "card-title", "Report filters"),
          div(
            class = "filter-block",
            selectInput("pitcher_team", "Team", choices = "All", selected = "All"),
            selectInput("game_date", "Game date", choices = NULL, multiple = TRUE),
            selectInput("pitcher", "Pitcher", choices = NULL)
          ),
          div(class = "card-title", "Game statistics"),
          uiOutput("quick_stats_ui"),
          downloadButton("download_report", "Download PDF report")
        )
      ),
      div(
        class = "main-panel",
        div(
          class = "card preview-card",
          uiOutput("report_status"),
          uiOutput("report_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$theme_toggle, {
    shinyjs::runjs("document.body.classList.toggle('dark-mode');")
  })

  preview_dir <- file.path(tempdir(), paste0("pitcher_preview_", session$token))
  dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)
  shiny::addResourcePath("pitcher_preview", preview_dir)
  session$onSessionEnded(function() {
    unlink(preview_dir, recursive = TRUE)
  })

  preview_path <- reactiveVal(NULL)
  preview_version <- reactiveVal(0)
  report_meta <- reactiveVal(list())

  input_or <- function(x, default = NULL) {
    if (is.null(x) || length(x) == 0 || identical(x, "")) default else x
  }

  refresh_local_csv_choices <- function() {
    paths <- list_local_csvs()
    if (length(paths) == 0) {
      updateSelectInput(session, "local_csv", choices = c("(no CSV in app folder)" = ""), selected = "")
      return(invisible(character(0)))
    }
    labels <- basename(paths)
    names(paths) <- labels
    current <- isolate(input$local_csv)
    selected <- if (!is.null(current) && nzchar(current) && current %in% paths) current else paths[[1]]
    updateSelectInput(session, "local_csv", choices = paths, selected = selected)
    invisible(paths)
  }

  observe({
    refresh_local_csv_choices()
  })

  raw_df <- reactive({
    if (has_csv_upload(input$csv_upload)) {
      return(read.csv(input$csv_upload$datapath[1], stringsAsFactors = FALSE))
    }
    path <- input_or(input$local_csv, "")
    validate(
      need(nzchar(path) && file.exists(path), paste0("Add a .csv to ", app_dir(), " or upload one."))
    )
    read.csv(path, stringsAsFactors = FALSE)
  })

  output$csv_status <- renderText({
    if (has_csv_upload(input$csv_upload)) {
      paste("Using upload:", input$csv_upload$name[1])
    } else if (nzchar(input_or(input$local_csv, ""))) {
      paste("Using:", normalizePath(input$local_csv, winslash = "/"))
    } else {
      paste0("CSV folder: ", app_dir())
    }
  })

  prepared_df <- reactive({
    prepare_tracking_data(raw_df())
  })

  team_filtered_df <- reactive({
    df <- prepared_df()
    if (!identical(input_or(input$pitcher_team, "All"), "All")) {
      df <- df %>% filter(PitcherTeam == input$pitcher_team)
    }
    df
  })

  observeEvent(raw_df(), {
    df <- prepared_df()
    teams <- sort(unique(stats::na.omit(df$PitcherTeam)))
    team_choices <- c("All", teams)
    current <- isolate(input$pitcher_team)
    selected <- if (!is.null(current) && current %in% team_choices) current else {
      if ("UCO_HUS" %in% teams) "UCO_HUS" else "All"
    }
    updateSelectInput(session, "pitcher_team", choices = team_choices, selected = selected)
  }, ignoreInit = FALSE)

  observeEvent(input$game_date, {
    selected <- input$game_date
    if (is.null(selected) || length(selected) <= 1L) {
      return()
    }
    sel_chr <- as.character(selected)
    if (GAME_DATE_ALL %in% sel_chr && length(sel_chr) > 1L) {
      updateSelectInput(session, "game_date", selected = GAME_DATE_ALL)
    }
  }, ignoreInit = TRUE)

  observeEvent(team_filtered_df(), {
    df <- team_filtered_df()
    dates <- sort(unique(df$Date[!is.na(df$Date)]))
    if (length(dates) == 0) {
      updateSelectInput(session, "game_date", choices = character(0))
      updateSelectInput(session, "pitcher", choices = character(0))
      return()
    }
    selected_dates <- preserve_game_date_selection(isolate(input$game_date), dates)
    updateSelectInput(session, "game_date", choices = game_date_choices(dates), selected = selected_dates)
  }, ignoreInit = FALSE)

  observeEvent(list(input$game_date, team_filtered_df(), input$pitcher_team), {
    df <- team_filtered_df()
    req(input$game_date)
    dates <- sort(unique(df$Date[!is.na(df$Date)]))
    df <- filter_df_by_game_dates(df, input$game_date, dates)
    pitchers <- sort(unique(df$Pitcher))
    if (length(pitchers) == 0) {
      updateSelectInput(session, "pitcher", choices = character(0))
      return()
    }
    current <- isolate(input$pitcher)
    selected <- if (!is.null(current) && current %in% pitchers) current else pitchers[1]
    updateSelectInput(session, "pitcher", choices = pitchers, selected = selected)
  }, ignoreInit = FALSE)

  report_data <- reactive({
    req(input$pitcher, input$game_date)
    df <- team_filtered_df() %>%
      filter(Pitcher == input$pitcher)
    df <- filter_df_by_game_dates(df, input$game_date, team_filtered_df()$Date)
    validate(need(nrow(df) > 0, "No data for this pitcher on the selected date(s)."))
    df
  })

  build_current_report <- function(dest) {
    df <- report_data()
    opponent <- guess_opponent(df, unique(df$PitcherTeam)[1])
    build_pitcher_report_pdf(dest, df, input$pitcher, opponent = opponent)
    report_meta(list(
      pitcher = input$pitcher,
      date = input$game_date,
      opponent = opponent,
      formatted_name = format_pitcher_name(input$pitcher)
    ))
    preview_version(preview_version() + 1)
  }

  observeEvent(
    list(input$pitcher, input$game_date, input$pitcher_team, input$local_csv, input$csv_upload),
    {
      req(input$pitcher, input$game_date)
      out <- file.path(preview_dir, "preview.pdf")
      tryCatch(
        {
          build_current_report(out)
          preview_path(out)
        },
        error = function(e) {
          preview_path(NULL)
          showNotification(paste("Report error:", conditionMessage(e)), type = "error", duration = 8)
        }
      )
    },
    ignoreInit = FALSE
  )

  quick_stats <- reactive({
    tryCatch(
      {
        req(input$pitcher, input$game_date)
        list(stats = calculate_pitcher_game_statistics(report_data()))
      },
      error = function(e) NULL
    )
  })

  output$quick_stats_ui <- renderUI({
    qs <- quick_stats()
    if (is.null(qs)) {
      return(build_game_stats_ui(NULL))
    }
    build_game_stats_ui(qs$stats)
  })

  output$report_status <- renderUI({
    preview_version()
    path <- preview_path()
    meta <- report_meta()
    if (is.null(path) || !file.exists(path)) {
      return(div(class = "status-empty", "Choose a game date and pitcher — your report will appear here."))
    }
    date_text <- game_date_status_text(meta$date)
    div(
      class = "status-banner ready",
      div(
        class = "status-text",
        div(class = "status-title", meta$formatted_name),
        div(
          class = "status-meta",
          paste0(date_text, " · vs ", meta$opponent)
        )
      ),
      div(
        class = "status-link",
        tags$a(href = "pitcher_preview/preview.pdf", target = "_blank", "Open full PDF")
      )
    )
  })

  output$report_preview <- renderUI({
    preview_version()
    path <- preview_path()
    validate(need(!is.null(path) && file.exists(path), "Report not ready yet."))
    tags$iframe(
      class = "report-frame",
      src = paste0("pitcher_preview/preview.pdf?v=", preview_version()),
      type = "application/pdf"
    )
  })

  output$download_report <- downloadHandler(
    filename = function() {
      meta <- report_meta()
      if (length(meta) == 0) {
        return("Pitching Report.pdf")
      }
      paste0(safe_pdf_name(meta$formatted_name), " Pitching Report.pdf")
    },
    content = function(file) {
      build_current_report(file)
    }
  )
}

shinyApp(ui, server)
