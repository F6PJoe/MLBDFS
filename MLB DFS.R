# Clear console and environment
cat("\014")
rm(list = ls())

# Required packages
packages <- c(
  "XML", "RCurl", "stringr", "rjson", "plyr", "dplyr", "httr",
  "jsonlite", "magrittr", "googlesheets4", "googledrive",
  "lubridate", "base64enc"
)
invisible(lapply(packages, library, character.only = TRUE))

# Authenticate Google Sheets
json_key <- rawToChar(base64decode(Sys.getenv("GCP_SHEETS_KEY_B64")))
temp_json_file <- tempfile(fileext = ".json")
writeLines(json_key, temp_json_file)
gs4_auth(path = temp_json_file)
#gs4_auth(cache = ".secrets", email = "joebond008@gmail.com")

# Google Sheets URL
gs_url <- "https://docs.google.com/spreadsheets/d/1dWsEg3HLa9KY1YES31P1Mam0vLFK9zrR91rOsDSKsA8"

# Helper function to fetch and process slate data
# Returns NULL if no slates are available or processing fails
get_processed_slate <- function(api_url, label) {
  
  # Fetch API response
  response <- tryCatch(
    GET(api_url, add_headers(Authorization = "FantasySixPack", `Content-Type` = "application/json")),
    error = function(e) { message(label, " API request failed: ", e$message); return(NULL) }
  )
  if (is.null(response)) return(NULL)
  
  # Parse response
  data <- tryCatch(
    content(response, "parsed", simplifyVector = TRUE),
    error = function(e) { message(label, " failed to parse response: ", e$message); return(NULL) }
  )
  if (is.null(data)) return(NULL)
  
  # Check if slates exist
  slates <- data$slates
  if (is.null(slates) || length(slates) == 0 ||
      (is.data.frame(slates) && nrow(slates) == 0) ||
      length(names(slates)) == 0) {
    message(label, " — no slates available yet. Skipping.")
    return(NULL)
  }
  
  # Find best slate: prefer MAIN, then ALL DAY/ALL, then largest by player count
  text_cols <- names(slates)[sapply(slates, is.character)]
  slate_index <- NA
  
  # 1. Look for MAIN slate
  for (col in text_cols) {
    idx <- which(grepl("MAIN", slates[[col]], ignore.case = TRUE))[1]
    if (!is.na(idx)) { slate_index <- idx; break }
  }
  
  # 2. Fall back to ALL DAY / ALL slate
  if (is.na(slate_index)) {
    for (col in text_cols) {
      idx <- which(grepl("ALL DAY|ALL", slates[[col]], ignore.case = TRUE))[1]
      if (!is.na(idx)) { slate_index <- idx; break }
    }
  }
  
  # 3. Fall back to largest slate by player count
  if (is.na(slate_index)) {
    player_counts <- sapply(seq_along(data$slates$info), function(i) {
      info <- data$slates$info[[i]]
      if (is.data.frame(info)) nrow(info) else 0
    })
    slate_index <- which.max(player_counts)
    message(label, " — no MAIN/ALL DAY slate found. Using slate index ", slate_index,
            " with ", player_counts[slate_index], " players.")
  }
  
  # Extract player data
  df <- tryCatch(
    data$slates$info[[slate_index]],
    error = function(e) { message(label, " — failed to extract slate info: ", e$message); return(NULL) }
  )
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    message(label, " — slate exists but contains no player data. Skipping.")
    return(NULL)
  }
  
  # Rename columns
  df <- dplyr::rename(df,
                      Opp    = opponent,
                      Player = name,
                      ID     = site_id,
                      Pos    = position,
                      Team   = team,
                      Proj   = projection,
                      Salary = salary,
                      Beta   = beta_proj,
                      Value  = value
  )
  
  df$Proj   <- round(as.numeric(df$Proj), 2)
  df$Salary <- as.numeric(df$Salary)
  df$Value  <- round(as.numeric(df$Value), 1)
  df <- df[!is.na(df$Proj) & df$Proj > 0, ]
  
  if (nrow(df) == 0) {
    message(label, " — slate found but no players have valid projections yet. Skipping.")
    return(NULL)
  }
  
  # Handle multi-position players
  df$OptPos <- df$Pos
  dualPos   <- grepl("/", df$Pos)
  df$Pos2   <- ""
  df$Pos2[dualPos] <- sub("/", "", str_extract(df$Pos[dualPos], "/[A-Z0-9]{1,2}$"))
  df$Pos[dualPos]  <- sub("/", "", str_extract(df$Pos[dualPos], "^[A-Z0-9]{1,2}/"))
  df$Pos1 <- df$Pos
  df$Pos  <- df$OptPos
  df <- arrange(df, desc(Proj))
  
  message(label, " — slate loaded with ", nrow(df), " players.")
  return(df)
}

# Helper to clear a sheet below the header and write a placeholder message
write_placeholder <- function(sheet_name, site_label) {
  range_clear(ss = gs_url, sheet = sheet_name, range = "A2:Z1000")
  range_write(
    ss        = gs_url,
    data      = data.frame(Message = paste0(site_label, " Projections for today's games will be coming soon")),
    sheet     = sheet_name,
    range     = "A2",
    col_names = FALSE
  )
  message(site_label, " — placeholder message written to ", sheet_name, ".")
}

# --- FanDuel ---
fd <- get_processed_slate("https://bluecollardfs.com/api/mlb_fanduel", "FanDuel")

if (!is.null(fd)) {
  sheet_write(fd[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "FD MLB DFS", ss = gs_url)
  message("FanDuel data written to Google Sheets.")
} else {
  write_placeholder("FD MLB DFS", "FanDuel")
}

# --- DraftKings ---
dk <- get_processed_slate("https://bluecollardfs.com/api/mlb_draftkings", "DraftKings")

if (!is.null(dk)) {
  sheet_write(dk[, c("Player", "Proj", "Salary", "Value", "Pos", "Team", "Opp")], sheet = "DK MLB DFS", ss = gs_url)
  message("DraftKings data written to Google Sheets.")
} else {
  write_placeholder("DK MLB DFS", "DraftKings")
}

# --- Timestamp — always runs ---
update_time    <- with_tz(Sys.time(), "America/New_York")
formatted_date <- format(update_time, "%B %d, %Y")
formatted_time <- format(update_time, "%I:%M %p ET")
range_write(ss = gs_url, data = data.frame(Date = formatted_date), sheet = "MLB Update Time", range = "A2", col_names = FALSE)
range_write(ss = gs_url, data = data.frame(Time = formatted_time), sheet = "MLB Update Time", range = "B2", col_names = FALSE)
message("Timestamp updated: ", formatted_date, " ", formatted_time)

# Exit cleanly for GitHub Actions
if (!interactive()) quit(status = 0)
