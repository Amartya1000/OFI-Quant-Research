data=read.csv("C:/Users/student/Downloads/first_25000_rows.csv", stringsAsFactors = FALSE)
compute_OFI_features <- function(data, target_symbol = "AAPL") {
  # Load required libraries
  library(dplyr)
  library(tidyr)
  library(glmnet)
  
  
  
  # Convert timestamp to POSIXct and truncate to seconds
  data$ts_event <- gsub("Z$", "", data$ts_event)
  data$datetime <- as.POSIXct(data$ts_event, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  data$second <- as.POSIXct(format(data$datetime, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  
  # Arrange by symbol and time
  data <- data %>% arrange(symbol, second, datetime)
  
  # Aggregate by second: first and last snapshot for each symbol
  book <- data %>%
    group_by(symbol, second) %>%
    summarise(
      # Best level (0) first and last
      bid_px_00_first = first(bid_px_00), bid_px_00_last = last(bid_px_00),
      bid_sz_00_first = first(bid_sz_00), bid_sz_00_last = last(bid_sz_00),
      ask_px_00_first = first(ask_px_00), ask_px_00_last = last(ask_px_00),
      ask_sz_00_first = first(ask_sz_00), ask_sz_00_last = last(ask_sz_00),
      # Levels 1–9 first and last
      across(matches("bid_px_0[1-9]"), list(first = ~first(.), last = ~last(.))),
      across(matches("bid_sz_0[1-9]"), list(first = ~first(.), last = ~last(.))),
      across(matches("ask_px_0[1-9]"), list(first = ~first(.), last = ~last(.))),
      across(matches("ask_sz_0[1-9]"), list(first = ~first(.), last = ~last(.))),
      .groups = "drop"
    )
  
  # Compute Best-Level OFI for level 0
  book <- book %>%
    mutate(
      bid_contrib = case_when(
        bid_px_00_last == bid_px_00_first ~ (bid_sz_00_last - bid_sz_00_first),
        bid_px_00_last >  bid_px_00_first ~  bid_sz_00_last,
        bid_px_00_last <  bid_px_00_first ~ -bid_sz_00_first
      ),
      ask_contrib = case_when(
        ask_px_00_last == ask_px_00_first ~ (ask_sz_00_first - ask_sz_00_last),
        ask_px_00_last <  ask_px_00_first ~  ask_sz_00_last,
        ask_px_00_last >  ask_px_00_first ~ -ask_sz_00_first
      ),
      best_level_ofi = bid_contrib + ask_contrib
    )
  
  # Compute Multi-Level OFI (sum of levels 0–9)
  levels <- 0:9
  ofi_matrix <- matrix(0, nrow = nrow(book), ncol = length(levels))
  for (m in levels) {
    px_b_first <- book[[paste0("bid_px_", sprintf("%02d", m), "_first")]]
    px_b_last  <- book[[paste0("bid_px_", sprintf("%02d", m), "_last")]]
    sz_b_first <- book[[paste0("bid_sz_", sprintf("%02d", m), "_first")]]
    sz_b_last  <- book[[paste0("bid_sz_", sprintf("%02d", m), "_last")]]
    px_a_first <- book[[paste0("ask_px_", sprintf("%02d", m), "_first")]]
    px_a_last  <- book[[paste0("ask_px_", sprintf("%02d", m), "_last")]]
    sz_a_first <- book[[paste0("ask_sz_", sprintf("%02d", m), "_first")]]
    sz_a_last  <- book[[paste0("ask_sz_", sprintf("%02d", m), "_last")]]
    
    bid_contrib_m <- ifelse(px_b_last == px_b_first, sz_b_last - sz_b_first,
                            ifelse(px_b_last > px_b_first, sz_b_last, -sz_b_first))
    ask_contrib_m <- ifelse(px_a_last == px_a_first, sz_a_first - sz_a_last,
                            ifelse(px_a_last < px_a_first, sz_a_last, -sz_a_first))
    ofi_matrix[, m+1] <- bid_contrib_m + ask_contrib_m
  }
  book$multi_level_ofi <- rowSums(ofi_matrix)
  
  # Integrated OFI via PCA (first principal component of OFI matrix)
  pca_res <- prcomp(ofi_matrix, center = TRUE, scale. = TRUE)
  book$integrated_ofi <- pca_res$x[, 1]
  
  # Cross-Asset OFI: LASSO regression with one-lag of other symbols' OFIs
  ofi_wide <- book %>%
    select(symbol, second, best_level_ofi) %>%
    pivot_wider(names_from = symbol, values_from = best_level_ofi) %>%
    arrange(second)
  symbols <- setdiff(colnames(ofi_wide), "second")
  if (!(target_symbol %in% symbols)) {
    stop("Target symbol not found in data.")
  }
  others <- setdiff(symbols, target_symbol)
  lasso_coefs <- NULL
  if (length(others) > 0) {
    # Create lag-1 data frame for other symbols
    ofi_lag <- ofi_wide %>%
      mutate(across(all_of(others), lag)) %>%
      drop_na()
    X <- as.matrix(ofi_lag[, others, drop = FALSE])
    y <- ofi_lag[[target_symbol]]
    cv_fit <- cv.glmnet(X, y, alpha = 1)
    fit    <- glmnet(X, y, alpha = 1, lambda = cv_fit$lambda.min)
    coef_mat <- as.matrix(coef(fit))
    lasso_coefs <- data.frame(
      Feature     = rownames(coef_mat),
      Coefficient = coef_mat[,1],
      row.names   = NULL
    )
  } else {
    message("No other symbols for cross-asset OFI; skipping LASSO.")
  }
  
  # Print results
  message("OFI values (per second) for target symbol ", target_symbol, ":")
  print(book %>% filter(symbol == target_symbol) %>%
          select(second, best_level_ofi, multi_level_ofi, integrated_ofi))
  if (!is.null(lasso_coefs)) {
    message("LASSO regression coefficients (target = ", target_symbol, "):")
    print(lasso_coefs)
  }
  
  # Return list of results
  return(list(ofi_data = book, lasso_coefficients = lasso_coefs))
}
result <- compute_OFI_features(data = data, target_symbol = "AAPL")
