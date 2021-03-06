# This function keeps only those columns whose id are given as vector.
# Also keeps columns whose ids are superstring of given id.
keep_cols <- function(data, keep=NULL){
  if (is.null(keep))
    return(data);
  
  to_keep <- rep(FALSE, ncol(data));
  for (k in keep)
    to_keep <- to_keep | grepl(k, colnames(data));
  
  ret <- data[,to_keep];
  ret <- ret[,order(colnames(ret))];
  return(ret);
}

# This function merges data frames from list.
merge_data <- function(data, keep=NULL, drop_cols=FALSE){
  ret <- NULL;
  for (i in 1:length(data)){
    if (is.null(keep) || names(data)[i] %in% keep){
      datum <- data[[i]];
      if (drop_cols)
        datum <- datum[,!is.na(datum[nrow(datum),])]
      
      index <- which(grepl("date", colnames(datum)));
      if (length(index) == 1){
        if (is.null(ret)){
          ret <- datum;
          byx <- colnames(datum)[index];
        } else {
          byy <- colnames(datum)[index];
          ret <- merge(x=ret, y=datum, by.x=byx, by.y=byy);
        }
      }
    }
  }
  
  return(ret);
}

# This function reads files from given directory.
read_dir <- function(dir){
  files <- list.files(dir, pattern="*.csv");
  
  ret <- list();
  for (file in files){
    datum <- read.csv(file=paste(dir, file, sep="/"));
    colnames(datum) <- paste(file, colnames(datum), sep=".");
    ret <- append(ret, list(datum));
  }
  names(ret) <- files;
  
  return(ret);
}

# This functions computes the open and close price.
make_open_close <- function(data, number_of_days=7, date="DATE", price="PRICE"){
  x <- data;
  
  # extrahiere ersten und letzten Tag der Woche
  if (number_of_days > 1){
    x <- cbind(x, "WEEK_DAY"=((1:nrow(x))%%number_of_days));
    open <- x[x$WEEK_DAY==1, price];
    close <- x[x$WEEK_DAY==0, price];
    open_date <- x[x$WEEK_DAY==1, date];
    close_date <- x[x$WEEK_DAY==0, date];
  } else {
    open <- x[,price];
    close <- x[,price];
    open_date <- x[,date];
    close_date <- x[,date];
  }
  
  # passe die Länge der Vektoren an sodass es für jeden Tag eine Entsprechung gibt, einer der Fälle ist glaube ich unnötig
  if (length(open) > length(close)) {open <- open[1:length(close)]; open_date <- open_date[1:length(close)];};
  if (length(open) < length(close)) {close <- close[1:length(open)]; close_date <- close_date[1:length(open)];};
  
  return(data.frame("OPEN_DATE"=open_date, "CLOSE_DATE"=close_date, "OPEN_PRICE"=open, "CLOSE_PRICE"=close));
}

# This function computes the return from matrix with corresponding columns.
make_return <- function(data){
  return((data$CLOSE_PRICE - data$OPEN_PRICE)/data$OPEN_PRICE);
}

# Some helpful function.
test_levels <- function(data, id=NULL){
  if (!is.null(id))
    print(id);
  bar <- table(data);
  print(bar[bar == 1]);
}

# This function takes the interval of a given factor, [min(F), max(F)],
# and cuts it into the given number of subintervals of same size. Each
# subinterval represents a level in the corresponding new factor.
make_breaks <- function(data, breaks=5, ignore=c()){
  ret <- data;
  for (i in 3:ncol(ret))
    if (!(colnames(ret)[i] %in% ignore))
      ret[,i] <- cut(ret[,i], breaks=breaks);
    
    return(ret);
}

# This function creates the model formula.
make_formula <- function(data, y=NULL, ignore=c()){
  if (is.null(y))
    stop("id for y-variable missing");
  
  ret <- paste(setdiff(colnames(data), c(y, ignore)), collapse=" + ");
  ret <- paste(y, ret, sep=" ~ ");
  
  return(ret);
}

# This function returns the closest already used level for the given column 
get_closest_level <- function(column_name, shifted_return, to_predict){
  # table containing the number a level was used
  lvl_usage_table <- table(shifted_return[,column_name]);
  
  # get levels for given column
  lvls <- levels(to_predict[[column_name]]);
  ids <- lvls;
  lvls <- gsub("\\(", "", lvls);
  lvls <- gsub("\\]", "", lvls);
  lvls <- strsplit(lvls, ",");
  
  lower <- sort(as.numeric(unlist(lapply(lvls, function(x){return(x[1])}))));
  names(lower) <- ids;
  
  # get missing level
  vals <- as.character(to_predict[[column_name]]);
  id <- vals;
  vals <- gsub("\\(", "", vals);
  vals <- gsub("\\]", "", vals);
  vals <- as.numeric(strsplit(vals, ",")[[1]][1]);
  
  names(vals) <- id;
  
  starti <- which(names(lower) %in% names(vals));
  
  # find first level above, which was used in fit
  upi <- Inf;
  for (i in starti:length(lower)){
    lvlid <- names(lower)[i];
    if (lvl_usage_table[lvlid] > 0){
      upi <- i;
      break;
    }
  }
  
  # find first level below, which was used in fit
  downi <- Inf;
  for (i in starti:1){
    lvlid <- names(lower)[i];
    if (lvl_usage_table[lvlid] > 0){
      downi <- i
      break;
    }
  }
  
  # if both are infinite, there is something going wrongly
  if (is.infinite(upi) && is.infinite(downi))
    stop("there is something going wrongly");
  
  # if the upper index is closer to the start index return corresponding level
  if ((upi - starti)^2 < (downi - starti)^2)
    return(names(lower)[upi]);
  
  # otherwise return the level which is below
  return(names(lower)[downi]);
}

# This function computes return prediction for the coming week.
make_prediction <- function(data, price_id=NULL, shift=1, breaks=5, ignore=c(), simple=FALSE){
  if (is.null(price_id))
    stop("price_id missing");
  
  breaked_return <- make_breaks(data, breaks=breaks, ignore=price_id);
  
  shifted_return <- breaked_return;
  shifted_return[1:(length(breaked_return[,price_id])-shift), price_id] <- breaked_return[(shift+1):length(breaked_return[,price_id]), price_id];
  shifted_return <- shifted_return[1:(length(breaked_return[,price_id])-shift),];
  
  formula <- make_formula(shifted_return, y=price_id, ignore=ignore);
  
  shifted_fit <- lm(formula, data=shifted_return);
  
  to_predict <- breaked_return[(nrow(breaked_return)-shift+1):nrow(breaked_return),];
  to_predict[,price_id] <- NA;
  
  # were levels for prediction used in fit?
  b <- rep(TRUE,3); for (i in 4:length(to_predict)){b <- c(b, to_predict[1,colnames(to_predict)[i]] %in% shifted_return[,colnames(to_predict)[i]])}
  if (sum(!b) > 0)
    cat(paste("New levels: ", paste(colnames(to_predict)[!b], collapse=", "), "\n", sep=""));
  # save to_predict row for further analysis if something seems wrong
  original_to_predict <- to_predict;
  
  if (simple){
    # replace non-existing factors with last one in shifted_R
    # actually it should be better to use factor from fit, which is closest
    to_predict[1,!b] <- shifted_return[nrow(shifted_return),!b];
  } else {
    cnames <- colnames(to_predict)[!b];
    
    for (n in cnames){
      replacement <- get_closest_level(n, shifted_return, to_predict);
      cat(paste("Replacing level ", as.character(to_predict[[n]]), " of factor ", n, " with ", replacement, "\n", sep=""));
      to_predict[1,n] <- replacement;
    }
  }
  
  # predict
  prediction <- predict(shifted_fit, to_predict);
  
  ret <- list(prediction, to_predict, breaked_return, shifted_return, shifted_fit, original_to_predict, b);
  names(ret) <- c("PREDICTION", "TO_PREDICT", "BREAKED_RETURN", "SHIFTED_RETURN", "SHIFTED_FIT", "ORIGINAL_TO_PREDICT", "USED_LEVELS");
  return(ret);
}

# This function computes the effect sizes of each factor.
make_effect_sizes <- function(shifted_fit, with_noise=FALSE){
  effsizes <- anova(shifted_fit);
  
  if (!with_noise)
    effsizes <- effsizes[rownames(effsizes)!="Residuals",];
    
  ret <- effsizes[["Sum Sq"]]/sum(effsizes[["Sum Sq"]]);
  names(ret) <- rownames(effsizes);
  
  return(ret);
}

get_google <- function(google_file="multiTimeline.csv"){
  library(anytime);
  
  google <- as.matrix(read.table(file=google_file, sep=",", quote=""));
  google <- cbind("DATE"=rownames(google), google);
  google <- google[2:nrow(google),];
  
  colnames(google) <- c("DATE", "FREQUENCY");
  rownames(google) <- NULL;
  google <- as.data.frame(google);
  
  # firstly, fill in missing days
  ret <- NULL;
  for (i in 1:(nrow(google)-1)){
    ret <- rbind(ret, google[i,]);
    diff <- as.numeric(as.Date(google$DATE[i+1]) - as.Date(google$DATE[i]));
    if (diff > 1){
      add <- matrix(NA, diff-1, ncol(google));
      add[,2] <- as.character(google$FREQUENCY[i]);
      colnames(add) <- colnames(google);
      for (j in 1:nrow(add)){
        add[j,"DATE"] <- as.character(as.Date(google$DATE[i])+j);
      }
      ret <- rbind(ret, add);
    }
  }
  ret <- rbind(ret, google[nrow(google),]);
  
  wdays <- weekdays(as.Date(ret$DATE, "%Y-%m-%d"));
  ret <- cbind(ret, "WEEK_DAY"=wdays);
  
  # get saturdays and fridays
  open <- head(ret[ret$WEEK_DAY=="Saturday",], -1);
  close <- tail(ret[ret$WEEK_DAY=="Friday",], -1);
  
  g <- data.frame("OPEN_DATE"=open$DATE, "CLOSE_DATE"=close$DATE, "OPEN_PRICE"=open$FREQUENCY, "CLOSE_PRICE"=close$FREQUENCY);
  g$OPEN_PRICE <- as.numeric(as.character(g$OPEN_PRICE));
  g$CLOSE_PRICE <- as.numeric(as.character(g$CLOSE_PRICE));
  
  g <- cbind(g, "google_search"=make_return(g));
  
  return(g);
}

# This functions computes return prediction for the coming week and plots it. 
doIt <- function(breaks=15,
                 dir="coinmetrics",
                 price_id="btc.csv.price.USD.",
                 date="btc.csv.date",
                 assets=c("btc", "ltc", "eth", "xrp", "doge", "usdt", "gold", "sp500"),
                 type=c("price", "value", ".txVolume.", "marketcap"),
                 google_file=NULL,
                 old=c(),
                 back=0,
                 simple=FALSE,
                 effsizes=0){
  
  # read coinmetrics data from directory
  x <- read_dir(dir);
  
  # only analyze files, which correspond to given assets
  keep = paste(assets, ".csv", sep="");
  x <- merge_data(x, keep=keep, drop_cols=TRUE);
  
  # only analyze given factors, also date column is needed
  x <- keep_cols(x, c(date, type));
  colnames(x)[colnames(x)==date] <- "DATE";
  
  # simply switches columns:
  index <- which(colnames(x)==price_id);
  indices <- 1:ncol(x);
  indices[2] <- index;
  indices[index] <- 2;
  x <- x[,indices];
  
  # remove rows, which contain NAs
  x <- x[apply(x, MARGIN=1, function(ret){!any(is.na(ret))}),];
  
  # keep Saturdays (Fridays) so that only weeks are considered
  x <- x[(nrow(x) %% 7 + 1):nrow(x),];
  
  # add open and close prices to martix
  return <- make_open_close(x, price=price_id)[,c("OPEN_DATE", "CLOSE_DATE")];
  for (i in 2:ncol(x))
    return <- cbind(return, make_return(make_open_close(x, price=colnames(x)[i])));
  colnames(return)[3:length(colnames(return))] <- colnames(x)[2:length(colnames(x))];

  if (!is.null(google_file)){  
    g <- get_google(google_file=google_file);
    g <- g[,c("OPEN_DATE", "google_search")];
    return <- merge(return, g, by="OPEN_DATE");
  }
  
  # only use submatrix if old predictions are needed
  return <- return[1:(nrow(return)-back),];
  
  # make prediction
  return_prediction <- make_prediction(return, price_id=price_id, shift=1, breaks=breaks, ignore=c("OPEN_DATE", "CLOSE_DATE"), simple=simple);
  prediction <- return_prediction$PREDICTION;
  breaked_return <- return_prediction$BREAKED_RETURN;
  shifted_return <- return_prediction$SHIFTED_RETURN;
  shifted_fit <- return_prediction$SHIFTED_FIT;
  
  # evaluations
  X <- model.matrix(shifted_fit);
  beta <- coefficients(shifted_fit);
  
  X <- X[,!is.na(beta)];
  beta <- beta[!is.na(beta)];
  
  evaluation_historically <- X %*% beta;
  correct_predictions <- (evaluation_historically < 0) == (shifted_return[,price_id] < 0);
  
  print(sum(correct_predictions)/nrow(shifted_return));
  
  #n <- 10^8; print(sum((ceiling(runif(n)*nrow(shifted_R)) <= sum(shifted_R[,price_id] < 0)) == (ceiling(runif(n)*nrow(shifted_R)) <= sum(shifted_R[,price_id] < 0)))/n);
  
  if (effsizes == 0){
    dev.new(width=19, height=3.5);
  } else {
    dev.new(width=19, height=7); 
    par(mfrow=c(2,1));
  }
  
  matplot(breaked_return[,price_id], type="l", col=c("blue"), lty = c(1), xaxt="n");
  abline(h=0, col="black");
  lines((1:length(evaluation_historically))+1, evaluation_historically, col="violet");
  points(length(breaked_return[,price_id])+1, prediction, col="violet", pch=16);
  
  if (length(old) > 0){
    old_predictions <- data.frame("x"=(nrow(breaked_return)-length(old)+1):nrow(breaked_return), "y"=old);
    points(old_predictions$x, old_predictions$y, col="violet", pch=1);
  }
  
  int <- floor(nrow(breaked_return)/7) - 1; # warum auch immer minus 1?
  axis(1, at = int*(0:7)+1, labels = breaked_return$CLOSE_DATE[int*(0:7)+1]);
  
  legend("left", legend = c("historic", "learned"), col = c("blue", "violet"), lty = c(1,1), lwd = 1 , xpd = T );
  title(paste("Weekly BTC return prediction via factorial regression (breaks = ", breaks, "):\nPredicted return for ", as.Date(breaked_return$CLOSE_DATE[nrow(breaked_return)])+7, ": ", prediction, sep=""));
  
  min_val <- min(c(breaked_return[,price_id], evaluation_historically));
  colors <- c("red", "green");
  correct_predictions <- (evaluation_historically < 0) == (shifted_return[,price_id] < 0);
  correct_predictions <- as.integer(correct_predictions)+1;
  for (i in 1:length(correct_predictions)){rect(i+1-0.5, min_val, i+2-0.5, min_val-0.1, col=colors[correct_predictions[i]], border=NA)}
  
  factors <- tail(colnames(breaked_return),-3);
  f1 <- paste(factors[1:14], collapse=", ");
  f2 <- paste(factors[15:19], collapse=", ");
  s <- paste(f1, f2, sep=",\n")
  
  title(sub=s, adj=0, line=3, font=2, cex.sub=0.9);
  
  es <- NULL;
  if (effsizes == 1){
    es <- make_effect_sizes(shifted_fit=shifted_fit, with_noise=TRUE);
    pie(es, main="Effect sizes with noise");
  }
  
  if (effsizes == 2){
    es <- make_effect_sizes(shifted_fit=shifted_fit, with_noise=FALSE);
    pie(es, main="Effect sizes without noise");
  }
  
  if (!is.null(es)){
    return_prediction <- append(return_prediction, list(es));
    names(return_prediction)[length(return_prediction)] <- "EFFECT_SIZES";
  }
    
  return(return_prediction);
}

#ret <- doIt(old=c(0.06927308, -0.108199108918301, 0.15401893716921), simple=TRUE);
#ret <- doIt(old=c(0.06927308, -0.108199108918301, 0.15401893716921), simple=TRUE, effsizes=1);
#ret <- doIt(old=c(0.06927308, -0.108199108918301, 0.15401893716921), simple=TRUE, effsizes=2);
ret <- doIt(google_file="multiTimeline.csv", old=c(0.06927308, -0.108199108918301, 0.15401893716921, 0.244917868419962), simple=TRUE, effsizes=1);

es_table <- t(t(ret$EFFECT_SIZES));
es_table[order(es_table[,1], decreasing=TRUE),,drop=FALSE];

## debugging
#a <- doIt(back=1);
#b <- doIt(back=0);
#
## are model matrices the same?
#aX <- model.matrix(a$SHIFTED_FIT);
#bX <- model.matrix(b$SHIFTED_FIT);
#sum(as.matrix(aX[1:nrow(aX),] != bX[1:nrow(aX),]));
#
## change in coefficients from last week to current one
#sum((coefficients(a$SHIFTED_FIT) - coefficients(b$SHIFTED_FIT))^2, na.rm = TRUE);
#
## some other test
#abeta <- coefficients(a$SHIFTED_FIT);
#aX_ <- aX[,!is.na(abeta)];
#abeta_ <- abeta[!is.na(abeta)];
#aret <- aX_%*%abeta_;
#
#bbeta <- coefficients(b$SHIFTED_FIT);
#bX_ <- bX[,!is.na(bbeta)];
#bbeta_ <- bbeta[!is.na(bbeta)];
#bret <- bX_%*%bbeta_;
#
## some other test
#sum(a$TO_PREDICT != b$SHIFTED_RETURN[nrow(b$SHIFTED_RETURN),], na.rm = TRUE);
#
## some other test
#solution_without_knowledge <- predict(a$SHIFTED_FIT, a$TO_PREDICT);
#X <- rbind(aX, bX[nrow(bX),]);
#X <- X[,!is.na(abeta)];
#beta <- abeta[!is.na(abeta)];
#solution_with_knowledge <- tail(X %*% beta, 1);
## seems to work correctly
#
## some other test
#last_row <- bX[nrow(bX),];
#last_row <- last_row[!is.na(abeta)];
#abeta_ <- abeta[!is.na(abeta)];
#abeta_ <- abeta_[last_row != 0];
#bbeta_ <- bbeta[!is.na(bbeta)];
#bbeta_ <- bbeta_[last_row != 0];
#last_row <- last_row[last_row != 0];
#last_row %*% abeta_; # same as sum(abeta_)
#last_row %*% bbeta_; # same as sum(bbeta_)
## difference between prediction and current value is
## caused by different beta values from different fits
