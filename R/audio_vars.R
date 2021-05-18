library(tidyverse)
# library(tuneR)

if (file.exists("data/210225_audio_vars.csv")) {
  audio <- read_csv("data/210225_audio_vars.csv")
  
} else {

# import start and end times for each survey so can map to audio files
source("R/audio_duration.R")
  
  # 18080915_1 = ADR
  # 18073114_1 = CRT
  # 
  # time <- time %>% mutate(group = str_remove(uid, "_[g|d|e][1-2]"),
  #                 member  = str_extract(uid, "[g|d|e][1-2]"))
  # 
  # c <- time %>% filter(group == "18080915_1")
  # create list of files to read
  files <- list.files("data/audio/csv")
  files <- files[1:5]
  # remove problematic uids
  files <- files[!files %in% c("17110112_1_d2.csv", "18081613_2_e1.csv", "18081613_2_e2.csv")]
  
  # # remove files for Ps not in final sample
  # d.grp <- read_csv("data/200525_dyad_data.csv")
  # 
  # remove <- data.frame(uid = str_remove(files, ".csv")) %>% 
  #   mutate(uid = ifelse(uid == "18073109_1_g2", "18073109_1_e1",
  #                ifelse(uid == "18073109_1_d2", "18073109_1_e2", 
  #                ifelse(uid == "17040610_1_d2", "18040610_1_d2",
  #                ifelse(uid == "17040610_1_g2", "18040610_1_g2", 
  #                ifelse(uid == "18050509_1_e1", "18060509_1_e1", 
  #                ifelse(uid == "18050509_1_e2", "18060509_1_e2",
  #                ifelse(uid == "18050712_1_e1", "18060712_1_e1", 
  #                ifelse(uid == "18050712_1_e2", "18060712_1_e2", 
  #                ifelse(uid == "18073110_2_d1", "18073110_2_e2",
  #                ifelse(uid == "18073110_2_g1", "18073110_2_e1", 
  #                ifelse(uid == "18073114_1_d2", "18073114_1_e2",
  #                ifelse(uid == "18073114_1_g2", "18073114_1_e1", uid))))))))))))) %>% 
  #   mutate(group = str_remove(uid, "_[d|g|e][1-2]"),
  #          member = str_extract(uid, "[d|g|e][1-2]"),
  #          uid = str_replace(uid, "-", "_"),
  #          group = str_replace(group, "-", "_"),
  #          uid = str_remove(uid, "_p1"),
  #          group = str_remove(group, "_p1"),
  #          group = str_remove(group, "_e[1-2]"),
  #          group = str_remove(group, ".csv"))
  # 
  # remove <- remove %>% filter(group %in% d.grp$group) %>% 
  #   group_by(group) %>% 
  #   mutate(n = n()) %>% 
  #   filter(n == 2)
  # 
  # files <- files[files %in% paste0(remove$uid, ".csv")]

# list of measures
measures <- c("rapm", "mdmt", "gk",  "adr", "crt")

# read audio files
d <- map(files, function(i) {
  if (file.exists(paste0("data/audio/csv/", i))) {
    d <- read_csv(paste0("data/audio/csv/", i)) %>% 
      select(-X1) %>% 
      mutate(uid = str_remove(i, ".csv"))
  } else {
    # create list of audio files to read
    # files <- list.files("data/audio")[-c(210,211,212,213,220)]
    # map(files, function(i) {
    # read text file
    d <- readLines(paste0("data/audio/", str_replace(i, ".csv", ".vol")))
    
    # convert character string to data frame
    x <- as.data.frame(strsplit(d, split="\t"))
    
    # rename column
    colnames(x) <- c("hz")
    
    c <- data.frame(chunk = rep(1:(nrow(x)/50), 50)) %>% 
      arrange(chunk)
    
    x <- x %>% 
      mutate(
        hz = as.character(hz),
        hz = as.numeric(hz),
        time_ms = 1:n(),
        time_sec = time_ms/100) %>% 
      bind_cols(c)
    
    # write dataframe as csv
    x %>% write.csv(paste0("data/audio/csv/", str_remove(i, ".vol"), ".csv"))
  }

  # remove NA rows
  # d <- map(d, filter, !is.na(hz))
  d <- d %>% filter(!is.na(hz))
  
  # convert values above threshold (.01) to 1 and everything below to 0
  # d <- map(d, mutate, hz = ifelse(hz < .01, 0, 1))
  d <- d %>% mutate(hz = ifelse(hz < .01, 0, 1))
  
  # d$hz[d$hz < .01] <- 0
  # d$hz[d$hz >= .01] <- 1
  
  # convert hz signal to 0 or 1
  # x <- map(1:length(d), function(i) {
  #   d[[i]] %>%
  #   group_by(chunk) %>%
  #   summarise(uid = uid,
  #             hz = mean(hz),
  #             time = time_sec[1],
  #             time_unit = .50) %>%
  #   mutate(hz = ifelse(lead(hz) < .3 & lag(hz) < .3, 0, hz),
  #          hz = ifelse(hz >= .1, 1, hz),
  #          hz = ifelse(hz < .1, 0, hz)) %>%
  #   ungroup() %>%
  #   mutate(hz = ifelse(hz == 0 & lead(hz == 1, n = 1) & lag(hz == 1, n = 1), 1, hz))
  # })

x <- d %>%
  group_by(chunk) %>%
  summarise(hz= mean(hz),
            time = time_sec[1],
            time_unit = .50) %>%
  mutate(hz = ifelse(lead(hz) < .3 & lag(hz) < .3, 0, hz),
         hz = ifelse(hz >= .1, 1, hz),
         hz = ifelse(hz < .1, 0, hz)) %>%
  ungroup() %>%
  mutate(hz = ifelse(hz == 0 & lead(hz == 1, n = 1) & lag(hz == 1, n = 1), 1, hz))

# need to adjust start and end times so they are the number of seconds since the start of the first test
# the start point differs for a and b participants
  # map(1:length(files), function(i) {
  map(measures, function(t) {
    times <- time %>% 
      ungroup() %>% 
      filter(uid == str_remove(i, ".csv")) %>% 
      select(version, contains(t)) %>% 
      select(version, contains("duration"), contains("secs"))
    names(times) <- c("version", "duration", "start")
    
    if (all(times$version == "a", t == "rapm")) {
      c <- x %>%
        filter(time > 40 & time <= times$duration+80)
      
      # c %>%
      #   ggplot(aes(x = time, y = hz)) +
      #   geom_line()
    } else if (all(times$version == "b", t == "mdmt")) {
        c <- x %>%
          filter(time > 40 & time <= times$duration+80)
        
        # c %>%
        #   ggplot(aes(x = time, y = hz)) +
        #   geom_line()
      } else {
      c <- x %>% 
        filter(time > times$start-30 & time <= times$start+times$duration+30)
      
      # c %>%
      #   ggplot(aes(x = time, y = hz)) +
      #   geom_line()
    }

    print(i)
    
    tmp <- rle(c$hz)
    tmp <- tmp$lengths[tmp$values==1]
    tmp <- tmp[!is.na(tmp)]
    
   data.frame(
      uid = str_remove(i, ".csv"),
      test = t,
      n_speak_turn = length(tmp),
      speak_total_dur = sum(tmp*.5),
      speak_mean_dur = mean(tmp*.5),
      speak_sd_dur = sd(tmp*.5)) %>% 
      gather(var, val, n_speak_turn:speak_sd_dur) %>% 
      unite(var, test, var) %>% 
      spread(var, val)
  })
})

# bind columns for each participant into single df
d <- map(1:length(d), function(i) {
  bind_cols(d[[i]]) %>% 
    rename(uid = `uid...1`) %>% 
    select(-matches("uid..."))
})

# bind the data for all participants into a single df
d <- bind_rows(d)

# save audio variables as csv
d %>% write_csv("data/210225_audio_vars.csv")

}

# play --------------------------------------------------------------------
# can use the vadeR package to detect voice in the audio files
# https://rdrr.io/github/akinori-ito/vadeR
# http://practicalcryptography.com/miscellaneous/machine-learning/voice-activity-detection-vad-tutorial/
#' 
#' library(tuneR)
#' library(vadeR)
#' 
#' d <- readWave("data/audio/test.wav")
#' 
#' v <- voiceActivity(d)
#' 
#' v <- data.frame(voice = v) %>% 
#'   mutate(ms = 1:n(),
#'          sec = ms/100)
#' 
#' v %>% 
#'   ggplot(aes(x = sec, y = as.numeric(voice))) +
#'   geom_line()
#' 
#' 
#' 
#' 
#' d <- readWave("/Users/blattymanchard/Desktop/S2_audio_test/raw/17100609_d2.wav")
#' 
#' v <- voiceActivity(d)
#' 
#' chunk <- d@left[1:length(d@left)/.5]
#' 
#' chunk <- data.frame(voice = chunk) %>% 
#'   mutate(ms = 1:n(),
#'          sec = ms/100)
#' 
#' 
#' chunk %>% 
#'   filter(voice >= 0) %>% 
#'   mutate(voice = ifelse(voice >= 1000, 1, 0)) %>% 
#'   ggplot(aes(x = ms, y = as.numeric(voice))) +
#'   geom_line()
#' 
#' 
#' v <- voiceActivity(chunk)
#' 
#' 
#' 
#' 
#' 1:length(d@left)/.5
#' (length(d@left)/.5+1):1:length(d@left)
#' 
#' # old ---------------------------------------------------------------------
#' # install.packages("remotes")
#' # remotes::install_github("akinori-ito/vadeR")
#' 
#' #' Steps:
#' #' identify speech and no speech
#' 
#' 
#' library(vadeR)
#' library(tuneR)
#' library(tidyverse)
#' library(here)
#' 
#' # laptop
#' # d <- readWave("/Users/blattymanchard/Desktop/S2_audio_test/17100609_d2.wav")
#' 
#' # office
#' d <- readWave(here("data/audio/test.wav"))
#' 
#' 
#' act <- voiceActivity(d)
#' 
#' voiceSegment(act)
#' 
#' x <- data.frame(act) %>% 
#'   mutate(cum_ms = 1:length(act),
#'          time = (1:length(act)/6000)) # audio recorded every 10ms?
#' 
#' # convert cum_ms variable to minutes and seconds
#' x <- x %>% mutate(time = format(as.POSIXct(Sys.Date()) + cum_ms/100, "%M:%S")) %>% 
#'   filter(cum_ms < 6000)
#' 
#' x %>% 
#'   group_by(time) %>% 
#'   summarise(act = act[1]) %>% 
#'   ggplot(aes(x = time, y = act)) +
#'   geom_line(group = 1)
#' 
#' 
#' 
#' 
#' function (x, simple = TRUE, minlen = 50, maxlen = 1000, nclust = 4, 
#'           frameshift = 0.01) 
#' {
#'   if (class(x) == "Wave") {
#'     xf <- melfcc(x, hoptime = frameshift, dither = T)
#'   }
#'   else {
#'     xf <- x
#'   }
#'   cls <- kmeans(xf, nclust)
#'   pow <- c(0, 0)
#'   for (cl in 1:nclust) {
#'     pow[cl] <- mean(xf[cls$cluster == cl, 1])
#'   }
#'   i.silent <- which.min(pow)
#'   if (simple) {
#'     r <- vadeR_simple(cls, i.silent, minlen)
#'   }
#'   else {
#'     r <- vadeR_heavy(cls, i.silent, minlen, maxlen)
#'   }
#'   attr(r, "frameshift") <- frameshift
#'   r
#' }
#' 
#' 
#' 
#' 
#' 
#' # Used the following webpage as a guide
#' # http://samcarcagno.altervista.org/blog/basic-sound-processing-r/?doing_wp_cron=1559032103.7004609107971191406250
#' 
#' library(tuneR)
#' 
#' d <- readWave("/Users/blattymanchard/Desktop/S2_audio_test/17100609_d2.wav")
#' 
#' str(d)
#' 
#' # WAV files have two audio channels we will select just one to work with
#' d_left <- d@left
#' 
#' # convert values to range between -1 and +1
#' d_left <- d_left / 2^(d@bit -1)
#' 
#' # create an array containing the time points
#' timeArray <- (0:(length(d_left)-1)) / d@samp.rate
#' timeArray <- timeArray * 1000 #scale to milliseconds
#' 
#' merge(d_left, )
#' 
#' 
#' # # don't need amplitude for analysis
#' # # plot the amplitude
#' # plot(timeArray, d_left, type='l', col='black', xlab='Time (ms)', ylab='Amplitude') 
#' 
#' # Frequency of audio
#' # https://web.archive.org/web/20120615002031/http://www.mathworks.com/support/tech-notes/1700/1702.html
#' # use fft function 
#' n <- length(d_left)
#' p <- fft(d_left)
#' 
#' # the fft is computed on the number of points of the signal n. 
#' # Since we’re not using a power of two the computation will be a bit slower
#' nUniquePts <- ceiling((n+1)/2)
#' p <- p[1:nUniquePts] #select just the first half since the second half 
#' # is a mirror image of the first
#' p <- abs(p)  #take the absolute value, or the magnitude 
#' 
#' 

