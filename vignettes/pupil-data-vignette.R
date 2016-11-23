## ---- message = FALSE----------------------------------------------------
library(itrak); library(tidyverse)

## ------------------------------------------------------------------------
pupil_data <- rename(pupil_data, id = RECORDING_SESSION_LABEL)

## ------------------------------------------------------------------------
samp_freq <- get_freq(pupil_data$TIMESTAMP)
samp_freq

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>%
  mutate(pupil = merge_eyes(LEFT_PUPIL_SIZE, RIGHT_PUPIL_SIZE)) %>%
  select(-LEFT_PUPIL_SIZE, -RIGHT_PUPIL_SIZE)

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>%
  mutate(trial = label_trial(SAMPLE_INDEX, SAMPLE_MESSAGE, "IAPSOnset")) %>%
  filter(!is.na(trial)) %>%
  select(-SAMPLE_INDEX)

## ------------------------------------------------------------------------
n_trials <- max(pupil_data$trial)
n_trials

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>%
  group_by(trial) %>%
  mutate(time = zero_onset(TIMESTAMP, SAMPLE_MESSAGE, "IAPSOnset")) %>%
  ungroup() %>%
  select(-SAMPLE_MESSAGE, -TIMESTAMP)

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>% 
  clip_trials(trial, time, -1000, 6000)
# The following is only necessary if you are missing one or more samples, but
# it can be safely run regardless. (It will leave the data unchanged if there
# is nothing to fix.)
pupil_data <- pupil_data %>% 
  complete(nesting(id, trial), time) %>%
  arrange(id, trial, time)

## ------------------------------------------------------------------------
pupil_data$time <- pupil_data$time / 1000

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>%
  group_by(trial) %>%
  mutate(artifact = get_artifacts(ts = pupil, samp_freq = 500)) %>%
  ungroup()

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>%
  group_by(trial) %>%
  mutate(oor = get_oor(ts = pupil, samp_freq = 500,
                       lim = c(-0.75, 1.25), 
                       baseline = between(time, -.1, 0),
                       artifacts = artifact)) %>%
  ungroup()

## ---- fig.height=8, fig.width=8, warning = FALSE-------------------------
pupil_data %>% 
  plot_artifacts(time, pupil, artifact, oor, trial) +
  xlab("time (s)") +
  ylab("pupil area")

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>%
  group_by(trial) %>%
  mutate(pupil_corrected = fix_artifacts(ts = pupil,
                                         samp_freq = 500,
                                         lim = c(-0.75, 1.25),
                                         baseline = between(time, -.1, 0),
                                         artifacts = artifact)) %>%
  ungroup()

## ---- fig.height=8, fig.width=8, warning = FALSE-------------------------
plot_comparison(pupil_data, time, pupil, pupil_corrected, trial)

## ---- warning=FALSE, fig.height=4, fig.width=8---------------------------
bad_trials <- pupil_data %>% filter(is.na(pupil_corrected)) %>%
  select(trial) %>% distinct %>% unlist
pupil_data %>% filter(trial %in% bad_trials) %>%
  plot_artifacts(time, pupil, artifact, oor, trial)

## ------------------------------------------------------------------------
pupil_data <- filter(pupil_data, time >= -0.1) %>%
  group_by(trial) %>% 
  mutate(pupil_normed = normalize(ts = pupil_corrected,
                                  baseline = time < 0)) %>%
  filter(time >= 0)

## ------------------------------------------------------------------------
pupil_data <- pupil_data %>%
  group_by(trial) %>% 
  mutate(pupil_smoothed = low_pass_filter(ts = pupil_normed,
                                          samp_freq = 500))

## ---- warning = FALSE----------------------------------------------------
pupil_data %>% filter(trial == 1) %>%
  plot_comparison(time, pupil_normed, pupil_smoothed, trial)

