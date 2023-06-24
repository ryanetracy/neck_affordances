########################################################
# perceptions of fighting ability, mate preference
# variations in trapezii and scm musculature
########################################################

library(devtools)

repo_url <- 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'
devtools::source_url(repo_url)

pckgs <- c(
  'haven',
  'emmeans',
  'rstatix',
  'afex',
  'ggcorrplot',
  'ggpubr',
  'effectsize',
  'parameters',
  'interactions',
  'tidyverse'
)

package_loader(pckgs)

plot_colors <- c('#003366', '#006666', '#ffd600')

# load data
df <- read_sav('Neck Affordances.sav')
colnames(df)

# convention
# S = small trap, L = large trap (first S/L)
# S = small scm, L = large scm (second S/L)
# STM = short-term mate, L = long-term mate
# N = nurture, P = protection


# get demographics
df %>%
  get_summary_stats(Age, type = 'mean_sd')

df %>%
  count(Sex) %>%
  mutate(prop = round((n / sum(n)) * 100, 2))

df %>%
  count(Race) %>%
  mutate(prop = round((n / sum(n)) * 100, 2))


df_main <- df %>%
  select(
    id:LLLTM
  ) %>%
  pivot_longer(cols = SSFight:LLLTM,
               names_to = 'motive',
               values_to = 'response') %>%
  separate(col = 'motive', into = c('trap_size', 'motive'), sep = 1) %>%
  separate(col = 'motive', into = c('scm_size', 'motive'), sep = 1) %>%
  mutate(
    trap_size = if_else(trap_size == 'S', 'small_trap', 'large_trap'),
    scm_size = if_else(scm_size == 'S', 'small_scm', 'large_scm')
  )


df_fight <- df_main %>%
  filter(motive == 'Fight') %>%
  pivot_wider(names_from = 'motive',
              values_from = 'response')


df_mate <- df_main %>%
  filter(motive %in% c('STM', 'LTM'))

df_parent <- df_main %>%
  filter(motive %in% c('N', 'P')) %>%
  mutate(motive = if_else(motive == 'N', 'nurture', 'protect'))



### explore fight perceptions
df_fight %>%
  ggplot(aes(Fight, fill = trap_size)) +
  geom_histogram(color = 'black',
                 alpha = .7,
                 binwidth = 1) +
  facet_wrap(~ scm_size) +
  scale_fill_manual(values = c(plot_colors[1],
                               plot_colors[2])) +
  theme_bw()


df_fight %>%
  ggplot(aes(trap_size, Fight, fill = scm_size)) +
  geom_boxplot(color = 'black',
               outlier.shape = 4,
               outlier.color = 'red',
               outlier.size = 2) +
  theme_bw()

df_fight %>%
  group_by(trap_size, scm_size) %>%
  identify_outliers('Fight')


## model
(mod_fight <- aov_4(Fight ~ scm_size * trap_size
                    + (scm_size * trap_size | id),
                    anova_table = list(es = 'pes'),
                    data = df_fight))

## descriptives
# trap size
df_fight %>%
  group_by(trap_size) %>%
  get_summary_stats(Fight, type = 'full')

# scm size
df_fight %>%
  group_by(scm_size) %>%
  get_summary_stats(Fight, type = 'full')


## plot
fight_sum_g <- df_fight %>%
  group_by(trap_size, scm_size) %>%
  get_summary_stats(Fight, type = 'full')
fight_sum_g

df_fight %>%
  ggplot(aes(trap_size, Fight, fill = scm_size)) +
  geom_violin(color = plot_colors[3],
              alpha = .8,
              position = position_dodge(.9),
              bw = .4) +
  geom_point(color = plot_colors[3],
             alpha = .2,
             size = .75,
             shape = 4,
             position = position_jitterdodge(.15, .15, .9)) +
  geom_point(data = fight_sum_g,
             aes(trap_size, mean),
             color = plot_colors[3],
             alpha = .75,
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = fight_sum_g,
                aes(trap_size, mean, ymin = mean - ci, ymax = mean + ci),
                width = .25,
                alpha = .75,
                color = plot_colors[3],
                position = position_dodge(.9)) +
  theme_bw() +
  scale_fill_manual(values = c(plot_colors[1],
                               plot_colors[2]),
                    labels = c('Large SCM',
                               'Small SCM')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_x_discrete(labels = c('Large Trapezii',
                              'Small Trapezii')) +
  labs(x = '',
       y = 'Perception of Fighting Ability',
       fill = '') +
  theme(legend.position = 'bottom')

# ggsave('fight perceptoins.jpg',
#        device = 'jpeg',
#        units = 'cm')



### explore mate perceptions
df_mate %>%
  ggplot(aes(response, fill = trap_size)) +
  geom_histogram(color = 'black',
                 alpha = .7,
                 binwidth = 1) +
  facet_wrap(~ scm_size + motive) +
  scale_fill_manual(values = c(plot_colors[1],
                               plot_colors[2])) +
  theme_bw()


df_mate %>%
  ggplot(aes(trap_size, response, fill = scm_size)) +
  geom_boxplot(color = 'black',
               outlier.shape = 4,
               outlier.color = 'red',
               outlier.size = 2) +
  facet_wrap(~ motive) +
  theme_bw()


## model
(mod_mate <- aov_4(response ~ trap_size * scm_size * motive
                   + (trap_size * scm_size * motive | id),
                   anova_table = list(es = 'pes'),
                   data = df_mate))

# explore interactions
# trap:scm
trap_scm_decomp <- emmeans(mod_mate, ~ trap_size * scm_size,
                           ref.group = 'trap_size:scm_size')

trap_scm_decomp

trap_scm_pwc <- summary(pairs(trap_scm_decomp))
trap_scm_d <- t_to_d(t = trap_scm_pwc$t.ratio,
                     df_error = trap_scm_pwc$df,
                     paired = T)
print(
  cbind(
    trap_scm_pwc,
    trap_scm_d
  )
)


# trap:motive
trap_motive_decomp <- emmeans(mod_mate, ~ trap_size * motive,
                              ref.group = 'trap_size:motive')
trap_motive_decomp

trap_motive_pwc <- summary(pairs(trap_motive_decomp))
trap_motive_d <- t_to_d(t = trap_motive_pwc$t.ratio,
                        df_error = trap_motive_pwc$df,
                        paired = T)
print(
  cbind(
    trap_motive_pwc,
    trap_motive_d
  )
)


## descriptives
# trap x scm
df_mate %>%
  group_by(trap_size, scm_size) %>%
  get_summary_stats(response, type = 'full')

# trap x motive
df_mate %>%
  group_by(trap_size, motive) %>%
  get_summary_stats(response, type = 'full')


## plot
mate_sum_g <- df_mate %>%
  group_by(trap_size, scm_size, motive) %>%
  get_summary_stats(response, type = 'full')
mate_sum_g

df_mate %>%
  ggplot(aes(trap_size, response, fill = scm_size)) +
  geom_violin(color = plot_colors[3],
              alpha = .8,
              position = position_dodge(.9),
              bw = .35) +
  geom_point(color = plot_colors[3],
             alpha = .2,
             size = .75,
             shape = 4,
             position = position_jitterdodge(.15, .15, .9)) +
  geom_point(data = mate_sum_g,
             aes(trap_size, mean),
             color = plot_colors[3],
             alpha = .75,
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = mate_sum_g,
                aes(trap_size, mean, ymin = mean - ci, ymax = mean + ci),
                width = .25,
                alpha = .75,
                color = plot_colors[3],
                position = position_dodge(.9)) +
  theme_bw() +
  facet_wrap(~ motive) +
  scale_fill_manual(values = c(plot_colors[1],
                               plot_colors[2]),
                    labels = c('Large SCM',
                               'Small SCM')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_x_discrete(labels = c('Large Trapezii',
                              'Small Trapezii')) +
  labs(x = '',
       y = 'Perception of Mate Preference',
       fill = '') +
  theme(legend.position = 'bottom')

# ggsave('mate preference.jpg',
#        device = 'jpeg',
#        units = 'cm')



### explore parenting ability
df_parent %>%
  ggplot(aes(response, fill = trap_size)) +
  geom_histogram(color = 'black',
                 alpha = .7,
                 binwidth = 1) +
  facet_wrap(~ scm_size + motive) +
  scale_fill_manual(values = c(plot_colors[1],
                               plot_colors[2])) +
  theme_bw()


df_parent %>%
  ggplot(aes(trap_size, response, fill = scm_size)) +
  geom_boxplot(color = 'black',
               outlier.shape = 4,
               outlier.color = 'red',
               outlier.size = 2) +
  facet_wrap(~ motive) +
  theme_bw()

df_parent %>%
  group_by(trap_size, scm_size, motive) %>%
  identify_outliers('response') %>%
  filter(is.extreme == T)


## model
(mod_parent <- aov_4(response ~ trap_size * scm_size * motive
                     + (trap_size * scm_size * motive | id),
                     anova_table = list(es = 'pes'),
                     data = df_parent))

# explore interaction
trap_motive_decomp2 <- emmeans(mod_parent, ~ trap_size * motive,
                               ref.group = 'trap_size:motive')
trap_motive_decomp2

trap_motive_pwc2 <- summary(pairs(trap_motive_decomp2))
trap_motive_d2 <- t_to_d(t = trap_motive_pwc2$t.ratio,
                         df_error = trap_motive_pwc2$df,
                         paired = T)
print(
  cbind(
    trap_motive_pwc2,
    trap_motive_d2
  )
)


## plot
parent_sum_g <- df_parent %>%
  group_by(trap_size, scm_size, motive) %>%
  get_summary_stats(response, type = 'full')
parent_sum_g

# change labels for facet wrap
parent_labs <- c('Nurture', 'Protect')
names(parent_labs) <- c('nurture', 'protect')

df_parent %>%
  ggplot(aes(trap_size, response, fill = scm_size)) +
  geom_violin(color = plot_colors[3],
              alpha = .8,
              position = position_dodge(.9),
              bw = .35) +
  geom_point(color = plot_colors[3],
             alpha = .2,
             size = .75,
             shape = 4,
             position = position_jitterdodge(.15, .15, .9)) +
  geom_point(data = parent_sum_g,
             aes(trap_size, mean),
             color = plot_colors[3],
             alpha = .75,
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = parent_sum_g,
                aes(trap_size, mean, ymin = mean - ci, ymax = mean + ci),
                width = .25,
                alpha = .75,
                color = plot_colors[3],
                position = position_dodge(.9)) +
  theme_bw() +
  facet_wrap(~ motive, labeller = labeller(motive = parent_labs)) +
  scale_fill_manual(values = c(plot_colors[1],
                               plot_colors[2]),
                    labels = c('Large SCM',
                               'Small SCM')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_x_discrete(labels = c('Large Trapezii',
                              'Small Trapezii')) +
  labs(x = '',
       y = 'Perception of Parent Preference',
       fill = '') +
  theme(legend.position = 'bottom')

# ggsave('parent preference.jpg',
#        device = 'jpeg',
#        units = 'cm')
