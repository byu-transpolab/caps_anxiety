# Download the csv files that have one column for params and one for the corresponding
# error

optimerror <- read_csv("optim_error.csv") %>%
  separate(col = "params error", 
           into = c("parameters", "error"), sep = " ") %>%
  mutate(iterations = c(1:(nrow(sannboxError)/4))) %>%
  select(c(iterations, parameters, error))

sannboxError <- read_csv("sannbox_error.csv") %>%
  separate(col = "params error", 
         into = c("parameters", "error"), sep = " ") %>%
  mutate(iterations = c(1:(nrow(sannboxError)/4))) %>%
  select(c(iterations, parameters, error))

# Plot the iterations vs params and error 

ggplot(data = sannboxError, aes(x = iterations, y = error)) +
  geom_line(aes(col = parameters)) +
  facet_wrap(~parameters, scales = 'free') +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Sannbox Parameters")

ggplot(data = optimTable, aes(x = iterations, y = error)) +
  geom_line(aes(col = parameters)) +
  facet_wrap(~parameters, scales = 'free') +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Optim Parameters")

ggplot(data = sannboxTable %>%
         filter(parameter == "eps"),
       aes(x = iterations, y = as.numeric(error))) +
  geom_path() +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Sannbox Error")

sggplot(data = optimTable, aes(x = iterations, y = error)) +
  geom_line(aes(col = 'red')) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Optim Error")
