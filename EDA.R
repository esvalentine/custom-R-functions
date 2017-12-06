# Useful functions for exploratory data analysis (EDA)
plot_missing <- function(data, title = NULL){
    temp <- as.data.frame(ifelse(is.na(data), 0, 1))
    temp <- temp[,order(colSums(temp))]
    plot_data <- expand.grid(list(x = 1:nrow(temp), 
                                  y = colnames(temp)))
    plot_data$m <- as.vector(as.matrix(temp))
    plot_data <- data.frame(x = unlist(plot_data$x), 
                            y = unlist(plot_data$y), 
                            m = unlist(plot_data$m))
    ggplot(plot_data,
           aes(x=x, y=y, fill=factor(m))) + 
        geom_tile() + 
        scale_fill_manual(values=c("white", "black"), 
                          name="Missing\n(0=Yes, 1=No)") + 
        theme_light() + 
        ylab("") + 
        xlab("") + 
        ggtitle(title)
}

missing_corr_plot <- function(data, outcome, title = NULL) {
    temp <- data.frame(x = rowSums(sapply(data, is.na)),
                       y = data[ , outcome])
    plot_data <- temp %>%
        group_by(x) %>%
        summarise(y = mean(y),
                  n = n())
    ggplot(plot_data, aes(x = x, y = y, size = n)) +
        geom_point() +
        geom_smooth(show.legend = F,
                    se = F) +
        scale_size_continuous(name = "Total\nObservations") +
        ggtitle(title) +
        theme_bw() 
}

color_plot <- function(data, x, y, color) {
    temp <- data %>%
        select_(x, y, color) %>%
        na.omit() %>%
        ggplot(aes_string(x = x, y = y, color = color)) +
        geom_smooth() +
        theme_bw()
}