
#FIGURES EN PLUS

# FIGURE: Cytokines

```{r Figure6,include=F}
fig_IL6  <- plot_cytokine(df, "IL6")
fig_IL5  <- plot_cytokine(df, "IL5")
fig_IL8  <- plot_cytokine(df, "IL8/CXCL8")
fig_IL10 <- plot_cytokine(df, "IL10")
fig_GDF  <- plot_cytokine(df, "GDF-15")
```

```{r Figure6-display, fig.width=8, fig.height=6, fig.scale=2, dpi=300, fig.align='center'}

fig_IL6
fig_IL5
fig_IL8
fig_IL10
fig_GDF

ggsave("FIGURES/Figure6_IL6.pdf",      fig_IL6,  width=160, height=120, units="mm", device=cairo_pdf, bg="white", scale=1.5)
ggsave("FIGURES/Figure6_IL5.pdf",      fig_IL5,  width=160, height=120, units="mm", device=cairo_pdf, bg="white")
ggsave("FIGURES/Figure6_IL8.pdf",      fig_IL8,  width=160, height=120, units="mm", device=cairo_pdf, bg="white")
ggsave("FIGURES/Figure6_IL10.pdf",     fig_IL10, width=160, height=120, units="mm", device=cairo_pdf, bg="white")
ggsave("FIGURES/Figure6_GDF15.pdf",    fig_GDF,  width=160, height=120, units="mm", device=cairo_pdf, bg="white")
```




# FIGURE: Association IL6 D0 and Other study co-inclusion

```{r Figure15, include= F}

MW <- wilcox.test(log_value_IL6_J0~autre_riph_interv, df)
p <- MW$p.value
Figure15 <- ggplot(df,aes(y=log_value_IL6_J0, x=as.factor(autre_riph_interv),fill=as.factor(autre_riph_interv)))+
  geom_boxplot()+
  annotate("text",
           x = 1,
           y = 8,
           label = paste0("p: ", sprintf("%.2f",p)),
           parse = FALSE,
           size = 4,
           hjust = 0)
```

```{r Figure15-display, fig.width=8, fig.height=6, fig.scale=1.5, dpi=300, fig.align='center'}


Figure15

ggsave("FIGURES/Figure15.pdf",
       plot   = Figure15,
       width  = 160, height = 120, units = "mm",
       device = cairo_pdf, bg = "white")

```


# FIGURE: Association with T4 cell and outcomes

```{r Figure12, include=F}

prep_A1 <- prep_marker_data(df, prefix = "T4", transform = "none")

test_A1 <- test_marker(prep_A1$data)

Figure12 <- plot_marker(
  dat      = prep_A1$data,
  counts   = prep_A1$counts,
  x_labels = prep_A1$x_labels,
  panel_title = "A",
  y_var    = "value_log",
  y_lab    = "Percentage of T4 lymphocytes"
) +
  annotate("text", x = 1, y = max(prep_A1$data$value_log, na.rm = TRUE) +1, label = test_A1$label, hjust = 0)+
  coord_cartesian(ylim=c(0,20))
```

```{r Figure12-display, fig.width=8, fig.height=6, fig.scale=1.5, dpi=300, fig.align='center'}

Figure12

ggsave("FIGURES/Figure12.pdf",
       plot   = Figure12,
       width  = 160, height = 120, units = "mm",
       device = cairo_pdf, bg = "white")
```

# FIGURE : IL6 and duration of Noradrenaline

```{r Figure10, include=FALSE}

Figure_10_plot <- make_il6_cor_plot(df,
                                    "log_value_IL6_J0",
                                    "duree_NAd",
                                    "Norepinephrine duration (days)",
                                    "")
```

```{r Figure10-display, fig.width=8, fig.height=6, fig.scale=1.5, dpi=300, fig.align='center'}

Figure_10_plot

ggsave("FIGURES/Figure10.pdf",
       plot   = Figure_10_plot,
       width  = 160, height = 120, units = "mm",
       device = cairo_pdf, bg = "white")
```
