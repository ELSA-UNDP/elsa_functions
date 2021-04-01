
require(ggplot2)
require(glue)

elsa_region_plot_pct = function(.data,
                            scenario,
                            plot_rows = 2){
        ggplot(.data, aes(x=action, fill=action)) +
        geom_bar(aes(y=pct),
                 stat = 'identity',
                 show.legend = FALSE) +
        scale_fill_manual(values= elsa_pallete)+
        labs(x = glue('ELSA Action - {scenario}'),
             y = '% of Region') +
        facet_wrap(~name,
                   nrow = plot_rows)
}