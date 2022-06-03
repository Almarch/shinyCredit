# shinyCredit

This is a (currently French) app designed to simulate loans, with an emphasize on the computation of interests.

## Dependencies

The app is designed to work on Linux and Windows environments. The following dependencies should be installed via R:

```
> install.packages(c("shiny","shinyjs","DT"))
```

## Launch

ShinyChess is a shiny app and can be launched from R using the ```source()``` command, from RStudio using the "Run App" button, or as a command line:
```
$ R -e "shiny::runApp('~/.../shinyCredit')"
```

## Use

The rate and number of mensualities can be edited using the left panel. From this, the credit mensualities and the total of interests are computed.

The insurance mensualities number and amount can be edited using the right panel. From this, the total of insurances is computed.

The interests, insurance, and repayment rows of the table can't be edited nor deleted. The other rows can be edited and deleted. New rows can be added, especially to cover other expenses generated by the loan contraction.

Two plots are provided:
- the first plot shows the composition of each mensuality along the loan: total amount, HA (hors-assurance), and the interests alone.
- the second plot shows the capital that remains to repay and that generates interests each month.
