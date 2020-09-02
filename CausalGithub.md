Causal Interence In Statistics - A Primer, By Judea Pearl et al
================

# Walkthrough by Matthew Monterosso

![](C:\\Users\\Matth\\Desktop\\causal.jpg)

Here I’ll use this rmd to walk through the book, practice some rmd
formatting / latex, and learn causality\! At the outset I’ll state for
the record I really don’t know much beyond a vague sense of confounding
and lurking variables. It’s actually kind of funny that my (lack of)
understanding is just about exactly as described in the preface for
typical statistical teachings.

It’s also good to know that Causality can be considered
*extra*-statistical and that the entire field has really only been
explored in any large capacity in the past 20 years or so. So I guess
it’s fun to be on the bleeding edge\! When an R package seems suitable
for the exercises or a quick test, I will do my best to implement that
as well.

-----

## Study Questions 1.2.1

**What is wrong with the following claims?**

  - 1)  *“Data show that income and marriage have a high positive
        correlation. Therefore, your earnings will increase if you get
        married.”*

Correlation is not causation. This simply means the two numbers happen
to co-vary in step with each other, not that one “leads” to the other.
Simple explanations for this could be that people only choose to get
married if they can afford to, or people are more likely to be married
the older they get (Survival Analysis?), and older people are
purportedly further along in their careers and thus higher earners.
Perhaps even the drive to earn increases as one acquires a family and
needs to provide. Sounds awful\!

  - 2)  *“Data Shows that as the number of fire increases, so does the
        number of firefighters.Therefore, to cut down on fires, you
        should reduce the number of firefighters.”*

This is “causally flipped”, if that makes sense? More firefighters have
to respond to a larger or more fires to fight it appropriately. Reducing
firefighters would quite obviously just increase the risk of
encountering fire(s) with insufficient firefighters to respond to it.

  - 3)  *“Data show that people who hurry tend to be late to their
        meetings. Don’t hurry, or you’ll be late”*

They are hurrying because they are already late\! This is obviously
nonsensical, but I lack the proper vocabulary to explain why.Hurrying
does not cause lateness, but is a symptom of it. Would it be correct to
say those who are late are causally often hurrying? I honestly don’t
know at this juncture.

## Study Questions 1.2.2

**A baseball batter Tim has a better batting average than his teammate
Frank. However, someone notices that Frank has a better batting average
than Tim against both right-handed and left-handed pitchers. How can
this happen?**

So yeah, this is definitely Simpson’s Paradox at work. I have to say
even after reading the chapter and staring at the contingency table in
the pharma example. I’m still having trouble wrapping my head around how
this is possible. I suppose that’s what makes it a paradox\! Let’s see
if we can’t synthesize a table for this.It’s tempting to use the same
data as the books, but I’m going to try to create my own.

\[ Batting Average = \frac{hits}{atbats} \]

``` r
library(gt)
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ----------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
Hand <- as.data.frame(c("Left-Handed", "Right-Handed", "Any"))
colnames(Hand) <- "Pitcher Hand"
Tim <- c("65/250","435/750", "500/1000")
Frank <- c("250/750","220/250", "470/1000")

df <- cbind(Hand,Tim, Frank)

gt_tbl2 <- 
  gt(data = df) %>%
  tab_header(
    title = "Simpson's Paradox in Baseball",
    subtitle = "A Contrived Example!"
  ) %>%
  tab_spanner(
    label = "Batter",
    columns = vars(Tim, Frank)
  )

gt_tbl2
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qnigmpceah .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qnigmpceah .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qnigmpceah .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qnigmpceah .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qnigmpceah .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qnigmpceah .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qnigmpceah .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qnigmpceah .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qnigmpceah .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qnigmpceah .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qnigmpceah .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qnigmpceah .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qnigmpceah .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qnigmpceah .gt_from_md > :first-child {
  margin-top: 0;
}

#qnigmpceah .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qnigmpceah .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qnigmpceah .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qnigmpceah .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qnigmpceah .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qnigmpceah .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qnigmpceah .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qnigmpceah .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qnigmpceah .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qnigmpceah .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qnigmpceah .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qnigmpceah .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qnigmpceah .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qnigmpceah .gt_left {
  text-align: left;
}

#qnigmpceah .gt_center {
  text-align: center;
}

#qnigmpceah .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qnigmpceah .gt_font_normal {
  font-weight: normal;
}

#qnigmpceah .gt_font_bold {
  font-weight: bold;
}

#qnigmpceah .gt_font_italic {
  font-style: italic;
}

#qnigmpceah .gt_super {
  font-size: 65%;
}

#qnigmpceah .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="qnigmpceah" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_header">

<tr>

<th colspan="3" class="gt_heading gt_title gt_font_normal" style>

Simpson’s Paradox in Baseball

</th>

</tr>

<tr>

<th colspan="3" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>

A Contrived Example\!

</th>

</tr>

</thead>

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Pitcher Hand

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Batter</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Tim

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Frank

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Left-Handed

</td>

<td class="gt_row gt_left">

65/250

</td>

<td class="gt_row gt_left">

250/750

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Right-Handed

</td>

<td class="gt_row gt_left">

435/750

</td>

<td class="gt_row gt_left">

220/250

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Any

</td>

<td class="gt_row gt_left">

500/1000

</td>

<td class="gt_row gt_left">

470/1000

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

It really seems like these can only happen as a result of heavily
imbalanced classes? Again I took a page from the book, but it seems to
be taking advantage of the total bats differing largely “diagonally”. As
you can see above, Tim batted vs only 250 left-handers but 750
right-handers. Much less realistically, Frank did the opposite of this.
