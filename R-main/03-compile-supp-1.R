
# this just compiles all the individual bird maps into a LaTeX document

plotpath <- "../outputs/birdmaps"
# first arrange everyone into regions and within regions descending by latitude
# and then create a print string for them:
tmp <- kbirds %>% arrange(Region, desc(lat)) %>%
  mutate(prstring = sprintf("{\\bf %s}, %s, near %s. %.3f$^\\circ$N, %.3f$^\\circ$W.  $\\delta^2\\mathrm{H} = %.4f$ \\nopagebreak \\\\ \n  \\includegraphics[width=\\textwidth]{%s/bird_%s.pdf}\\\\\n",
                            Short_Name, Region, Near_Town, lat, -long, Isotope.Value, plotpath, Short_Name)) %>%
  select(prstring)


cat(tmp$prstring, file = "supp/body_supp1.tex")


### Then, after this you go and LaTeX supp/supp1.tex  ###

