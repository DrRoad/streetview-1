## ---- eval=TRUE, size='tiny', echo=FALSE, warning=FALSE------------------
library(data.table)
library(knitr)
dt <- data.table(address = '6975 E Sandown Rd, Denver, CO 80216',
                 copyright = '© 2017 Google',
                 date = '2016-09',
                 location.lat = '39.7...',
                 location.lng = '-104.9...',
                 pano_id = 'LDQK5dSu5...',
                 statius = 'OK'
                 )
kable(dt, table.attr = "style='width:100%;'")

## ---- eval=TRUE, cache=TRUE, fig.width=6, fig.height=6-------------------
library(streetview)
l.plots <- streetview::plot_panoids(path.root = '~/Dropbox/pkg.data')
l.plots$year

