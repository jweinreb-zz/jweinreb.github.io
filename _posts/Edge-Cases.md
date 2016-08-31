---
layout: post
title:  "Edge Cases"
date:   2015-02-17 11:04:01
categories: post
---
Some edge cases and cautionary examples on using Markdown for writing content using this theme. In particular, list syntax can really knot things up.

Here is the same content as above, with a blank line separating the list items. Notice how the sidenotes get squashed into the main content area:


### Remarks on ADMM version 2 - one blank line between Markdown list items

Related algorithms

+ Split Bregman iteration {% sidenote 1 'Goldstein, T. and Osher, S. (2009). The split Bregman method for l1-regularized problems. SIAM J. Img. Sci., 2:323-343.' %}

+ Dykstra's alternating projection algorithm {% sidenote 2 'Dykstra, R. L. (1983). An algorithm for restricted least squares regression. J. Amer. Statist. Assoc., 78(384):837-842.' %}

+ Proximal point algorithm applied to the dual

+ Numerous applications in statistics and machine learning: lasso, gen. lasso, graphical lasso, (overlapping) group lasso, ...

### Liquid tag parsing strangeness

Example of the proper way to write an url inside a Liquid full-width image tag.

This code: ```{{ '{% fullwidth "assets/img/rhino.png" "Tufte pet rhino (via <a href=\"//www.edwardtufte.com/tufte/\">Edward Tufte</a>)" ' }} %}```

{% fullwidth "assets/img/rhino.png" "Tufte's pet rhino (via <a href=\"//www.edwardtufte.com/tufte/\">Edward Tufte</a>)" %}
