Quick analysis of the #TidyTuesday [GDPR violation data](https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-04-21)

## Question 1: What's the single most expensive GDPR violation by country?

![changes across time](https://github.com/EvaMurzyn/TidyTuesdays/blob/master/2020-04-21-GDPR/Composite.png) <!-- .element height="40%" width="40%" -->

France's Google incident dwarfs everything else, so this was constructed by creating two graphs, one with all data points, and one with just fines < 2000 EUR, and pasting them together in MSPaint. Hey, it works!

## Question 2: What are the fines like across violation types?

I decided to use a table rather than a graph here, due to the disparities in the fine values, and the amount of information I could show in a table. This also helped me learn the **gt** package.

![changes across time](https://github.com/EvaMurzyn/TidyTuesdays/blob/master/2020-04-21-GDPR/table.png) <!-- .element height="40%" width="40%" -->
