# network_science_flights

## Initial EDA
- Times are read in as integers.  Need to be joined with the date to be used at date times.
- All values are 1 for Flights, confirming this dataset is for unique flights each day.
- Cancelled and diverted are so rare, we likely dont need them as columns
- Distance can be used for comparing to future GIS solutions or plots.  Could also be a good metric for each different airline or day to see how many miles were covered in total.
- WN is the most numerous airline, and it stands for Southwest.

## Some Questions to explore
- Look at flights by day, and maybe eventually by month.  What are the trends?  Are there differences across different airline or at different airports?
- Do different airlines operate more or less are different airports?
- Need geo coords for all the nodes in our data so I can plot them.
