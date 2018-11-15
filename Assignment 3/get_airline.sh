
# Write the uncompressed data to a single file.
# pbzip2 -dc 1987.csv.bz2 > airline.csv
for year in {1987..1989}
do
  pbzip2 -dc $year.csv.bz2 | sed -e "1d" >> airline.csv
done
