# Endline halving

Reproducible example of how to subset max value from first half of endline.

- `haul_data.csv:` - CSV of load cell hauls including UID, max endline load, and max endline index.
- `lc_data.csv` - CSV of processed load cell data including UID, index, and load.
- `load_on_first_half_endline.R` - R script for processing first two CSVs.
- `hauls_half_el.csv` - `haul_data.csv` with the max load on the first half of the endline added as columns `first_half_el_load` and `first_half_el_index`

