# File Upload Guide

## Uploading Shapefiles

Shapefiles consist of **multiple files** that work together. You must upload ALL required files:

### Required Files
- `.shp` - Main file containing geometry
- `.shx` - Shape index file
- `.dbf` - Attribute database file

### Optional (but recommended)
- `.prj` - Projection information
- `.cpg` - Character encoding

### How to Upload in the App

1. Click "Browse" on the file input
2. **Select all shapefile components at once** (hold Ctrl/Cmd to select multiple)
   - Example: Select `habitat.shp`, `habitat.shx`, `habitat.dbf`, `habitat.prj` together
3. Click "Open"

### Common Errors

**Error: "Cannot open shapefile; source corrupt"**
- **Cause:** Missing required files (.shx or .dbf)
- **Solution:** Make sure you selected ALL shapefile components when uploading

**Error: "Shapefile upload incomplete"**
- **Cause:** Only one file was uploaded
- **Solution:** Select all files together in the file browser

## Alternative Formats

If you have trouble with shapefiles, consider using:

- **GeoTIFF** (`.tif`, `.tiff`) - Single file, easier to upload
- **GeoJSON** (`.geojson`) - Single file, text-based vector format

You can convert shapefiles to these formats using:
- QGIS (free, open source)
- ArcGIS
- R: `terra::writeRaster()` or `sf::st_write()`

## Example: Converting Shapefile to GeoJSON in R

```r
library(sf)

# Read shapefile
habitat <- st_read("path/to/habitat.shp")

# Write as GeoJSON
st_write(habitat, "habitat.geojson")
```

## Using Example Data

If you don't upload files, the app will use built-in example data (Superb Fairy Wren habitat).
