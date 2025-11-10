library(httr)

# Define URLs
urls <- c(
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-2/2025-11-04/f3f041c13adaad09a8373c1f446e87e8.grib",
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-1/2025-11-04/249dff367c1100e985020e5304f9f529.grib",
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-2/2025-11-04/f9da5a364080c86a5f4bfce5dd61418b.grib",
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-3/2025-11-05/9f604b2d47ada2de44057b40d1161f79.grib"
)

# Function to extract filename from URL
get_filename <- function(url) {
    basename(url)
}

# Download each file if it doesn't exist
for (url in urls) {
    filename <- get_filename(url)
    filename <- file.path(getwd(), "CERRA", filename)

    if (!file.exists(filename)) {
        message("Downloading ", filename, "...")
        response <- GET(url, write_disk(filename, overwrite = TRUE))
        stop_for_status(response)
        message("Downloaded ", filename)
    } else {
        message("File ", filename, " already exists. Skipping download.")
    }
}
