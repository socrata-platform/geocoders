# geocoders

Interface for geocoding sequences of addresses. 

Includes:
* A geocoder that uses [MapQuest's API](https://developer.mapquest.com/products/geocoding)
* A geocoder that uses [Esri's ArcGIS REST API](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-geocode-addresses.htm)
  - This geocoder will use the same country for the Esri `sourceCountry` parameter for all addresses
  - There is a geocoder that batches addresses by country to be used in conjunction
* A geocoder that caches results
  - Supports Cassandra as a cache

## Releasing

Run `sbt-release` and set an appropriate version.
