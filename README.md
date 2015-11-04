# geocoders

Interface for geocoding sequences of addresses. 

Includes:
- A geocoder that uses [MapQuest's Api](https://developer.mapquest.com/products/geocoding)
- A geocoder that caches results
  - Supports Cassandra as a cache

## Releasing

Run `sbt-release` and set an appropriate version.
